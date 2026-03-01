use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use clap::Parser;
use convert_case::{Case, Casing};
use handlebars::{Context as HbContext, Handlebars, Helper, Output, RenderContext};
use serde_json::Value;
use walkdir::WalkDir;

#[derive(Parser)]
pub struct Args {
    /// Path to the TII JSON file
    #[arg(long)]
    pub tii: PathBuf,

    /// Path to the templates directory
    #[arg(long)]
    pub templates: PathBuf,

    /// Output directory for rendered templates
    #[arg(short, long)]
    pub output: PathBuf,
}

fn make_helper<F>(name: &'static str, f: F) -> impl handlebars::HelperDef + Send + Sync + 'static
where
    F: Fn(&str) -> String + Send + Sync + 'static,
{
    move |h: &Helper, _: &Handlebars, _: &HbContext, _: &mut RenderContext, out: &mut dyn Output| {
        let param = h
            .param(0)
            .ok_or_else(|| handlebars::RenderErrorReason::ParamNotFoundForIndex(name, 0))?;
        let input = param
            .value()
            .as_str()
            .ok_or_else(|| handlebars::RenderErrorReason::InvalidParamType("Expected a string"))?;
        out.write(&f(input))?;
        Ok(())
    }
}

fn schema_type_for(schema: &Value, language: &str) -> String {
    if let Some(schema_map) = schema.as_object() {
        if let Some(reference) = schema_map.get("$ref").and_then(|r| r.as_str()) {
            let type_name = reference.rsplit('#').next().unwrap_or(reference);
            return map_ref_type(type_name, language);
        }

        if let Some(schema_type) = schema_map.get("type").and_then(|t| t.as_str()) {
            return map_schema_type(schema_type, schema_map, language);
        }
    }

    default_json_type(language)
}

fn map_ref_type(type_name: &str, language: &str) -> String {
    match language {
        "rust" => match type_name {
            "Bytes" => "Vec<u8>".to_string(),
            "Address" => "Address".to_string(),
            "UtxoRef" => "UtxoRef".to_string(),
            "AnyAsset" => "String".to_string(),
            _ => default_json_type(language),
        },
        "typescript" => match type_name {
            "Bytes" => "Uint8Array".to_string(),
            "Address" => "string".to_string(),
            "UtxoRef" => "string".to_string(),
            "AnyAsset" => "string".to_string(),
            _ => default_json_type(language),
        },
        "python" => match type_name {
            "Bytes" => "bytes".to_string(),
            "Address" => "str".to_string(),
            "UtxoRef" => "str".to_string(),
            "AnyAsset" => "str".to_string(),
            _ => default_json_type(language),
        },
        "go" => match type_name {
            "Bytes" => "[]byte".to_string(),
            "Address" => "string".to_string(),
            "UtxoRef" => "string".to_string(),
            "AnyAsset" => "string".to_string(),
            _ => default_json_type(language),
        },
        _ => default_json_type(language),
    }
}

fn map_schema_type(
    schema_type: &str,
    schema: &serde_json::Map<String, Value>,
    language: &str,
) -> String {
    match (schema_type, language) {
        ("integer", "rust") => "i64".to_string(),
        ("integer", "typescript") => "number".to_string(),
        ("integer", "python") => "int".to_string(),
        ("integer", "go") => "int64".to_string(),
        ("boolean", "rust") => "bool".to_string(),
        ("boolean", "typescript") => "boolean".to_string(),
        ("boolean", "python") => "bool".to_string(),
        ("boolean", "go") => "bool".to_string(),
        ("string", "rust") => "String".to_string(),
        ("string", "typescript") => "string".to_string(),
        ("string", "python") => "str".to_string(),
        ("string", "go") => "string".to_string(),
        ("null", "rust") => "()".to_string(),
        ("null", "typescript") => "null".to_string(),
        ("null", "python") => "None".to_string(),
        ("null", "go") => "interface{}".to_string(),
        ("array", _) => map_array_type(schema, language),
        ("object", _) => map_object_type(schema, language),
        _ => default_json_type(language),
    }
}

fn map_array_type(schema: &serde_json::Map<String, Value>, language: &str) -> String {
    let item_type = schema
        .get("items")
        .map(|items| schema_type_for(items, language))
        .unwrap_or_else(|| default_json_type(language));

    match language {
        "rust" => format!("Vec<{item_type}>"),
        "typescript" => format!("Array<{item_type}>"),
        "python" => format!("list[{item_type}]"),
        "go" => format!("[]{item_type}"),
        _ => default_json_type(language),
    }
}

fn map_object_type(schema: &serde_json::Map<String, Value>, language: &str) -> String {
    let value_type = schema
        .get("additionalProperties")
        .map(|value| schema_type_for(value, language));

    match (language, value_type) {
        ("rust", Some(value_type)) => format!("std::collections::HashMap<String, {value_type}>"),
        ("typescript", Some(value_type)) => format!("Record<string, {value_type}>"),
        ("python", Some(value_type)) => format!("dict[str, {value_type}]"),
        ("go", Some(value_type)) => format!("map[string]{value_type}"),
        _ => default_json_type(language),
    }
}

fn default_json_type(language: &str) -> String {
    match language {
        "rust" => "serde_json::Value".to_string(),
        "typescript" => "any".to_string(),
        "python" => "Any".to_string(),
        "go" => "interface{}".to_string(),
        _ => "any".to_string(),
    }
}

fn register_helpers(handlebars: &mut Handlebars<'_>) {
    #[allow(clippy::type_complexity)]
    let helpers: &[(&str, fn(&str) -> String)] = &[
        ("pascalCase", |s| s.to_case(Case::Pascal)),
        ("camelCase", |s| s.to_case(Case::Camel)),
        ("constantCase", |s| s.to_case(Case::Constant)),
        ("snakeCase", |s| s.to_case(Case::Snake)),
        ("lowerCase", |s| s.to_case(Case::Lower)),
    ];

    for (name, func) in helpers {
        handlebars.register_helper(name, Box::new(make_helper(name, func)));
    }

    handlebars.register_helper(
        "schemaTypeFor",
        Box::new(
            |h: &Helper,
             _: &Handlebars,
             _: &HbContext,
             _: &mut RenderContext,
             out: &mut dyn Output| {
                let schema_param = h.param(0).ok_or_else(|| {
                    handlebars::RenderErrorReason::ParamNotFoundForIndex("schemaTypeFor", 0)
                })?;
                let lang_param = h.param(1).ok_or_else(|| {
                    handlebars::RenderErrorReason::ParamNotFoundForIndex("schemaTypeFor", 1)
                })?;

                let language = lang_param.value().as_str().ok_or_else(|| {
                    handlebars::RenderErrorReason::InvalidParamType("Expected language as string")
                })?;

                let output_type = schema_type_for(schema_param.value(), language);
                out.write(&output_type)?;
                Ok(())
            },
        ),
    );
}

fn register_templates(
    handlebars: &mut Handlebars<'_>,
    templates_dir: &Path,
) -> Result<Vec<(PathBuf, PathBuf)>> {
    let mut static_files = Vec::new();

    for entry in WalkDir::new(templates_dir) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }

        let path = entry.path();
        let relative = path.strip_prefix(templates_dir).context("template path")?;
        let relative_str = relative.to_string_lossy();

        if relative_str.ends_with(".hbs") {
            let template_name = relative_str.trim_end_matches(".hbs");
            let content = std::fs::read_to_string(path)?;
            handlebars
                .register_template_string(template_name, content)
                .with_context(|| format!("registering template {template_name}"))?;
        } else {
            static_files.push((path.to_path_buf(), relative.to_path_buf()));
        }
    }

    Ok(static_files)
}

fn render_templates(handlebars: &Handlebars<'_>, data: &Value, output_dir: &Path) -> Result<()> {
    for (name, _) in handlebars.get_templates() {
        let rendered = handlebars
            .render(name, data)
            .with_context(|| format!("rendering template {name}"))?;
        if rendered.is_empty() {
            continue;
        }

        let output_path = output_dir.join(name);
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&output_path, rendered)?;
    }

    Ok(())
}

fn copy_static_files(static_files: &[(PathBuf, PathBuf)], output_dir: &Path) -> Result<()> {
    for (src, relative) in static_files {
        let dest_path = output_dir.join(relative);
        if let Some(parent) = dest_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::copy(src, dest_path)?;
    }

    Ok(())
}

pub fn run(args: Args) -> Result<()> {
    let tii_contents = std::fs::read_to_string(&args.tii)
        .with_context(|| format!("reading TII file {}", args.tii.display()))?;
    let tii: Value = serde_json::from_str(&tii_contents)
        .with_context(|| format!("parsing TII file {}", args.tii.display()))?;

    let mut handlebars = Handlebars::new();
    register_helpers(&mut handlebars);

    let static_files = register_templates(&mut handlebars, &args.templates)?;

    std::fs::create_dir_all(&args.output)
        .with_context(|| format!("creating output dir {}", args.output.display()))?;

    let data = serde_json::json!({
        "tii": tii,
    });

    render_templates(&handlebars, &data, &args.output)?;
    copy_static_files(&static_files, &args.output)?;

    println!(
        "Generated code from {} into {}",
        args.tii.display(),
        args.output.display()
    );

    Ok(())
}
