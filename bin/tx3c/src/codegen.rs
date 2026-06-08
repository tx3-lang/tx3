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

    /// Path to the template directory
    #[arg(long)]
    pub template: PathBuf,

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
            return map_ref_type(extract_ref_name(reference), language);
        }

        if let Some(schema_type) = schema_map.get("type").and_then(|t| t.as_str()) {
            return map_schema_type(schema_type, schema_map, language);
        }
    }

    default_json_type(language)
}

/// Extracts the bare type name from a `$ref`, handling both the builtin form
/// (`…/tii#/$defs/Bytes`) and the custom-type form (`#/components/schemas/Foo`):
/// take the fragment after `#`, then its last path segment.
fn extract_ref_name(reference: &str) -> &str {
    let fragment = reference.rsplit('#').next().unwrap_or(reference);
    fragment.rsplit('/').next().unwrap_or(fragment)
}

/// Maps a referenced type name to a language type. Builtins map to native
/// types; any other name is a user-defined type from `components.schemas` and
/// maps to its generated name (PascalCase).
fn map_ref_type(type_name: &str, language: &str) -> String {
    let builtin = match language {
        "rust" => match type_name {
            "Bytes" => Some("Vec<u8>"),
            "Address" => Some("Address"),
            "UtxoRef" => Some("UtxoRef"),
            "AnyAsset" => Some("String"),
            "Utxo" => Some("serde_json::Value"),
            _ => None,
        },
        "typescript" => match type_name {
            "Bytes" => Some("Uint8Array"),
            "Address" | "UtxoRef" | "AnyAsset" => Some("string"),
            "Utxo" => Some("unknown"),
            _ => None,
        },
        "python" => match type_name {
            "Bytes" => Some("bytes"),
            "Address" | "UtxoRef" | "AnyAsset" => Some("str"),
            "Utxo" => Some("Any"),
            _ => None,
        },
        "go" => match type_name {
            "Bytes" => Some("[]byte"),
            "Address" | "UtxoRef" | "AnyAsset" => Some("string"),
            "Utxo" => Some("interface{}"),
            _ => None,
        },
        _ => None,
    };

    match builtin {
        Some(ty) => ty.to_string(),
        // Not a builtin: a user-defined type, referenced by its generated name.
        None => type_name.to_case(Case::Pascal),
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

/// Renders the named type declarations for every entry in `components.schemas`.
///
/// Records (single-case types) get a full named struct / interface / dataclass.
/// Variants (`oneOf`) get a named but permissive alias with a TODO: tagged-union
/// codegen is deferred until the SDK can encode variant arg values (the resolver
/// side of `ParamType.custom` is not implemented yet), so emitting elaborate
/// union types the SDK cannot serialize would be premature.
fn render_component_types(schemas: &Value, language: &str) -> String {
    let Some(map) = schemas.as_object() else {
        return String::new();
    };

    let mut names: Vec<&String> = map.keys().collect();
    names.sort();

    let mut out = String::new();
    for name in names {
        let schema = &map[name];
        let decl = if schema.get("oneOf").is_some() {
            render_variant_alias(name, language)
        } else {
            render_record_type(name, schema, language)
        };
        out.push_str(&decl);
        out.push('\n');
    }
    out
}

/// Collects a record's `(original field name, language type)` pairs, in the
/// declared `properties` order.
fn record_fields(schema: &Value, language: &str) -> Vec<(String, String)> {
    schema
        .get("properties")
        .and_then(|p| p.as_object())
        .map(|props| {
            props
                .iter()
                .map(|(key, value)| (key.clone(), schema_type_for(value, language)))
                .collect()
        })
        .unwrap_or_default()
}

fn render_record_type(name: &str, schema: &Value, language: &str) -> String {
    let type_name = name.to_case(Case::Pascal);
    let fields = record_fields(schema, language);

    match language {
        "rust" => {
            let mut body = String::new();
            for (field, ty) in &fields {
                let snake = field.to_case(Case::Snake);
                if &snake != field {
                    body.push_str(&format!("    #[serde(rename = \"{field}\")]\n"));
                }
                body.push_str(&format!("    pub {snake}: {ty},\n"));
            }
            format!("#[derive(Debug, Clone, Serialize)]\npub struct {type_name} {{\n{body}}}\n")
        }
        "typescript" => {
            let mut body = String::new();
            for (field, ty) in &fields {
                body.push_str(&format!("    {field}: {ty};\n"));
            }
            format!("export type {type_name} = {{\n{body}}};\n")
        }
        "python" => {
            let mut body = String::new();
            for (field, ty) in &fields {
                body.push_str(&format!("    {}: {ty}\n", field.to_case(Case::Snake)));
            }
            if body.is_empty() {
                body.push_str("    pass\n");
            }
            format!("@dataclass\nclass {type_name}:\n{body}")
        }
        "go" => {
            let mut body = String::new();
            for (field, ty) in &fields {
                let pascal = field.to_case(Case::Pascal);
                body.push_str(&format!("\t{pascal} {ty} `json:\"{field}\"`\n"));
            }
            format!("type {type_name} struct {{\n{body}}}\n")
        }
        _ => String::new(),
    }
}

fn render_variant_alias(name: &str, language: &str) -> String {
    let type_name = name.to_case(Case::Pascal);
    let todo = "TODO: tagged-union codegen pending the variant arg encoder";
    match language {
        "rust" => format!("// {todo}\npub type {type_name} = serde_json::Value;\n"),
        "typescript" => format!("// {todo}\nexport type {type_name} = unknown;\n"),
        "python" => format!("# {todo}\n{type_name} = Any\n"),
        "go" => format!("// {todo}\ntype {type_name} = interface{{}}\n"),
        _ => String::new(),
    }
}

fn register_helpers(handlebars: &mut Handlebars<'_>) {
    #[allow(clippy::type_complexity)]
    let helpers: &[(&str, fn(&str) -> String)] = &[
        ("pascalCase", |s| s.to_case(Case::Pascal)),
        ("camelCase", |s| s.to_case(Case::Camel)),
        ("constantCase", |s| s.to_case(Case::UpperSnake)),
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

    handlebars.register_helper(
        "componentTypes",
        Box::new(
            |h: &Helper,
             _: &Handlebars,
             _: &HbContext,
             _: &mut RenderContext,
             out: &mut dyn Output| {
                // param(0) is `tii.components.schemas`, which is absent when the
                // protocol declares no custom types — render nothing in that case.
                let schemas = h.param(0).map(|p| p.value()).unwrap_or(&Value::Null);
                let lang_param = h.param(1).ok_or_else(|| {
                    handlebars::RenderErrorReason::ParamNotFoundForIndex("componentTypes", 1)
                })?;
                let language = lang_param.value().as_str().ok_or_else(|| {
                    handlebars::RenderErrorReason::InvalidParamType("Expected language as string")
                })?;

                out.write(&render_component_types(schemas, language))?;
                Ok(())
            },
        ),
    );

    handlebars.register_helper(
        "json",
        Box::new(
            |h: &Helper,
             _: &Handlebars,
             _: &HbContext,
             _: &mut RenderContext,
             out: &mut dyn Output| {
                let param = h.param(0).ok_or_else(|| {
                    handlebars::RenderErrorReason::ParamNotFoundForIndex("json", 0)
                })?;
                let rendered = serde_json::to_string(param.value())
                    .expect("serializing a parsed JSON value cannot fail");
                out.write(&rendered)?;
                Ok(())
            },
        ),
    );
}

fn register_templates(
    handlebars: &mut Handlebars<'_>,
    template_dir: &Path,
) -> Result<Vec<(PathBuf, PathBuf)>> {
    let mut static_files = Vec::new();

    for entry in WalkDir::new(template_dir) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }

        let path = entry.path();
        let relative = path.strip_prefix(template_dir).context("template path")?;
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

    let static_files = register_templates(&mut handlebars, &args.template)?;

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
