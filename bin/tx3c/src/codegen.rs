use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

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
        "java" => match type_name {
            "Bytes" => Some("byte[]"),
            "Address" => Some("land.tx3.sdk.Address"),
            "UtxoRef" => Some("land.tx3.sdk.UtxoRef"),
            "AnyAsset" | "Utxo" => Some("land.tx3.sdk.ArgValue"),
            _ => None,
        },
        _ => None,
    };

    match builtin {
        Some(ty) => ty.to_string(),
        // Not a builtin: a user-defined type, referenced by its generated name.
        None if language == "java" => java_identifier(type_name, Case::Pascal),
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
        ("integer", "java") => "java.math.BigInteger".to_string(),
        ("boolean", "rust") => "bool".to_string(),
        ("boolean", "typescript") => "boolean".to_string(),
        ("boolean", "python") => "bool".to_string(),
        ("boolean", "go") => "bool".to_string(),
        ("boolean", "java") => "Boolean".to_string(),
        ("string", "rust") => "String".to_string(),
        ("string", "typescript") => "string".to_string(),
        ("string", "python") => "str".to_string(),
        ("string", "go") => "string".to_string(),
        ("string", "java") => "String".to_string(),
        ("null", "rust") => "()".to_string(),
        ("null", "typescript") => "null".to_string(),
        ("null", "python") => "None".to_string(),
        ("null", "go") => "interface{}".to_string(),
        ("null", "java") => "land.tx3.sdk.ArgValue".to_string(),
        ("array", _) => map_array_type(schema, language),
        ("object", _) => map_object_type(schema, language),
        _ => default_json_type(language),
    }
}

fn map_array_type(schema: &serde_json::Map<String, Value>, language: &str) -> String {
    // Java has no native tuple type. A tuple gets its fixed, typed surface from
    // `render_java_tuple` when it has a declaration name; an anonymous tuple is
    // deliberately left on the SDK's safe fallback.
    if language == "java"
        && schema
            .get("prefixItems")
            .and_then(Value::as_array)
            .is_some()
    {
        return default_json_type(language);
    }

    let item_type = schema
        .get("items")
        .map(|items| schema_type_for(items, language))
        .unwrap_or_else(|| default_json_type(language));

    match language {
        "rust" => format!("Vec<{item_type}>"),
        "typescript" => format!("Array<{item_type}>"),
        "python" => format!("list[{item_type}]"),
        "go" => format!("[]{item_type}"),
        "java" => format!("java.util.List<{item_type}>"),
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
        ("java", Some(value_type)) => format!("java.util.Map<String, {value_type}>"),
        _ => default_json_type(language),
    }
}

fn default_json_type(language: &str) -> String {
    match language {
        "rust" => "serde_json::Value".to_string(),
        "typescript" => "any".to_string(),
        "python" => "Any".to_string(),
        "go" => "interface{}".to_string(),
        "java" => "land.tx3.sdk.ArgValue".to_string(),
        _ => "any".to_string(),
    }
}

const JAVA_KEYWORDS: &[&str] = &[
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extends",
    "final",
    "finally",
    "float",
    "for",
    "goto",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "native",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "strictfp",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "try",
    "void",
    "volatile",
    "while",
    // Reserved literals and restricted identifiers cannot be used as names in
    // the generated positions either.
    "true",
    "false",
    "null",
    "_",
    "exports",
    "module",
    "non-sealed",
    "open",
    "opens",
    "permits",
    "provides",
    "record",
    "requires",
    "sealed",
    "to",
    "transitive",
    "uses",
    "var",
    "when",
    "with",
    "yield",
];

fn java_identifier(source: &str, case: Case) -> String {
    let mut normalized = source.to_case(case);
    if normalized.is_empty() {
        normalized.push('_');
    }
    if normalized
        .chars()
        .next()
        .is_some_and(|first| !first.is_alphabetic() && first != '_' && first != '$')
    {
        normalized.insert(0, '_');
    }
    if JAVA_KEYWORDS.contains(&normalized.as_str()) {
        normalized.push('_');
    }
    normalized
}

fn java_names<'a, I>(sources: I, case: Case, scope: &str) -> Result<BTreeMap<&'a str, String>>
where
    I: IntoIterator<Item = &'a str>,
{
    let mut sources: Vec<&str> = sources.into_iter().collect();
    sources.sort_unstable();

    let mut by_normalized = BTreeMap::new();
    let mut result = BTreeMap::new();
    for source in sources {
        let normalized = java_identifier(source, case);
        if let Some(previous) = by_normalized.insert(normalized.clone(), source) {
            anyhow::bail!(
                "Java identifier collision in {scope}: source identifiers `{previous}` and `{source}` both normalize to `{normalized}`"
            );
        }
        result.insert(source, normalized);
    }

    Ok(result)
}

/// Renders the named type declarations for every entry in `components.schemas`.
///
/// Java records, tuples, and variants get their approved native declarations.
/// In the existing languages, records get a full named struct / interface /
/// dataclass while variants (`oneOf`) retain the permissive alias used before
/// Java support. This keeps their generated output unchanged.
fn render_component_types(schemas: &Value, language: &str) -> Result<String> {
    let Some(map) = schemas.as_object() else {
        return Ok(String::new());
    };

    let mut names: Vec<&String> = map.keys().collect();
    names.sort();

    let java_type_names = if language == "java" {
        Some(java_names(
            names.iter().map(|name| name.as_str()),
            Case::Pascal,
            "components.schemas",
        )?)
    } else {
        None
    };

    let mut out = String::new();
    for name in names {
        let schema = &map[name];
        let decl = if let Some(java_type_names) = &java_type_names {
            render_java_declaration(
                java_type_names
                    .get(name.as_str())
                    .expect("component name was normalized"),
                schema,
                0,
            )?
        } else if schema.get("oneOf").is_some() {
            render_variant_alias(name, language)
        } else {
            render_record_type(name, schema, language)
        };
        out.push_str(&decl);
        out.push('\n');
    }
    Ok(out)
}

fn ordered_properties(schema: &Value) -> Vec<(&str, &Value)> {
    let Some(properties) = schema.get("properties").and_then(Value::as_object) else {
        return Vec::new();
    };

    let mut ordered = Vec::new();
    if let Some(required) = schema.get("required").and_then(Value::as_array) {
        for name in required.iter().filter_map(Value::as_str) {
            if let Some(value) = properties.get(name) {
                ordered.push((name, value));
            }
        }
    }

    let mut remaining: Vec<_> = properties
        .iter()
        .filter(|(name, _)| !ordered.iter().any(|(ordered_name, _)| ordered_name == name))
        .map(|(name, value)| (name.as_str(), value))
        .collect();
    remaining.sort_by_key(|(name, _)| *name);
    ordered.extend(remaining);
    ordered
}

fn java_named_type(schema: &Value, suggested_name: &str) -> String {
    if schema.get("oneOf").is_some()
        || schema
            .get("prefixItems")
            .and_then(Value::as_array)
            .is_some()
        || (schema.get("type").and_then(Value::as_str) == Some("object")
            && schema
                .get("properties")
                .and_then(Value::as_object)
                .is_some())
    {
        suggested_name.to_string()
    } else {
        schema_type_for(schema, "java")
    }
}

fn java_nested_declarations(
    owner_name: &str,
    fields: &[(&str, &Value)],
    indent: usize,
) -> Result<String> {
    let mut out = String::new();
    for (source_name, schema) in fields {
        let nested_name = format!("{owner_name}{}", java_identifier(source_name, Case::Pascal));
        if java_named_type(schema, &nested_name) == nested_name {
            out.push('\n');
            out.push_str(&render_java_declaration(&nested_name, schema, indent)?);
        }
    }
    Ok(out)
}

fn render_java_record(
    name: &str,
    schema: &Value,
    implementation: Option<&str>,
    indent: usize,
) -> Result<String> {
    let fields = ordered_properties(schema);
    let field_names = java_names(
        fields.iter().map(|(field, _)| *field),
        Case::Camel,
        &format!("record {name}"),
    )?;
    let padding = " ".repeat(indent);
    let components = fields
        .iter()
        .map(|(source_name, field_schema)| {
            let nested_name = format!("{name}{}", java_identifier(source_name, Case::Pascal));
            format!(
                "{} {}",
                java_named_type(field_schema, &nested_name),
                field_names
                    .get(source_name)
                    .expect("record field name was normalized")
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    let implements = implementation
        .map(|interface| format!(" implements {interface}"))
        .unwrap_or_default();
    let nested = java_nested_declarations(name, &fields, indent + 4)?;

    if nested.is_empty() {
        Ok(format!(
            "{padding}record {name}({components}){implements} {{}}\n"
        ))
    } else {
        Ok(format!(
            "{padding}record {name}({components}){implements} {{{nested}{padding}}}\n"
        ))
    }
}

fn render_java_tuple(name: &str, schema: &Value, indent: usize) -> Result<String> {
    let items = schema
        .get("prefixItems")
        .and_then(Value::as_array)
        .map(Vec::as_slice)
        .unwrap_or_default();
    let padding = " ".repeat(indent);
    let components = items
        .iter()
        .enumerate()
        .map(|(index, item)| {
            let nested_name = format!("{name}Item{index}");
            format!("{} item{index}", java_named_type(item, &nested_name))
        })
        .collect::<Vec<_>>()
        .join(", ");

    let mut nested = String::new();
    for (index, item) in items.iter().enumerate() {
        let nested_name = format!("{name}Item{index}");
        if java_named_type(item, &nested_name) == nested_name {
            nested.push('\n');
            nested.push_str(&render_java_declaration(&nested_name, item, indent + 4)?);
        }
    }

    if nested.is_empty() {
        Ok(format!("{padding}record {name}({components}) {{}}\n"))
    } else {
        Ok(format!(
            "{padding}record {name}({components}) {{{nested}{padding}}}\n"
        ))
    }
}

fn variant_cases<'a>(name: &str, schema: &'a Value) -> Result<Vec<(&'a str, &'a Value)>> {
    let Some(cases) = schema.get("oneOf").and_then(Value::as_array) else {
        anyhow::bail!("Java variant `{name}` is missing a oneOf array");
    };

    let mut result = Vec::with_capacity(cases.len());
    for (index, case) in cases.iter().enumerate() {
        let required = case
            .get("required")
            .and_then(Value::as_array)
            .and_then(|values| (values.len() == 1).then_some(values))
            .and_then(|values| values[0].as_str())
            .with_context(|| {
                format!("Java variant `{name}` case {index} must name exactly one required tag")
            })?;
        let payload = case
            .get("properties")
            .and_then(Value::as_object)
            .and_then(|properties| properties.get(required))
            .with_context(|| {
                format!("Java variant `{name}` case `{required}` is missing its payload schema")
            })?;
        result.push((required, payload));
    }
    Ok(result)
}

fn render_java_variant(name: &str, schema: &Value, indent: usize) -> Result<String> {
    let cases = variant_cases(name, schema)?;
    let case_names = java_names(
        cases.iter().map(|(case, _)| *case),
        Case::Pascal,
        &format!("variant {name}"),
    )?;
    let padding = " ".repeat(indent);
    let permits = cases
        .iter()
        .map(|(case, _)| format!("{name}.{}", case_names[case]))
        .collect::<Vec<_>>()
        .join(", ");
    let mut out = format!("{padding}sealed interface {name} permits {permits} {{\n");
    for (case, payload) in cases {
        let case_name = &case_names[case];
        out.push_str(&render_java_record(
            case_name,
            payload,
            Some(name),
            indent + 4,
        )?);
    }
    out.push_str(&format!("{padding}}}\n"));
    Ok(out)
}

fn render_java_declaration(name: &str, schema: &Value, indent: usize) -> Result<String> {
    if schema.get("oneOf").is_some() {
        render_java_variant(name, schema, indent)
    } else if schema
        .get("prefixItems")
        .and_then(Value::as_array)
        .is_some()
    {
        render_java_tuple(name, schema, indent)
    } else {
        render_java_record(name, schema, None, indent)
    }
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
        ("javaPascalCase", |s| java_identifier(s, Case::Pascal)),
        ("javaCamelCase", |s| java_identifier(s, Case::Camel)),
        ("javaConstantCase", |s| java_identifier(s, Case::UpperSnake)),
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

                let declarations = render_component_types(schemas, language)
                    .map_err(|error| handlebars::RenderErrorReason::Other(error.to_string()))?;
                out.write(&declarations)?;
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
    for name in handlebars.get_templates().keys() {
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn java_type(schema: Value) -> String {
        schema_type_for(&schema, "java")
    }

    #[test]
    fn java_schema_types_cover_approved_native_mappings() {
        assert_eq!(java_type(json!({ "type": "boolean" })), "Boolean");
        assert_eq!(
            java_type(json!({ "type": "integer" })),
            "java.math.BigInteger"
        );
        assert_eq!(java_type(json!({ "type": "string" })), "String");
        assert_eq!(
            java_type(json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/Bytes" })),
            "byte[]"
        );
        assert_eq!(
            java_type(json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/Address" })),
            "land.tx3.sdk.Address"
        );
        assert_eq!(
            java_type(json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/UtxoRef" })),
            "land.tx3.sdk.UtxoRef"
        );
        assert_eq!(
            java_type(json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/Utxo" })),
            "land.tx3.sdk.ArgValue"
        );
        assert_eq!(
            java_type(json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/AnyAsset" })),
            "land.tx3.sdk.ArgValue"
        );
        assert_eq!(
            java_type(json!({ "type": "array", "items": { "type": "integer" } })),
            "java.util.List<java.math.BigInteger>"
        );
        assert_eq!(
            java_type(json!({
                "type": "object",
                "additionalProperties": { "type": "boolean" }
            })),
            "java.util.Map<String, Boolean>"
        );
        assert_eq!(
            java_type(json!({ "$ref": "#/components/schemas/payment datum" })),
            "PaymentDatum"
        );
        assert_eq!(
            java_type(json!({ "future": true })),
            "land.tx3.sdk.ArgValue"
        );
    }

    #[test]
    fn java_component_declarations_match_the_canonical_fixture() {
        let fixture: Value =
            serde_json::from_str(include_str!("../tests/fixtures/java/complex.tii")).unwrap();
        let rendered = render_component_types(&fixture["components"]["schemas"], "java").unwrap();

        assert_eq!(
            rendered,
            concat!(
                "record AssetClass(byte[] policy, byte[] name) {}\n",
                "\n",
                "sealed interface Side permits Side.Buy, Side.Sell {\n",
                "    record Buy() implements Side {}\n",
                "    record Sell(java.math.BigInteger price) implements Side {}\n",
                "}\n",
                "\n",
            )
        );
    }

    #[test]
    fn java_declarations_cover_tuples_reserved_names_and_nesting() {
        let schema = json!({
            "type": "object",
            "properties": {
                "class": { "type": "boolean" },
                "pair": {
                    "type": "array",
                    "prefixItems": [
                        { "type": "integer" },
                        {
                            "type": "object",
                            "properties": { "record": { "type": "string" } },
                            "required": ["record"]
                        }
                    ],
                    "items": false
                }
            },
            "required": ["class", "pair"]
        });

        assert_eq!(
            render_java_declaration("Envelope", &schema, 0).unwrap(),
            concat!(
                "record Envelope(Boolean class_, EnvelopePair pair) {\n",
                "    record EnvelopePair(java.math.BigInteger item0, EnvelopePairItem1 item1) {\n",
                "        record EnvelopePairItem1(String record_) {}\n",
                "    }\n",
                "}\n",
            )
        );
    }

    #[test]
    fn java_identifier_collisions_name_both_sources() {
        let schemas = json!({
            "payment-value": { "type": "object", "properties": {} },
            "payment_value": { "type": "object", "properties": {} }
        });

        let error = render_component_types(&schemas, "java").unwrap_err();
        assert_eq!(
            error.to_string(),
            "Java identifier collision in components.schemas: source identifiers `payment-value` and `payment_value` both normalize to `PaymentValue`"
        );
    }

    #[test]
    fn java_naming_helpers_apply_case_and_keyword_escaping() {
        let mut handlebars = Handlebars::new();
        register_helpers(&mut handlebars);
        handlebars
            .register_template_string(
                "names",
                "{{javaPascalCase type}} {{javaCamelCase method}} {{javaConstantCase constant}}",
            )
            .unwrap();

        assert_eq!(
            handlebars
                .render(
                    "names",
                    &json!({ "type": "payment datum", "method": "class", "constant": "api url" })
                )
                .unwrap(),
            "PaymentDatum class_ API_URL"
        );
    }
}
