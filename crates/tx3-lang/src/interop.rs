//! Resolves plutus.json (CIP-57) imports and maps blueprint definitions to tx3 types.

use std::collections::HashSet;
use std::path::Path;

use cip_57::{Blueprint, DataType, Definition, Definitions, Field, ReferencesArray, Schema};

use crate::ast::{Identifier, Program, RecordField, Span, Type, TypeDef, VariantCase};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("cannot resolve imports without a root path (use Workspace::from_file instead of from_string)")]
    #[diagnostic(code(tx3::interop::missing_root))]
    MissingRoot,

    #[error("I/O error reading import file: {0}")]
    #[diagnostic(code(tx3::interop::io))]
    Io(#[from] std::io::Error),

    #[error("invalid JSON in plutus file: {0}")]
    #[diagnostic(code(tx3::interop::json))]
    Json(#[from] serde_json::Error),

    #[error("duplicate type name: {name}")]
    #[diagnostic(code(tx3::interop::duplicate_type))]
    DuplicateType {
        name: String,

        #[source_code]
        src: Option<String>,

        #[label("type already defined here or from another import")]
        span: Span,
    },

    #[error("plutus schema error: {message}")]
    #[diagnostic(code(tx3::interop::schema))]
    Schema { message: String },
}

impl Error {
    fn duplicate_type(name: String, span: Span) -> Self {
        Error::DuplicateType {
            name,
            src: None,
            span,
        }
    }
}

/// Resolves `#/definitions/cardano~1address~1Address` to definition key `cardano/address/Address`.
/// JSON pointer uses ~1 for / and ~0 for ~.
fn ref_to_key_owned(r: &str) -> String {
    let prefix = "#/definitions/";
    let s = r
        .strip_prefix(prefix)
        .unwrap_or(r)
        .replace("~1", "/")
        .replace("~0", "~");
    s
}

/// Normalize definition key to a single identifier: replace `/` and `$` with `_`.
fn key_to_normalized_name(key: &str) -> String {
    key.replace('/', "_").replace('$', "_")
}

/// Full type name for an imported definition: with alias use `Alias_NormalizedKey`, else `NormalizedKey`.
fn import_type_name(key: &str, alias: Option<&str>) -> String {
    let base = key_to_normalized_name(key);
    match alias {
        Some(a) => format!("{}_{}", a, base),
        None => base,
    }
}

/// Resolve a $ref to a tx3 Type using definitions and the current import alias.
fn resolve_ref_to_type(
    ref_str: &str,
    definitions: &Definitions,
    alias: Option<&str>,
) -> Result<Type, Error> {
    let key = ref_to_key_owned(ref_str);
    let def = definitions
        .inner
        .get(&key)
        .ok_or_else(|| Error::Schema {
            message: format!("definition not found: {}", key),
        })?;

    if let Some(dt) = &def.data_type {
        match dt {
            DataType::Integer => return Ok(Type::Int),
            DataType::Bytes => return Ok(Type::Bytes),
            DataType::List => {
                let inner = match &def.items {
                    Some(ReferencesArray::Single(r)) => {
                        r.reference.as_ref().map(|s| resolve_ref_to_type(s, definitions, alias))
                    }
                    Some(ReferencesArray::Array(arr)) => arr.first().and_then(|r| {
                        r.reference.as_ref().map(|s| resolve_ref_to_type(s, definitions, alias))
                    }),
                    None => None,
                };
                let inner = inner.ok_or_else(|| Error::Schema {
                    message: "list without items".to_string(),
                })?;
                return Ok(Type::List(Box::new(inner?)));
            }
            DataType::Map => {
                let k = def
                    .keys
                    .as_ref()
                    .and_then(|r| r.reference.as_ref())
                    .ok_or_else(|| Error::Schema {
                        message: "map without keys".to_string(),
                    })?;
                let v = def
                    .values
                    .as_ref()
                    .and_then(|r| r.reference.as_ref())
                    .ok_or_else(|| Error::Schema {
                        message: "map without values".to_string(),
                    })?;
                let key_ty = resolve_ref_to_type(k, definitions, alias)?;
                let val_ty = resolve_ref_to_type(v, definitions, alias)?;
                return Ok(Type::Map(Box::new(key_ty), Box::new(val_ty)));
            }
            DataType::Constructor => {}
        }
    }

    if def.any_of.is_some() {
        return Ok(Type::Custom(Identifier::new(import_type_name(&key, alias))));
    }

    if def.data_type == Some(DataType::Integer) {
        return Ok(Type::Int);
    }
    if def.data_type == Some(DataType::Bytes) {
        return Ok(Type::Bytes);
    }

    Ok(Type::Custom(Identifier::new(import_type_name(&key, alias))))
}

fn field_to_record_field(f: &Field, definitions: &Definitions, alias: Option<&str>) -> Result<RecordField, Error> {
    let name = f
        .title
        .as_deref()
        .unwrap_or("field")
        .to_string();
    let r#type = resolve_ref_to_type(&f.reference, definitions, alias)?;
    Ok(RecordField {
        name: Identifier::new(name),
        r#type,
        span: Span::DUMMY,
    })
}

fn schema_to_variant_case(schema: &Schema, definitions: &Definitions, alias: Option<&str>) -> Result<VariantCase, Error> {
    let name = schema
        .title
        .as_deref()
        .unwrap_or("Variant")
        .to_string();
    let fields: Vec<RecordField> = schema
        .fields
        .iter()
        .map(|f| field_to_record_field(f, definitions, alias))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(VariantCase {
        name: Identifier::new(name),
        fields,
        span: Span::DUMMY,
    })
}

/// Convert a CIP-57 Definition to a tx3 TypeDef if it is a product or sum type. Primitives and raw List/Map return None.
fn definition_to_type_def(
    key: &str,
    def: &Definition,
    definitions: &Definitions,
    alias: Option<&str>,
) -> Result<Option<TypeDef>, Error> {
    if let Some(dt) = &def.data_type {
        match dt {
            DataType::Integer | DataType::Bytes => return Ok(None),
            DataType::List | DataType::Map => return Ok(None),
            DataType::Constructor => {}
        }
    }

    let cases = if let Some(any_of) = &def.any_of {
        any_of
            .iter()
            .map(|s| schema_to_variant_case(s, definitions, alias))
            .collect::<Result<Vec<_>, _>>()?
    } else {
        return Ok(None);
    };

    if cases.is_empty() {
        return Ok(None);
    }

    let type_name = import_type_name(key, alias);
    Ok(Some(TypeDef {
        name: Identifier::new(type_name),
        cases,
        span: Span::DUMMY,
    }))
}

/// Resolve all plutus.json imports: read files, parse blueprints, map definitions to TypeDefs, append to program.types with collision checks.
pub fn resolve_imports(program: &mut Program, root: Option<&Path>) -> Result<(), Error> {
    if program.imports.is_empty() {
        return Ok(());
    }
    let root = root.ok_or(Error::MissingRoot)?;

    let existing_names: HashSet<String> = program
        .types
        .iter()
        .map(|t| t.name.value.clone())
        .chain(program.aliases.iter().map(|a| a.name.value.clone()))
        .collect();
    let mut added_names = HashSet::<String>::new();

    for import in &program.imports {
        let path = root.join(import.path.value.as_str());
        let json = std::fs::read_to_string(&path)?;
        let blueprint: Blueprint = serde_json::from_str(&json)?;

        let definitions = match &blueprint.definitions {
            Some(d) => d,
            None => continue,
        };

        let alias = import.alias.as_ref().map(|a| a.value.as_str());

        for (key, def) in &definitions.inner {
            if let Some(type_def) = definition_to_type_def(key, def, definitions, alias)? {
                let name = type_def.name.value.clone();
                if existing_names.contains(&name) || added_names.contains(&name) {
                    return Err(Error::duplicate_type(name, import.span.clone()));
                }
                added_names.insert(name.clone());
                program.types.push(type_def);
            }
        }
    }

    Ok(())
}
