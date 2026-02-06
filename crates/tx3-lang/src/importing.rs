//! Resolves plutus.json (CIP-57) imports and maps blueprint definitions to tx3 types.

use std::collections::HashSet;
use std::path::PathBuf;

use cip_57::{Blueprint, DataType, Definition, Definitions, Field, ReferencesArray, Schema};

use crate::ast::{Identifier, Program, RecordField, Span, Type, TypeDef, VariantCase};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("cannot resolve imports without a root path (use Workspace::from_file instead of from_string)")]
    #[diagnostic(code(tx3::importing::missing_root))]
    MissingRoot,

    #[error("I/O error reading import file: {0}")]
    #[diagnostic(code(tx3::importing::io))]
    Io(#[from] std::io::Error),

    #[error("invalid JSON in plutus file: {0}")]
    #[diagnostic(code(tx3::importing::json))]
    Json(#[from] serde_json::Error),

    #[error("duplicate type name: {name}")]
    #[diagnostic(code(tx3::importing::duplicate_type))]
    DuplicateType {
        name: String,

        #[source_code]
        src: Option<String>,

        #[label("type already defined here or from another import")]
        span: Span,
    },

    #[error("plutus schema error: {message}")]
    #[diagnostic(code(tx3::importing::schema))]
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

pub trait ImportLoader {
    fn load_source(&self, path: &str) -> std::io::Result<String>;
}

pub struct FsLoader {
    root: PathBuf,
}

impl FsLoader {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }
}

impl ImportLoader for FsLoader {
    fn load_source(&self, path: &str) -> std::io::Result<String> {
        let full_path = self.root.join(path);
        std::fs::read_to_string(full_path)
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

fn import_type_name(key: &str, alias: Option<&str>) -> String {
    let base = key_to_normalized_name(key);
    match alias {
        Some(a) => format!("{}_{}", a, base),
        None => base,
    }
}

fn resolve_ref_to_type(
    ref_str: &str,
    definitions: &Definitions,
    alias: Option<&str>,
) -> Result<Type, Error> {
    let key = ref_to_key_owned(ref_str);
    let def = definitions.inner.get(&key).ok_or_else(|| Error::Schema {
        message: format!("definition not found: {}", key),
    })?;

    if let Some(dt) = &def.data_type {
        match dt {
            DataType::Integer => return Ok(Type::Int),
            DataType::Bytes => return Ok(Type::Bytes),
            DataType::List => {
                let inner = match &def.items {
                    Some(ReferencesArray::Single(r)) => r
                        .reference
                        .as_ref()
                        .map(|s| resolve_ref_to_type(s, definitions, alias)),
                    Some(ReferencesArray::Array(arr)) => arr.first().and_then(|r| {
                        r.reference
                            .as_ref()
                            .map(|s| resolve_ref_to_type(s, definitions, alias))
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

    Ok(Type::Custom(Identifier::new(import_type_name(&key, alias))))
}

fn field_to_record_field(
    f: &Field,
    definitions: &Definitions,
    alias: Option<&str>,
) -> Result<RecordField, Error> {
    let name = f.title.as_deref().unwrap_or("field").to_string();
    let r#type = resolve_ref_to_type(&f.reference, definitions, alias)?;
    Ok(RecordField {
        name: Identifier::new(name),
        r#type,
        span: Span::DUMMY,
    })
}

fn schema_to_variant_case(
    schema: &Schema,
    definitions: &Definitions,
    alias: Option<&str>,
) -> Result<VariantCase, Error> {
    let name = schema.title.as_deref().unwrap_or("Variant").to_string();
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
        // Data type has no structure but should still be imported.
        // TODO: There is no Any for our type system, so for now we'll just import it as a single empty variant case.
        if key == "Data" {
            return Ok(Some(TypeDef {
                name: Identifier::new(import_type_name(key, alias)),
                cases: vec![VariantCase {
                    name: Identifier::new("Default"),
                    fields: vec![],
                    span: Span::DUMMY,
                }],
                span: Span::DUMMY,
            }));
        }
        return Ok(None);
    };

    if cases.is_empty() {
        return Ok(None);
    }

    let mut cases = cases;
    if cases.len() == 1 {
        cases[0].name = Identifier::new("Default");
    }

    let type_name = import_type_name(key, alias);
    Ok(Some(TypeDef {
        name: Identifier::new(type_name),
        cases,
        span: Span::DUMMY,
    }))
}

pub fn resolve_imports(
    program: &mut Program,
    loader: Option<&impl ImportLoader>,
) -> Result<(), Error> {
    if program.imports.is_empty() {
        return Ok(());
    }
    let loader = loader.ok_or(Error::MissingRoot)?;

    let existing_names: HashSet<String> = program
        .types
        .iter()
        .map(|t| t.name.value.clone())
        .chain(program.aliases.iter().map(|a| a.name.value.clone()))
        .collect();
    let mut added_names = HashSet::<String>::new();

    for import in &program.imports {
        let json = loader.load_source(import.path.value.as_str())?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Identifier, ImportDef, Program, Span, StringLiteral};

    #[derive(Default, Clone)]
    pub struct InMemoryLoader {
        map: std::collections::HashMap<String, String>,
    }

    impl InMemoryLoader {
        pub fn new() -> Self {
            Self {
                map: std::collections::HashMap::new(),
            }
        }

        pub fn add(&mut self, path: impl Into<String>, contents: impl Into<String>) -> &mut Self {
            self.map.insert(path.into(), contents.into());
            self
        }
    }

    impl ImportLoader for InMemoryLoader {
        fn load_source(&self, path: &str) -> std::io::Result<String> {
            self.map.get(path).cloned().ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("import not found: {}", path),
                )
            })
        }
    }

    #[test]
    fn resolve_imports_with_in_memory_loader() {
        let mut program = Program {
            imports: vec![ImportDef {
                path: StringLiteral::new("test.json"),
                alias: Some(Identifier::new("types")),
                span: Span::DUMMY,
            }],
            env: None,
            txs: vec![],
            types: vec![],
            aliases: vec![],
            assets: vec![],
            parties: vec![],
            policies: vec![],
            span: Span::DUMMY,
            scope: None,
        };

        let json = r#"{
            "preamble": { "title": "test", "version": "0", "plutusVersion": "v3" },
            "validators": [],
            "definitions": {
                "Bool": {
                    "title": "Bool",
                    "anyOf": [
                        { "title": "False", "dataType": "constructor", "index": 0, "fields": [] },
                        { "title": "True", "dataType": "constructor", "index": 1, "fields": [] }
                    ]
                }
            }
        }"#;

        let mut loader = InMemoryLoader::new();
        loader.add("test.json", json);
        resolve_imports(&mut program, Some(&loader)).unwrap();

        let type_names: Vec<String> = program.types.iter().map(|t| t.name.value.clone()).collect();
        assert!(
            type_names.contains(&"types_Bool".to_string()),
            "expected types_Bool in program.types, got: {:?}",
            type_names
        );
    }

    #[test]
    fn import_with_alias_adds_types() {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let root = std::path::Path::new(manifest_dir);

        let mut program =
            crate::parsing::parse_string(r#"import "../cip-57/examples/plutus.json" as types;"#)
                .unwrap();

        let loader = FsLoader::new(root);
        resolve_imports(&mut program, Some(&loader)).unwrap();

        let type_names: Vec<String> = program.types.iter().map(|t| t.name.value.clone()).collect();
        assert!(
            type_names.iter().any(|n| n.starts_with("types_")),
            "expected at least one type prefixed with 'types_', got: {:?}",
            type_names
        );
    }

    #[test]
    fn import_without_root_errors() {
        let src = r#"
            import "some/file.json";
            party X;
            tx dummy() {}
        "#;
        let mut program = crate::parsing::parse_string(src).unwrap();
        let res = resolve_imports(&mut program, None::<&InMemoryLoader>);

        assert!(res.is_err(), "expected error when importing without root");
        let err = res.unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("root") || msg.contains("import"),
            "expected error about root or import, got: {}",
            msg
        );
    }

    #[test]
    fn duplicate_type_name_from_imports_errors() {
        let json = r#"{
            "preamble": { "title": "test", "version": "0", "plutusVersion": "v3" },
            "validators": [],
            "definitions": {
                "One": {
                     "title": "One",
                     "anyOf": [ { "title": "A", "dataType": "constructor", "index": 0, "fields": [] } ]
                }
            }
        }"#;

        let src = r#"
            import "schema1.json" as types;
            import "schema2.json" as types;
        "#;

        let mut program = crate::parsing::parse_string(src).unwrap();
        let mut loader = InMemoryLoader::new();
        loader.add("schema1.json", json);
        loader.add("schema2.json", json);
        let res = resolve_imports(&mut program, Some(&loader));

        assert!(
            res.is_err(),
            "expected error for duplicate type names from two imports"
        );
        let err = res.unwrap_err();
        assert!(
            err.to_string().contains("duplicate") || err.to_string().contains("type"),
            "expected duplicate/type error, got: {}",
            err
        );
    }

    #[test]
    fn invalid_import_path_errors() {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let root = std::path::Path::new(manifest_dir);
        let src = r#"
            import "nonexistent/plutus.json" as types;
        "#;
        let mut program = crate::parsing::parse_string(src).unwrap();
        let loader = FsLoader::new(root);

        let res = resolve_imports(&mut program, Some(&loader));
        assert!(res.is_err(), "expected error for missing import file");
    }

    #[test]
    fn single_variant_type_import_with_analyze() {
        let dollar = "$";
        let hash = "#";
        let json = format!(
            r#"{{
            "preamble": {{ "title": "test", "version": "0", "plutusVersion": "v3" }},
            "validators": [],
            "definitions": {{
                "OutputReference": {{
                    "title": "OutputReference",
                    "anyOf": [
                        {{
                            "title": "OutputReference",
                            "dataType": "constructor",
                            "index": 0,
                            "fields": [
                                {{
                                    "title": "transaction_id",
                                    "{}ref": "{}/definitions/ByteArray"
                                }},
                                {{
                                    "title": "output_index",
                                    "{}ref": "{}/definitions/Int"
                                }}
                            ]
                        }}
                    ]
                }},
                "ByteArray": {{
                    "title": "ByteArray",
                    "dataType": "bytes"
                }},
                "Int": {{
                    "title": "Int",
                    "dataType": "integer"
                }}
            }}
        }}"#,
            dollar, hash, dollar, hash
        );

        let src = r#"
            import "test.json" as types;
            party Alice;
            tx test(outref: types_OutputReference) {
                output my_output {
                    to: Alice,
                    amount: 1000000,
                }
            }
        "#;

        let mut program = crate::parsing::parse_string(src).unwrap();
        let mut loader = InMemoryLoader::new();
        loader.add("test.json", json);

        resolve_imports(&mut program, Some(&loader)).unwrap();

        let output_ref_type = program
            .types
            .iter()
            .find(|t| t.name.value == "types_OutputReference")
            .expect("types_OutputReference should be imported");
        assert_eq!(output_ref_type.cases.len(), 1);
        assert_eq!(output_ref_type.cases[0].name.value, "Default");

        let report = crate::analyzing::analyze(&mut program);
        assert!(
            report.errors.is_empty(),
            "expected no analysis errors, got: {:?}",
            report.errors
        );
    }
}
