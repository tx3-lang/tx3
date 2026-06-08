use anyhow::{anyhow, Context};
use schemars::Schema;
use serde_json::{json, Value};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::PathBuf,
};

pub mod types;

use tx3_lang::ast;
pub use types::*;

use crate::build::Args;

/// Attaches a `description` key to a JSON Schema property when the AST field
/// carries a docstring. Mutates `schema` in place and is a no-op when the
/// schema is not a JSON object (the `map_ast_type_to_json_schema` outputs are
/// always objects, but stay defensive in case the shape changes).
fn attach_description(schema: &mut Value, docstring: Option<&String>) {
    let Some(doc) = docstring else { return };
    if let Some(obj) = schema.as_object_mut() {
        obj.insert("description".to_string(), json!(doc));
    }
}

/// Builds JSON Schema for AST types, resolving user-defined record / variant
/// types into named entries under `components.schemas` and referencing them by
/// `$ref`. Resolution is by name against the program's own `types` / `aliases`
/// rather than via `Identifier::symbol`: the analyzer resolves symbols on param
/// and env types, but not on types nested inside other type definitions, so a
/// name lookup is the only thing that handles nesting reliably.
struct SchemaCtx<'a> {
    types: HashMap<&'a str, &'a ast::TypeDef>,
    aliases: HashMap<&'a str, &'a ast::AliasDef>,
    schemas: BTreeMap<String, Value>,
}

impl<'a> SchemaCtx<'a> {
    fn new(program: &'a ast::Program) -> Self {
        Self {
            types: program
                .types
                .iter()
                .map(|t| (t.name.value.as_str(), t))
                .collect(),
            aliases: program
                .aliases
                .iter()
                .map(|a| (a.name.value.as_str(), a))
                .collect(),
            schemas: BTreeMap::new(),
        }
    }

    fn map_type(&mut self, r#type: &ast::Type) -> Value {
        match r#type {
            ast::Type::Int => json!({"type": "integer"}),
            ast::Type::Bool => json!({"type": "boolean"}),
            ast::Type::Bytes => {
                json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/Bytes" })
            }
            ast::Type::Address => {
                json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/Address" })
            }
            ast::Type::UtxoRef => {
                json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/UtxoRef" })
            }
            ast::Type::Unit => json!({"type": "null"}),
            ast::Type::List(inner) => json!({
                "type": "array",
                "items": self.map_type(inner)
            }),
            ast::Type::Map(_, value) => json!({
                "type": "object",
                "additionalProperties": self.map_type(value)
            }),
            ast::Type::Custom(id) => self.map_custom(&id.value),
            ast::Type::Undefined => json!({"type": "null"}),
            ast::Type::Utxo => {
                json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/Utxo" })
            }
            ast::Type::AnyAsset => {
                json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/AnyAsset" })
            }
        }
    }

    /// Resolves a custom type name. Aliases are transparent (inlined as their
    /// target); records / variants register a schema under `components.schemas`
    /// on first encounter and yield a `$ref`. An unknown name (e.g. an
    /// unresolvable forward reference) falls back to a bare object so emit never
    /// hard-fails.
    fn map_custom(&mut self, name: &str) -> Value {
        if let Some(alias) = self.aliases.get(name).copied() {
            return self.map_type(&alias.alias_type);
        }

        let Some(def) = self.types.get(name).copied() else {
            return json!({"type": "object"});
        };

        if !self.schemas.contains_key(name) {
            // Reserve the key before recursing so a self-referential type terminates.
            self.schemas.insert(name.to_string(), Value::Null);
            let schema = self.build_type_def(def);
            self.schemas.insert(name.to_string(), schema);
        }

        json!({ "$ref": format!("#/components/schemas/{name}") })
    }

    /// A single-case type is a record (a plain object of its fields); a
    /// multi-case type is a tagged union, emitted as the idiomatic JSON Schema
    /// `oneOf` of externally tagged case objects. Record / variant-case fields
    /// carry no docstrings in the AST, so they get no `description` (unlike
    /// params / env fields, which do).
    fn build_type_def(&mut self, def: &ast::TypeDef) -> Value {
        if def.cases.len() == 1 {
            return self.case_fields(&def.cases[0]);
        }

        let variants: Vec<Value> = def
            .cases
            .iter()
            .map(|case| {
                let tag = case.name.value.clone();
                json!({
                    "type": "object",
                    "additionalProperties": false,
                    "required": [tag.clone()],
                    "properties": { tag: self.case_fields(case) }
                })
            })
            .collect();

        json!({ "oneOf": variants })
    }

    /// An object schema over a case's fields (empty object when fieldless).
    fn case_fields(&mut self, case: &ast::VariantCase) -> Value {
        let mut properties = serde_json::Map::new();
        let mut required = Vec::new();

        for field in case.fields.iter() {
            properties.insert(field.name.value.clone(), self.map_type(&field.r#type));
            required.push(field.name.value.clone());
        }

        json!({
            "type": "object",
            "properties": properties,
            "required": required
        })
    }
}

fn infer_env_schema(ctx: &mut SchemaCtx<'_>, ast: &'_ ast::Program) -> Schema {
    let mut properties = serde_json::Map::new();
    let mut required = Vec::new();

    if let Some(env) = &ast.env {
        for field in env.fields.iter() {
            let mut field_schema = ctx.map_type(&field.r#type);
            attach_description(&mut field_schema, field.docstring.as_ref());
            properties.insert(field.name.clone(), field_schema);
            required.push(field.name.clone());
        }
    }

    to_object_schema(properties, required)
}

fn infer_tx_params_schema(ctx: &mut SchemaCtx<'_>, tx: &'_ ast::TxDef) -> Schema {
    let mut properties = serde_json::Map::new();
    let mut required = Vec::new();

    for param in tx.parameters.parameters.iter() {
        let mut field_schema = ctx.map_type(&param.r#type);
        attach_description(&mut field_schema, param.docstring.as_ref());
        properties.insert(param.name.value.clone(), field_schema);
        required.push(param.name.value.clone());
    }

    to_object_schema(properties, required)
}

fn to_object_schema(properties: serde_json::Map<String, Value>, required: Vec<String>) -> Schema {
    let schema_json = json!({
        "type": "object",
        "properties": properties,
        "required": required
    });

    serde_json::from_value(schema_json)
        .unwrap_or_else(|_| serde_json::from_value(json!({"type": "object"})).unwrap())
}

fn infer_available_profiles(args: &Args) -> HashSet<String> {
    let mut profiles = HashSet::new();

    for entry in args.profile_env_files.iter() {
        profiles.insert(entry.profile.clone());
    }

    // Add profiles from --profile flag (forced inclusion)
    for profile in args.profiles.iter() {
        profiles.insert(profile.clone());
    }

    profiles
}

// This is not to be meant as a comprehensive conversion between String -> TIR, it's just a
// mapping between the string representation of a value in a dotfile to its JSON representation.
//
// The only casess that mappers are those where there's a natural JSON representation (number and boolean so far).
//
// The strict mapping of values onto TIR artifacts ocurrs in the tx3-sdk at _resolve-time_.
fn map_dotfile_value_to_json(value: &str, target: &ast::Type) -> serde_json::Value {
    match target {
        ast::Type::Int => {
            if let Ok(i) = value.parse::<i128>() {
                json!(i)
            } else {
                json!(value)
            }
        }
        ast::Type::Bool => {
            if let Ok(b) = value.parse::<bool>() {
                json!(b)
            } else {
                json!(value)
            }
        }
        _ => json!(value),
    }
}

fn infer_environment_values_from_dotfile(
    env: &BTreeMap<String, String>,
    ast: &ast::Program,
) -> serde_json::Value {
    let mut obj = serde_json::Map::new();

    if let Some(def) = &ast.env {
        for field in def.fields.iter() {
            let key = field.name.clone();

            if let Some(value) = env.get(key.as_str()) {
                obj.insert(key, map_dotfile_value_to_json(value, &field.r#type));
            }
        }
    }

    serde_json::Value::Object(obj)
}

fn infer_party_values_from_dotfile(
    env: &BTreeMap<String, String>,
    ast: &ast::Program,
) -> serde_json::Value {
    let mut obj = serde_json::Map::new();

    for party in &ast.parties {
        let key = party.name.value.to_lowercase();
        if let Some(value) = env.get(key.as_str()) {
            obj.insert(key, json!(value));
        }
    }

    serde_json::Value::Object(obj)
}

fn load_dotfile(path: Option<&PathBuf>) -> anyhow::Result<BTreeMap<String, String>> {
    let Some(path) = path else {
        return Ok(BTreeMap::new());
    };

    let source = std::fs::read_to_string(path).context("loading env file")?;

    let env = dotenv_parser::parse_dotenv(&source)
        .map_err(|e| anyhow!("Failed to parse env file: {}", e))?;

    let env = env
        .into_iter()
        .map(|(k, v)| (k.to_lowercase(), v))
        .collect();

    Ok(env)
}

pub fn emit_tii(args: Args, ws: &tx3_lang::Workspace) -> anyhow::Result<()> {
    let ast = ws.ast().ok_or(anyhow!("Failed to get AST"))?;

    // Accumulates custom-type schemas referenced by env + params, emitted under
    // `components.schemas`.
    let mut ctx = SchemaCtx::new(ast);

    let env_schema = infer_env_schema(&mut ctx, ast);

    let mut tii = TiiFile {
        tii: TiiInfo {
            version: TII_VERSION.to_string(),
        },
        protocol: Protocol {
            scope: args.protocol_scope.clone().unwrap_or("unknown".to_string()),
            name: args.protocol_name.clone().unwrap_or("unknown".to_string()),
            version: args.protocol_version.clone().unwrap_or("0.0.1".to_string()),
            description: args.protocol_description.clone(),
        },
        environment: Some(env_schema),
        parties: HashMap::new(),
        transactions: HashMap::new(),
        profiles: HashMap::new(),
        components: None,
    };

    for party in ast.parties.iter() {
        tii.parties.insert(
            party.name.value.to_lowercase(),
            Party {
                description: party.docstring.clone(),
            },
        );
    }

    for tx in ast.txs.iter() {
        let tir = ws
            .tir(&tx.name.value)
            .ok_or_else(|| anyhow!("Failed to get TIR for transaction: {}", tx.name.value))?;

        let params_schema = infer_tx_params_schema(&mut ctx, tx);

        // Convert TIR to bytes
        let (bytes, version) = tx3_tir::encoding::to_bytes(tir);

        // Hex-encode the bytes
        let hex_string = hex::encode(&bytes);

        // Add to output map with transaction name as key
        tii.transactions.insert(
            tx.name.value.clone(),
            Transaction {
                description: tx.docstring.clone(),
                tir: TirEnvelope {
                    content: hex_string,
                    encoding: BytesEncoding::Hex,
                    version: version.to_string(),
                },
                params: params_schema,
            },
        );
    }

    if !ctx.schemas.is_empty() {
        let schemas = ctx
            .schemas
            .into_iter()
            .map(|(name, value)| Ok((name, serde_json::from_value(value)?)))
            .collect::<anyhow::Result<HashMap<String, Schema>>>()?;
        tii.components = Some(Components { schemas });
    }

    for profile in infer_available_profiles(&args) {
        let env_file = args
            .profile_env_files
            .iter()
            .find(|entry| entry.profile == profile);

        let dotfile = load_dotfile(env_file.map(|entry| &entry.value))?;

        let environment = infer_environment_values_from_dotfile(&dotfile, ast);
        let parties = infer_party_values_from_dotfile(&dotfile, ast);

        tii.profiles.insert(
            profile,
            Profile {
                description: None,
                environment,
                parties,
            },
        );
    }

    // Temp: Ensure there's always a "local" profile
    if !tii.profiles.contains_key("local") {
        tii.profiles.insert(
            "local".to_string(),
            Profile {
                description: None,
                environment: json!({}),
                parties: json!({}),
            },
        );
    }

    let output_json = json!(tii);

    let output_path = args
        .output
        .unwrap_or_else(|| args.source.with_extension("tii"));

    std::fs::write(&output_path, serde_json::to_string_pretty(&output_json)?)?;

    println!(
        "Compiled {} to {}",
        args.source.display(),
        output_path.display()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tx3_lang::Workspace;

    fn schemas_for(src: &str) -> BTreeMap<String, Value> {
        let mut ws = Workspace::from_string(src.to_string());
        ws.analyze().expect("analyze");
        let ast = ws.ast().expect("ast");

        let mut ctx = SchemaCtx::new(ast);
        infer_env_schema(&mut ctx, ast);
        for tx in ast.txs.iter() {
            infer_tx_params_schema(&mut ctx, tx);
        }
        ctx.schemas
    }

    const PARTIES: &str = "party Sender; party Receiver;";
    const TX_TAIL: &str = "{ input source { from: Sender, min_amount: Ada(1), } \
        output { to: Receiver, amount: Ada(1), } }";

    #[test]
    fn record_param_registers_object_schema_and_ref() {
        let src = format!(
            "{PARTIES} type AssetClass {{ policy_id: Bytes, asset_name: Bytes, }} \
             tx t(asset: AssetClass) {TX_TAIL}"
        );
        let schemas = schemas_for(&src);

        let asset = schemas.get("AssetClass").expect("AssetClass registered");
        assert_eq!(asset["type"], json!("object"));
        assert_eq!(asset["required"], json!(["policy_id", "asset_name"]));
        assert_eq!(
            asset["properties"]["policy_id"]["$ref"],
            json!("https://tx3.land/specs/v1beta0/tii#/$defs/Bytes")
        );
    }

    #[test]
    fn nested_custom_type_is_registered_and_referenced() {
        let src = format!(
            "{PARTIES} type Inner {{ x: Int, }} type Outer {{ inner: Inner, y: Int, }} \
             tx t(outer: Outer) {TX_TAIL}"
        );
        let schemas = schemas_for(&src);

        assert!(schemas.contains_key("Inner"));
        assert_eq!(
            schemas["Outer"]["properties"]["inner"]["$ref"],
            json!("#/components/schemas/Inner")
        );
        assert_eq!(schemas["Inner"]["properties"]["x"]["type"], json!("integer"));
    }

    #[test]
    fn variant_emits_tagged_one_of() {
        let src = format!(
            "{PARTIES} type Action {{ Buy, Sell {{ price: Int, }}, }} \
             tx t(action: Action) {TX_TAIL}"
        );
        let schemas = schemas_for(&src);

        let cases = schemas["Action"]["oneOf"].as_array().expect("oneOf array");
        assert_eq!(cases.len(), 2);
        // Each branch is an externally tagged object keyed by the case name.
        assert_eq!(cases[0]["required"], json!(["Buy"]));
        assert_eq!(cases[1]["required"], json!(["Sell"]));
        assert_eq!(
            cases[1]["properties"]["Sell"]["properties"]["price"]["type"],
            json!("integer")
        );
    }

    #[test]
    fn alias_is_inlined_transparently() {
        let src = format!(
            "{PARTIES} type Lovelace = Int; type Holding {{ amount: Lovelace, }} \
             tx t(h: Holding) {TX_TAIL}"
        );
        let schemas = schemas_for(&src);

        // The alias resolves to its target and is not itself registered.
        assert!(!schemas.contains_key("Lovelace"));
        assert_eq!(
            schemas["Holding"]["properties"]["amount"]["type"],
            json!("integer")
        );
    }

    #[test]
    fn primitive_only_protocol_registers_nothing() {
        let src = format!("{PARTIES} tx t(quantity: Int) {TX_TAIL}");
        assert!(schemas_for(&src).is_empty());
    }
}
