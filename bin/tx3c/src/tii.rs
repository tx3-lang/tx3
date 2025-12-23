use schemars::Schema;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::{BTreeMap, HashMap};

pub const TII_VERSION: &str = "v1beta0";

/// Root structure for TII (Transaction Invocation Interface) JSON files
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TiiFile {
    pub tii: TiiInfo,
    pub protocol: Protocol,
    pub transactions: HashMap<String, Transaction>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub profiles: HashMap<String, Profile>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Components>,
}

/// TII version information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TiiInfo {
    pub version: String,
}

/// Protocol metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Protocol {
    pub scope: String,
    pub name: String,
    pub version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub environment: Option<Schema>,
}

/// Transaction definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transaction {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub tir: tx3_tir::interop::json::TirEnvelope,
    pub params: Schema,
}

/// Environment definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Profile {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub environment: serde_json::Value,
}

/// Components section containing schemas and other components
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Components {
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub schemas: HashMap<String, Schema>,
}

use tx3_tir::model::v1beta0 as tir;

/// Maps a tx3_tir Type to a JSON Schema type value
fn map_tir_type_to_json_schema(tir_type: &tir::Type) -> Value {
    match tir_type {
        tir::Type::Int => json!({"type": "integer"}),
        tir::Type::Bool => json!({"type": "boolean"}),
        tir::Type::Bytes => json!({"type": "string", "format": "byte"}),
        tir::Type::Address => json!({"type": "string"}),
        tir::Type::Unit => json!({"type": "null"}),
        tir::Type::List => json!({
            "type": "array",
            "items": {"type": "object"}
        }),
        tir::Type::Map => json!({
            "type": "object",
            "additionalProperties": {"type": "object"}
        }),
        tir::Type::Utxo | tir::Type::UtxoRef | tir::Type::AnyAsset => json!({"type": "object"}),
        tir::Type::Custom(_) => json!({"type": "object"}),
        tir::Type::Undefined => json!({"type": "object"}),
    }
}

pub fn infer_schema_from_params(params: &BTreeMap<String, tir::Type>) -> Schema {
    let mut properties = serde_json::Map::new();
    let mut required = Vec::new();

    for (name, tir_type) in params.iter() {
        let field_schema = map_tir_type_to_json_schema(tir_type);
        properties.insert(name.clone(), field_schema);
        required.push(name.clone());
    }

    let schema_json = json!({
        "type": "object",
        "properties": properties,
        "required": required
    });

    // Deserialize the JSON into a Schema
    serde_json::from_value(schema_json).unwrap_or_else(|_| {
        // Fallback to a basic object schema if deserialization fails
        serde_json::from_value(json!({"type": "object"})).unwrap()
    })
}

pub fn map_ast_type_to_json_schema(r#type: &tx3_lang::ast::Type) -> Value {
    match r#type {
        tx3_lang::ast::Type::Int => json!({"type": "integer"}),
        tx3_lang::ast::Type::Bool => json!({"type": "boolean"}),
        tx3_lang::ast::Type::Bytes => json!({"type": "string", "format": "byte"}),
        tx3_lang::ast::Type::Address => json!({"type": "string"}),
        tx3_lang::ast::Type::Unit => json!({"type": "null"}),
        tx3_lang::ast::Type::List(inner) => json!({
            "type": "array",
            "items": map_ast_type_to_json_schema(inner)
        }),
        tx3_lang::ast::Type::Map(key, value) => json!({
            "type": "object",
            "additionalProperties": map_ast_type_to_json_schema(value)
        }),
        tx3_lang::ast::Type::Custom(_) => json!({"type": "object"}),
        tx3_lang::ast::Type::Undefined => json!({"type": "null"}),
        tx3_lang::ast::Type::Utxo => json!({"type": "object"}),
        tx3_lang::ast::Type::UtxoRef => json!({"type": "object"}),
        tx3_lang::ast::Type::AnyAsset => json!({"type": "object"}),
    }
}

pub fn infer_env_schema(ast: &tx3_lang::ast::Program) -> Schema {
    let mut properties = serde_json::Map::new();
    let mut required = Vec::new();

    if let Some(env) = &ast.env {
        for field in env.fields.iter() {
            let field_schema = map_ast_type_to_json_schema(&field.r#type);
            properties.insert(field.name.clone(), field_schema);
            required.push(field.name.clone());
        }
    }

    let schema_json = json!({
        "type": "object",
        "properties": properties,
        "required": required
    });

    serde_json::from_value(schema_json)
        .unwrap_or_else(|_| serde_json::from_value(json!({"type": "object"})).unwrap())
}
