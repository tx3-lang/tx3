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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub environment: Option<Schema>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub parties: HashMap<String, Party>,
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
}

/// Transaction definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transaction {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub tir: tx3_tir::interop::json::TirEnvelope,
    pub params: Schema,
}

/// Party definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Party {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

/// Environment definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Profile {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub environment: serde_json::Value,
    pub parties: serde_json::Value,
}

/// Components section containing schemas and other components
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Components {
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub schemas: HashMap<String, Schema>,
}

pub fn map_ast_type_to_json_schema(r#type: &tx3_lang::ast::Type) -> Value {
    match r#type {
        tx3_lang::ast::Type::Int => json!({"type": "integer"}),
        tx3_lang::ast::Type::Bool => json!({"type": "boolean"}),
        tx3_lang::ast::Type::Bytes => {
            json!({ "$ref": "https://tx3.land/specs/v1beta0/core#Bytes" })
        }
        tx3_lang::ast::Type::Address => {
            json!({ "$ref": "https://tx3.land/specs/v1beta0/core#Address" })
        }
        tx3_lang::ast::Type::UtxoRef => {
            json!({ "$ref": "https://tx3.land/specs/v1beta0/core#UtxoRef" })
        }
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
        tx3_lang::ast::Type::Utxo => {
            json!({ "$ref": "https://tx3.land/specs/v1beta0/core#Utxo" })
        }
        tx3_lang::ast::Type::AnyAsset => {
            json!({ "$ref": "https://tx3.land/specs/v1beta0/core#AnyAsset" })
        }
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

pub fn infer_tx_params_schema(ast: &tx3_lang::ast::TxDef) -> Schema {
    let mut properties = serde_json::Map::new();
    let mut required = Vec::new();

    for param in ast.parameters.parameters.iter() {
        let field_schema = map_ast_type_to_json_schema(&param.r#type);
        properties.insert(param.name.value.clone(), field_schema);
        required.push(param.name.value.clone());
    }

    let schema_json = json!({
        "type": "object",
        "properties": properties,
        "required": required
    });

    serde_json::from_value(schema_json)
        .unwrap_or_else(|_| serde_json::from_value(json!({"type": "object"})).unwrap())
}
