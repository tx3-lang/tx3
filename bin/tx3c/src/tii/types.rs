use schemars::Schema;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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

#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum BytesEncoding {
    Base64,
    Hex,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct TirEnvelope {
    pub content: String,
    pub encoding: BytesEncoding,
    pub version: String,
}

/// Transaction definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transaction {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub tir: TirEnvelope,
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
