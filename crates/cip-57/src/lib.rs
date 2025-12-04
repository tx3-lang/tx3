//! This module defines the structures for a Blueprint, including its preamble, validators, and definitions.

use serde::{Deserialize, Serialize};
use serde_json::Number;
use std::collections::BTreeMap;
/// Represents a blueprint containing preamble, validators, and optional definitions.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Blueprint {
    pub preamble: Preamble,
    pub validators: Vec<Validator>,
    pub definitions: Option<Definitions>,
}

/// Represents the preamble of a blueprint, including metadata such as title, description, version, and compiler information.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Preamble {
    pub title: String,
    pub description: Option<String>,
    pub version: String,
    pub plutus_version: String,
    pub compiler: Option<Compiler>,
    pub license: Option<String>,
}

/// Represents the compiler information in the preamble.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Compiler {
    pub name: String,
    pub version: Option<String>,
}

/// Represents a validator in the blueprint, including its title, description, compiled code, hash, datum, redeemer, and parameters.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Validator {
    pub title: String,
    pub description: Option<String>,
    pub compiled_code: Option<String>,
    pub hash: Option<String>,
    pub datum: Option<Argument>,
    pub redeemer: Option<Argument>,
    pub parameters: Option<Vec<Parameter>>,
}

/// Represents an argument in a validator, including its title, description, purpose, and schema reference.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Argument {
    pub title: Option<String>,
    pub description: Option<String>,
    pub purpose: Option<PurposeArray>,
    pub schema: Reference,
}

/// Represents a purpose array which can be either a single purpose or an array of purposes.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum PurposeArray {
    Single(Purpose),
    Array(Vec<Purpose>),
}

/// Represents the purpose of an argument, which can be spend, mint, withdraw, or publish.
#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum Purpose {
    Spend,
    Mint,
    Withdraw,
    Publish,
}

/// Represents a reference to a schema.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Reference {
    #[serde(rename = "$ref")]
    pub reference: Option<String>,
}

/// Represents a parameter in a validator, including its title, description, purpose, and schema reference.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Parameter {
    pub title: Option<String>,
    pub description: Option<String>,
    pub purpose: Option<PurposeArray>,
    pub schema: Reference,
}

/// Represents the definitions in a blueprint, which is a map of definition names to their corresponding definitions.
#[derive(Debug, Default, Deserialize, Serialize, Clone)]
pub struct Definitions {
    #[serde(flatten, default)]
    pub inner: BTreeMap<String, Definition>,
}

/// Represents a definition in the blueprint, including its title, description, data type, any_of schemas, items, keys, and values.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Definition {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data_type: Option<DataType>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub any_of: Option<Vec<Schema>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub items: Option<ReferencesArray>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keys: Option<Reference>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub values: Option<Reference>,
}

/// Represents an array of references which can be either a single reference or an array of references.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(untagged)]
pub enum ReferencesArray {
    Single(Reference),
    Array(Vec<Reference>),
}

/// Represents a schema in a definition, including its title, description, data type, index, and fields.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Schema {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub data_type: DataType,
    pub index: Number,
    pub fields: Vec<Field>,
}

/// Represents the data type of a schema, which can be integer, bytes, list, map, or constructor.
#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum DataType {
    Integer,
    Bytes,
    List,
    Map,
    Constructor,
}

/// Represents a field in a schema, including its title and reference.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Field {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(rename = "$ref")]
    pub reference: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn deserialize_plutus_json_into_blueprint() {
        let manifest = env!("CARGO_MANIFEST_DIR");
        let path = PathBuf::from(manifest).join("examples").join("plutus.json");

        let failure_msg = format!("failed to read example file: {}", path.display());
        let json = fs::read_to_string(&path).expect(&failure_msg);

        let bp: Blueprint = serde_json::from_str(&json).expect("failed to deserialize blueprint");

        assert!(
            !bp.preamble.title.is_empty(),
            "preamble.title should not be empty"
        );
        assert!(!bp.validators.is_empty(), "expected at least one validator");
    }
}
