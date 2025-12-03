//! This module defines the structures and implementations for schemas, including types, references, and validators.

use serde::{Deserialize, Serialize};
use std::str;

/// Represents the different types a schema can have.
#[derive(Debug, Deserialize, Serialize, Clone, Copy, PartialEq)]
#[serde(rename_all = "camelCase")]
pub enum TypeName {
    AnyData,
    Integer,
    Bytes,
    Literal,
    Nullable,
    Object,
    Enum,
    Tuple,
    List,
}

/// Represents a schema with a name, type, optional properties, and JSON representation.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Schema {
    pub name: String,
    pub type_name: TypeName,
    pub properties: Option<Vec<Reference>>,
    pub json: String,
}

/// Represents a reference to another schema, including an optional name and schema name.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Reference {
    pub name: Option<String>,
    pub schema_name: String,
}

/// Represents a validator with a name, optional datum and redeemer, and parameters.
#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct Validator {
    pub name: String,
    pub datum: Option<Reference>,
    pub redeemer: Option<Reference>,
    pub parameters: Vec<Reference>,
}

impl Schema {
    /// Creates a new any data schema.
    ///
    /// # Arguments
    ///
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `AnyData`.
    pub fn new_anydata(json: String) -> Self {
        Self {
            name: "AnyData".to_string(),
            type_name: TypeName::AnyData,
            properties: None,
            json,
        }
    }

    /// Creates a new integer schema.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the schema.
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `Integer`.
    pub fn new_integer(name: String, json: String) -> Self {
        Self {
            name,
            type_name: TypeName::Integer,
            properties: None,
            json,
        }
    }

    /// Creates a new bytes schema.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the schema.
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `Bytes`.
    pub fn new_bytes(name: String, json: String) -> Self {
        Self {
            name,
            type_name: TypeName::Bytes,
            properties: None,
            json,
        }
    }

    /// Creates a new literal schema.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the schema.
    /// * `value` - The literal value of the schema.
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `Literal`.
    pub fn new_literal(name: String, value: String, json: String) -> Self {
        Self {
            name,
            type_name: TypeName::Literal,
            properties: Some(vec![Reference {
                name: None,
                schema_name: value,
            }]),
            json,
        }
    }

    /// Creates a new nullable schema.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the schema.
    /// * `reference` - The reference schema name.
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `Nullable`.
    pub fn new_nullable(name: String, reference: String, json: String) -> Self {
        Self {
            name,
            type_name: TypeName::Nullable,
            properties: Some(vec![Reference {
                name: None,
                schema_name: reference,
            }]),
            json,
        }
    }

    /// Creates a new object schema.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the schema.
    /// * `properties` - The properties of the object schema.
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `Object`.
    pub fn new_object(name: String, properties: Vec<Reference>, json: String) -> Self {
        Self {
            name,
            type_name: TypeName::Object,
            properties: Some(properties),
            json,
        }
    }

    /// Creates a new enum schema.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the schema.
    /// * `schemas` - The schemas that make up the enum.
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `Enum`.
    pub fn new_enum(name: String, schemas: &Vec<Schema>, json: String) -> Self {
        Self {
            name,
            type_name: TypeName::Enum,
            properties: Some(
                schemas
                    .iter()
                    .map(|s| Reference {
                        name: None,
                        schema_name: s.name.clone(),
                    })
                    .collect(),
            ),
            json,
        }
    }

    /// Creates a new tuple schema.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the schema.
    /// * `properties` - The properties of the tuple schema.
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `Tuple`.
    pub fn new_tuple(name: String, properties: Vec<Reference>, json: String) -> Self {
        Self {
            name,
            type_name: TypeName::Tuple,
            properties: Some(properties),
            json,
        }
    }

    /// Creates a new list schema.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the schema.
    /// * `reference` - The reference schema.
    /// * `json` - The JSON representation of the schema.
    ///
    /// # Returns
    ///
    /// A new `Schema` instance with type `List`.
    pub fn new_list(name: String, reference: Reference, json: String) -> Self {
        Self {
            name,
            type_name: TypeName::List,
            properties: Some(vec![reference]),
            json,
        }
    }
}

impl str::FromStr for TypeName {
    type Err = ();

    /// Converts a string to a `TypeName`.
    ///
    /// # Arguments
    ///
    /// * `input` - The string representation of the type name.
    ///
    /// # Returns
    ///
    /// A `Result` containing the `TypeName` or an error.
    fn from_str(input: &str) -> Result<TypeName, Self::Err> {
        match input {
            "AnyData" => Ok(TypeName::AnyData),
            "Integer" => Ok(TypeName::Integer),
            "Bytes" => Ok(TypeName::Bytes),
            "Literal" => Ok(TypeName::Literal),
            "Nullable" => Ok(TypeName::Nullable),
            "Object" => Ok(TypeName::Object),
            "Enum" => Ok(TypeName::Enum),
            "Tuple" => Ok(TypeName::Tuple),
            "List" => Ok(TypeName::List),
            _ => Err(()),
        }
    }
}
