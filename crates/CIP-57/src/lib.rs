//! This module provides functions to work with blueprints and schemas, including
//! parsing JSON, reading from files, and JSON-to-JSON conversion helpers.

use anyhow::Result;
use schema::TypeName;
use serde_json;
use std::fs;
use tx3_lang::ir::{Expression, StructExpr};

pub mod blueprint;
pub mod schema;
// Templates are intentionally removed for a pure JSON-to-JSON crate.

pub struct Codegen {}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {}
    }

    fn get_schema_name(&self, key: String) -> String {
        // Keep a simple normalization without external deps (heck)
        let normalized = key
            .replace("#/definitions/", "")
            .replace("~1", " ")
            .replace("/", " ")
            .replace("_", " ")
            .replace("$", " ");
        // Basic PascalCase conversion
        normalized
            .split_whitespace()
            .map(|w| {
                let mut chars = w.chars();
                match chars.next() {
                    Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                    None => String::new(),
                }
            })
            .collect::<Vec<_>>()
            .join("")
    }

    fn parse_bytes_string(s: &str) -> Vec<u8> {
        if let Some(hex) = s.strip_prefix("0x") {
            return hex::decode(hex).unwrap_or_default();
        }
        s.as_bytes().to_vec()
    }

    /// Parses a JSON string into a `Blueprint`.
    ///
    /// # Arguments
    ///
    /// * `json` - The JSON data from a `plutus.json` file
    ///
    /// # Returns
    ///
    /// A `Blueprint` instance or an error.
    pub fn get_blueprint_from_json(&self, json: String) -> Result<blueprint::Blueprint> {
        let bp = serde_json::from_str(&json)?;
        Ok(bp)
    }

    /// Reads a JSON file from a specified path and parses it into a `Blueprint`.
    ///
    /// # Arguments
    ///
    /// * `path` - The `plutus.json` file path in the filesystem
    ///
    /// # Returns
    ///
    /// A `Blueprint` instance or an error.
    pub fn get_blueprint_from_path(&self, path: String) -> Result<blueprint::Blueprint> {
        let json = fs::read_to_string(path)?;
        self.get_blueprint_from_json(json)
    }

    /// Obtains the list of schemas from a given `Blueprint`.
    ///
    /// # Arguments
    ///
    /// * `blueprint` - A `Blueprint` from which to obtain the schemas.
    ///
    /// # Returns
    ///
    /// A vector of `Schema` from the blueprint.
    pub fn get_schemas_from_blueprint(
        &self,
        blueprint: blueprint::Blueprint,
    ) -> Vec<schema::Schema> {
        let mut schemas: Vec<schema::Schema> = vec![];
        if blueprint.definitions.is_some() {
            for definition in blueprint.definitions.unwrap().inner.iter() {
                let definition_name = self.get_schema_name(definition.0.clone());
                let definition_json = serde_json::to_string(&definition.1).unwrap();
                if definition.1.data_type.is_some() {
                    match definition.1.data_type.unwrap() {
                        blueprint::DataType::Integer => {
                            schemas.push(schema::Schema::new_integer(
                                definition_name.clone(),
                                definition_json.clone(),
                            ));
                        }
                        blueprint::DataType::Bytes => {
                            schemas.push(schema::Schema::new_bytes(
                                definition_name.clone(),
                                definition_json.clone(),
                            ));
                        }
                        blueprint::DataType::List => {
                            if definition.1.items.is_some() {
                                match definition.1.items.as_ref().unwrap() {
                                    blueprint::ReferencesArray::Single(reference) => {
                                        if reference.reference.is_some() {
                                            schemas.push(schema::Schema::new_list(
                                                definition_name.clone(),
                                                schema::Reference {
                                                    name: None,
                                                    schema_name: self.get_schema_name(
                                                        reference
                                                            .reference
                                                            .as_ref()
                                                            .unwrap()
                                                            .clone(),
                                                    ),
                                                },
                                                definition_json.clone(),
                                            ));
                                        }
                                    }
                                    blueprint::ReferencesArray::Array(references) => {
                                        let mut properties: Vec<schema::Reference> = vec![];
                                        for reference in references {
                                            if reference.reference.is_some() {
                                                properties.push(schema::Reference {
                                                    name: None,
                                                    schema_name: self.get_schema_name(
                                                        reference
                                                            .reference
                                                            .as_ref()
                                                            .unwrap()
                                                            .clone(),
                                                    ),
                                                });
                                            }
                                        }
                                        schemas.push(schema::Schema::new_tuple(
                                            definition_name.clone(),
                                            properties,
                                            definition_json.clone(),
                                        ));
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                if definition.1.title.is_some() {
                    if definition.1.title.as_ref().unwrap() == "Data" && definition_name == "Data" {
                        schemas.push(schema::Schema::new_anydata(definition_json.clone()));
                    }
                }
                if definition.1.any_of.is_some() {
                    let mut internal_schemas: Vec<schema::Schema> = vec![];
                    for (index, parameter) in
                        definition.1.any_of.as_ref().unwrap().iter().enumerate()
                    {
                        match parameter.data_type {
                            blueprint::DataType::Constructor => {
                                let schema_name = format!(
                                    "{}{}",
                                    definition_name,
                                    parameter.title.clone().unwrap_or((index + 1).to_string())
                                );
                                let mut properties: Vec<schema::Reference> = vec![];
                                for property in &parameter.fields {
                                    let mut schema_name =
                                        self.get_schema_name(property.reference.clone());
                                    if schema_name == "Data" {
                                        schema_name = "AnyData".to_string();
                                    }
                                    properties.push(schema::Reference {
                                        name: property.title.clone(),
                                        schema_name,
                                    });
                                }
                                let schema: schema::Schema;
                                if properties.len().gt(&0) || parameter.title.is_none() {
                                    if properties.iter().any(|p| p.name.is_none()) {
                                        schema = schema::Schema::new_tuple(
                                            schema_name,
                                            properties,
                                            definition_json.clone(),
                                        );
                                    } else {
                                        schema = schema::Schema::new_object(
                                            schema_name,
                                            properties,
                                            definition_json.clone(),
                                        );
                                    }
                                } else {
                                    schema = schema::Schema::new_literal(
                                        schema_name,
                                        parameter.title.clone().unwrap(),
                                        definition_json.clone(),
                                    );
                                }
                                internal_schemas.push(schema);
                            }
                            _ => {}
                        }
                    }
                    if internal_schemas.len().eq(&1) {
                        let mut schema = internal_schemas.first().unwrap().clone();
                        schema.name = definition_name.clone();
                        schemas.push(schema);
                    }
                    if internal_schemas.len().gt(&1) {
                        if internal_schemas.len().eq(&2)
                            && internal_schemas
                                .iter()
                                .any(|s| s.type_name.eq(&TypeName::Literal))
                            && !internal_schemas
                                .iter()
                                .all(|s| s.type_name.eq(&TypeName::Literal))
                        {
                            let reference = internal_schemas
                                .iter()
                                .find(|s| s.type_name.ne(&TypeName::Literal));
                            schemas.push(reference.unwrap().clone());
                            schemas.push(schema::Schema::new_nullable(
                                definition_name.clone(),
                                reference.unwrap().name.clone(),
                                definition_json.clone(),
                            ));
                        } else {
                            for schema in &internal_schemas {
                                schemas.push(schema.clone());
                            }
                            schemas.push(schema::Schema::new_enum(
                                definition_name.clone(),
                                &internal_schemas,
                                definition_json.clone(),
                            ));
                        }
                    }
                }
            }
        }

        schemas
    }

    /// Obtains the list of validators from a given `Blueprint`.
    ///
    /// # Arguments
    ///
    /// * `blueprint` - A `Blueprint` from which to obtain the validators.
    ///
    /// # Returns
    ///
    /// A vector of `Validator` from the blueprint.
    pub fn get_validators_from_blueprint(
        &self,
        blueprint: blueprint::Blueprint,
    ) -> Vec<schema::Validator> {
        let mut validators: Vec<schema::Validator> = vec![];
        for validator in blueprint.validators.iter() {
            let mut datum: Option<schema::Reference> = None;
            if validator.datum.is_some()
                && validator.datum.as_ref().unwrap().schema.reference.is_some()
            {
                datum = Some(schema::Reference {
                    name: validator.datum.as_ref().unwrap().title.clone(),
                    schema_name: self.get_schema_name(
                        validator
                            .datum
                            .as_ref()
                            .unwrap()
                            .schema
                            .reference
                            .as_ref()
                            .unwrap()
                            .clone(),
                    ),
                });
            }
            let mut redeemer: Option<schema::Reference> = None;
            if validator.redeemer.is_some()
                && validator
                    .redeemer
                    .as_ref()
                    .unwrap()
                    .schema
                    .reference
                    .is_some()
            {
                redeemer = Some(schema::Reference {
                    name: validator.redeemer.as_ref().unwrap().title.clone(),
                    schema_name: self.get_schema_name(
                        validator
                            .redeemer
                            .as_ref()
                            .unwrap()
                            .schema
                            .reference
                            .as_ref()
                            .unwrap()
                            .clone(),
                    ),
                });
            }
            let mut parameters: Vec<schema::Reference> = vec![];
            if let Some(p) = &validator.parameters {
                for parameter in p {
                    if parameter.schema.reference.is_some() {
                        parameters.push(schema::Reference {
                            name: parameter.title.clone(),
                            schema_name: self.get_schema_name(
                                parameter.schema.reference.as_ref().unwrap().clone(),
                            ),
                        })
                    }
                }
            }
            validators.push(schema::Validator {
                name: validator.title.clone(),
                datum: datum,
                redeemer: redeemer,
                parameters: parameters,
            });
        }
        validators
    }

    /// Converts a `Blueprint` into a JSON value containing extracted schemas and validators.


    /// Convert a JSON value according to a schema name into a tx3 IR Expression.
    /// This expects `value` to match the referenced schema (by name) present in the blueprint definitions.
    pub fn convert_value_by_schema_name(
        &self,
        blueprint: &blueprint::Blueprint,
        schema_name: &str,
        value: &serde_json::Value,
    ) -> Result<Expression> {
        // Build a quick lookup of definitions by normalized schema name
        let mut defs = std::collections::BTreeMap::new();
        if let Some(all) = &blueprint.definitions {
            for (raw_name, def) in &all.inner {
                let name = self.get_schema_name(raw_name.clone());
                defs.insert(name, def);
            }
        }

        let def = defs
            .get(schema_name)
            .ok_or_else(|| anyhow::anyhow!("unknown schema: {}", schema_name))?;

        // Primitive types
        if let Some(dt) = def.data_type {
            match dt {
                blueprint::DataType::Integer => {
                    let num = match value {
                        serde_json::Value::Number(n) => n.as_i64().unwrap_or(0) as i128,
                        serde_json::Value::String(s) => s.parse::<i128>().unwrap_or(0),
                        _ => 0,
                    };
                    return Ok(Expression::Number(num));
                }
                blueprint::DataType::Bytes => {
                    let bytes = match value {
                        serde_json::Value::String(s) => Self::parse_bytes_string(s),
                        serde_json::Value::Array(arr) => arr
                            .iter()
                            .filter_map(|v| v.as_u64().map(|x| x as u8))
                            .collect(),
                        _ => Vec::new(),
                    };
                    return Ok(Expression::Bytes(bytes));
                }
                blueprint::DataType::List => {
                    // For lists, expect a single reference in items (or tuple in Array case already handled elsewhere)
                    if let Some(items) = &def.items {
                        match items {
                            blueprint::ReferencesArray::Single(r) => {
                                let inner_name = r
                                    .reference
                                    .as_ref()
                                    .map(|s| self.get_schema_name(s.clone()))
                                    .ok_or_else(|| anyhow::anyhow!("list items missing $ref"))?;
                                let list = match value {
                                    serde_json::Value::Array(arr) => {
                                        let mut out = Vec::new();
                                        for v in arr {
                                            out.push(self.convert_value_by_schema_name(
                                                blueprint,
                                                &inner_name,
                                                v,
                                            )?);
                                        }
                                        out
                                    }
                                    _ => Vec::new(),
                                };
                                return Ok(Expression::List(list));
                            }
                            blueprint::ReferencesArray::Array(_) => {
                                // Treat as tuple
                                let arr = value.as_array().cloned().unwrap_or_default();
                                let mut fields = Vec::new();
                                if let blueprint::ReferencesArray::Array(refs) = items {
                                    for (i, r) in refs.iter().enumerate() {
                                        let inner_name = r
                                            .reference
                                            .as_ref()
                                            .map(|s| self.get_schema_name(s.clone()))
                                            .ok_or_else(|| {
                                                anyhow::anyhow!("tuple item missing $ref")
                                            })?;
                                        let v =
                                            arr.get(i).cloned().unwrap_or(serde_json::Value::Null);
                                        fields.push(self.convert_value_by_schema_name(
                                            blueprint,
                                            &inner_name,
                                            &v,
                                        )?);
                                    }
                                }
                                return Ok(Expression::Struct(StructExpr {
                                    constructor: 0,
                                    fields,
                                }));
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Constructors (anyOf with data_type = Constructor). We choose variant by matching expected shape or `index`.
        if let Some(any_of) = &def.any_of {
            // Strategy: if `value` is an object with fields, pick matching constructor by field count; else use first.
            let chosen = any_of
                .first()
                .ok_or_else(|| anyhow::anyhow!("empty anyOf"))?;
            let index = chosen.index.as_i64().unwrap_or(0) as usize;
            let mut fields_expr = Vec::new();
            for f in &chosen.fields {
                let ref_name = self.get_schema_name(f.reference.clone());
                // Try to fetch field by title from value object; fallback to Null
                let field_json = match value {
                    serde_json::Value::Object(map) => {
                        if let Some(title) = &f.title {
                            map.get(title).cloned().unwrap_or(serde_json::Value::Null)
                        } else {
                            serde_json::Value::Null
                        }
                    }
                    _ => serde_json::Value::Null,
                };
                fields_expr.push(self.convert_value_by_schema_name(
                    blueprint,
                    &ref_name,
                    &field_json,
                )?);
            }
            return Ok(Expression::Struct(StructExpr {
                constructor: index,
                fields: fields_expr,
            }));
        }

        // Fallback
        Ok(Expression::None)
    }

// (no free functions)
}
