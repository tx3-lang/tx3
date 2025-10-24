//! Validation rules for semantic analysis.
//!
//! This module contains helper functions that implement specific validation
//! rules for the Tx3 language. These rules are used by the semantic analyzer
//! to check constraints and produce appropriate errors.

use crate::analyzing::{
    Error, MetadataInvalidKeyTypeError, MetadataSizeLimitError, OptionalOutputError,
};
use crate::ast::{DataExpr, OutputBlock, Type};
use crate::parsing::AstNode;

const METADATA_MAX_SIZE_BYTES: usize = 64;

pub fn validate_optional_output(output: &OutputBlock) -> Option<Error> {
    if output.optional && output.find("datum").is_some() {
        return Some(Error::InvalidOptionalOutput(OptionalOutputError {
            name: output
                .name
                .as_ref()
                .map(|i| i.value.clone())
                .unwrap_or_else(|| "<anonymous>".to_string()),
            src: None,
            span: output.span.clone(),
        }));
    }

    None
}

pub fn validate_metadata_value_size(expr: &DataExpr) -> Result<(), MetadataSizeLimitError> {
    match expr {
        DataExpr::String(string_literal) => {
            let utf8_bytes = string_literal.value.as_bytes();
            if utf8_bytes.len() > METADATA_MAX_SIZE_BYTES {
                return Err(MetadataSizeLimitError {
                    size: utf8_bytes.len(),
                    src: None,
                    span: string_literal.span.clone(),
                });
            }
        }
        DataExpr::HexString(hex_literal) => {
            let hex_str = &hex_literal.value;
            let hex_str = hex_str.strip_prefix("0x").unwrap_or(hex_str);
            let byte_length = hex_str.len() / 2;

            if byte_length > METADATA_MAX_SIZE_BYTES {
                return Err(MetadataSizeLimitError {
                    size: byte_length,
                    src: None,
                    span: hex_literal.span.clone(),
                });
            }
        }
        _ => {}
    }
    Ok(())
}

pub fn validate_metadata_key_type(
    expr: &DataExpr,
    target_type: Option<&Type>,
) -> Result<(), MetadataInvalidKeyTypeError> {
    match expr {
        DataExpr::Number(_) => Ok(()),
        DataExpr::Identifier(id) => match target_type {
            Some(Type::Int) => Ok(()),
            Some(other_type) => Err(MetadataInvalidKeyTypeError {
                key_type: format!("identifier of type {}", other_type),
                src: None,
                span: id.span().clone(),
            }),
            None => Err(MetadataInvalidKeyTypeError {
                key_type: "unresolved identifier".to_string(),
                src: None,
                span: id.span().clone(),
            }),
        },
        _ => {
            let key_type = match expr {
                DataExpr::String(_) => "string",
                DataExpr::HexString(_) => "hex string",
                DataExpr::ListConstructor(_) => "list",
                DataExpr::MapConstructor(_) => "map",
                DataExpr::StructConstructor(_) => "struct",
                _ => "unknown",
            };

            Err(MetadataInvalidKeyTypeError {
                key_type: key_type.to_string(),
                src: None,
                span: expr.span().clone(),
            })
        }
    }
}
