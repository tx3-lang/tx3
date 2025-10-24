//! Validation rules for semantic analysis.
//!
//! This module contains helper functions that implement specific validation
//! rules for the Tx3 language. These rules are used by the semantic analyzer
//! to check constraints and produce appropriate errors.

use crate::analyzing::{Error, OptionalOutputError};
use crate::ast::OutputBlock;

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
