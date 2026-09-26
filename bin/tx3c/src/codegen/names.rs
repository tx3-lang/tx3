//! Identifier normalization shared by every backend.
//!
//! A backend supplies only data: the case used for each identifier role, its
//! reserved words, and which leading characters need an underscore. The rules
//! for applying them, and for rejecting collisions, live here once.

use std::collections::BTreeMap;

use anyhow::{bail, Result};
use convert_case::{Case, Casing};

use super::backend::Backend;

/// The position an identifier occupies in generated code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    /// Declared type names: records, tuples, variants, params.
    Type,
    /// Record and variant-case members.
    Field,
    /// Members of a transaction params declaration.
    Param,
    /// Variant case names.
    Case,
    /// Generated methods and functions, such as per-transaction builders.
    Method,
    /// Generated constants.
    Constant,
}

impl Role {
    pub const NAMES: &'static [&'static str] =
        &["type", "field", "param", "case", "method", "constant"];

    pub fn parse(name: &str) -> Option<Self> {
        match name {
            "type" => Some(Self::Type),
            "field" => Some(Self::Field),
            "param" => Some(Self::Param),
            "case" => Some(Self::Case),
            "method" => Some(Self::Method),
            "constant" => Some(Self::Constant),
            _ => None,
        }
    }
}

/// Normalizes `source` for `role`, then escapes it for the backend.
pub fn identifier(backend: &dyn Backend, source: &str, role: Role) -> Result<String> {
    let normalized = match backend.naming(role) {
        Some(case) => source.to_case(case),
        None => source.to_string(),
    };
    escape(backend, source, normalized)
}

/// Normalizes `source` to an explicit case, then escapes it for the backend.
/// Used to compose names of nested declarations from their parts.
pub fn identifier_in(backend: &dyn Backend, source: &str, case: Case) -> Result<String> {
    escape(backend, source, source.to_case(case))
}

fn escape(backend: &dyn Backend, source: &str, mut normalized: String) -> Result<String> {
    if normalized.is_empty() {
        bail!(
            "{} identifier `{source}` is empty after normalization",
            backend.display_name()
        );
    }
    if normalized
        .chars()
        .next()
        .is_some_and(|first| backend.needs_leading_underscore(first))
    {
        normalized.insert(0, '_');
    }
    if backend.keywords().contains(&normalized.as_str()) {
        normalized.push('_');
    }
    Ok(normalized)
}

/// A namespace in which distinct source identifiers must stay distinct after
/// normalization. A collision is an error naming both sources; it is never
/// silently disambiguated.
pub struct Scope {
    label: String,
    claimed: BTreeMap<String, String>,
}

impl Scope {
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            label: label.into(),
            claimed: BTreeMap::new(),
        }
    }

    pub fn claim(&mut self, backend: &dyn Backend, source: &str, normalized: &str) -> Result<()> {
        if let Some(previous) = self.claimed.get(normalized) {
            let (first, second) = if previous.as_str() <= source {
                (previous.as_str(), source)
            } else {
                (source, previous.as_str())
            };
            bail!(
                "{} identifier collision in {}: source identifiers `{first}` and `{second}` both normalize to `{normalized}`",
                backend.display_name(),
                self.label
            );
        }
        self.claimed
            .insert(normalized.to_string(), source.to_string());
        Ok(())
    }
}
