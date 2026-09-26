//! Per-language backends.
//!
//! A backend is data plus syntax: scalar and builtin tables, naming policy,
//! and how a planned [`Declaration`] is spelled. Traversal, naming of nested
//! declarations, and collision checks live in the shared planner. Adding a
//! language means adding one module here and registering it in [`for_language`].

use anyhow::{bail, Result};
use convert_case::Case;

use super::{
    names::Role,
    plan::{Declaration, Usage},
    schema::{Builtin, Scalar, Shape},
};

mod go;
mod java;
mod python;
mod rust;
mod swift;
mod typescript;

/// Where a backend puts declarations for anonymous nested shapes, such as a
/// tuple-typed field.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Placement {
    /// Nested shapes are not declared; they use [`Backend::undeclared`].
    None,
    /// Nested declarations live inside their parent declaration.
    Nested,
    /// Nested declarations are emitted at top level, before their parent.
    Hoisted,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FieldOrder {
    /// Declaration order, as recorded by the schema's `required` list.
    Declared,
    /// Sorted by source name.
    Alphabetical,
}

pub trait Backend: Sync {
    /// Language name used by templates, such as `"rust"`.
    fn language(&self) -> &'static str;
    /// Human-readable name used in error messages.
    fn display_name(&self) -> &'static str;

    fn scalar(&self, scalar: Scalar) -> &'static str;
    fn builtin(&self, builtin: Builtin) -> &'static str;
    /// The SDK's catch-all type for shapes without a native mapping.
    fn fallback(&self) -> &'static str;
    fn list(&self, item: &str) -> String;
    fn map(&self, value: &str) -> String;
    /// How a compound shape is spelled when it has no declaration of its own.
    fn undeclared(&self, _shape: &Shape) -> String {
        self.fallback().to_string()
    }

    /// Case applied for `role`. `None` keeps the source identifier verbatim.
    fn naming(&self, role: Role) -> Option<Case>;
    fn keywords(&self) -> &'static [&'static str] {
        &[]
    }
    fn needs_leading_underscore(&self, first: char) -> bool {
        first.is_ascii_digit()
    }

    fn placement(&self) -> Placement;
    fn field_order(&self) -> FieldOrder {
        FieldOrder::Declared
    }
    /// Spells one top-level declaration. Backends with
    /// [`Placement::Nested`] also spell its nested declarations.
    fn declaration(&self, declaration: &Declaration) -> String;
    /// Joins rendered top-level declarations into one block.
    fn join(&self, rendered: Vec<String>) -> String {
        rendered
            .into_iter()
            .map(|declaration| format!("{declaration}\n"))
            .collect()
    }
    /// Import lines needed by the planned declarations.
    fn imports(&self, _usage: &Usage) -> String {
        String::new()
    }
}

static BACKENDS: &[&dyn Backend] = &[
    &rust::Rust,
    &typescript::TypeScript,
    &python::Python,
    &go::Go,
    &java::Java,
    &swift::Swift,
];

pub fn for_language(language: &str) -> Result<&'static dyn Backend> {
    match BACKENDS
        .iter()
        .find(|backend| backend.language() == language)
    {
        Some(backend) => Ok(*backend),
        None => {
            let known: Vec<_> = BACKENDS.iter().map(|backend| backend.language()).collect();
            bail!(
                "unknown codegen language `{language}`; expected one of: {}",
                known.join(", ")
            )
        }
    }
}

/// Leading whitespace for a declaration nested `depth` columns deep.
pub(super) fn indent(depth: usize) -> String {
    " ".repeat(depth)
}

/// Spells an undeclared tuple as a list of the fallback type, which is how
/// the original four backends have always typed anonymous tuples.
pub(super) fn tuple_as_fallback_list(backend: &(impl Backend + ?Sized), shape: &Shape) -> String {
    match shape {
        Shape::Tuple(_) => backend.list(backend.fallback()),
        _ => backend.fallback().to_string(),
    }
}
