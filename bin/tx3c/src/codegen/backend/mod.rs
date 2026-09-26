//! Per-language backends.
//!
//! A backend is data plus syntax: scalar and builtin tables, naming policy,
//! how a planned [`Declaration`] is spelled, and the client templates that
//! lay out the generated client. Traversal, naming of nested declarations, and
//! collision checks live in the shared planner. Adding a language means adding
//! one module here and registering it in [`for_language`]; giving it a client
//! means adding `bin/tx3c/templates/<language>/` and listing the files in its
//! [`Backend::client_templates`].

use anyhow::{bail, Result};
use convert_case::Case;

use super::{
    names::Role,
    plan::{Declaration, Usage},
    schema::{Builtin, Scalar, Shape},
};

/// One client template file, addressed by its path relative to the
/// language's template directory.
pub struct TemplateFile {
    pub path: &'static str,
    pub content: &'static str,
}

/// Lists a language's client templates from `bin/tx3c/templates/<language>/`
/// and compiles their contents into tx3c.
macro_rules! client_templates {
    ($language:literal: $($path:literal),+ $(,)?) => {
        &[$($crate::codegen::backend::TemplateFile {
            path: $path,
            content: include_str!(concat!("../../../templates/", $language, "/", $path)),
        }),+]
    };
}

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
    /// The templates that lay out this language's generated client, rendered
    /// by `tx3c codegen --language`. Empty when tx3c has no client for the
    /// language yet; its type mapping still serves custom templates.
    fn client_templates(&self) -> &'static [TemplateFile] {
        &[]
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

/// The client templates for `language`, or an error naming the languages
/// tx3c generates clients for.
pub fn client_templates(language: &str) -> Result<&'static [TemplateFile]> {
    let with_client: Vec<_> = BACKENDS
        .iter()
        .filter(|backend| !backend.client_templates().is_empty())
        .map(|backend| backend.language())
        .collect();

    match BACKENDS
        .iter()
        .find(|backend| backend.language() == language)
    {
        Some(backend) if !backend.client_templates().is_empty() => Ok(backend.client_templates()),
        Some(_) => bail!(
            "tx3c has no client templates for `{language}` yet; its type backend is available \
             to custom templates through --template. Languages with a client: {}",
            with_client.join(", ")
        ),
        None => bail!(
            "unknown codegen language `{language}`; languages with a client: {}",
            with_client.join(", ")
        ),
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

#[cfg(test)]
mod tests {
    use std::{collections::BTreeMap, fs, path::Path};

    use super::*;

    /// Every template directory must belong to a language whose backend lists
    /// exactly the files on disk, so a template added without being listed
    /// fails the tests.
    #[test]
    fn client_templates_match_the_template_directories() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("templates");

        let mut on_disk: Vec<String> = fs::read_dir(&root)
            .unwrap()
            .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        on_disk.sort();
        let mut with_client: Vec<String> = BACKENDS
            .iter()
            .filter(|backend| !backend.client_templates().is_empty())
            .map(|backend| backend.language().to_string())
            .collect();
        with_client.sort();
        assert_eq!(with_client, on_disk, "template directories");

        for language in &with_client {
            let dir = root.join(language);
            let expected: BTreeMap<String, String> = walkdir::WalkDir::new(&dir)
                .into_iter()
                .map(Result::unwrap)
                .filter(|entry| entry.file_type().is_file())
                .map(|entry| {
                    let relative = entry.path().strip_prefix(&dir).unwrap();
                    (
                        relative.to_string_lossy().replace('\\', "/"),
                        fs::read_to_string(entry.path()).unwrap(),
                    )
                })
                .collect();
            let listed: BTreeMap<String, String> = client_templates(language)
                .unwrap()
                .iter()
                .map(|file| (file.path.to_string(), file.content.to_string()))
                .collect();
            assert_eq!(listed, expected, "client templates for {language}");
        }
    }

    #[test]
    fn languages_without_a_client_explain_the_alternative() {
        let error = client_templates("java").err().unwrap().to_string();
        assert!(
            error.contains("no client templates for `java` yet"),
            "{error}"
        );

        let error = client_templates("cobol").err().unwrap().to_string();
        assert_eq!(
            error,
            "unknown codegen language `cobol`; languages with a client: rust, typescript, python, go"
        );
    }
}
