//! `tx3c codegen`: renders a template against a TII document.
//!
//! A template is one use case in one language, such as `rust-client`.
//! `--template <name>` renders a built-in template from [`templates`];
//! `--template <dir>` renders a custom template directory with the same
//! helpers.
//!
//! Type rendering is split into layers so every language shares one
//! traversal: [`schema`] parses JSON Schema nodes into shapes, [`plan`] turns
//! shapes into named declarations, and each module under [`backend`] only
//! spells them. [`helpers`] exposes the result to Handlebars templates.

use std::path::{Path, PathBuf};

use anyhow::{bail, Context, Result};
use clap::Parser;
use handlebars::Handlebars;
use serde_json::Value;
use walkdir::WalkDir;

mod backend;
mod helpers;
mod names;
mod plan;
mod schema;
mod templates;

#[derive(Parser)]
pub struct Args {
    /// Path to the TII JSON file
    #[arg(long)]
    pub tii: PathBuf,

    /// Template to render: a built-in template name (ts-client, rust-client,
    /// python-client, go-client) or a path to a custom template directory.
    /// A bare name is looked up among the built-in templates first; use
    /// `./name` to force a directory.
    #[arg(long)]
    pub template: String,

    /// Output directory for rendered templates
    #[arg(short, long)]
    pub output: PathBuf,
}

fn register_templates(
    handlebars: &mut Handlebars<'_>,
    template_dir: &Path,
) -> Result<Vec<(PathBuf, PathBuf)>> {
    let mut static_files = Vec::new();

    for entry in WalkDir::new(template_dir) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }

        let path = entry.path();
        let relative = path.strip_prefix(template_dir).context("template path")?;
        let relative_str = relative.to_string_lossy();

        if relative_str.ends_with(".hbs") {
            let template_name = relative_str.trim_end_matches(".hbs");
            let content = std::fs::read_to_string(path)?;
            handlebars
                .register_template_string(template_name, content)
                .with_context(|| format!("registering template {template_name}"))?;
        } else {
            static_files.push((path.to_path_buf(), relative.to_path_buf()));
        }
    }

    Ok(static_files)
}

/// Where a `--template` value points.
enum TemplateSource {
    BuiltIn(&'static [templates::File]),
    Directory(PathBuf),
}

/// Resolves a `--template` value. A bare name, with no path separator and no
/// leading `.`, is a built-in template if one has that name; anything else is
/// a directory.
fn resolve_template(value: &str) -> Result<TemplateSource> {
    let bare_name = !value.starts_with('.')
        && !value.contains('/')
        && !value.contains(std::path::MAIN_SEPARATOR);

    if bare_name {
        if let Some(files) = templates::built_in(value) {
            return Ok(TemplateSource::BuiltIn(files));
        }
    }

    let path = PathBuf::from(value);
    if path.is_dir() {
        return Ok(TemplateSource::Directory(path));
    }
    if bare_name {
        bail!(
            "unknown template `{value}`: it is neither a built-in template ({}) nor a directory",
            templates::names()
        );
    }
    bail!("template directory `{value}` does not exist")
}

/// Registers a built-in template. Every built-in file is text; non-`.hbs`
/// files are returned as static files to write verbatim.
fn register_built_in(
    handlebars: &mut Handlebars<'_>,
    files: &'static [templates::File],
) -> Result<Vec<(&'static str, &'static str)>> {
    let mut static_files = Vec::new();
    for file in files {
        match file.path.strip_suffix(".hbs") {
            Some(template_name) => handlebars
                .register_template_string(template_name, file.content)
                .with_context(|| format!("registering template {template_name}"))?,
            None => static_files.push((file.path, file.content)),
        }
    }
    Ok(static_files)
}

fn render_templates(handlebars: &Handlebars<'_>, data: &Value, output_dir: &Path) -> Result<()> {
    for name in handlebars.get_templates().keys() {
        let rendered = handlebars
            .render(name, data)
            .with_context(|| format!("rendering template {name}"))?;
        if rendered.is_empty() {
            continue;
        }

        let output_path = output_dir.join(name);
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&output_path, rendered)?;
    }

    Ok(())
}

fn copy_static_files(static_files: &[(PathBuf, PathBuf)], output_dir: &Path) -> Result<()> {
    for (src, relative) in static_files {
        let dest_path = output_dir.join(relative);
        if let Some(parent) = dest_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::copy(src, dest_path)?;
    }

    Ok(())
}

fn create_output(output: &Path) -> Result<()> {
    std::fs::create_dir_all(output)
        .with_context(|| format!("creating output dir {}", output.display()))
}

pub fn run(args: Args) -> Result<()> {
    let tii_contents = std::fs::read_to_string(&args.tii)
        .with_context(|| format!("reading TII file {}", args.tii.display()))?;
    let tii: Value = serde_json::from_str(&tii_contents)
        .with_context(|| format!("parsing TII file {}", args.tii.display()))?;

    let mut handlebars = Handlebars::new();
    helpers::register(&mut handlebars);

    let data = serde_json::json!({
        "tii": tii,
    });

    match resolve_template(&args.template)? {
        TemplateSource::BuiltIn(files) => {
            let static_files = register_built_in(&mut handlebars, files)?;
            create_output(&args.output)?;
            render_templates(&handlebars, &data, &args.output)?;
            for (relative, content) in static_files {
                let dest_path = args.output.join(relative);
                if let Some(parent) = dest_path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                std::fs::write(dest_path, content)?;
            }
        }
        TemplateSource::Directory(template) => {
            let static_files = register_templates(&mut handlebars, &template)?;
            create_output(&args.output)?;
            render_templates(&handlebars, &data, &args.output)?;
            copy_static_files(&static_files, &args.output)?;
        }
    }

    println!(
        "Generated code from {} into {}",
        args.tii.display(),
        args.output.display()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn resolved(value: &str) -> std::result::Result<&'static str, String> {
        match resolve_template(value) {
            Ok(TemplateSource::BuiltIn(_)) => Ok("built-in"),
            Ok(TemplateSource::Directory(_)) => Ok("directory"),
            Err(error) => Err(error.to_string()),
        }
    }

    #[test]
    fn template_values_resolve_to_built_ins_or_directories() {
        let custom = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/codegen/custom/java");
        let custom = custom.to_str().unwrap();

        assert_eq!(resolved("rust-client"), Ok("built-in"));
        assert_eq!(resolved(custom), Ok("directory"));
        assert_eq!(resolved("."), Ok("directory"));
        assert_eq!(
            resolved("kotlin-client"),
            Err(
                "unknown template `kotlin-client`: it is neither a built-in template \
                 (ts-client, rust-client, python-client, go-client) nor a directory"
                    .to_string()
            )
        );
        assert_eq!(
            resolved("./rust-client"),
            Err("template directory `./rust-client` does not exist".to_string())
        );
    }
}
