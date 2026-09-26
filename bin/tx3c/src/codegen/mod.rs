//! `tx3c codegen`: renders client code for a TII document.
//!
//! `--language <language>` renders tx3c's client for that language, laid out by
//! the client templates its backend lists. `--template <dir>` renders custom
//! templates with the same helpers.
//!
//! Type rendering is split into layers so every language shares one
//! traversal: [`schema`] parses JSON Schema nodes into shapes, [`plan`] turns
//! shapes into named declarations, and each module under [`backend`] only
//! spells them. [`helpers`] exposes the result to Handlebars templates.

use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use clap::Parser;
use handlebars::Handlebars;
use serde_json::Value;
use walkdir::WalkDir;

mod backend;
mod helpers;
mod names;
mod plan;
mod schema;

#[derive(Parser)]
#[command(group(clap::ArgGroup::new("source").required(true).args(["language", "template"])))]
pub struct Args {
    /// Path to the TII JSON file
    #[arg(long)]
    pub tii: PathBuf,

    /// Render tx3c's client for this language (rust, typescript, python, go)
    #[arg(long)]
    pub language: Option<String>,

    /// Render custom templates from this directory instead
    #[arg(long)]
    pub template: Option<PathBuf>,

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

/// Registers the client templates for `language`. Every client template file
/// is text; non-`.hbs` files are returned as static files to write verbatim.
fn register_client(
    handlebars: &mut Handlebars<'_>,
    language: &str,
) -> Result<Vec<(&'static str, &'static str)>> {
    let mut static_files = Vec::new();
    for file in backend::client_templates(language)? {
        match file.path.strip_suffix(".hbs") {
            Some(template_name) => handlebars
                .register_template_string(template_name, file.content)
                .with_context(|| format!("registering client template {template_name}"))?,
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

    match (&args.language, &args.template) {
        (Some(language), _) => {
            let static_files = register_client(&mut handlebars, language)?;
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
        (None, Some(template)) => {
            let static_files = register_templates(&mut handlebars, template)?;
            create_output(&args.output)?;
            render_templates(&handlebars, &data, &args.output)?;
            copy_static_files(&static_files, &args.output)?;
        }
        (None, None) => unreachable!("clap requires --language or --template"),
    }

    println!(
        "Generated code from {} into {}",
        args.tii.display(),
        args.output.display()
    );

    Ok(())
}
