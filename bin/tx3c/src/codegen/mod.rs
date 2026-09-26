//! `tx3c codegen`: renders a template directory against a TII document.
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
pub struct Args {
    /// Path to the TII JSON file
    #[arg(long)]
    pub tii: PathBuf,

    /// Path to the template directory
    #[arg(long)]
    pub template: PathBuf,

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

pub fn run(args: Args) -> Result<()> {
    let tii_contents = std::fs::read_to_string(&args.tii)
        .with_context(|| format!("reading TII file {}", args.tii.display()))?;
    let tii: Value = serde_json::from_str(&tii_contents)
        .with_context(|| format!("parsing TII file {}", args.tii.display()))?;

    let mut handlebars = Handlebars::new();
    helpers::register(&mut handlebars);

    let static_files = register_templates(&mut handlebars, &args.template)?;

    std::fs::create_dir_all(&args.output)
        .with_context(|| format!("creating output dir {}", args.output.display()))?;

    let data = serde_json::json!({
        "tii": tii,
    });

    render_templates(&handlebars, &data, &args.output)?;
    copy_static_files(&static_files, &args.output)?;

    println!(
        "Generated code from {} into {}",
        args.tii.display(),
        args.output.display()
    );

    Ok(())
}
