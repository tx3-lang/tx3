//! `tx3c codegen`: renders a template against a TII document.
//!
//! A template is one use case in one language, such as `rust-client`.
//! `--template <name>` renders a built-in template from [`templates`];
//! `--template <dir>` renders a custom template directory with the same
//! helpers.
//!
//! A template file's relative path is itself a Handlebars template, rendered
//! with the same helpers and data as the file's contents, so a template can
//! lay its output out by protocol name (`Sources/{{pascalCase
//! tii.protocol.name}}Client/`). Paths without expressions render unchanged.
//!
//! Type rendering is split into layers so every language shares one
//! traversal: [`schema`] parses JSON Schema nodes into shapes, [`plan`] turns
//! shapes into named declarations, and each module under [`backend`] only
//! spells them. [`helpers`] exposes the result to Handlebars templates.

use std::{
    collections::BTreeMap,
    path::{Component, Path, PathBuf},
};

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
    /// python-client, go-client, swift-client) or a path to a custom template
    /// directory.
    /// A bare name is looked up among the built-in templates first; use
    /// `./name` to force a directory.
    #[arg(long)]
    pub template: String,

    /// Output directory for rendered templates
    #[arg(short, long)]
    pub output: PathBuf,
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

/// What a template file writes to its output path.
enum Content {
    /// A `.hbs` file, registered with Handlebars under its source path and
    /// rendered against the TII.
    Rendered,
    /// A static file compiled into tx3c, written verbatim.
    Text(&'static str),
    /// A static file in a custom template directory, copied verbatim.
    Copy(PathBuf),
}

/// One file of a template.
struct TemplateFile {
    /// The file's path relative to the template root, with `/` separators,
    /// as written in the template. It names the file in errors.
    source: String,
    content: Content,
}

impl TemplateFile {
    /// The output path template: the source path without its `.hbs` suffix.
    fn path_template(&self) -> &str {
        self.source.strip_suffix(".hbs").unwrap_or(&self.source)
    }
}

/// Registers a template's `.hbs` files with Handlebars, under their source
/// paths, and lists every file of the template in source order.
fn load_built_in(
    handlebars: &mut Handlebars<'_>,
    files: &'static [templates::File],
) -> Result<Vec<TemplateFile>> {
    files
        .iter()
        .map(|file| {
            let content = if file.path.ends_with(".hbs") {
                handlebars
                    .register_template_string(file.path, file.content)
                    .with_context(|| format!("registering template {}", file.path))?;
                Content::Rendered
            } else {
                Content::Text(file.content)
            };
            Ok(TemplateFile {
                source: file.path.to_string(),
                content,
            })
        })
        .collect()
}

/// Registers a custom template directory's `.hbs` files with Handlebars,
/// under their source paths, and lists every file of the directory in path
/// order.
fn load_directory(
    handlebars: &mut Handlebars<'_>,
    template_dir: &Path,
) -> Result<Vec<TemplateFile>> {
    let mut files = Vec::new();

    for entry in WalkDir::new(template_dir).sort_by_file_name() {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }

        let path = entry.path();
        let relative = path.strip_prefix(template_dir).context("template path")?;
        let source = relative
            .components()
            .map(|component| component.as_os_str().to_string_lossy())
            .collect::<Vec<_>>()
            .join("/");

        let content = if source.ends_with(".hbs") {
            let content = std::fs::read_to_string(path)
                .with_context(|| format!("reading template {}", path.display()))?;
            handlebars
                .register_template_string(&source, content)
                .with_context(|| format!("registering template {source}"))?;
            Content::Rendered
        } else {
            Content::Copy(path.to_path_buf())
        };
        files.push(TemplateFile { source, content });
    }

    Ok(files)
}

/// Renders a template file's output path. A path without a Handlebars
/// expression is its own rendering, so existing templates keep their layout.
fn render_output_path(
    handlebars: &Handlebars<'_>,
    file: &TemplateFile,
    data: &Value,
) -> Result<String> {
    let template = file.path_template();
    let rendered = if template.contains("{{") {
        handlebars
            .render_template(template, data)
            .with_context(|| {
                format!(
                    "rendering the output path of template file `{}`",
                    file.source
                )
            })?
    } else {
        template.to_string()
    };
    validate_output_path(&file.source, &rendered)?;
    Ok(rendered)
}

/// Checks that a rendered output path stays inside the output directory: it
/// is relative, non-empty, and has no empty, `.` or `..` segment.
fn validate_output_path(source: &str, rendered: &str) -> Result<()> {
    if rendered.is_empty() {
        bail!("template file `{source}` renders to an empty output path");
    }

    let path = Path::new(rendered);
    if path.has_root()
        || path
            .components()
            .any(|component| matches!(component, Component::Prefix(_)))
    {
        bail!(
            "template file `{source}` renders to the absolute output path `{rendered}`; \
             output paths are relative to the output directory"
        );
    }

    for segment in rendered.split(['/', std::path::MAIN_SEPARATOR]) {
        match segment {
            "" => bail!(
                "template file `{source}` renders to the output path `{rendered}`, \
                 which has an empty segment"
            ),
            "." | ".." => bail!(
                "template file `{source}` renders to the output path `{rendered}`, \
                 which has a `{segment}` segment; output paths stay inside the output directory"
            ),
            _ => {}
        }
    }

    Ok(())
}

/// Renders every file's output path, rejecting two files that render to the
/// same path. The result is in output-path order.
fn resolve_output_paths<'a>(
    handlebars: &Handlebars<'_>,
    files: &'a [TemplateFile],
    data: &Value,
) -> Result<BTreeMap<String, &'a TemplateFile>> {
    let mut outputs = BTreeMap::new();
    for file in files {
        let output = render_output_path(handlebars, file, data)?;
        if let Some(previous) = outputs.insert(output.clone(), file) {
            bail!(
                "template files `{}` and `{}` both render to the output path `{output}`",
                previous.source,
                file.source
            );
        }
    }
    Ok(outputs)
}

/// Writes a template's files into the output directory: rendered files whose
/// rendering is empty are skipped, static files are written verbatim.
fn write_output(
    handlebars: &Handlebars<'_>,
    files: &[TemplateFile],
    data: &Value,
    output_dir: &Path,
) -> Result<()> {
    let outputs = resolve_output_paths(handlebars, files, data)?;

    std::fs::create_dir_all(output_dir)
        .with_context(|| format!("creating output dir {}", output_dir.display()))?;

    for (output, file) in outputs {
        let output_path = output_dir.join(&output);
        let write = || -> Result<()> {
            match &file.content {
                Content::Rendered => {
                    let rendered = handlebars
                        .render(&file.source, data)
                        .with_context(|| format!("rendering template {}", file.source))?;
                    if rendered.is_empty() {
                        return Ok(());
                    }
                    create_parent(&output_path)?;
                    std::fs::write(&output_path, rendered)?;
                }
                Content::Text(content) => {
                    create_parent(&output_path)?;
                    std::fs::write(&output_path, content)?;
                }
                Content::Copy(src) => {
                    create_parent(&output_path)?;
                    std::fs::copy(src, &output_path)?;
                }
            }
            Ok(())
        };
        write().with_context(|| format!("writing {}", output_path.display()))?;
    }

    Ok(())
}

fn create_parent(path: &Path) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
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

    let data = serde_json::json!({
        "tii": tii,
    });

    let files = match resolve_template(&args.template)? {
        TemplateSource::BuiltIn(files) => load_built_in(&mut handlebars, files)?,
        TemplateSource::Directory(template) => load_directory(&mut handlebars, &template)?,
    };
    write_output(&handlebars, &files, &data, &args.output)?;

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
                 (ts-client, rust-client, python-client, go-client, swift-client) nor a \
                 directory"
                    .to_string()
            )
        );
        assert_eq!(
            resolved("./rust-client"),
            Err("template directory `./rust-client` does not exist".to_string())
        );
    }

    fn file(source: &str) -> TemplateFile {
        TemplateFile {
            source: source.to_string(),
            content: Content::Text(""),
        }
    }

    /// Renders the output paths of `sources` for the protocol `name`,
    /// with every helper available, in output-path order.
    fn output_paths(sources: &[&str], name: &str) -> std::result::Result<Vec<String>, String> {
        let mut handlebars = Handlebars::new();
        helpers::register(&mut handlebars);
        let data = serde_json::json!({ "tii": { "protocol": { "name": name } } });
        let files: Vec<TemplateFile> = sources.iter().map(|source| file(source)).collect();

        resolve_output_paths(&handlebars, &files, &data)
            .map(|outputs| outputs.into_keys().collect())
            .map_err(|error| format!("{error:#}"))
    }

    #[test]
    fn output_paths_render_with_the_template_helpers_and_data() {
        assert_eq!(
            output_paths(
                &[
                    "Sources/{{pascalCase tii.protocol.name}}Client/Types.swift.hbs",
                    "src/main/java/{{snakeCase tii.protocol.name}}/Types.java.hbs",
                    "{{tii.protocol.name}}/README.md",
                ],
                "my-protocol"
            ),
            Ok(vec![
                "Sources/MyProtocolClient/Types.swift".to_string(),
                "my-protocol/README.md".to_string(),
                "src/main/java/my_protocol/Types.java".to_string(),
            ])
        );
    }

    #[test]
    fn output_paths_without_expressions_render_unchanged() {
        assert_eq!(
            output_paths(&["README.md", "src/lib.rs.hbs", "a b/c.d.hbs"], "x"),
            Ok(vec![
                "README.md".to_string(),
                "a b/c.d".to_string(),
                "src/lib.rs".to_string(),
            ])
        );
    }

    #[test]
    fn empty_output_paths_are_errors() {
        assert_eq!(
            output_paths(&["{{tii.protocol.missing}}"], "x"),
            Err(
                "template file `{{tii.protocol.missing}}` renders to an empty output path"
                    .to_string()
            )
        );
    }

    #[test]
    fn absolute_output_paths_are_errors() {
        assert_eq!(
            output_paths(&["/{{tii.protocol.name}}/lib.rs.hbs"], "x"),
            Err(
                "template file `/{{tii.protocol.name}}/lib.rs.hbs` renders to the absolute \
                 output path `/x/lib.rs`; output paths are relative to the output directory"
                    .to_string()
            )
        );
    }

    #[test]
    fn output_paths_leaving_the_output_directory_are_errors() {
        assert_eq!(
            output_paths(&["../{{tii.protocol.name}}/lib.rs.hbs"], "x"),
            Err(
                "template file `../{{tii.protocol.name}}/lib.rs.hbs` renders to the output \
                 path `../x/lib.rs`, which has a `..` segment; output paths stay inside the \
                 output directory"
                    .to_string()
            )
        );
        assert_eq!(
            output_paths(&["{{tii.protocol.name}}/lib.rs.hbs"], ".."),
            Err(
                "template file `{{tii.protocol.name}}/lib.rs.hbs` renders to the output \
                 path `../lib.rs`, which has a `..` segment; output paths stay inside the \
                 output directory"
                    .to_string()
            )
        );
        assert_eq!(
            output_paths(&["./lib.rs.hbs"], "x"),
            Err(
                "template file `./lib.rs.hbs` renders to the output path `./lib.rs`, which \
                 has a `.` segment; output paths stay inside the output directory"
                    .to_string()
            )
        );
    }

    #[test]
    fn empty_output_path_segments_are_errors() {
        assert_eq!(
            output_paths(&["src/{{tii.protocol.missing}}/lib.rs.hbs"], "x"),
            Err(
                "template file `src/{{tii.protocol.missing}}/lib.rs.hbs` renders to the \
                 output path `src//lib.rs`, which has an empty segment"
                    .to_string()
            )
        );
        assert_eq!(
            output_paths(&["{{tii.protocol.name}}/"], "x"),
            Err(
                "template file `{{tii.protocol.name}}/` renders to the output path `x/`, \
                 which has an empty segment"
                    .to_string()
            )
        );
    }

    #[test]
    fn colliding_output_paths_are_errors() {
        assert_eq!(
            output_paths(
                &[
                    "{{lowerCase tii.protocol.name}}/lib.rs.hbs",
                    "{{tii.protocol.name}}/lib.rs",
                    "x/lib.rs.hbs",
                ],
                "x"
            ),
            Err(
                "template files `{{lowerCase tii.protocol.name}}/lib.rs.hbs` and \
                 `{{tii.protocol.name}}/lib.rs` both render to the output path `x/lib.rs`"
                    .to_string()
            )
        );
    }

    #[test]
    fn output_path_rendering_errors_name_the_template_file() {
        let error = output_paths(&["{{pascalCase}}/lib.rs.hbs"], "x").unwrap_err();
        assert!(
            error.starts_with(
                "rendering the output path of template file `{{pascalCase}}/lib.rs.hbs`: "
            ),
            "{error}"
        );
        assert!(error.contains("pascalCase: missing argument 0"), "{error}");
    }
}
