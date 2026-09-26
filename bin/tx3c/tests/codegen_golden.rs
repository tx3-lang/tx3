//! Golden-output tests for `tx3c codegen`.
//!
//! The real `tx3c` binary renders every fixture in `tests/codegen/fixtures`
//! with every built-in template (`--template <name>`, expected under
//! `expected/<name>/`) and every custom template directory under
//! `tests/codegen/custom/<name>` (`--template <dir>`, expected under
//! `expected/custom/<name>/`). The rendered files must match byte for byte. A render
//! failure is recorded as an `ERROR` file holding the binary's stderr, so error
//! behavior is pinned the same way as successful output.
//!
//! Set `TX3C_BLESS=1` to rewrite the expected files after an intended change,
//! then review the resulting diff.

use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    process::Command,
};

fn corpus_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/codegen")
}

fn sorted_entries(dir: &Path) -> Vec<PathBuf> {
    let mut entries: Vec<PathBuf> = fs::read_dir(dir)
        .unwrap_or_else(|error| panic!("reading {}: {error}", dir.display()))
        .map(|entry| entry.expect("directory entry").path())
        .collect();
    entries.sort();
    entries
}

/// Reads every file below `dir` into a map keyed by its relative path.
fn read_tree(dir: &Path) -> BTreeMap<String, String> {
    fn visit(root: &Path, dir: &Path, files: &mut BTreeMap<String, String>) {
        for path in sorted_entries(dir) {
            if path.is_dir() {
                visit(root, &path, files);
            } else {
                let relative = path
                    .strip_prefix(root)
                    .expect("path below root")
                    .to_string_lossy()
                    .replace('\\', "/");
                let content = fs::read_to_string(&path)
                    .unwrap_or_else(|error| panic!("reading {}: {error}", path.display()));
                files.insert(relative, content);
            }
        }
    }

    let mut files = BTreeMap::new();
    if dir.exists() {
        visit(dir, dir, &mut files);
    }
    files
}

fn write_tree(dir: &Path, files: &BTreeMap<String, String>) {
    if dir.exists() {
        fs::remove_dir_all(dir).expect("clearing expected output");
    }
    for (relative, content) in files {
        let path = dir.join(relative);
        fs::create_dir_all(path.parent().expect("file has a parent")).expect("creating dir");
        fs::write(&path, content).expect("writing expected output");
    }
}

fn render(label: &str, template: &str, fixture: &Path) -> BTreeMap<String, String> {
    let stem = fixture.file_stem().unwrap().to_string_lossy();
    let output =
        std::env::temp_dir().join(format!("tx3c-golden-{}-{label}-{stem}", std::process::id()));
    let _ = fs::remove_dir_all(&output);

    let mut command = Command::new(env!("CARGO_BIN_EXE_tx3c"));
    command
        .arg("codegen")
        .arg("--tii")
        .arg(fixture)
        .arg("--template")
        .arg(template);
    let result = command
        .arg("--output")
        .arg(&output)
        .output()
        .expect("running tx3c");

    let files = if result.status.success() {
        read_tree(&output)
    } else {
        BTreeMap::from([(
            "ERROR".to_string(),
            String::from_utf8_lossy(&result.stderr).into_owned(),
        )])
    };
    let _ = fs::remove_dir_all(&output);
    files
}

#[test]
fn codegen_templates_match_golden_output() {
    let root = corpus_root();
    let bless = std::env::var_os("TX3C_BLESS").is_some();
    let fixtures: Vec<PathBuf> = sorted_entries(&root.join("fixtures"))
        .into_iter()
        .filter(|path| path.extension().is_some_and(|ext| ext == "tii"))
        .collect();

    // Built-in templates by name, then custom template directories by path.
    let built_in = sorted_entries(&Path::new(env!("CARGO_MANIFEST_DIR")).join("templates"))
        .into_iter()
        .map(|dir| {
            let name = dir.file_name().unwrap().to_string_lossy().into_owned();
            (name.clone(), name)
        });
    let custom = sorted_entries(&root.join("custom")).into_iter().map(|dir| {
        let name = dir.file_name().unwrap().to_string_lossy().into_owned();
        (format!("custom/{name}"), dir.to_string_lossy().into_owned())
    });
    let cases: Vec<(String, String)> = built_in.chain(custom).collect();

    let mut failures = Vec::new();
    for (label, template) in &cases {
        for fixture in &fixtures {
            let stem = fixture.file_stem().unwrap().to_string_lossy().into_owned();
            let actual = render(label, template, fixture);
            let expected_dir = root.join("expected").join(label).join(&stem);

            if bless {
                write_tree(&expected_dir, &actual);
                continue;
            }

            let expected = read_tree(&expected_dir);
            if actual != expected {
                let mut detail = format!("{label}/{stem}:");
                for name in expected
                    .keys()
                    .chain(actual.keys())
                    .collect::<std::collections::BTreeSet<_>>()
                {
                    match (expected.get(name), actual.get(name)) {
                        (Some(want), Some(got)) if want != got => {
                            detail.push_str(&format!(
                                "\n--- expected {name}\n{want}\n+++ actual {name}\n{got}"
                            ));
                        }
                        (Some(_), None) => detail.push_str(&format!("\nmissing {name}")),
                        (None, Some(got)) => {
                            detail.push_str(&format!("\nunexpected {name}:\n{got}"))
                        }
                        _ => {}
                    }
                }
                failures.push(detail);
            }
        }
    }

    assert!(
        failures.is_empty(),
        "codegen output differs from the golden files (rerun with TX3C_BLESS=1 to accept):\n\n{}",
        failures.join("\n\n")
    );
}
