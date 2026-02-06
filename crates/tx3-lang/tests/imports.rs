//! Tests for plutus.json import support.

use std::path::Path;

use tx3_lang::Workspace;

#[test]
fn import_with_alias_adds_types() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("tests/fixtures/import_test.tx3");
    let mut workspace = Workspace::from_file(&path).unwrap();
    workspace.parse().unwrap();
    let ast = workspace.ast().unwrap();

    let type_names: Vec<String> = ast.types.iter().map(|t| t.name.value.clone()).collect();
    assert!(
        type_names.iter().any(|n| n.starts_with("types_")),
        "expected at least one type prefixed with 'types_', got: {:?}",
        type_names
    );
}

#[test]
fn import_without_root_errors() {
    let src = r#"
cardano::import "some/file.json";
party X;
tx dummy() {}
"#;
    let mut workspace = Workspace::from_string(src.to_string());
    let res = workspace.parse();
    assert!(res.is_err(), "expected error when importing without root");
    let err = res.unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("root") || msg.contains("import"),
        "expected error about root or import, got: {}",
        msg
    );
}

#[test]
fn duplicate_type_name_from_imports_errors() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("tests/fixtures/import_test.tx3");
    let content = std::fs::read_to_string(&path).unwrap();
    let content_twice = format!(
        "{}\ncardano::import \"../../../cip-57/examples/plutus.json\" as types;\n{}",
        content,
        "tx dummy2() {}"
    );
    let fixtures = Path::new(manifest_dir).join("tests/fixtures");
    let temp_tx3 = fixtures.join("temp_duplicate_import.tx3");
    std::fs::write(&temp_tx3, content_twice).unwrap();
    let res = Workspace::from_file(&temp_tx3).and_then(|mut w| w.parse());
    let _ = std::fs::remove_file(&temp_tx3);
    assert!(res.is_err(), "expected error for duplicate type names from two imports");
    let err = res.unwrap_err();
    assert!(
        err.to_string().contains("duplicate") || err.to_string().contains("type"),
        "expected duplicate/type error, got: {}",
        err
    );
}

#[test]
fn invalid_import_path_errors() {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let fixtures = Path::new(manifest_dir).join("tests/fixtures");
    let temp_tx3 = fixtures.join("temp_nonexistent_import.tx3");
    let content = r#"
cardano::import "nonexistent/plutus.json" as types;
party X;
tx dummy() {}
"#;
    std::fs::write(&temp_tx3, content).unwrap();
    let res = Workspace::from_file(&temp_tx3).and_then(|mut w| w.parse());
    let _ = std::fs::remove_file(&temp_tx3);
    assert!(res.is_err(), "expected error for missing import file");
}
