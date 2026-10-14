//! Loader tests for package versions (v11 draft §6, milestone 37), over the
//! in-memory file table of `tests.rs`.

use super::tests::{codes, run};

const MAIN: &[u8] = b"import geo.shapes;\nfun main() {}\n";
const SHAPES: (&str, &[u8]) = ("libs/geo/shapes.la", b"public fun area(): i64 = 1;\n");

fn check(required: &str, geo_manifest: Option<&str>) -> Vec<(&'static str, String)> {
    let root = format!("[packages]\ngeo = {{ path = \"libs/geo\", version = \"{required}\" }}\n");
    let mut files: Vec<(&str, &[u8])> =
        vec![("main.la", MAIN), ("lugha.toml", root.as_bytes()), SHAPES];
    if let Some(text) = geo_manifest {
        files.push(("libs/geo/lugha.toml", text.as_bytes()));
    }
    codes(&run(&files).unwrap_or_else(|e| panic!("{e}")))
}

#[test]
fn a_compatible_version_loads() {
    assert!(check("1.2", Some("[package]\nversion = \"1.4.0\"\n")).is_empty());
}

#[test]
fn a_mismatch_or_a_missing_version_is_e0324() {
    let line = "geo = { path = \"libs/geo\", version = \"1.2\" }".to_string();
    assert_eq!(
        check("1.2", Some("[package]\nversion = \"2.0.0\"\n")),
        [("E0324", line.clone())]
    );
    assert_eq!(check("1.2", None), [("E0324", line.clone())]);
    assert_eq!(check("1.2", Some("[packages]\n")), [("E0324", line)]);
}

#[test]
fn a_required_package_nothing_imports_is_checked_too() {
    let root = "[packages]\ngeo = { path = \"libs/geo\", version = \"0.3\" }\n";
    let loaded = run(&[
        ("main.la", b"fun main() {}\n"),
        ("lugha.toml", root.as_bytes()),
        SHAPES,
        ("libs/geo/lugha.toml", b"[package]\nversion = \"0.4.1\"\n"),
    ])
    .unwrap_or_else(|e| panic!("{e}"));
    let found = codes(&loaded);
    assert_eq!(found.len(), 1);
    assert_eq!(found[0].0, "E0324");
    assert!(
        loaded.diagnostics[0].message.contains("0.4.1"),
        "{:?}",
        loaded.diagnostics[0]
    );
}

#[test]
fn a_missing_folder_is_only_e0322() {
    let root = "[packages]\ngeo = { path = \"libs/nope\", version = \"1\" }\n";
    let loaded = run(&[
        ("main.la", b"fun main() {}\n"),
        ("lugha.toml", root.as_bytes()),
    ])
    .unwrap_or_else(|e| panic!("{e}"));
    let found: Vec<&str> = codes(&loaded).into_iter().map(|(c, _)| c).collect();
    assert_eq!(found, ["E0322"]);
}
