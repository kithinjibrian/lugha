//! Loader tests for `[link]` tables (v16, milestone 87, PRP-117): which
//! packages' tables a build gets, in what order, and E0322 for missing
//! folders and files, over the in-memory file table of `tests.rs`.

use std::path::PathBuf;

use super::link_table::LinkKey;
use super::tests::{codes, run};

const IMPORTS_GEO: &[u8] = b"import geo.shapes;\nfun main() {}\n";
const SHAPES: (&str, &[u8]) = ("libs/geo/shapes.la", b"public fun area(): i64 = 1;\n");

#[test]
fn the_programs_table_comes_with_its_folder() {
    let root = "[link]\nlibs = [\"raylib\"]\npaths = [\"vendor/lib\"]\nc = [\"shim.c\"]\n";
    let loaded = run(&[
        ("main.la", b"fun main() {}\n"),
        ("lugha.toml", root.as_bytes()),
        ("vendor/lib/libraylib.a", b""),
        ("shim.c", b"int f(void) { return 1; }\n"),
    ])
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
    assert_eq!(loaded.links.len(), 1);
    let link = &loaded.links[0];
    assert_eq!(link.package, None);
    assert_eq!(link.folder, PathBuf::from("dir"));
    assert_eq!(link.table.values(LinkKey::Libs), ["raylib"]);
    assert_eq!(link.table.values(LinkKey::C), ["shim.c"]);
}

#[test]
fn an_imported_packages_table_follows_the_programs() {
    let root = "[packages]\ngeo = \"libs/geo\"\n[link]\nlibs = [\"m\"]\n";
    let loaded = run(&[
        ("main.la", IMPORTS_GEO),
        ("lugha.toml", root.as_bytes()),
        SHAPES,
        ("libs/geo/lugha.toml", b"[link]\nlibs = [\"geos\"]\n"),
    ])
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
    let found: Vec<(Option<&str>, &[String])> = loaded
        .links
        .iter()
        .map(|l| (l.package.as_deref(), l.table.values(LinkKey::Libs)))
        .collect();
    assert_eq!(
        found,
        [
            (None, &["m".to_string()][..]),
            (Some("geo"), &["geos".to_string()][..])
        ]
    );
    assert_eq!(loaded.links[1].folder, PathBuf::from("dir/libs/geo"));
}

#[test]
fn a_package_nothing_imports_adds_nothing() {
    let root = "[packages]\ngeo = { path = \"libs/geo\", version = \"0.1\" }\n";
    let geo = "[package]\nversion = \"0.1.0\"\n[link]\nlibs = [\"geos\"]\n";
    let loaded = run(&[
        ("main.la", b"fun main() {}\n"),
        ("lugha.toml", root.as_bytes()),
        SHAPES,
        ("libs/geo/lugha.toml", geo.as_bytes()),
    ])
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
    assert!(loaded.links.is_empty(), "{:?}", loaded.links);
}

#[test]
fn no_table_or_an_empty_one_adds_nothing() {
    for root in ["[packages]\n", "[link]\n", "[link]\nlibs = []\n"] {
        let loaded = run(&[
            ("main.la", b"fun main() {}\n"),
            ("lugha.toml", root.as_bytes()),
        ])
        .unwrap_or_else(|e| panic!("{e}"));
        assert!(loaded.diagnostics.is_empty(), "{root}");
        assert!(loaded.links.is_empty(), "{root}");
    }
}

#[test]
fn missing_folders_and_files_are_e0322_at_their_line() {
    let root =
        "[link]\npaths = [\"vendor/lib\"]\ninclude = [\"vendor/include\"]\nc = [\"shim.c\"]\n";
    let loaded = run(&[
        ("main.la", b"fun main() {}\n"),
        ("lugha.toml", root.as_bytes()),
    ])
    .unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(
        codes(&loaded),
        [
            ("E0322", "paths = [\"vendor/lib\"]".to_string()),
            ("E0322", "include = [\"vendor/include\"]".to_string()),
            ("E0322", "c = [\"shim.c\"]".to_string()),
        ]
    );
    assert!(loaded.diagnostics[2].message.contains("shim.c"));
}
