//! The loader with package layouts (milestone 51): `source`, `name`
//! (E0327), the implicit package of a program inside one, E0328, and the
//! `..` path fix, over an in-memory file table.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

use super::LoadEnv;
use crate::modules::tests::codes;
use crate::modules::{Loaded, load};

/// Loads `main` from `files` (paths as written), with `/` as the current
/// folder and no home folder.
fn load_at(main: &str, files: &[(&str, &str)]) -> Loaded {
    let table: HashMap<PathBuf, Vec<u8>> = files
        .iter()
        .map(|(p, text)| (PathBuf::from(p), text.as_bytes().to_vec()))
        .collect();
    let mut read = |path: &Path| {
        let path = crate::modules::paths::normalized(path);
        match table.get(&path) {
            Some(bytes) => Ok(bytes.clone()),
            None if table.keys().any(|f| f.starts_with(&path)) => {
                Err(io::Error::from(io::ErrorKind::IsADirectory))
            }
            None => Err(io::Error::from(io::ErrorKind::NotFound)),
        }
    };
    load(Path::new(main), &mut read, LoadEnv::plain()).unwrap()
}

fn paths(loaded: &Loaded) -> Vec<String> {
    loaded.modules.iter().map(|m| m.path.join(".")).collect()
}

const GLOW: &str = "[package]\nname = \"glowworm\"\nversion = \"0.1.0\"\nsource = \"src\"\n";
const ARRAY: (&str, &str) = ("glow/src/array.la", "public fun zeros(): i64 = 0;\n");
const EXAMPLE: &str = "import glowworm.array;\nfun main() { println(array.zeros()); }\n";

#[test]
fn a_program_inside_a_package_imports_it_by_name() {
    let files = [
        ("glow/lugha.toml", GLOW),
        ARRAY,
        ("glow/examples/basics.la", EXAMPLE),
    ];
    let loaded = load_at("glow/examples/basics.la", &files);
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
    assert_eq!(paths(&loaded), ["", "glowworm.array"]);
    assert_eq!(loaded.modules[1].file, PathBuf::from("glow/src/array.la"));
}

#[test]
fn source_and_name_apply_to_listed_packages() {
    let app = |key: &str| format!("[packages]\n{key} = \"../glow\"\n");
    let main = |m: &str| format!("import {m}.array;\nfun main() {{}}\n");
    let listed = app("glowworm");
    let program = main("glowworm");
    let files = [
        ("glow/lugha.toml", GLOW),
        ARRAY,
        ("app/lugha.toml", &listed[..]),
        ("app/main.la", &program[..]),
    ];
    let loaded = load_at("app/main.la", &files);
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
    assert_eq!(paths(&loaded), ["", "glowworm.array"]);
    // Listed under another key: E0327 at the listing.
    let (listed, program) = (app("gw"), main("gw"));
    let files = [
        ("glow/lugha.toml", GLOW),
        ARRAY,
        ("app/lugha.toml", &listed[..]),
        ("app/main.la", &program[..]),
    ];
    let found = codes(&load_at("app/main.la", &files));
    assert_eq!(found, [("E0327", "gw = \"../glow\"".to_string())]);
}

#[test]
fn a_missing_source_folder_is_e0322() {
    let files = [
        (
            "glow/lugha.toml",
            "[package]\nname = \"glowworm\"\nsource = \"lib\"\n",
        ),
        ARRAY,
        ("glow/examples/basics.la", EXAMPLE),
    ];
    let found: Vec<&str> = codes(&load_at("glow/examples/basics.la", &files))
        .into_iter()
        .map(|(c, _)| c)
        .collect();
    assert_eq!(found[0], "E0322");
}

#[test]
fn one_file_is_one_module() {
    let files = [
        (
            "tool/lugha.toml",
            "[package]\nname = \"tool\"\nsource = \"src\"\n",
        ),
        (
            "tool/src/main.la",
            "import cli;\nimport tool.cli as c;\nfun main() {}\n",
        ),
        ("tool/src/cli.la", ""),
    ];
    let found = codes(&load_at("tool/src/main.la", &files));
    assert_eq!(found, [("E0328", "tool.cli".to_string())]);
}

#[test]
fn parent_folder_paths_resolve_v15_fix() {
    let files = [
        ("glow/lugha.toml", "[package]\nversion = \"0.1.0\"\n"),
        ("glow/array.la", ""),
        ("glow/example/lugha.toml", "[packages]\nglowworm = \"..\"\n"),
        (
            "glow/example/main.la",
            "import glowworm.array;\nfun main() {}\n",
        ),
    ];
    let loaded = load_at("glow/example/main.la", &files);
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
}

#[test]
fn the_implicit_name_may_not_be_a_local_folder() {
    let files = [
        ("glow/lugha.toml", GLOW),
        ARRAY,
        ("glow/examples/basics.la", EXAMPLE),
        ("glow/examples/glowworm.la", ""),
    ];
    let found: Vec<&str> = codes(&load_at("glow/examples/basics.la", &files))
        .into_iter()
        .map(|(c, _)| c)
        .collect();
    assert_eq!(found, ["E0320"]);
}
