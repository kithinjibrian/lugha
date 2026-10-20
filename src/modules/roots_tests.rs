//! Several roots in one load (v17, milestone 124, PRP-156): a package
//! folder's test files share one program, each the module named by its stem.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

use super::layout::LoadEnv;
use super::tests::codes;
use super::{Loaded, load_all};

/// Loads `roots` (under `dir/`) from `files` as one program, as modules.
fn roots(files: &[(&str, &[u8])], roots: &[&str]) -> Loaded {
    let table: HashMap<PathBuf, Vec<u8>> = files
        .iter()
        .map(|(p, bytes)| (Path::new("dir").join(p), bytes.to_vec()))
        .collect();
    let mut read = |path: &Path| match table.get(path) {
        Some(bytes) => Ok(bytes.clone()),
        None if table.keys().any(|f| f.starts_with(path)) => {
            Err(io::Error::from(io::ErrorKind::IsADirectory))
        }
        None => Err(io::Error::from(io::ErrorKind::NotFound)),
    };
    let paths: Vec<PathBuf> = roots.iter().map(|r| Path::new("dir").join(r)).collect();
    let env = LoadEnv {
        as_module: true,
        ..LoadEnv::plain()
    };
    load_all(&paths, &mut read, env).unwrap_or_else(|e| panic!("{e}"))
}

fn paths(loaded: &Loaded) -> Vec<String> {
    loaded.modules.iter().map(|m| m.path.join(".")).collect()
}

#[test]
fn each_root_is_the_module_named_by_its_stem_and_shared_imports_load_once() {
    let loaded = roots(
        &[
            ("a.la", b"import shared;\nfun helper(): i64 = 1;\n"),
            (
                "b.la",
                b"import shared;\nfun helper(): i64 = 2;\nfun main() {}\n",
            ),
            ("shared.la", b"public fun one(): i64 = 1;\n"),
        ],
        &["a.la", "b.la"],
    );
    assert_eq!(codes(&loaded), []);
    assert_eq!(paths(&loaded), ["a", "b", "shared"]);
}

#[test]
fn a_root_another_root_imports_keeps_one_name() {
    let loaded = roots(
        &[
            (
                "user.la",
                b"import fixtures;\nfun f(): i64 = fixtures.base();\n",
            ),
            ("fixtures.la", b"public fun base(): i64 = 40;\n"),
        ],
        &["user.la", "fixtures.la"],
    );
    assert_eq!(codes(&loaded), []);
    assert_eq!(paths(&loaded), ["user", "fixtures"]);
}

#[test]
fn a_stem_that_is_no_name_is_a_hidden_module() {
    let loaded = roots(
        &[("a.la", b"fun f() {}\n"), ("my-test.la", b"fun f() {}\n")],
        &["a.la", "my-test.la"],
    );
    assert_eq!(codes(&loaded), []);
    assert_eq!(paths(&loaded), ["a", "$test1"]);
}

#[test]
fn one_root_loads_as_before() {
    let loaded = roots(&[("main.la", b"fun main() {}\n")], &["main.la"]);
    assert_eq!(paths(&loaded), [""]);
}
