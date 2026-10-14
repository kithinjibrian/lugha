//! Loader tests over an in-memory file table.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

use super::layout::LoadEnv;
use super::{LoadError, Loaded, load};

/// Loads `dir/main.la` from `files` (paths relative to `dir`).
pub(super) fn run(files: &[(&str, &[u8])]) -> Result<Loaded, LoadError> {
    let table: HashMap<PathBuf, Vec<u8>> = files
        .iter()
        .map(|(p, bytes)| (Path::new("dir").join(p), bytes.to_vec()))
        .collect();
    // A folder exists when some file is under it; reading one fails as
    // `std::fs::read` does on a directory (v8 manifests probe folders so).
    let mut read = |path: &Path| match table.get(path) {
        Some(bytes) => Ok(bytes.clone()),
        None if table.keys().any(|f| f.starts_with(path)) => {
            Err(io::Error::from(io::ErrorKind::IsADirectory))
        }
        None => Err(io::Error::from(io::ErrorKind::NotFound)),
    };
    load(Path::new("dir/main.la"), &mut read, LoadEnv::plain())
}

fn ok(files: &[(&str, &[u8])]) -> Loaded {
    run(files).unwrap_or_else(|e| panic!("{e}"))
}

/// `(code, text the span covers)`, looking the offset up in the right module.
pub(super) fn codes(loaded: &Loaded) -> Vec<(&'static str, String)> {
    loaded
        .diagnostics
        .iter()
        .map(|d| {
            // Modules and manifests (v8) share the offset space.
            let files = loaded.modules.iter().map(|m| (m.base, &m.text));
            let manifests = loaded.manifests.iter().map(|m| (m.base, &m.text));
            let (base, text) = files
                .chain(manifests)
                .filter(|(base, _)| *base <= d.span.start)
                .max_by_key(|(base, _)| *base)
                .expect("a file holds every span");
            (
                d.code,
                text[d.span.start - base..d.span.end - base].to_string(),
            )
        })
        .collect()
}

#[test]
fn modules_load_root_first_with_increasing_bases() {
    let loaded = ok(&[
        (
            "main.la",
            b"import a;\nimport b.c;\nfun main() { let x = 1; }\n",
        ),
        ("a.la", b"fun f(): i64 = 2;\n"),
        ("b/c.la", b"fun g(): i64 = 3;\n"),
    ]);
    let paths: Vec<Vec<String>> = loaded.modules.iter().map(|m| m.path.clone()).collect();
    assert_eq!(
        paths,
        [vec![], vec!["a".to_string()], vec!["b".into(), "c".into()]]
    );
    let bases: Vec<usize> = loaded.modules.iter().map(|m| m.base).collect();
    assert!(
        bases[0] == 0 && bases[0] < bases[1] && bases[1] < bases[2],
        "{bases:?}"
    );
    // A module's spans live in its own range of the shared offset space.
    let a = &loaded.modules[1];
    let crate::ast::Item::Fun(f) = &a.program.as_ref().expect("parsed").items[0] else {
        unreachable!("a.la holds one function")
    };
    let span = f.span;
    assert!(span.start >= a.base && span.end <= a.base + a.text.len());
    assert!(loaded.diagnostics.is_empty());
    // Expression ids continue across modules (PRP-034).
    let counts: Vec<u32> = loaded
        .modules
        .iter()
        .map(|m| m.program.as_ref().expect("parsed").expr_count)
        .collect();
    assert!(
        counts[0] <= counts[1] && counts[1] <= counts[2],
        "{counts:?}"
    );
}

#[test]
fn cycles_and_repeats_load_each_module_once() {
    let loaded = ok(&[
        ("main.la", b"import a;\nimport b;\nfun main() {}\n"),
        ("a.la", b"import b;\nfun f() {}\n"),
        ("b.la", b"import a;\nfun g() {}\n"),
    ]);
    assert_eq!(loaded.modules.len(), 3);
    assert!(loaded.diagnostics.is_empty());
}

#[test]
fn a_missing_module_is_e0312() {
    let loaded = ok(&[("main.la", b"import shapes.cirlce;\nfun main() {}\n")]);
    assert_eq!(codes(&loaded), [("E0312", "shapes.cirlce".to_string())]);
    let help = loaded.diagnostics[0].help.as_deref().expect("help");
    assert!(help.contains("shapes/cirlce.la"), "{help}");
}

#[test]
fn clashing_imports_are_e0314() {
    let files: &[(&str, &[u8])] = &[
        ("main.la", b"import a;\nimport a;\nimport x.util;\nimport y.util;\nimport geo;\nfun geo() {}\nfun main() {}\n"),
        ("a.la", b""),
        ("x/util.la", b""),
        ("y/util.la", b""),
        ("geo.la", b""),
    ];
    let loaded = ok(files);
    assert_eq!(
        codes(&loaded),
        [
            ("E0314", "a".to_string()),
            ("E0314", "y.util".to_string()),
            ("E0314", "geo".to_string())
        ]
    );
    // `as` (v14) names the binding: two `util`s coexist; an alias can clash.
    let files: &[(&str, &[u8])] = &[
        (
            "main.la",
            b"import x.util as u1;\nimport y.util as u2;\nimport a as u1;\nfun main() {}\n",
        ),
        ("a.la", b""),
        ("x/util.la", b""),
        ("y/util.la", b""),
    ];
    assert_eq!(codes(&ok(files)), [("E0314", "a".to_string())]);
}

#[test]
fn the_main_file_cannot_be_imported() {
    let loaded = ok(&[
        ("main.la", b"import a;\nfun main() {}\n"),
        ("a.la", b"import main;\n"),
    ]);
    assert_eq!(codes(&loaded), [("E0319", "main".to_string())]);
}

#[test]
fn errors_in_imported_files_point_into_them() {
    let loaded = ok(&[
        ("main.la", b"import a;\nimport b;\nfun main() {}\n"),
        ("a.la", b"fun f( {}\n"),
        ("b.la", b"fun g() {}\n\xff"),
    ]);
    assert_eq!(
        codes(&loaded),
        [("E0201", "{".to_string()), ("E0110", String::new())]
    );
    let b = &loaded.modules[2];
    assert_eq!(loaded.diagnostics[1].span.start, b.base + 11);
}

#[test]
fn unreadable_files_are_internal() {
    let mut read = |_: &Path| Err(io::Error::from(io::ErrorKind::PermissionDenied));
    assert!(matches!(
        load(Path::new("main.la"), &mut read, LoadEnv::plain()),
        Err(LoadError::Io { .. })
    ));
}

/// The module paths of `loaded`, as `a.b` strings ("" for the main file).
fn paths(loaded: &Loaded) -> Vec<String> {
    loaded.modules.iter().map(|m| m.path.join(".")).collect()
}

const GEO: &[u8] = b"[packages]\ngeo = \"libs/geo\"\n";

#[test]
fn package_imports_resolve_through_the_manifest() {
    let loaded = ok(&[
        ("main.la", b"import geo.shapes;\nfun main() {}\n"),
        ("lugha.toml", GEO),
        (
            "libs/geo/shapes.la",
            b"import util.x;\nfun area(): i64 = 1;\n",
        ),
        ("libs/geo/util/x.la", b"fun y(): i64 = 2;\n"),
    ]);
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
    assert_eq!(paths(&loaded), ["", "geo.shapes", "geo.util.x"]);
    let shapes = &loaded.modules[1];
    assert_eq!(shapes.package.as_deref(), Some("geo"));
    assert_eq!(
        shapes.imports,
        [vec!["geo".to_string(), "util".into(), "x".into()]]
    );
    assert_eq!(
        loaded.modules[0].imports,
        [vec!["geo".to_string(), "shapes".into()]]
    );
    assert_eq!(loaded.manifests.len(), 1);
    assert!(
        loaded.manifests[0].base > loaded.modules[0].base,
        "main keeps offset 0"
    );
}

#[test]
fn a_packages_own_packages_are_not_transitive() {
    let loaded = ok(&[
        (
            "main.la",
            b"import geo.shapes;\nimport mathx.trig;\nfun main() {}\n",
        ),
        ("lugha.toml", GEO),
        ("libs/geo/lugha.toml", b"[packages]\nmathx = \"../mathx\"\n"),
        (
            "libs/geo/shapes.la",
            b"import mathx.trig;\nfun area(): i64 = 1;\n",
        ),
        ("libs/mathx/trig.la", b"fun sin(): i64 = 0;\n"),
    ]);
    // The root doesn't list `mathx`: its import is a missing local module.
    assert_eq!(codes(&loaded), [("E0312", "mathx.trig".to_string())]);
    assert!(
        paths(&loaded).contains(&"mathx.trig".to_string()),
        "{:?}",
        paths(&loaded)
    );
}

#[test]
fn package_mistakes_are_diagnostics() {
    let clash = ok(&[
        ("main.la", b"import geo.shapes;\nfun main() {}\n"),
        ("lugha.toml", GEO),
        ("geo/other.la", b"fun f() {}\n"),
        ("libs/geo/shapes.la", b"fun f() {}\n"),
    ]);
    assert_eq!(codes(&clash)[0].0, "E0320");
    let missing = ok(&[
        ("main.la", b"fun main() {}\n"),
        ("lugha.toml", b"[packages]\ngeo = \"nowhere\"\n"),
    ]);
    assert_eq!(
        codes(&missing),
        [("E0322", "geo = \"nowhere\"".to_string())]
    );
    let two = ok(&[
        ("main.la", b"import geo.shapes;\nfun main() {}\n"),
        (
            "lugha.toml",
            b"[packages]\ngeo = \"libs/geo\"\nmathx = \"libs/m1\"\n",
        ),
        ("libs/geo/lugha.toml", b"[packages]\nmathx = \"../m2\"\n"),
        ("libs/geo/shapes.la", b"fun f() {}\n"),
        ("libs/m1/a.la", b"fun f() {}\n"),
        ("libs/m2/a.la", b"fun f() {}\n"),
    ]);
    assert_eq!(codes(&two), [("E0323", "mathx = \"../m2\"".to_string())]);
    let bare = ok(&[
        ("main.la", b"import geo;\nimport geo.nope;\nfun main() {}\n"),
        ("lugha.toml", GEO),
        ("libs/geo/shapes.la", b"fun f() {}\n"),
    ]);
    let found: Vec<&str> = codes(&bare).iter().map(|(c, _)| *c).collect();
    assert_eq!(found, ["E0312", "E0312"]);
    let bad = ok(&[
        ("main.la", b"fun main() {}\n"),
        ("lugha.toml", b"geo = 1\n"),
    ]);
    assert_eq!(codes(&bad), [("E0321", "geo = 1".to_string())]);
}

#[test]
fn packages_may_import_each_other() {
    let loaded = ok(&[
        ("main.la", b"import a.x;\nfun main() {}\n"),
        ("lugha.toml", b"[packages]\na = \"pa\"\nb = \"pb\"\n"),
        ("pa/lugha.toml", b"[packages]\nb = \"../pb\"\n"),
        ("pb/lugha.toml", b"[packages]\na = \"../pa\"\n"),
        ("pa/x.la", b"import b.y;\nfun f() {}\n"),
        ("pb/y.la", b"import a.x;\nfun g() {}\n"),
    ]);
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
    assert_eq!(paths(&loaded), ["", "a.x", "b.y"]);
}
