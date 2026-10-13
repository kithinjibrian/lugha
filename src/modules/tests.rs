//! Loader tests over an in-memory file table.

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

use super::{LoadError, Loaded, load};

/// Loads `dir/main.la` from `files` (paths relative to `dir`).
fn run(files: &[(&str, &[u8])]) -> Result<Loaded, LoadError> {
    let table: HashMap<PathBuf, Vec<u8>> = files
        .iter()
        .map(|(p, bytes)| (Path::new("dir").join(p), bytes.to_vec()))
        .collect();
    let mut read = |path: &Path| {
        table
            .get(path)
            .cloned()
            .ok_or_else(|| io::Error::from(io::ErrorKind::NotFound))
    };
    load(Path::new("dir/main.la"), &mut read)
}

fn ok(files: &[(&str, &[u8])]) -> Loaded {
    run(files).unwrap_or_else(|e| panic!("{e}"))
}

/// `(code, text the span covers)`, looking the offset up in the right module.
fn codes(loaded: &Loaded) -> Vec<(&'static str, String)> {
    loaded
        .diagnostics
        .iter()
        .map(|d| {
            let m = loaded
                .modules
                .iter()
                .rev()
                .find(|m| m.base <= d.span.start)
                .expect("a module holds every span");
            let text = &m.text[d.span.start - m.base..d.span.end - m.base];
            (d.code, text.to_string())
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
        load(Path::new("main.la"), &mut read),
        Err(LoadError::Io { .. })
    ));
}
