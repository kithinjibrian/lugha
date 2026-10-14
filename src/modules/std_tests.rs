//! The standard library in the loader (milestone 53): `std.<module>` is
//! embedded, `std` is reserved, and an unknown module is E0312.

use super::tests::{codes, run};

#[test]
fn std_modules_load_from_lughac_itself() {
    let loaded = run(&[("main.la", b"import std.math;\nfun main() {}\n")]).unwrap();
    assert!(loaded.diagnostics.is_empty(), "{:?}", codes(&loaded));
    let math = &loaded.modules[1];
    assert_eq!(math.path, ["std", "math"]);
    assert_eq!(math.package.as_deref(), Some("std"));
    assert_eq!(math.file.to_string_lossy(), "<std>/math.la");
}

#[test]
fn std_is_reserved() {
    let unknown = run(&[("main.la", b"import std.maths;\nfun main() {}\n")]).unwrap();
    assert_eq!(codes(&unknown), [("E0312", "std.maths".to_string())]);
    let local = run(&[
        ("main.la", b"import std.math;\nfun main() {}\n"),
        ("std.la", b""),
    ])
    .unwrap();
    assert_eq!(codes(&local), [("E0320", "std.math".to_string())]);
    for manifest in [
        &b"[packages]\nstd = \"lib\"\n"[..],
        b"[package]\nname = \"std\"\n",
    ] {
        let listed = run(&[
            ("main.la", b"fun main() {}\n"),
            ("lugha.toml", manifest),
            ("lib/x.la", b""),
        ])
        .unwrap();
        assert_eq!(codes(&listed)[0].0, "E0321", "{:?}", codes(&listed));
    }
}
