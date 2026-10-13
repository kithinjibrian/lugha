//! Programs of several files through the `lughac` binary (PRP-033): every
//! module is loaded, `--emit` shows each one, and diagnostics name the file
//! they are in. Programs that import still stop at the milestone 10 gate.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary directory of source files, removed on drop.
struct Fixture(PathBuf);

impl Fixture {
    fn new() -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-modules-{}-{n}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        Fixture(dir)
    }

    fn file(&self, rel: &str, contents: &str) -> &Self {
        let path = self.0.join(rel);
        let parent = path.parent().expect("fixture paths have a parent");
        std::fs::create_dir_all(parent).expect("temp dir is writable");
        std::fs::write(path, contents).expect("temp dir is writable");
        self
    }

    fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn lughac(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(dir)
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

/// The v2 draft's two-module program.
fn two_modules() -> Fixture {
    let f = Fixture::new();
    f.file(
        "main.la",
        "import geometry;\n\nfun main() {\n    let mut p = geometry.origin();\n    println(p.x);\n}\n",
    )
    .file(
        "geometry.la",
        "struct Vec2 { x: f64, y: f64 }\n\nfun origin(): Vec2 = Vec2 { x: 0.0, y: 0.0 };\n",
    );
    f
}

#[test]
fn emit_ast_shows_every_module_after_a_header() {
    let f = two_modules();
    let out = lughac(f.path(), &["build", "--emit=ast", "main.la"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    let ast = text(&out.stdout);
    assert!(
        ast.starts_with("; module main\n(import geometry)\n(fun main"),
        "{ast}"
    );
    assert!(ast.contains("; module geometry\n(struct Vec2"), "{ast}");
    let tokens = lughac(f.path(), &["build", "--emit=tokens", "main.la"]);
    let tokens = text(&tokens.stdout);
    // Lines and columns are within each module's own file.
    assert!(
        tokens.contains("; module geometry\n1:1 Struct\n"),
        "{tokens}"
    );
}

#[test]
fn programs_that_import_stop_at_the_gate() {
    let f = two_modules();
    let out = lughac(f.path(), &["check", "main.la"]);
    assert_eq!(out.status.code(), Some(2));
    let stderr = text(&out.stderr);
    assert!(
        stderr.contains("not implemented yet: modules (milestone 10)"),
        "{stderr}"
    );
}

#[test]
fn errors_in_a_module_name_its_file() {
    let f = Fixture::new();
    f.file("main.la", "import shapes.circle;\nfun main() {}\n")
        .file("shapes/circle.la", "fun area( {}\n");
    let human = lughac(f.path(), &["check", "main.la"]);
    assert_eq!(human.status.code(), Some(1));
    let stderr = text(&human.stderr);
    assert!(stderr.contains("--> shapes/circle.la:1:11"), "{stderr}");
    let json = lughac(f.path(), &["check", "--diagnostics=json", "main.la"]);
    let line = text(&json.stderr);
    assert!(
        line.contains(
            r#""file":"shapes/circle.la","span":{"start":{"line":1,"col":11,"offset":10}"#
        ),
        "{line}"
    );
}
