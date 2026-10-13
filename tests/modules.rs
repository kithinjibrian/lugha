//! Programs of several files through the `lughac` binary (PRP-033): every
//! module is loaded, `--emit` shows each one, and diagnostics name the file
//! they are in, and the program runs (PRP-034).

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
fn programs_that_import_run() {
    // PRP-034: the milestone 10 gate is gone.
    let f = two_modules();
    let out = lughac(f.path(), &["run", "main.la"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(text(&out.stdout), "0.0\n");
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

#[test]
fn modules_get_their_own_symbols() {
    let f = Fixture::new();
    f.file(
        "main.la",
        "import a;\nimport b;\nimport shapes.circle;\nextern fun abs(x: i32): i32;\n\
         fun main() { let n = a.f() + b.f(); let c = circle.Circle { r: 1.0 }; let x = c.area(); let y = abs(-1); }\n",
    )
    .file("a.la", "extern fun abs(x: i32): i32;\nfun f(): i64 = 1;\n")
    .file("b.la", "fun f(): i64 = 2;\n")
    .file(
        "shapes/circle.la",
        "struct Circle { r: f64 }\nimpl Circle { fun area(self): f64 = 3.0 * self.r * self.r; }\n",
    );
    let out = lughac(f.path(), &["build", "--emit=ir", "main.la"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    let ir = text(&out.stdout);
    // Two modules' `f` are different functions, called as the checker resolved them.
    assert!(ir.contains("define i64 @\"lugha_fn_a/f\"()"), "{ir}");
    assert!(ir.contains("define i64 @\"lugha_fn_b/f\"()"), "{ir}");
    let main = &ir[ir
        .find("define void @lugha_fn_main()")
        .expect("main keeps its symbol")..];
    assert!(
        main.contains("call i64 @\"lugha_fn_a/f\"()")
            && main.contains("call i64 @\"lugha_fn_b/f\"()"),
        "{ir}"
    );
    assert!(
        ir.contains("@\"lugha_fn_shapes/circle/Circle.area\""),
        "{ir}"
    );
    // An extern declared in two modules is one declaration.
    assert_eq!(ir.matches("declare i32 @abs(").count(), 1, "{ir}");
}

#[test]
fn generic_instances_get_their_own_symbols() {
    // PRP-036: type arguments as they print, module paths with `/`.
    let f = Fixture::new();
    f.file(
        "main.la",
        "import a.b;\nstruct Stack<T> { items: T[] }\n\
         impl Stack<T> { fun push(mut self, x: T) { self.items.push(x); } fun tag<U>(self, u: U): U = u; }\n\
         fun main() { let mut s = Stack { items: [\"x\"] }; s.push(\"y\"); let t = s.tag(1.5); \
         let p = b.max(b.Point { x: 1 }); }\n",
    )
    .file(
        "a/b.la",
        "struct Point { x: i64 }\nfun max<T>(x: T): T = x;\n",
    );
    let out = lughac(f.path(), &["build", "--emit=ir", "main.la"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    let ir = text(&out.stdout);
    for symbol in [
        "@\"lugha_fn_Stack<string>.push\"",
        "@\"lugha_fn_Stack<string>.tag<f64>\"",
        "@\"lugha_fn_a/b/max<a/b/Point>\"",
    ] {
        let defined = ir
            .lines()
            .any(|l| l.starts_with("define") && l.contains(symbol));
        assert!(defined, "{symbol}: {ir}");
    }
}

#[test]
fn the_prelude_shows_only_as_compiled_instances() {
    // PRP-041: the helpers live in a hidden prelude module.
    let f = Fixture::new();
    f.file(
        "main.la",
        "fun main() { let xs = [1, 2]; let ys = xs.map(fun(x) = x + 1); }\n",
    )
    .file("plain.la", "fun main() { println(1); }\n");
    let ast = text(&lughac(f.path(), &["build", "--emit=ast", "main.la"]).stdout);
    assert!(
        !ast.contains("prelude") && !ast.contains("sort_by"),
        "{ast}"
    );
    let ir = text(&lughac(f.path(), &["build", "--emit=ir", "main.la"]).stdout);
    assert!(ir.contains("@\"lugha_fn_$prelude/map<i64, i64>\""), "{ir}");
    assert!(!ir.contains("sort_by"), "only what is used compiles: {ir}");
    let plain = text(&lughac(f.path(), &["build", "--emit=ir", "plain.la"]).stdout);
    assert!(!plain.contains("prelude"), "{plain}");
}
