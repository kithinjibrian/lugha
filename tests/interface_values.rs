//! Interface values through the `lughac` binary (PRP-046): `check` accepts
//! them, and `build`, `run` and `--emit=ir` stop at the milestone 20 gate
//! until PRP-047 compiles them.

use std::path::PathBuf;
use std::process::{Command, Output};

/// The v5 draft's §10 interface-values program.
const SHAPES: &str = "interface Shape {
    fun area(self): f64;
    fun name(self): string;
    fun grow(mut self, k: f64);
}

struct Circle { r: f64 }

impl Circle {
    fun area(self): f64 = 3.0 * self.r * self.r;
    fun name(self): string = \"circle\";
    fun grow(mut self, k: f64) { self.r *= k; }
}

struct Square { side: f64 }

impl Square {
    fun area(self): f64 = self.side * self.side;
    fun name(self): string = \"square\";
    fun grow(mut self, k: f64) { self.side *= k; }
}

fun total(shapes: Shape[]): f64 {
    let mut sum = 0.0;
    for s of shapes {
        sum += s.area();
    }
    sum
}

fun main() {
    let c = Circle { r: 1.0 };
    let mut shapes: Shape[] = [c, Square { side: 2.0 }];
    shapes[0].grow(2.0);
    println(total(shapes));
    println(c.r);
    let first = shapes[0];
    println(first.name());
}
";

/// A temporary directory holding `shapes.la`, removed on drop.
struct Dir(PathBuf);

impl Dir {
    fn new() -> Self {
        let dir = std::env::temp_dir().join(format!("lugha-iface-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        std::fs::write(dir.join("shapes.la"), SHAPES).expect("temp dir is writable");
        Dir(dir)
    }
}

impl Drop for Dir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn lughac(dir: &Dir, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(&dir.0)
        .output()
        .expect("lughac runs")
}

#[test]
fn interface_values_check_and_stop_at_the_gate() {
    let dir = Dir::new();
    let check = lughac(&dir, &["check", "shapes.la"]);
    let stderr = String::from_utf8_lossy(&check.stderr);
    assert_eq!(check.status.code(), Some(0), "{stderr}");
    for args in [
        &["build", "shapes.la"][..],
        &["run", "shapes.la"],
        &["build", "--emit=ir", "shapes.la"],
    ] {
        let out = lughac(&dir, args);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(out.status.code(), Some(2), "{args:?}: {stderr}");
        assert!(
            stderr.contains("not implemented yet: interface values (milestone 20)"),
            "{args:?}: {stderr}"
        );
    }
    assert!(!dir.0.join("shapes").exists(), "nothing is built");
}
