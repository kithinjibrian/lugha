//! Interface values through the `lughac` binary: `check` accepts them
//! (PRP-046), and `run` and `--emit=ir` compile them (PRP-047).

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

/// A temporary directory holding `shapes.la` and `errors.la`, removed on drop.
struct Dir(PathBuf);

/// `?` converting a `Bad` into an `Error` (v5 milestone 22).
const ERRORS: &str = "interface Error { fun message(self): string; }
struct Bad { t: string }
impl Bad { fun message(self): string = self.t; }
fun get(): Result<i64, Bad> = Err(Bad { t: \"no\" });
fun read(): Result<i64, Error> { let n = get()?; Ok(n) }
fun main() { match read() { Ok(n) => println(n), Err(e) => println(e.message()), } }
";

impl Dir {
    fn new(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("lugha-{name}-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        std::fs::write(dir.join("shapes.la"), SHAPES).expect("temp dir is writable");
        std::fs::write(dir.join("errors.la"), ERRORS).expect("temp dir is writable");
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
fn interface_values_check_and_run() {
    let dir = Dir::new("iface");
    let check = lughac(&dir, &["check", "shapes.la"]);
    let stderr = String::from_utf8_lossy(&check.stderr);
    assert_eq!(check.status.code(), Some(0), "{stderr}");
    let run = lughac(&dir, &["run", "shapes.la"]);
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(run.status.code(), Some(0), "{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "16.0\n1.0\ncircle\n");
    let ir = lughac(&dir, &["build", "--emit=ir", "shapes.la"]);
    let ir = String::from_utf8_lossy(&ir.stdout);
    assert!(ir.contains("@lugha_vt_Circle.Shape = "), "{ir}");
}

#[test]
fn question_mark_boxes_a_converted_error_on_the_failure_path() {
    let dir = Dir::new("try");
    let run = lughac(&dir, &["run", "errors.la"]);
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert_eq!(run.status.code(), Some(0), "{stderr}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "no\n");
    let ir = lughac(&dir, &["build", "--emit=ir", "errors.la"]);
    let ir = String::from_utf8_lossy(&ir.stdout);
    let read = &ir[ir.find("@lugha_fn_read(").expect("read is defined")..];
    let fail = &read[read.find("try.fail:").expect("a failure branch")..];
    let fail = &fail[..fail.find("\n}").unwrap_or(fail.len())];
    assert!(
        fail.contains("@lugha_rt_alloc") && fail.contains("@lugha_vt_Bad.Error"),
        "{fail}"
    );
}
