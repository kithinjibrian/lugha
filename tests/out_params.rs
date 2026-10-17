//! `out` parameters end to end (v17, milestone 102, PRP-134): C fills
//! numbers, `bool`, handles, `Option` of a handle and extern structs through
//! pointers, against C code compiled by the system compiler; the shapes
//! lugha-wasmtime needs (fuel, trap codes, module creation).

use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary folder holding one program, removed on drop.
struct Case(PathBuf);

impl Case {
    /// `main.la` and `side.c`, linked by `[link] c = ["side.c"]`.
    fn new(main: &str, side: &str) -> Case {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-out-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("main.la"), main).unwrap();
        std::fs::write(dir.join("side.c"), side).unwrap();
        std::fs::write(dir.join("lugha.toml"), "[link]\nc = [\"side.c\"]\n").unwrap();
        Case(dir)
    }

    fn lughac(&self, args: &[&str]) -> Output {
        Command::new(env!("CARGO_BIN_EXE_lughac"))
            .args(args)
            .current_dir(&self.0)
            .env("HOME", &self.0)
            .output()
            .expect("lughac runs")
    }

    /// Runs the program at -O0 and -O2 and checks both print `stdout` and exit 0.
    fn runs(&self, stdout: &str) {
        for opt in ["-O0", "-O2"] {
            let out = self.lughac(&["run", "main.la", opt]);
            assert_eq!(
                (out.status.code(), text(&out.stdout).as_str()),
                (Some(0), stdout),
                "{opt}, stderr: {}",
                text(&out.stderr)
            );
        }
    }

    /// Checks `lughac check` rejects the program with exactly these codes, in order.
    fn rejects(&self, codes: &[&str]) -> String {
        let out = self.lughac(&["check", "main.la", "--diagnostics=json"]);
        let stderr = text(&out.stderr) + &text(&out.stdout);
        assert_eq!(out.status.code(), Some(1), "{stderr}");
        let found: Vec<&str> = stderr
            .lines()
            .filter_map(|l| l.split("\"code\":\"").nth(1))
            .map(|rest| &rest[..5])
            .collect();
        assert_eq!(found, codes, "{stderr}");
        stderr
    }
}

impl Drop for Case {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

const SIDE: &str = r#"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
typedef struct { uint64_t store_id; uint64_t private_; } instance;
typedef struct module module;
static int a_module, b_module;
/* What C found in the slot before writing: 0 unless lughac passed the old value. */
static uint64_t seen = 77;
const char *get_fuel(uint64_t *fuel) { seen = *fuel; *fuel = 42; return NULL; }
bool trap_code(int ok, uint8_t *code) { if (ok) *code = 7; return ok; }
bool is_ready(bool *ready) { *ready = true; return false; }
void module_new(int fail, module **ret, module **maybe) {
    *ret = (module *)&b_module;
    *maybe = fail ? NULL : (module *)&b_module;
}
module *any_module(void) { return (module *)&a_module; }
bool is_b(module *m) { return m == (module *)&b_module; }
void instantiate(instance *out) { out->store_id = 3; out->private_ = 9; }
void two(int64_t *a, int64_t *b) { *a = 1; *b = 2; }
uint64_t last_seen(void) { return seen; }
"#;

const MAIN: &str = r#"
extern type Module;
extern struct Instance { store_id: u64, hidden: u64 }
extern fun get_fuel(out fuel: u64): Option<Module>;
extern fun trap_code(ok: i32, out code: u8): bool;
extern fun is_ready(out ready: bool): bool;
extern fun module_new(fail: i32, out ret: Module, out maybe: Option<Module>);
extern fun any_module(): Module;
extern fun is_b(m: Module): bool;
extern fun instantiate(out i: Instance);
extern fun two(out a: i64, out b: i64);
extern fun last_seen(): u64;

struct Pair {
    a: i64,
    b: i64,
}

fun main() {
    let mut fuel: u64 = 99;
    let err = get_fuel(out fuel);
    println(fuel);
    println(last_seen());
    println(err.is_none());
    let mut code: u8 = 0;
    println(trap_code(1, out code));
    println(code);
    let mut ready = false;
    is_ready(out ready);
    println(ready);
    let mut module = any_module();
    let mut maybe: Option<Module> = None;
    module_new(0, out module, out maybe);
    println(is_b(module));
    println(maybe.is_some());
    module_new(1, out module, out maybe);
    println(maybe.is_none());
    let mut inst = Instance { store_id: 0, hidden: 0 };
    instantiate(out inst);
    println(inst.store_id + inst.hidden);
    let mut xs = [0, 0];
    two(out xs[1], out xs[0]);
    println(xs);
    let mut p = Pair { a: 0, b: 0 };
    two(out p.b, out p.a);
    println(p.a * 10 + p.b);
}
"#;

#[test]
fn c_fills_numbers_bools_handles_and_structs() {
    Case::new(MAIN, SIDE).runs("42\n0\ntrue\ntrue\n7\ntrue\ntrue\ntrue\ntrue\n12\n[2, 1]\n21\n");
}

#[test]
fn out_is_checked_at_both_ends() {
    let main = "extern fun get_fuel(out fuel: u64): bool;\n\
                fun main() { let n: u64 = 0; get_fuel(out n); let mut m: u64 = 0; get_fuel(m); }\n";
    Case::new(main, SIDE).rejects(&["E0501", "E0443"]);
}
