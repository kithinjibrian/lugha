//! Arrays and unions at the C boundary end to end (v17, milestone 104,
//! PRP-136): `T[]` and `mut T[]` extern parameters and `extern union`,
//! against C code compiled by the system compiler.

use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary folder holding one program and its C file, removed on drop.
struct Case(PathBuf);

impl Case {
    fn new(main: &str, side: &str) -> Case {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-carr-{}-{n}", std::process::id()));
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

    fn rejects(&self, codes: &[&str]) {
        let out = self.lughac(&["check", "main.la", "--diagnostics=json"]);
        let stderr = text(&out.stderr);
        let found: Vec<&str> = stderr
            .lines()
            .filter_map(|l| l.split("\"code\":\"").nth(1))
            .map(|rest| &rest[..5])
            .collect();
        assert_eq!(found, codes, "{stderr}");
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
typedef union { int32_t i32; int64_t i64; double f64; } val_union;
typedef struct { uint8_t kind; val_union of; } val;
uint64_t sum_bytes(const uint8_t *xs, size_t n) { uint64_t s = 0; for (size_t i = 0; i < n; i++) s += xs[i]; return s; }
int64_t sum_i64(const int64_t *xs, size_t n) { int64_t s = 0; for (size_t i = 0; i < n; i++) s += xs[i]; return s; }
/* `results[i] = args[i] * 2`, as i64 values. */
void call(const val *args, size_t nargs, val *results, size_t nresults) {
    for (size_t i = 0; i < nresults && i < nargs; i++) { results[i].kind = 1; results[i].of.i64 = args[i].of.i64 * 2; }
}
double as_double(val_union u) { return u.f64; }
int64_t read_union(const val_union *u) { return u->i64; }
void set_f64(val_union *u) { u->f64 = 2.5; }
static int a, b;
typedef struct thing thing;
thing *thing_a(void) { return (thing *)&a; }
thing *thing_b(void) { return (thing *)&b; }
int count_a(thing *const *xs, size_t n) { int c = 0; for (size_t i = 0; i < n; i++) c += xs[i] == (thing *)&a; return c; }
"#;

const MAIN: &str = r#"
extern union ValUnion { int32: i32, int64: i64, float64: f64 }
extern struct Val { kind: u8, value: ValUnion }
extern type Thing;
extern fun sum_bytes(xs: u8[], n: u64): u64;
extern fun sum_i64(xs: i64[], n: u64): i64;
extern fun call(args: Val[], nargs: u64, mut results: Val[], nresults: u64);
extern fun read_union(ptr u: ValUnion): i64;
extern fun set_f64(mut u: ValUnion);
extern fun thing_a(): Thing;
extern fun thing_b(): Thing;
extern fun count_a(xs: Thing[], n: u64): i32;

fun main() {
    let bytes: u8[] = [1, 2, 3, 250];
    println(sum_bytes(bytes, bytes.len as u64));
    let empty: i64[] = [];
    println(sum_i64(empty, 0));
    let nums = [5, -7, 100];
    println(sum_i64(nums, 3));
    let args = [Val { kind: 1, value: ValUnion { int64: 20 } }, Val { kind: 1, value: ValUnion { int64: 21 } }];
    let mut results = [Val { kind: 0, value: ValUnion { int64: 0 } }; 2];
    let before = results;
    call(args, 2, mut results, 2);
    println(results[0].value.int64 + results[1].value.int64);
    println(before[0].value.int64);
    let u = ValUnion { int64: 9 };
    println(read_union(ptr u));
    let mut w = ValUnion { int32: 0 };
    set_f64(mut w);
    println(w.float64);
    println(w);
    let things = [thing_a(), thing_b(), thing_a()];
    println(count_a(things, 3));
}
"#;

#[test]
fn arrays_and_unions_cross_the_boundary() {
    Case::new(MAIN, SIDE).runs("256\n0\n98\n82\n0\n9\n2.5\n<union>\n2\n");
}

#[test]
fn what_cant_cross_is_rejected() {
    let main = "extern union U { a: i32, b: f64 }\n\
                extern fun by_value(u: U): i32;\n\
                extern fun names(xs: string[], n: u64);\n\
                fun main() { let u = U { a: 1, b: 2.0 }; let v = U { a: 1 }; println(v == v); }\n";
    Case::new(main, SIDE).rejects(&["E0409", "E0409", "E0444", "E0404"]);
}
