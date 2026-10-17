//! C memory and callbacks with arrays end to end (v17, milestone 105,
//! PRP-137): `CPtr`, `c_read`/`c_write`, and callback parameters `T[]` and
//! `Ref<T[]>`, against C code compiled by the system compiler.

use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary folder holding one program and its C file, removed on drop.
struct Case(PathBuf);

impl Case {
    fn new(main: &str, side: &str) -> Case {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-cmem-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("main.la"), main).unwrap();
        std::fs::write(dir.join("side.c"), side).unwrap();
        std::fs::write(dir.join("lugha.toml"), "[link]\nc = [\"side.c\"]\n").unwrap();
        Case(dir)
    }

    fn run(&self, opt: &str) -> Output {
        Command::new(env!("CARGO_BIN_EXE_lughac"))
            .args(["run", "main.la", opt])
            .current_dir(&self.0)
            .env("HOME", &self.0)
            .output()
            .expect("lughac runs")
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
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
typedef struct { size_t size; uint8_t *data; } byte_vec;
typedef union { int64_t i64; double f64; } val_union;
typedef struct { uint8_t kind; val_union of; } val;
static uint8_t memory[16];
void message(byte_vec *out) { out->size = 5; out->data = malloc(5); memcpy(out->data, "hello", 5); }
void byte_vec_delete(byte_vec *v) { free(v->data); v->data = NULL; v->size = 0; }
uint8_t *memory_data(void) { return memory; }
uint64_t memory_sum(void) { uint64_t s = 0; for (int i = 0; i < 16; i++) s += memory[i]; return s; }
uint8_t *nothing(void) { return NULL; }
typedef void (*host)(void *env, const val *args, size_t nargs, val *results, size_t nresults);
int64_t call_host(host f, void *env) {
    val args[2] = { { 1, { .i64 = 40 } }, { 1, { .i64 = 2 } } };
    val results[1] = { { 0, { .i64 = 0 } } };
    f(env, args, 2, results, 1);
    return results[0].of.i64;
}
"#;

const MAIN: &str = r#"
extern union ValUnion { int64: i64, float64: f64 }
extern struct Val { kind: u8, value: ValUnion }
extern struct ByteVec { size: u64, data: CPtr }
extern fun message(out v: ByteVec);
extern fun byte_vec_delete(mut v: ByteVec);
extern fun memory_data(): CPtr;
extern fun memory_sum(): u64;
extern fun nothing(): CPtr;
extern fun call_host(f: fun(data, Val[], Ref<Val[]>), d: data): i64;

fun main() {
    let mut msg = ByteVec { size: 0, data: CPtr.null };
    message(out msg);
    let bytes = c_read(msg.data, 0, msg.size);
    println(string_from_utf8(bytes).unwrap());
    byte_vec_delete(mut msg);
    println(msg.data == CPtr.null);
    let mem = memory_data();
    c_write(mem, 4, [1, 2, 3]);
    println(memory_sum());
    println(c_read(mem, 3, 3));
    let base = 100;
    let total = call_host(fun(args: Val[], results: Ref<Val[]>) {
        let mut out = results.get();
        out[0] = Val { kind: 1, value: ValUnion { int64: args[0].value.int64 + args[1].value.int64 + base } };
        results.set(out);
    });
    println(total);
    c_read(nothing(), 0, 1);
}
"#;

#[test]
fn c_memory_is_copied_and_callbacks_take_arrays() {
    for opt in ["-O0", "-O2"] {
        let out = Case::new(MAIN, SIDE).run(opt);
        assert_eq!(
            text(&out.stdout),
            "hello\ntrue\n6\n[0, 1, 2]\n142\n",
            "{opt}: {}",
            text(&out.stderr)
        );
        assert_eq!(out.status.code(), Some(101), "{opt}");
        assert!(
            text(&out.stderr).contains("NULL CPtr given to c_read"),
            "{}",
            text(&out.stderr)
        );
    }
}
