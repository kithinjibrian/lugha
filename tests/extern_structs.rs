//! C structs by value end to end (v16, milestone 90, PRP-120): every shape
//! of `extern struct` to and from C, as arguments, results and through
//! callbacks, against C code compiled by the system compiler, which computes
//! every result: a wrong calling convention shows as a wrong number.

use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

#[path = "common/c_shapes.rs"]
mod c_shapes;
use c_shapes::{DECLS, PROGRAMS, SIDE};

/// A temporary folder holding one program, removed on drop.
struct Case(PathBuf);

impl Case {
    /// `main.la` and `side.c`, linked by `[link] c = ["side.c"]`.
    fn new(main: &str, side: &str) -> Case {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-cb-{}-{n}", std::process::id()));
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

#[test]
fn every_shape_goes_to_c_and_back() {
    let (_, main, prints) = PROGRAMS[0];
    Case::new(&[DECLS, main].concat(), SIDE).runs(prints);
}

#[test]
fn large_structs_and_full_registers_go_in_memory() {
    let (_, main, prints) = PROGRAMS[1];
    Case::new(&[DECLS, main].concat(), SIDE).runs(prints);
}

#[test]
fn callbacks_take_and_return_structs() {
    let (_, main, prints) = PROGRAMS[2];
    Case::new(&[DECLS, main].concat(), SIDE).runs(prints);
}

#[test]
fn bad_extern_structs_are_rejected() {
    let c = "long f(void) { return 0; }\n";
    Case::new("extern struct S { s: string }\nfun main() {}\n", c).rejects(&["E0437"]);
    Case::new(
        "struct P { x: i64 }\nextern fun f(p: P);\nfun main() {}\n",
        c,
    )
    .rejects(&["E0409"]);
}

#[test]
fn nullable_handle_fields_v17() {
    let side = "#include <stddef.h>\n\
        typedef struct W W;\n\
        typedef struct { long n; W *w; } Slot;\n\
        static int thing = 7;\n\
        W *make(void) { return (W *)&thing; }\n\
        long peek(W *w) { return *(int *)w; }\n\
        void fill(Slot *s, int some) { s->n = 5; s->w = some ? make() : NULL; }\n\
        long sum(Slot s) { return s.n + (s.w ? peek(s.w) : 100); }\n\
        long count(const Slot *xs, long n) { long k = 0; for (long i = 0; i < n; i++) k += xs[i].w != NULL; return k; }\n";
    Case::new(
        "extern type W;\n\
         extern struct Slot { n: i64, w: Option<W> }\n\
         extern fun make(): W;\n\
         extern fun peek(w: W): i64;\n\
         extern fun fill(out s: Slot, some: i32);\n\
         extern fun sum(s: Slot): i64;\n\
         extern fun count(xs: Slot[], n: i64): i64;\n\
         fun main() {\n\
             let mut s = Slot { n: 0, w: None };\n\
             println(sum(s));\n\
             fill(out s, 1);\n\
             if let Some(w) = s.w { println(peek(w)); }\n\
             println(sum(s));\n\
             fill(out s, 0);\n\
             println(s.w.is_none());\n\
             s.w = Some(make());\n\
             let xs = [s, Slot { n: 1, w: None }, Slot { n: 2, w: Some(make()) }];\n\
             println(count(xs, 3));\n\
             println(sum(xs[2]));\n\
         }\n",
        side,
    )
    .runs("100\n7\n12\ntrue\n2\n9\n");
}

#[test]
fn option_handles_held_anywhere_v17() {
    let side = "typedef struct W W;\n\
        static int a = 3, b = 4;\n\
        W *make(int which) { return (W *)(which ? &b : &a); }\n\
        long peek(W *w) { return *(int *)w; }\n";
    Case::new(
        "extern type W;\n\
         extern fun make(which: i32): W;\n\
         extern fun peek(w: W): i64;\n\
         struct Holder { label: string, w: Option<W>, nested: Option<Option<W>> }\n\
         enum Slot { Empty, Full(Option<W>, i64) }\n\
         fun total(xs: Option<W>[]): i64 {\n\
             let mut sum = 0;\n\
             for x of xs { if let Some(w) = x { sum += peek(w); } }\n\
             sum\n\
         }\n\
         fun main() {\n\
             let mut xs: Option<W>[] = [Some(make(0)), None];\n\
             xs.push(Some(make(1)));\n\
             println(total(xs));\n\
             xs[1] = Some(make(1));\n\
             let last = xs.pop();\n\
             println(total(xs));\n\
             println(last.is_some());\n\
             let mut h = Holder { label: \"h\", w: None, nested: Some(None) };\n\
             println(h.w.is_none());\n\
             h.w = xs[0];\n\
             h.nested = Some(xs[1]);\n\
             if let Some(Some(w)) = h.nested { println(peek(w)); }\n\
             let copy = h;\n\
             println(copy.w == h.w);\n\
             let s = Slot.Full(h.w, 2);\n\
             match s { Full(Some(w), n) => println(peek(w) * n), _ => println(0) }\n\
             let r = Ref.new(xs[0]);\n\
             r.set(None);\n\
             println(r.get().is_none());\n\
             let t = (xs[1], 5);\n\
             match t { (Some(w), n) => println(peek(w) + n), (None, _) => println(0) }\n\
             match xs[0] { Some(w) => println(peek(w)), None => println(0) }\n\
             let ys: (Option<W>, i64)[] = [(None, 1), (Some(make(1)), 2)];\n\
             for (o, k) of ys { if let Some(w) = o { println(peek(w) * k); } }\n\
         }\n",
        side,
    )
    .runs("7\n7\ntrue\ntrue\n4\ntrue\n6\ntrue\n9\n3\n8\n");
}
