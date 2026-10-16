//! Callbacks and handles end to end (v16, milestone 88, PRP-118): each case
//! is a program with its own C file, linked through a `[link]` table
//! (milestone 87), run by `lughac`.

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

const THINGS: &str = "#include <stddef.h>\n\
    typedef struct { long id; } thing;\n\
    static thing things[3] = {{1}, {2}, {3}};\n\
    thing *thing_make(long i) { return &things[i]; }\n\
    long thing_id(thing *t) { return t->id; }\n\
    thing *thing_find(long i) { return i < 3 ? &things[i] : NULL; }\n\
    long thing_is_null(thing *t) { return t == NULL; }\n";

#[test]
fn handles_come_from_c_and_go_back() {
    Case::new(
        "extern type Thing;\n\
         extern fun thing_make(i: i64): Thing;\n\
         extern fun thing_id(t: Thing): i64;\n\
         extern fun thing_find(i: i64): Option<Thing>;\n\
         extern fun thing_is_null(t: Option<Thing>): i64;\n\
         struct Holder { t: Thing, name: string }\n\
         impl Thing { fun id(self): i64 = thing_id(self); }\n\
         fun main() {\n\
             let a = thing_make(0);\n\
             let b = thing_make(1);\n\
             let h = Holder { t: b, name: \"b\" };\n\
             println(thing_id(a));\n\
             println(h.t.id());\n\
             println(a == thing_make(0));\n\
             println(a == b);\n\
             println(a != b);\n\
             println(a);\n\
             println(\"{h.t}\");\n\
             match thing_find(2) { Some(t) => println(thing_id(t)), None => println(\"none\") }\n\
             match thing_find(5) { Some(t) => println(thing_id(t)), None => println(\"none\") }\n\
             println(thing_is_null(None));\n\
             println(thing_is_null(Some(a)));\n\
             let ts = [a, b];\n\
             println(ts[1].id());\n\
             println(ts == [a, b]);\n\
             println(h);\n\
         }\n",
        THINGS,
    )
    .runs(
        "1\n2\ntrue\nfalse\ntrue\n<Thing>\n<Thing>\n3\nnone\n1\n0\n2\ntrue\nHolder { t: <Thing>, name: \"b\" }\n",
    );
}
