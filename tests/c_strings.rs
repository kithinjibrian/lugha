//! Strings from C end to end (v16, milestone 89, PRP-119): extern results
//! and callback parameters copied into Lugha strings, `NULL` as `None`,
//! invalid UTF-8 replaced, each case with its own C file through `[link]`.

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

const SIDE: &str = "#include <stddef.h>\n\
    #include <string.h>\n\
    static char buf[32];\n\
    const char *greeting(void) { strcpy(buf, \"hello\"); return buf; }\n\
    void scribble(void) { strcpy(buf, \"XXXXX\"); }\n\
    const char *maybe_name(long i) { return i == 0 ? NULL : \"ada\"; }\n\
    const char *raw_bytes(void) { return \"Hi\\xFF!\\xE2\\x82\"; }\n\
    long text_len(const char *s) { return s == NULL ? -1 : (long)strlen(s); }\n\
    const char *null_text(void) { return NULL; }\n\
    void each_word(void (*f)(const char *, void *), void *d) { f(\"alpha\", d); f(\"\\xCE\\xB2\" \"eta\", d); }\n\
    void maybe_word(void (*f)(const char *, void *), void *d) { f(NULL, d); f(\"x\", d); }\n";

const EXTERNS: &str = "extern fun greeting(): string;\n\
    extern fun scribble();\n\
    extern fun maybe_name(i: i64): Option<string>;\n\
    extern fun raw_bytes(): string;\n\
    extern fun text_len(s: Option<string>): i64;\n\
    extern fun null_text(): string;\n\
    extern fun getenv(name: string): Option<string>;\n\
    extern fun each_word(f: fun(string, data), u: data);\n\
    extern fun maybe_word(f: fun(Option<string>, data), u: data);\n";

#[test]
fn c_text_is_copied_into_lugha_strings() {
    Case::new(
        &[
            EXTERNS,
            "fun main() {\n\
                 let g = greeting();\n\
                 scribble();\n\
                 println(g);\n\
                 println(maybe_name(0));\n\
                 println(maybe_name(1));\n\
                 println(text_len(None));\n\
                 println(text_len(Some(\"four\")));\n\
                 println(getenv(\"LUGHA_SURELY_UNSET_89\"));\n\
                 println(getenv(\"HOME\") != None);\n\
             }\n",
        ]
        .concat(),
        SIDE,
    )
    .runs("hello\nNone\nSome(\"ada\")\n-1\n4\nNone\ntrue\n");
}

#[test]
fn invalid_utf8_from_c_becomes_replacement_characters() {
    Case::new(
        &[
            EXTERNS,
            "fun main() {\n\
                 let r = raw_bytes();\n\
                 println(r);\n\
                 println(r.len);\n\
                 println(r.char_count());\n\
             }\n",
        ]
        .concat(),
        SIDE,
    )
    .runs("Hi\u{FFFD}!\u{FFFD}\u{FFFD}\n12\n6\n");
}

#[test]
fn callbacks_receive_strings() {
    Case::new(
        &[
            EXTERNS,
            "fun main() {\n\
                 let total = Ref.new(0);\n\
                 each_word(fun(w: string) { total.update(fun(n) = n + w.len); println(w); });\n\
                 println(total.get());\n\
                 maybe_word(fun(w: Option<string>) { println(w); });\n\
             }\n",
        ]
        .concat(),
        SIDE,
    )
    .runs("alpha\n\u{3b2}eta\n10\nNone\nSome(\"x\")\n");
}

#[test]
fn null_for_a_plain_string_panics_with_advice() {
    let case = Case::new(
        &[EXTERNS, "fun main() { println(null_text()); }\n"].concat(),
        SIDE,
    );
    let out = case.lughac(&["run", "main.la"]);
    assert_eq!(out.status.code(), Some(101), "{}", text(&out.stderr));
    assert!(
        text(&out.stderr)
            .contains("panic: C gave NULL for a `string`; declare it `Option<string>`"),
        "{}",
        text(&out.stderr)
    );
}

#[test]
fn a_callback_can_not_hand_c_a_string() {
    Case::new(
        "extern fun ask(f: fun(i64): string);\nfun main() {}\n",
        "long ask(void) { return 0; }\n",
    )
    .rejects(&["E0409"]);
}

#[test]
fn replacement_follows_the_preludes_decoding_rules() {
    // Each invalid sequence is U+FFFD and the copy moves one byte on, as `chars()` does:
    // overlong `C0 80`, a surrogate `ED A0 80`, above U+10FFFF `F4 90 80 80`,
    // truncated `F0 9F`, and a valid four-byte `F0 9F 98 80` (U+1F600).
    Case::new(
        "extern fun tricky(): string;\n\
         fun main() { let t = tricky(); println(t); for c of t.chars() { print(\"{c as u32} \"); } println(); }\n",
        "const char *tricky(void) { return \"\\xC0\\x80|\\xED\\xA0\\x80|\\xF4\\x90\\x80\\x80|\\xF0\\x9F\\x98\\x80|\\xF0\\x9F\"; }\n",
    )
    .runs(
        "\u{FFFD}\u{FFFD}|\u{FFFD}\u{FFFD}\u{FFFD}|\u{FFFD}\u{FFFD}\u{FFFD}\u{FFFD}|\u{1F600}|\u{FFFD}\u{FFFD}\n\
         65533 65533 124 65533 65533 65533 124 65533 65533 65533 65533 124 128512 124 65533 65533 \n",
    );
}
