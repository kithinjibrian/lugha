//! `include_bytes` and `include_str` through lughac (milestone 128,
//! PRP-160): the package folder as the boundary, symlinks, a package's own
//! files, the size cap, rebuilds, `check`, and the IR's read-only global.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary folder of files, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn with(files: &[(&str, &str)]) -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-includes-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        for (name, text) in files {
            let path = dir.join(name);
            std::fs::create_dir_all(path.parent().expect("files sit in folders")).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Temp(dir)
    }

    fn path(&self, rel: &str) -> PathBuf {
        self.0.join(rel)
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// `lughac <args>` run in `cwd`, with `cwd` as `$HOME` so no manifest above is found.
fn lughac(cwd: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(cwd)
        .env("HOME", cwd)
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

/// The program's stdout, after asserting it ran and exited 0.
fn ran(out: &Output) -> String {
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    text(&out.stdout)
}

/// The program's stderr, after asserting lughac rejected it (exit 1).
fn rejected(out: &Output) -> String {
    assert_eq!(out.status.code(), Some(1), "{}", text(&out.stdout));
    text(&out.stderr)
}

#[test]
fn a_symlink_leading_out_of_the_package_is_e0447() {
    let t = Temp::with(&[
        ("secret.txt", "no"),
        (
            "app/main.la",
            "fun main() { print(include_str(\"link.txt\")); }\n",
        ),
    ]);
    std::os::unix::fs::symlink(t.path("secret.txt"), t.path("app/link.txt")).unwrap();
    let err = rejected(&lughac(&t.path("app"), &["run", "main.la"]));
    assert!(err.contains("error[E0447]"), "{err}");
    assert!(err.contains("outside"), "{err}");
}

#[test]
fn a_symlink_inside_the_package_is_followed() {
    let t = Temp::with(&[
        ("app/data/real.txt", "inside\n"),
        (
            "app/main.la",
            "fun main() { print(include_str(\"link.txt\")); }\n",
        ),
    ]);
    std::os::unix::fs::symlink(t.path("app/data/real.txt"), t.path("app/link.txt")).unwrap();
    assert_eq!(
        ran(&lughac(&t.path("app"), &["run", "main.la"])),
        "inside\n"
    );
}

#[test]
fn the_manifest_folder_is_the_boundary() {
    // `examples/` reaches the package's `assets/` through `..`.
    let t = Temp::with(&[
        (
            "pkg/lugha.toml",
            "[package]\nname = \"pkg\"\nversion = \"0.1.0\"\n",
        ),
        ("pkg/assets/a.txt", "asset\n"),
        (
            "pkg/examples/show.la",
            "fun main() { print(include_str(\"../assets/a.txt\")); }\n",
        ),
    ]);
    let out = lughac(&t.path("pkg"), &["run", "examples/show.la"]);
    assert_eq!(ran(&out), "asset\n");
}

#[test]
fn a_package_includes_from_its_own_folder() {
    let t = Temp::with(&[
        (
            "lib/lugha.toml",
            "[package]\nname = \"lib\"\nversion = \"0.1.0\"\n",
        ),
        (
            "lib/words.la",
            "public fun word(): string = include_str(\"word.txt\");\n",
        ),
        ("lib/word.txt", "from lib"),
        ("app/lugha.toml", "[packages]\nlib = \"../lib\"\n"),
        ("app/word.txt", "from app"),
        (
            "app/main.la",
            "import lib.words;\nfun main() { println(words.word()); }\n",
        ),
    ]);
    assert_eq!(
        ran(&lughac(&t.path("app"), &["run", "main.la"])),
        "from lib\n"
    );
}

#[test]
fn a_package_cant_reach_the_program_using_it() {
    let t = Temp::with(&[
        (
            "lib/lugha.toml",
            "[package]\nname = \"lib\"\nversion = \"0.1.0\"\n",
        ),
        (
            "lib/words.la",
            "public fun word(): string = include_str(\"../app/secret.txt\");\n",
        ),
        ("app/lugha.toml", "[packages]\nlib = \"../lib\"\n"),
        ("app/secret.txt", "secret"),
        (
            "app/main.la",
            "import lib.words;\nfun main() { println(words.word()); }\n",
        ),
    ]);
    let err = rejected(&lughac(&t.path("app"), &["check", "main.la"]));
    assert!(err.contains("error[E0447]"), "{err}");
    assert!(err.contains("lib/words.la"), "{err}");
}

#[test]
fn a_file_over_16_mib_is_e0447() {
    let t = Temp::with(&[(
        "main.la",
        "fun main() { println(include_bytes(\"big.bin\").len); }\n",
    )]);
    // A sparse file: nothing is written but its length.
    let big = std::fs::File::create(t.path("big.bin")).unwrap();
    big.set_len(16 * 1024 * 1024 + 1).unwrap();
    let err = rejected(&lughac(&t.0, &["check", "main.la"]));
    assert!(err.contains("error[E0447]"), "{err}");
    assert!(err.contains("16777217 bytes"), "{err}");
    // Exactly at the limit is fine.
    big.set_len(16 * 1024 * 1024).unwrap();
    assert_eq!(lughac(&t.0, &["check", "main.la"]).status.code(), Some(0));
}

#[test]
fn a_changed_file_is_read_again() {
    let t = Temp::with(&[
        ("main.la", "fun main() { print(include_str(\"v.txt\")); }\n"),
        ("v.txt", "one\n"),
    ]);
    assert_eq!(ran(&lughac(&t.0, &["run", "main.la"])), "one\n");
    std::fs::write(t.path("v.txt"), "two\n").unwrap();
    assert_eq!(ran(&lughac(&t.0, &["run", "main.la"])), "two\n");
}

#[test]
fn check_reports_a_missing_file() {
    let t = Temp::with(&[(
        "main.la",
        "fun main() { print(include_str(\"gone.txt\")); }\n",
    )]);
    let err = rejected(&lughac(&t.0, &["check", "main.la"]));
    assert!(
        err.contains("error[E0447]: can't include `gone.txt`: no such file"),
        "{err}"
    );
}

#[test]
fn the_bytes_are_a_read_only_global_marked_shared() {
    let t = Temp::with(&[
        (
            "main.la",
            "const B: u8[] = include_bytes(\"b.bin\");\nfun main() { println(B.len); }\n",
        ),
        ("b.bin", "abc"),
    ]);
    let out = lughac(&t.0, &["build", "main.la", "--emit=ir"]);
    let ir = ran(&out);
    // len 3, and cap 3 with the top bit set: -9223372036854775805.
    assert!(
        ir.contains("private unnamed_addr constant { i64, i64, [3 x i8] } { i64 3, i64 -9223372036854775805, [3 x i8] c\"abc\" }"),
        "{ir}"
    );
}
