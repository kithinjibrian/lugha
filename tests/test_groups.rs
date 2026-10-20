//! `lughac test` building each package folder's test files into one
//! executable (PRP-156, milestone 124): one build per group, files kept
//! apart, a broken file reported alone, filters, shared constants.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// An empty temporary folder, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn with(files: &[(&str, &str)]) -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-groups-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("writable");
        for (name, text) in files {
            let path = dir.join(name);
            std::fs::create_dir_all(path.parent().expect("a parent")).expect("writable");
            // `{dir}` in a source is this folder: tests write there by absolute path.
            let text = text.replace("{dir}", &dir.display().to_string());
            std::fs::write(path, text).expect("writable");
        }
        Temp(dir)
    }

    /// The executable path a test recorded in `name`.
    fn exe(&self, name: &str) -> String {
        std::fs::read_to_string(self.0.join(name)).unwrap_or_else(|e| panic!("{name}: {e}"))
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

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

const MANIFEST: &str = "[package]\nname = \"pack\"\nversion = \"0.1.0\"\n";

/// A test file that records its executable's path in `{dir}/<record>`, with
/// a helper and a test whose names every such file shares.
fn recording(record: &str, value: i64) -> String {
    format!(
        "fun helper(): i64 = {value};\n\
         @test\nfun works() {{\n    assert_eq(helper(), {value});\n    \
         assert(write_file(\"{{dir}}/{record}\", args()[0]).is_ok());\n}}\n"
    )
}

#[test]
fn a_package_folder_builds_once_and_keeps_its_files_apart() {
    let a = recording("a.exe", 1);
    let b = recording("b.exe", 2);
    let t = Temp::with(&[
        ("lugha.toml", MANIFEST),
        ("test/a.la", &a),
        ("test/b.la", &b),
    ]);
    let out = lughac(&t.0, &["test", "test"]);
    let stdout = text(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}{}", text(&out.stderr));
    assert!(stdout.contains("a.works ... ok"), "{stdout}");
    assert!(stdout.contains("b.works ... ok"), "{stdout}");
    assert!(stdout.contains("2 tests: 2 passed, 0 failed"), "{stdout}");
    assert_eq!(
        t.exe("a.exe"),
        t.exe("b.exe"),
        "one executable for the folder"
    );
}

#[test]
fn a_test_file_another_imports_is_loaded_once() {
    let fixtures = "public fun base(): i64 = 40;\n@test\nfun fixed() { assert_eq(base(), 40); }\n";
    let user = "import fixtures;\n@test\nfun uses() { assert_eq(fixtures.base() + 2, 42); }\n";
    let t = Temp::with(&[
        ("lugha.toml", MANIFEST),
        ("test/fixtures.la", fixtures),
        ("test/user.la", user),
    ]);
    let out = lughac(&t.0, &["test", "test"]);
    let stdout = text(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}{}", text(&out.stderr));
    assert!(stdout.contains("2 tests: 2 passed, 0 failed"), "{stdout}");
}

#[test]
fn a_broken_file_is_reported_alone_and_the_rest_share_a_build() {
    let a = recording("a.exe", 1);
    let c = recording("c.exe", 3);
    let broken = "@test\nfun bad() { let x: i64 = \"no\"; }\n";
    let t = Temp::with(&[
        ("lugha.toml", MANIFEST),
        ("test/a.la", &a),
        ("test/b.la", broken),
        ("test/c.la", &c),
    ]);
    let out = lughac(&t.0, &["test", "test"]);
    let (stdout, stderr) = (text(&out.stdout), text(&out.stderr));
    assert_eq!(out.status.code(), Some(1), "{stdout}{stderr}");
    assert!(
        stderr.contains("E0403") && stderr.contains("b.la"),
        "{stderr}"
    );
    assert!(
        stdout.contains("2 tests: 2 passed, 0 failed, 1 file didn't compile"),
        "{stdout}"
    );
    assert_eq!(t.exe("a.exe"), t.exe("c.exe"));
}

#[test]
fn other_folders_and_loose_files_build_apart() {
    let t = Temp::with(&[
        ("lugha.toml", MANIFEST),
        ("test/x/a.la", &recording("a.exe", 1)),
        ("test/y/b.la", &recording("b.exe", 2)),
    ]);
    let out = lughac(&t.0, &["test", "test"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_ne!(t.exe("a.exe"), t.exe("b.exe"));
    // No manifest: each file is its own build, as before.
    let loose = Temp::with(&[
        ("a.la", &recording("a.exe", 1)),
        ("b.la", &recording("b.exe", 2)),
    ]);
    let out = lughac(&loose.0, &["test"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_ne!(loose.exe("a.exe"), loose.exe("b.exe"));
}

#[test]
fn a_filter_builds_only_the_files_it_selects() {
    let broken = "@test\nfun bad() { let x: i64 = \"no\"; }\n";
    let t = Temp::with(&[
        ("lugha.toml", MANIFEST),
        ("test/a.la", &recording("a.exe", 1)),
        ("test/b.la", broken),
    ]);
    let out = lughac(&t.0, &["test", "test", "--filter", "a.works"]);
    let (stdout, stderr) = (text(&out.stdout), text(&out.stderr));
    assert_eq!(out.status.code(), Some(0), "{stdout}{stderr}");
    assert!(stderr.is_empty(), "{stderr}");
    assert!(
        stdout.contains("1 tests: 1 passed") || stdout.contains("1 test: 1 passed"),
        "{stdout}"
    );
}

#[test]
fn a_panicking_constant_fails_every_test_of_its_build() {
    let a = "const BAD: i64 = i64.MAX + 1;\n@test\nfun one() { assert(BAD > 0); }\n";
    let b = "@test\nfun two() { assert(true); }\n";
    let t = Temp::with(&[("lugha.toml", MANIFEST), ("test/a.la", a), ("test/b.la", b)]);
    let out = lughac(&t.0, &["test", "test"]);
    let stdout = text(&out.stdout);
    assert_eq!(out.status.code(), Some(1), "{stdout}");
    assert!(stdout.contains("a.one ... FAILED"), "{stdout}");
    assert!(stdout.contains("b.two ... FAILED"), "{stdout}");
    assert!(stdout.contains("integer overflow"), "{stdout}");
}
