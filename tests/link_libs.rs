//! Linking C libraries end to end (v16, milestone 87, PRP-117): `[link]`
//! tables in the program's and its packages' `lugha.toml` — `libs`,
//! `paths`, `include`, `c`, `pkg-config` — through `lughac`, against a
//! static library each test builds with `cc` and `ar`, and a fake
//! `pkg-config` named by `$PKG_CONFIG`.

use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary folder of files, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn with(files: &[(&str, &str)]) -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-link-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let temp = Temp(dir);
        for (name, text) in files {
            temp.write(name, text);
        }
        temp
    }

    fn write(&self, name: &str, text: &str) {
        let path = self.0.join(name);
        std::fs::create_dir_all(path.parent().expect("files sit in folders")).unwrap();
        std::fs::write(path, text).unwrap();
    }

    /// `libfixture.a` in `folder`, defining `fixture_add`.
    fn static_library(&self, folder: &str) {
        let dir = self.0.join(folder);
        std::fs::create_dir_all(&dir).unwrap();
        let source = dir.join("fixture.c");
        std::fs::write(
            &source,
            "long fixture_add(long a, long b) { return a + b; }\n",
        )
        .unwrap();
        let object = dir.join("fixture.o");
        let cc = Command::new("cc")
            .arg("-c")
            .arg(&source)
            .arg("-o")
            .arg(&object)
            .status();
        assert!(cc.unwrap().success(), "cc builds the fixture");
        let ar = Command::new("ar")
            .arg("rcs")
            .arg(dir.join("libfixture.a"))
            .arg(&object)
            .status();
        assert!(ar.unwrap().success(), "ar builds the fixture library");
    }

    /// An executable shell script `name` whose body is `body`.
    fn script(&self, name: &str, body: &str) -> PathBuf {
        let path = self.0.join(name);
        std::fs::write(&path, format!("#!/bin/sh\n{body}\n")).unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
        path
    }

    /// `lughac <args>` in this folder, with `$HOME` here and `pkg_config` as `$PKG_CONFIG`.
    fn lughac(&self, args: &[&str], pkg_config: Option<&Path>) -> Output {
        let mut command = Command::new(env!("CARGO_BIN_EXE_lughac"));
        command.args(args).current_dir(&self.0).env("HOME", &self.0);
        match pkg_config {
            Some(path) => command.env("PKG_CONFIG", path),
            None => command.env_remove("PKG_CONFIG"),
        };
        command.output().expect("lughac runs")
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

const ADD: &str =
    "extern fun fixture_add(a: i64, b: i64): i64;\nfun main() { println(fixture_add(2, 3)); }\n";

/// Asserts `out` is a success that printed `stdout`.
fn ran(out: &Output, stdout: &str) {
    assert_eq!(
        (out.status.code(), text(&out.stdout).as_str()),
        (Some(0), stdout),
        "stderr: {}",
        text(&out.stderr)
    );
}

/// Asserts `out` failed with exit 1 and a stderr holding each of `parts`.
fn failed(out: &Output, parts: &[&str]) {
    let stderr = text(&out.stderr);
    assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
    for part in parts {
        assert!(stderr.contains(part), "no `{part}` in: {stderr}");
    }
}

#[test]
fn libs_and_paths_link_a_static_library() {
    let t = Temp::with(&[
        ("main.la", ADD),
        (
            "lugha.toml",
            "[link]\nlibs = [\"fixture\"]\npaths = [\"vendor/lib\"]\n",
        ),
    ]);
    t.static_library("vendor/lib");
    ran(&t.lughac(&["run", "main.la"], None), "5\n");
    let built = t.lughac(&["build", "main.la", "-o", "prog"], None);
    assert_eq!(built.status.code(), Some(0), "{}", text(&built.stderr));
    let out = Command::new(t.0.join("prog")).output().unwrap();
    assert_eq!(text(&out.stdout), "5\n");
}

#[test]
fn a_packages_table_reaches_the_programs_link() {
    let t = Temp::with(&[
        (
            "game/main.la",
            "import fix.math;\nfun main() { println(math.add(4, 5)); }\n",
        ),
        ("game/lugha.toml", "[packages]\nfix = \"../fix\"\n"),
        (
            "fix/lugha.toml",
            "[package]\nname = \"fix\"\n[link]\nlibs = [\"fixture\"]\npaths = [\"lib\"]\n",
        ),
        (
            "fix/math.la",
            "extern fun fixture_add(a: i64, b: i64): i64;\npublic fun add(a: i64, b: i64): i64 = fixture_add(a, b);\n",
        ),
    ]);
    t.static_library("fix/lib");
    ran(&t.lughac(&["run", "game/main.la"], None), "9\n");
}

#[test]
fn c_files_compile_with_their_packages_headers() {
    let t = Temp::with(&[
        (
            "main.la",
            "extern fun shim_twice(x: i64): i64;\nfun main() { println(shim_twice(21)); }\n",
        ),
        (
            "lugha.toml",
            "[link]\nc = [\"shim/shim.c\"]\ninclude = [\"inc\"]\n",
        ),
        (
            "inc/helper.h",
            "static inline long twice(long x) { return 2 * x; }\n",
        ),
        (
            "shim/shim.c",
            "#include \"helper.h\"\nlong shim_twice(long x) { return twice(x); }\n",
        ),
    ]);
    ran(&t.lughac(&["run", "main.la"], None), "42\n");
}

#[test]
fn pkg_config_flags_link_the_library() {
    let t = Temp::with(&[
        ("main.la", ADD),
        ("lugha.toml", "[link]\npkg-config = [\"fixture\"]\n"),
    ]);
    t.static_library("vendor/lib");
    let lib = t.0.join("vendor/lib");
    let fake = t.script(
        "fake-pkg-config",
        &format!(
            "[ \"$1\" = --libs ] && [ \"$2\" = fixture ] && echo \"-L{} -lfixture -pthread\" && exit 0\nexit 1",
            lib.display()
        ),
    );
    ran(&t.lughac(&["run", "main.la"], Some(&fake)), "5\n");
}

#[test]
fn c_files_build_with_gtks_sse_cflags_v17() {
    let t = Temp::with(&[
        (
            "main.la",
            "extern fun shim_twice(x: i64): i64;\nfun main() { println(shim_twice(21)); }\n",
        ),
        (
            "lugha.toml",
            "[link]\nc = [\"shim.c\"]\npkg-config = [\"gtkish\"]\n",
        ),
        ("shim.c", "long shim_twice(long x) { return 2 * x; }\n"),
    ]);
    let fake = t.script(
        "fake-pkg-config",
        "[ \"$1\" = --cflags ] && echo \"-mfpmath=sse -msse -msse2 -pthread\" && exit 0\n\
         [ \"$1\" = --libs ] && echo \"-pthread\" && exit 0\nexit 1",
    );
    ran(&t.lughac(&["run", "main.la"], Some(&fake)), "42\n");
}

#[test]
fn pkg_config_problems_are_e0329() {
    let t = Temp::with(&[
        ("main.la", ADD),
        ("lugha.toml", "[link]\npkg-config = [\"gtk4\"]\n"),
    ]);
    let missing = t.0.join("no-such-pkg-config");
    failed(
        &t.lughac(&["build", "main.la"], Some(&missing)),
        &["E0329", "pkg-config = [\"gtk4\"]", "isn't installed"],
    );
    let unknown = t.script("unknown", "echo 'Package gtk4 was not found' >&2\nexit 1");
    failed(
        &t.lughac(&["build", "main.la"], Some(&unknown)),
        &["E0329", "Package gtk4 was not found"],
    );
    let sneaky = t.script("sneaky", "echo '-lgtk-4 -Wl,-rpath,/x'");
    failed(
        &t.lughac(&["build", "main.la"], Some(&sneaky)),
        &["E0329", "`-Wl,-rpath,/x`"],
    );
}

#[test]
fn a_c_file_that_doesnt_compile_is_e0330() {
    let t = Temp::with(&[
        ("main.la", ADD),
        ("lugha.toml", "[link]\nc = [\"bad.c\"]\n"),
        ("bad.c", "long f(void) { return 1 }\n"),
    ]);
    failed(
        &t.lughac(&["build", "main.la"], None),
        &["E0330", "bad.c", "c = [\"bad.c\"]", "the C compiler said"],
    );
}

#[test]
fn a_missing_library_is_e0331_not_an_internal_error() {
    let t = Temp::with(&[
        ("main.la", ADD),
        ("lugha.toml", "[link]\nlibs = [\"nosuchlib_lugha\"]\n"),
    ]);
    failed(
        &t.lughac(&["build", "main.la"], None),
        &[
            "E0331",
            "linking failed",
            "libs = [\"nosuchlib_lugha\"]",
            "nosuchlib_lugha",
        ],
    );
    let json = t.lughac(&["build", "main.la", "--diagnostics=json"], None);
    assert_eq!(json.status.code(), Some(1));
    let all = format!("{}{}", text(&json.stdout), text(&json.stderr));
    assert!(all.contains("\"E0331\""), "{all}");
}

#[test]
fn without_a_table_an_unresolved_extern_stays_internal() {
    let t = Temp::with(&[("main.la", ADD)]);
    let out = t.lughac(&["build", "main.la"], None);
    assert_eq!(out.status.code(), Some(2), "{}", text(&out.stderr));
}

#[test]
fn check_validates_the_table_and_starts_nothing() {
    let t = Temp::with(&[
        ("main.la", ADD),
        ("lugha.toml", "[link]\nc = [\"missing.c\"]\n"),
    ]);
    failed(
        &t.lughac(&["check", "main.la"], None),
        &["E0322", "missing.c"],
    );
    t.write("lugha.toml", "[link]\nlibs = [\"-lx\"]\n");
    failed(&t.lughac(&["check", "main.la"], None), &["E0321", "-lx"]);
    t.write("lugha.toml", "[link]\npkg-config = [\"fixture\"]\n");
    let marker = t.0.join("ran");
    let fake = t.script(
        "marking-pkg-config",
        &format!("touch '{}'\nexit 1", marker.display()),
    );
    let out = t.lughac(&["check", "main.la"], Some(&fake));
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert!(!marker.exists(), "`lughac check` ran pkg-config");
    // `build` does run it, so the fake is a fair witness.
    failed(&t.lughac(&["build", "main.la"], Some(&fake)), &["E0329"]);
    assert!(marker.exists());
}

#[test]
fn lughac_test_links_the_same_libraries() {
    let t = Temp::with(&[
        (
            "add_test.la",
            "extern fun fixture_add(a: i64, b: i64): i64;\n@test\nfun adds() { assert_eq(fixture_add(2, 3), 5); }\n",
        ),
        (
            "lugha.toml",
            "[link]\nlibs = [\"fixture\"]\npaths = [\"vendor/lib\"]\n",
        ),
    ]);
    t.static_library("vendor/lib");
    let out = t.lughac(&["test", "add_test.la"], None);
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}{}",
        text(&out.stdout),
        text(&out.stderr)
    );
    assert!(
        text(&out.stdout).ends_with("1 test: 1 passed, 0 failed\n"),
        "{}",
        text(&out.stdout)
    );
}

#[test]
fn a_packages_own_example_links_its_c_files_once_v17() {
    let t = Temp::with(&[
        (
            "shimlib/lugha.toml",
            "[package]\nname = \"shimlib\"\nsource = \"src\"\n\n[link]\nc = [\"c/shim.c\"]\n",
        ),
        (
            "shimlib/c/shim.c",
            "long shim_twice(long x) { return 2 * x; }\n",
        ),
        (
            "shimlib/src/api.la",
            "extern fun shim_twice(x: i64): i64;\npublic fun twice(x: i64): i64 = shim_twice(x);\n",
        ),
        (
            "shimlib/examples/demo.la",
            "import shimlib.api;\nfun main() { println(api.twice(21)); }\n",
        ),
    ]);
    ran(
        &t.lughac(&["run", "shimlib/examples/demo.la"], None),
        "42\n",
    );
}
