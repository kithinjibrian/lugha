//! Package layout end to end (PRP-080, milestone 51): a library with
//! `name` and `source = "src"`, an `examples/` program without a manifest,
//! a user listing the library by path, the `$HOME` boundary, E0327, E0328.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary folder of files, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn with(files: &[(&str, &str)]) -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-layout-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        for (name, text) in files {
            let path = dir.join(name);
            std::fs::create_dir_all(path.parent().expect("files sit in folders")).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Temp(dir)
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// `lughac <args>` run in `cwd`, with `home` as `$HOME`.
fn lughac(cwd: &Path, home: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(cwd)
        .env("HOME", home)
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

const MANIFEST: &str = "[package]\nname = \"glowworm\"\nversion = \"0.1.0\"\nsource = \"src\"\n";
const ARRAY: &str = "public fun zeros(n: i64): f64[] = [0.0; n];\n";
const BASICS: &str = "import glowworm.array;\nfun main() { println(array.zeros(3)); }\n";

fn library() -> Temp {
    Temp::with(&[
        ("glow/lugha.toml", MANIFEST),
        ("glow/src/array.la", ARRAY),
        ("glow/examples/basics.la", BASICS),
    ])
}

#[test]
fn an_example_uses_its_package_by_name_from_anywhere() {
    let t = library();
    let from_parent = lughac(&t.0, &t.0, &["run", "glow/examples/basics.la"]);
    assert_eq!(
        text(&from_parent.stdout),
        "[0.0, 0.0, 0.0]\n",
        "{}",
        text(&from_parent.stderr)
    );
    let inside = lughac(&t.0.join("glow/examples"), &t.0, &["run", "basics.la"]);
    assert_eq!(
        text(&inside.stdout),
        "[0.0, 0.0, 0.0]\n",
        "{}",
        text(&inside.stderr)
    );
}

#[test]
fn a_user_lists_the_package_by_path() {
    let t = library();
    let app = [
        ("app/lugha.toml", "[packages]\nglowworm = \"../glow\"\n"),
        ("app/main.la", BASICS),
    ];
    for (name, body) in app {
        std::fs::create_dir_all(t.0.join("app")).unwrap();
        std::fs::write(t.0.join(name), body).unwrap();
    }
    let run = lughac(&t.0, &t.0, &["run", "app/main.la"]);
    assert_eq!(
        text(&run.stdout),
        "[0.0, 0.0, 0.0]\n",
        "{}",
        text(&run.stderr)
    );
    std::fs::write(t.0.join("app/lugha.toml"), "[packages]\ngw = \"../glow\"\n").unwrap();
    std::fs::write(t.0.join("app/main.la"), "import gw.array;\nfun main() {}\n").unwrap();
    let misnamed = lughac(&t.0, &t.0, &["check", "app/main.la"]);
    assert_eq!(misnamed.status.code(), Some(1));
    assert!(
        text(&misnamed.stderr).contains("E0327"),
        "{}",
        text(&misnamed.stderr)
    );
}

#[test]
fn the_lookup_never_reads_above_home() {
    let t = library();
    // `glow` is the home folder here, so the manifest *in* it is used, but
    // one above it is not.
    let inside = lughac(&t.0, &t.0.join("glow"), &["run", "glow/examples/basics.la"]);
    assert_eq!(
        text(&inside.stdout),
        "[0.0, 0.0, 0.0]\n",
        "{}",
        text(&inside.stderr)
    );
    let above = Temp::with(&[
        ("lugha.toml", MANIFEST),
        ("src/array.la", ARRAY),
        ("me/examples/basics.la", BASICS),
    ]);
    let run = lughac(
        &above.0,
        &above.0.join("me"),
        &["check", "me/examples/basics.la"],
    );
    assert_eq!(run.status.code(), Some(1), "{}", text(&run.stderr));
    assert!(text(&run.stderr).contains("E0312"), "{}", text(&run.stderr));
}

#[test]
fn one_file_imported_twice_is_e0328() {
    let t = Temp::with(&[
        (
            "tool/lugha.toml",
            "[package]\nname = \"tool\"\nsource = \"src\"\n",
        ),
        (
            "tool/src/main.la",
            "import cli;\nimport tool.cli as c;\nfun main() {}\n",
        ),
        ("tool/src/cli.la", ""),
    ]);
    let check = lughac(&t.0, &t.0, &["build", "tool/src/main.la"]);
    assert_eq!(check.status.code(), Some(1));
    assert!(
        text(&check.stderr).contains("E0328"),
        "{}",
        text(&check.stderr)
    );
}

#[test]
fn a_programs_in_test_sees_its_packages_package_level_items_v17() {
    let diff = "fun patch(n: i64): i64 = n + 1;\nprivate fun hidden(): i64 = 0;\n";
    let uses = "import glowworm.diff;\n@test fun patches() { assert_eq(diff.patch(1), 2); }\n";
    let t = Temp::with(&[
        ("glow/lugha.toml", MANIFEST),
        ("glow/src/diff.la", diff),
        ("glow/test/diff_test.la", uses),
        (
            "glow/test/private_test.la",
            "import glowworm.diff;\n@test fun hides() { assert_eq(diff.hidden(), 0); }\n",
        ),
    ]);
    let glow = t.0.join("glow");
    let tested = lughac(&glow, &t.0, &["test", "test/diff_test.la"]);
    assert_eq!(tested.status.code(), Some(0), "{}", text(&tested.stderr));
    // `private` stays private to its module.
    let private = lughac(&glow, &t.0, &["check", "test/private_test.la"]);
    assert_eq!(private.status.code(), Some(1));
    assert!(
        text(&private.stderr).contains("E0313"),
        "{}",
        text(&private.stderr)
    );
    // A program outside the package still sees only `public` items.
    let outside = Temp::with(&[
        ("glow/lugha.toml", MANIFEST),
        ("glow/src/diff.la", diff),
        ("app/lugha.toml", "[packages]\nglowworm = \"../glow\"\n"),
        (
            "app/main.la",
            "import glowworm.diff;\nfun main() { println(diff.patch(1)); }\n",
        ),
    ]);
    let app = lughac(&outside.0.join("app"), &outside.0, &["check", "main.la"]);
    assert!(text(&app.stderr).contains("E0313"), "{}", text(&app.stderr));
}

#[test]
fn a_package_file_imports_its_dependencies_from_any_folder_v17() {
    let live = "import dep.raw;\nfun show(): i64 = raw.one();\n@test fun shows() { assert_eq(show(), 1); }\n";
    let t = Temp::with(&[
        (
            "kiv/lugha.toml",
            "[package]\nname = \"kiv\"\nsource = \"src\"\n\n[packages]\ndep = \"../dep\"\n",
        ),
        ("kiv/src/live.la", live),
        ("dep/lugha.toml", "[package]\nname = \"dep\"\n"),
        ("dep/raw.la", "public fun one(): i64 = 1;\n"),
    ]);
    let kiv = t.0.join("kiv");
    for (cwd, file) in [(kiv.clone(), "src/live.la"), (kiv.join("src"), "live.la")] {
        for command in ["check", "test"] {
            let out = lughac(&cwd, &t.0, &[command, file]);
            assert_eq!(
                out.status.code(),
                Some(0),
                "{command} {file}: {}",
                text(&out.stderr)
            );
        }
        // From either folder the file is the package's module.
        let out = lughac(&cwd, &t.0, &["test", file]);
        assert!(
            text(&out.stdout).contains("test kiv.live.shows ... ok"),
            "{}",
            text(&out.stdout)
        );
    }
}

#[test]
fn import_n_reaches_the_packages_lib_la_v17() {
    let lib = "/**\n * Glowworm.\n */\n\npublic import array { zeros };\n\n\
               public fun size(): i64 = 3;\n\n@test fun sized() { assert_eq(size(), 3); }\n";
    let t = Temp::with(&[
        ("glow/lugha.toml", MANIFEST),
        ("glow/src/array.la", ARRAY),
        ("glow/src/lib.la", lib),
        (
            "glow/examples/entry.la",
            "import glowworm;\nfun main() { println(glowworm.zeros(glowworm.size())); }\n",
        ),
        ("app/lugha.toml", "[packages]\nglowworm = \"../glow\"\n"),
        (
            "app/main.la",
            "import glowworm { zeros, size };\nfun main() { println(zeros(size())); }\n",
        ),
    ]);
    for (cwd, file) in [("glow", "examples/entry.la"), ("app", "main.la")] {
        let out = lughac(&t.0.join(cwd), &t.0, &["run", file]);
        assert_eq!(
            text(&out.stdout),
            "[0.0, 0.0, 0.0]\n",
            "{file}: {}",
            text(&out.stderr)
        );
    }
    let tested = lughac(&t.0.join("glow"), &t.0, &["test", "src/lib.la"]);
    assert!(
        text(&tested.stdout).contains("test glowworm.sized ... ok"),
        "{}{}",
        text(&tested.stdout),
        text(&tested.stderr)
    );
    let listed = lughac(&t.0.join("glow"), &t.0, &["test", "--list", "src/lib.la"]);
    assert_eq!(
        text(&listed.stdout),
        "test glowworm.sized src/lib.la:9:11\n"
    );
}

#[test]
fn a_package_entry_flattens_a_module_with_a_glob_v17() {
    let t = Temp::with(&[
        ("glow/lugha.toml", MANIFEST),
        ("glow/src/array.la", ARRAY),
        ("glow/src/lib.la", "public import array { * };\n"),
        ("app/lugha.toml", "[packages]\nglowworm = \"../glow\"\n"),
        (
            "app/main.la",
            "import glowworm { zeros };\nfun main() { println(zeros(2)); println(glowworm.zeros(1)); }\n",
        ),
    ]);
    let out = lughac(&t.0.join("app"), &t.0, &["run", "main.la"]);
    assert_eq!(
        text(&out.stdout),
        "[0.0, 0.0]\n[0.0]\n",
        "{}",
        text(&out.stderr)
    );
}
