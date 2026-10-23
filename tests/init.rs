//! `lughac init` end to end (PRP-098, milestone 69): the program and library
//! layouts are created, then checked, run and tested (PRP-161, milestone
//! 129: a `test/` folder); each refusal; kept files.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// An empty temporary folder, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn new() -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-init-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        Temp(dir)
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// `lughac <args>` run in `cwd`, with the temp folder as `$HOME` so the
/// manifest lookup (v15) is bounded the same way on every machine.
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

fn files(dir: &Path) -> Vec<String> {
    fn walk(root: &Path, dir: &Path, out: &mut Vec<String>) {
        for entry in std::fs::read_dir(dir).expect("readable") {
            let path = entry.expect("entry").path();
            if path.is_dir() {
                walk(root, &path, out);
            } else {
                let rel = path.strip_prefix(root).expect("inside root");
                out.push(rel.to_string_lossy().into_owned());
            }
        }
    }
    let mut out = Vec::new();
    walk(dir, dir, &mut out);
    out.sort();
    out
}

#[test]
fn a_program_is_created_checked_and_run() {
    let t = Temp::new();
    let out = lughac(&t.0, &t.0, &["init", "oyster"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "created oyster/lugha.toml\n\
         created oyster/src/main.la\n\
         created oyster/src/greeting.la\n\
         created oyster/test/greeting_test.la\n\
         created oyster/.gitignore\n\
         run it with: lughac run oyster/src/main.la\n\
         test it with: lughac test oyster\n"
    );
    assert_eq!(
        files(&t.0.join("oyster")),
        [
            ".gitignore",
            "lugha.toml",
            "src/greeting.la",
            "src/main.la",
            "test/greeting_test.la"
        ]
    );
    let manifest = std::fs::read_to_string(t.0.join("oyster/lugha.toml")).unwrap();
    assert!(
        manifest.contains("[package]\nname = \"oyster\"\nversion = \"0.1.0\"\nsource = \"src\"\n"),
        "{manifest}"
    );
    let check = lughac(&t.0, &t.0, &["check", "oyster/src/main.la"]);
    assert_eq!(check.status.code(), Some(0), "{}", text(&check.stderr));
    assert_eq!(text(&check.stderr), "");
    let run = lughac(&t.0, &t.0, &["run", "oyster/src/main.la"]);
    assert_eq!(
        text(&run.stdout),
        "Hello, world, from oyster!\n",
        "{}",
        text(&run.stderr)
    );
    assert_eq!(run.status.code(), Some(0));
    passes_its_test_and_is_formatted(&t, "oyster");
}

/// `lughac test` on the new package runs its one test, and `lughac fmt`
/// would change none of the files `init` wrote.
fn passes_its_test_and_is_formatted(t: &Temp, folder: &str) {
    let test = lughac(&t.0, &t.0, &["test", folder]);
    assert_eq!(test.status.code(), Some(0), "{}", text(&test.stderr));
    assert!(
        text(&test.stdout).contains("1 test: 1 passed, 0 failed"),
        "{}",
        text(&test.stdout)
    );
    let fmt = lughac(&t.0, &t.0, &["fmt", "--check", folder]);
    assert_eq!(fmt.status.code(), Some(0), "{}", text(&fmt.stdout));
    assert_eq!(text(&fmt.stdout), "");
}

#[test]
fn a_library_is_created_and_its_example_runs() {
    let t = Temp::new();
    let out = lughac(&t.0, &t.0, &["init", "shapes", "--lib"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "created shapes/lugha.toml\n\
         created shapes/src/lib.la\n\
         created shapes/examples/hello.la\n\
         created shapes/test/shapes_test.la\n\
         created shapes/.gitignore\n\
         run the example with: lughac run shapes/examples/hello.la\n\
         test it with: lughac test shapes\n"
    );
    assert_eq!(
        files(&t.0.join("shapes")),
        [
            ".gitignore",
            "examples/hello.la",
            "lugha.toml",
            "src/lib.la",
            "test/shapes_test.la"
        ]
    );
    let run = lughac(&t.0, &t.0, &["run", "shapes/examples/hello.la"]);
    assert_eq!(
        text(&run.stdout),
        "Hello, world, from shapes!\n",
        "{}",
        text(&run.stderr)
    );
    // From inside the package too.
    let inside = lughac(&t.0.join("shapes"), &t.0, &["run", "examples/hello.la"]);
    assert_eq!(
        text(&inside.stdout),
        "Hello, world, from shapes!\n",
        "{}",
        text(&inside.stderr)
    );
    // The library module is checked through its example, and alone (v17).
    let check = lughac(&t.0, &t.0, &["check", "shapes/examples/hello.la"]);
    assert_eq!(text(&check.stderr), "");
    let example = std::fs::read_to_string(t.0.join("shapes/examples/hello.la")).unwrap();
    assert!(example.contains("import shapes;\n"), "{example}");
    let alone = lughac(&t.0.join("shapes"), &t.0, &["check", "src/lib.la"]);
    assert_eq!(text(&alone.stderr), "");
    passes_its_test_and_is_formatted(&t, "shapes");
    // From inside the package, with no path.
    let inside = lughac(&t.0.join("shapes"), &t.0, &["test"]);
    assert_eq!(inside.status.code(), Some(0), "{}", text(&inside.stderr));
}

#[test]
fn the_current_folder_gives_the_name() {
    let t = Temp::new();
    let dir = t.0.join("glowworm");
    std::fs::create_dir(&dir).unwrap();
    let out = lughac(&dir, &t.0, &["init"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "created lugha.toml\n\
         created src/main.la\n\
         created src/greeting.la\n\
         created test/greeting_test.la\n\
         created .gitignore\n\
         run it with: lughac run src/main.la\n\
         test it with: lughac test\n"
    );
    let test = lughac(&dir, &t.0, &["test"]);
    assert_eq!(test.status.code(), Some(0), "{}", text(&test.stderr));
    let manifest = std::fs::read_to_string(dir.join("lugha.toml")).unwrap();
    assert!(manifest.contains("name = \"glowworm\""), "{manifest}");
    // `.` names the same folder.
    let again = Temp::new();
    let dot = again.0.join("firefly");
    std::fs::create_dir(&dot).unwrap();
    let out = lughac(&dot, &again.0, &["init", "."]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    let manifest = std::fs::read_to_string(dot.join("lugha.toml")).unwrap();
    assert!(manifest.contains("name = \"firefly\""), "{manifest}");
}

#[test]
fn name_overrides_the_folder() {
    let t = Temp::new();
    let out = lughac(&t.0, &t.0, &["init", "my-shell", "--name", "my_shell"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    let manifest = std::fs::read_to_string(t.0.join("my-shell/lugha.toml")).unwrap();
    assert!(manifest.contains("name = \"my_shell\""), "{manifest}");
    let run = lughac(&t.0, &t.0, &["run", "my-shell/src/main.la"]);
    assert_eq!(
        text(&run.stdout),
        "Hello, world, from my_shell!\n",
        "{}",
        text(&run.stderr)
    );
    let lib = lughac(&t.0, &t.0, &["init", "geo-lib", "--lib", "--name", "geo"]);
    assert_eq!(lib.status.code(), Some(0), "{}", text(&lib.stderr));
    let run = lughac(&t.0, &t.0, &["run", "geo-lib/examples/hello.la"]);
    assert_eq!(
        text(&run.stdout),
        "Hello, world, from geo!\n",
        "{}",
        text(&run.stderr)
    );
    // The library's test is named after the package, not the folder.
    assert!(t.0.join("geo-lib/test/geo_test.la").is_file());
    passes_its_test_and_is_formatted(&t, "geo-lib");
}

#[test]
fn an_existing_manifest_stops_it_before_anything_is_written() {
    let t = Temp::new();
    let dir = t.0.join("app");
    std::fs::create_dir(&dir).unwrap();
    std::fs::write(dir.join("lugha.toml"), "[packages]\n").unwrap();
    let out = lughac(&t.0, &t.0, &["init", "app"]);
    assert_eq!(out.status.code(), Some(2));
    assert_eq!(text(&out.stdout), "");
    assert_eq!(text(&out.stderr), "error: `app` already has a lugha.toml\n");
    assert_eq!(files(&dir), ["lugha.toml"]);
    assert_eq!(
        std::fs::read_to_string(dir.join("lugha.toml")).unwrap(),
        "[packages]\n"
    );
}

#[test]
fn a_name_that_isnt_an_identifier_is_refused_with_a_hint() {
    let t = Temp::new();
    let out = lughac(&t.0, &t.0, &["init", "my-shell"]);
    assert_eq!(out.status.code(), Some(2));
    assert_eq!(
        text(&out.stderr),
        "error: `my-shell` isn't a valid package name\n  \
         = help: try `lughac init my-shell --name my_shell`\n"
    );
    assert!(!t.0.join("my-shell").exists());
    // A name starting with a digit gets a leading `_`.
    let out = lughac(&t.0, &t.0, &["init", "2d"]);
    assert!(
        text(&out.stderr).contains("--name _2d"),
        "{}",
        text(&out.stderr)
    );
}

#[test]
fn keywords_and_std_are_refused() {
    let t = Temp::new();
    for name in ["fun", "i64", "std"] {
        let out = lughac(&t.0, &t.0, &["init", "--lib", "--name", name, "pkg"]);
        assert_eq!(out.status.code(), Some(2), "{name}");
        assert!(
            text(&out.stderr).starts_with(&format!("error: `{name}` isn't a valid package name")),
            "{}",
            text(&out.stderr)
        );
        assert!(!t.0.join("pkg").exists(), "{name}");
    }
}

#[test]
fn existing_files_are_kept_byte_for_byte() {
    let t = Temp::new();
    let dir = t.0.join("tool");
    std::fs::create_dir_all(dir.join("src")).unwrap();
    std::fs::write(dir.join(".gitignore"), "mine\n").unwrap();
    std::fs::write(
        dir.join("src/main.la"),
        "fun main() { println(\"mine\"); }\n",
    )
    .unwrap();
    std::fs::create_dir(dir.join("test")).unwrap();
    std::fs::write(dir.join("test/greeting_test.la"), "// mine\n").unwrap();
    let out = lughac(&t.0, &t.0, &["init", "tool"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "created tool/lugha.toml\n\
         kept    tool/src/main.la (already exists)\n\
         created tool/src/greeting.la\n\
         kept    tool/test/greeting_test.la (already exists)\n\
         kept    tool/.gitignore (already exists)\n\
         run it with: lughac run tool/src/main.la\n\
         test it with: lughac test tool\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.join("test/greeting_test.la")).unwrap(),
        "// mine\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.join(".gitignore")).unwrap(),
        "mine\n"
    );
    let run = lughac(&t.0, &t.0, &["run", "tool/src/main.la"]);
    assert_eq!(text(&run.stdout), "mine\n");
}

#[test]
fn an_unwritable_folder_is_exit_2_with_the_system_message() {
    let t = Temp::new();
    // A file where the folder should be: creating `src/` inside it fails.
    std::fs::write(t.0.join("blocked"), "").unwrap();
    let out = lughac(&t.0, &t.0, &["init", "blocked"]);
    assert_eq!(out.status.code(), Some(2));
    assert!(
        text(&out.stderr).starts_with("error: cannot create `blocked"),
        "{}",
        text(&out.stderr)
    );
}
