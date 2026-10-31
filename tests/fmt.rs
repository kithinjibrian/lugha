//! `lughac fmt` end to end (PRP-110, milestone 80): files in place, folders,
//! `--check`, untouched files, syntax errors and exit codes.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

const MESSY: &str = "fun  main(){println(1+2);}\n";
const TIDY: &str = "fun main() {\n    println(1 + 2);\n}\n";

/// An empty temporary folder, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn new() -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-fmt-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        Temp(dir)
    }

    fn write(&self, rel: &str, text: &str) -> PathBuf {
        let path = self.0.join(rel);
        std::fs::create_dir_all(path.parent().expect("a parent")).expect("writable");
        std::fs::write(&path, text).expect("writable");
        path
    }

    fn read(&self, rel: &str) -> String {
        std::fs::read_to_string(self.0.join(rel)).expect("readable")
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
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

#[test]
fn a_file_is_formatted_in_place() {
    let t = Temp::new();
    t.write("main.la", MESSY);
    let out = lughac(&t.0, &["fmt", "main.la"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(t.read("main.la"), TIDY);
    assert_eq!(text(&out.stdout), "");
}

#[test]
fn a_folder_formats_every_la_file_under_it_but_hidden_folders_and_target() {
    let t = Temp::new();
    t.write("src/a.la", MESSY);
    t.write("src/deep/b.la", MESSY);
    t.write("src/notes.txt", MESSY);
    t.write(".hidden/c.la", MESSY);
    t.write("target/d.la", MESSY);
    let out = lughac(&t.0, &["fmt"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(t.read("src/a.la"), TIDY);
    assert_eq!(t.read("src/deep/b.la"), TIDY);
    assert_eq!(t.read("src/notes.txt"), MESSY);
    assert_eq!(t.read(".hidden/c.la"), MESSY);
    assert_eq!(t.read("target/d.la"), MESSY);
}

#[test]
fn check_lists_what_would_change_writes_nothing_and_exits_1() {
    let t = Temp::new();
    t.write("src/a.la", MESSY);
    t.write("src/b.la", TIDY);
    let out = lughac(&t.0, &["fmt", "--check", "src"]);
    assert_eq!(out.status.code(), Some(1));
    let a = Path::new("src").join("a.la");
    assert_eq!(
        text(&out.stdout),
        format!("would reformat {}\n", a.display())
    );
    assert_eq!(t.read("src/a.la"), MESSY);
    let tidy = lughac(&t.0, &["fmt", "--check", "src/b.la"]);
    assert_eq!(tidy.status.code(), Some(0));
    assert_eq!(text(&tidy.stdout), "");
}

#[test]
fn a_formatted_file_is_not_rewritten() {
    let t = Temp::new();
    let path = t.write("main.la", TIDY);
    let before = std::fs::metadata(&path)
        .and_then(|m| m.modified())
        .expect("mtime");
    std::thread::sleep(std::time::Duration::from_millis(20));
    let out = lughac(&t.0, &["fmt", "main.la"]);
    assert_eq!(out.status.code(), Some(0));
    let after = std::fs::metadata(&path)
        .and_then(|m| m.modified())
        .expect("mtime");
    assert_eq!(before, after);
}

#[test]
fn a_syntax_error_is_reported_the_file_left_alone_and_the_rest_formatted() {
    let t = Temp::new();
    t.write("bad.la", "fun main( {\n");
    t.write("good.la", MESSY);
    let out = lughac(&t.0, &["fmt", "."]);
    assert_eq!(out.status.code(), Some(1));
    let stderr = text(&out.stderr);
    assert!(stderr.contains("error[E02"), "{stderr}");
    assert!(stderr.contains("bad.la"), "{stderr}");
    assert_eq!(t.read("bad.la"), "fun main( {\n");
    assert_eq!(t.read("good.la"), TIDY);
}

#[test]
fn a_missing_path_is_exit_2() {
    let t = Temp::new();
    let out = lughac(&t.0, &["fmt", "nowhere.la"]);
    assert_eq!(out.status.code(), Some(2));
    assert!(text(&out.stderr).contains("nowhere.la"));
}

#[test]
fn a_derive_block_is_formatted_like_an_impl() {
    let t = Temp::new();
    t.write(
        "main.la",
        "interface Show { fun show(self): string; }\nderive<T: Show> Show for T[] { fun show(self): string = \"list\"; }\nfun main() {}\n",
    );
    let out = lughac(&t.0, &["fmt", "main.la"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(
        t.read("main.la"),
        "interface Show {\n    fun show(self): string;\n}\nderive<T: Show> Show for T[] {\n    fun show(self): string = \"list\";\n}\nfun main() {}\n"
    );
}
