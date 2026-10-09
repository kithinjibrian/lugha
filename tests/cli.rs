//! The `lughac` binary end to end (PRP-005): commands, emit modes,
//! diagnostics formats and exit codes.

use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

const ARITH: &str = "fun main(): i32 { 2 + 3 * 4 }\n";

/// A temporary working directory removed on drop.
struct TempDir(PathBuf);

impl TempDir {
    fn with(files: &[(&str, &[u8])]) -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-cli-{}-{n}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        for (name, contents) in files {
            std::fs::write(dir.join(name), contents).expect("temp dir is writable");
        }
        TempDir(dir)
    }

    fn entries(&self) -> Vec<String> {
        let mut names: Vec<_> = std::fs::read_dir(&self.0)
            .expect("temp dir is readable")
            .map(|e| e.expect("entry").file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        names
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn lughac(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(dir)
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

#[test]
fn build_writes_the_file_stem_or_dash_o() {
    let dir = TempDir::with(&[("arith.la", ARITH.as_bytes())]);
    let out = lughac(&dir.0, &["build", "arith.la"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    let status = Command::new(dir.0.join("arith"))
        .status()
        .expect("binary runs");
    assert_eq!(status.code(), Some(14));
    assert_eq!(
        lughac(&dir.0, &["build", "arith.la", "-o", "out"])
            .status
            .code(),
        Some(0)
    );
    assert_eq!(dir.entries(), ["arith", "arith.la", "out"]);
}

#[test]
fn run_passes_the_exit_code_through() {
    let dir = TempDir::with(&[
        ("arith.la", ARITH.as_bytes()),
        ("abort.la", b"extern fun abort();\nfun main() { abort(); }"),
    ]);
    assert_eq!(lughac(&dir.0, &["run", "arith.la"]).status.code(), Some(14));
    assert_eq!(
        lughac(&dir.0, &["run", "-O2", "arith.la"]).status.code(),
        Some(14)
    );
    // 128 + SIGABRT, as a shell would report it.
    assert_eq!(
        lughac(&dir.0, &["run", "abort.la"]).status.code(),
        Some(134)
    );
    assert_eq!(
        dir.entries(),
        ["abort.la", "arith.la"],
        "run leaves no files behind"
    );
}

#[test]
fn check_reports_errors_with_exit_1() {
    let dir = TempDir::with(&[("ok.la", ARITH.as_bytes()), ("bad.la", b"fun main() { @ }")]);
    let ok = lughac(&dir.0, &["check", "ok.la"]);
    assert_eq!(
        (ok.status.code(), ok.stdout.len(), ok.stderr.len()),
        (Some(0), 0, 0)
    );
    let bad = lughac(&dir.0, &["check", "bad.la"]);
    assert_eq!(bad.status.code(), Some(1));
    assert!(
        text(&bad.stderr).starts_with("error[E0101]: unexpected character '@'\n"),
        "{}",
        text(&bad.stderr)
    );
}

#[test]
fn emit_prints_a_stage_and_writes_nothing() {
    let dir = TempDir::with(&[("arith.la", ARITH.as_bytes())]);
    let tokens = text(&lughac(&dir.0, &["build", "--emit=tokens", "arith.la"]).stdout);
    assert!(
        tokens.starts_with("1:1 Fun\n1:5 Ident(\"main\")\n1:9 LParen\n"),
        "{tokens}"
    );
    assert!(!tokens.contains("Eof"), "{tokens}");
    let ast = text(&lughac(&dir.0, &["build", "--emit=ast", "arith.la"]).stdout);
    assert_eq!(ast, "(fun main () i32 (block (+ 2 (* 3 4))))\n");
    let ir = text(&lughac(&dir.0, &["build", "--emit=ir", "arith.la"]).stdout);
    assert!(ir.contains("define i32 @main()"), "{ir}");
    assert_eq!(dir.entries(), ["arith.la"]);
}

#[test]
fn json_diagnostics_are_one_line_each_and_nothing_else() {
    let dir = TempDir::with(&[("bad.la", b"fun main() { @ # }")]);
    let out = lughac(&dir.0, &["check", "--diagnostics=json", "bad.la"]);
    assert_eq!(out.status.code(), Some(1));
    let stderr = text(&out.stderr);
    let lines: Vec<_> = stderr.lines().collect();
    assert_eq!(lines.len(), 2, "{stderr}");
    for line in lines {
        assert!(
            line.starts_with(r#"{"severity":"error","code":"E0101","#),
            "{line}"
        );
        assert!(line.ends_with('}'), "{line}");
    }
}

#[test]
fn unreadable_files_and_bad_usage_exit_2() {
    let dir = TempDir::with(&[]);
    let missing = lughac(&dir.0, &["check", "nope.la"]);
    assert_eq!(missing.status.code(), Some(2));
    assert!(
        text(&missing.stderr).contains("cannot read `nope.la`"),
        "{}",
        text(&missing.stderr)
    );
    assert_eq!(lughac(&dir.0, &[]).status.code(), Some(2));
    assert_eq!(
        lughac(&dir.0, &["build", "--frobnicate", "x.la"])
            .status
            .code(),
        Some(2)
    );
    assert_eq!(lughac(&dir.0, &["--help"]).status.code(), Some(0));
}

#[test]
fn invalid_utf8_is_e0110() {
    let dir = TempDir::with(&[("latin1.la", b"fun main() { \xe9 }")]);
    let out = lughac(&dir.0, &["check", "latin1.la"]);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        text(&out.stderr).starts_with("error[E0110]: source is not valid UTF-8\n"),
        "{}",
        text(&out.stderr)
    );
}

#[test]
fn internal_errors_have_no_code_or_span() {
    // Every v0 construct compiles since PRP-015; an unreadable file is the
    // remaining internal error a user can trigger without a broken toolchain.
    let dir = TempDir::with(&[("ok.la", b"fun main() {}")]);
    let json = lughac(&dir.0, &["build", "--diagnostics=json", "nope.la"]);
    assert_eq!(json.status.code(), Some(2));
    let line = text(&json.stderr);
    assert!(
        line.starts_with(r#"{"severity":"error","code":null,"#),
        "{line}"
    );
    assert!(line.contains(r#""span":null"#), "{line}");
    assert_eq!(dir.entries(), ["ok.la"]);
}

#[test]
fn signals_are_not_exit_codes() {
    // Sanity check of the mapping used above: the program itself dies by SIGABRT (6).
    let dir = TempDir::with(&[("abort.la", b"extern fun abort();\nfun main() { abort(); }")]);
    assert_eq!(
        lughac(&dir.0, &["build", "abort.la"]).status.code(),
        Some(0)
    );
    let status = Command::new(dir.0.join("abort"))
        .status()
        .expect("binary runs");
    assert_eq!(status.signal(), Some(6));
}

#[test]
fn e0401_json_matches_the_spec_example() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let out = lughac(
        root,
        &["check", "--diagnostics=json", "tests/programs/m3/e0401.la"],
    );
    assert_eq!(out.status.code(), Some(1));
    let expected = concat!(
        r#"{"severity":"error","code":"E0401","message":"float literal where i32 expected","file":"tests/programs/m3/e0401.la","#,
        r#""span":{"start":{"line":3,"col":17,"offset":49},"end":{"line":3,"col":20,"offset":52}},"#,
        r#""label":"expected i32","#,
        r#""labels":[{"span":{"start":{"line":3,"col":13,"offset":45},"end":{"line":3,"col":14,"offset":46}},"message":"this operand is i32"}],"#,
        r#""help":null}"#,
        "\n"
    );
    assert_eq!(text(&out.stderr), expected);
}

#[test]
fn every_valid_acceptance_program_type_checks() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut checked = 0;
    for dir in ["tests/programs/m1", "tests/programs/m2"] {
        for entry in std::fs::read_dir(root.join(dir)).expect("program dir exists") {
            let path = entry.expect("entry").path();
            // Reject-mode cases (with a .stderr and no .exit) are meant to fail.
            if path.extension().is_some_and(|e| e == "la") && path.with_extension("exit").exists() {
                let rel = path.strip_prefix(root).expect("under the repo");
                let out = lughac(root, &["check", rel.to_str().expect("UTF-8 path")]);
                assert_eq!(
                    out.status.code(),
                    Some(0),
                    "{}: {}",
                    rel.display(),
                    text(&out.stderr)
                );
                checked += 1;
            }
        }
    }
    assert!(checked >= 15, "only {checked} programs checked");
}

#[test]
fn spec_prints_the_bundled_specification() {
    let dir = TempDir::with(&[]);
    let out = lughac(&dir.0, &["spec"]);
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(text(&out.stdout), lugha::SPEC);
    assert!(out.stderr.is_empty(), "{}", text(&out.stderr));
    assert_eq!(lughac(&dir.0, &["spec", "extra"]).status.code(), Some(2));
}
