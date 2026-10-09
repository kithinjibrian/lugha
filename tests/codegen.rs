//! Codegen and linking end to end through the library (PRP-004): real
//! executables are built, run, and judged by their exit status.

use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::sync::atomic::{AtomicUsize, Ordering};

use lugha::codegen::{self, OptLevel};
use lugha::link::{self, LinkError};
use lugha::{check, lexer, parser};

/// SIGILL, raised by `llvm.trap` on x86-64 (`ud2`).
const SIGILL: i32 = 4;

/// A temporary directory removed on drop.
struct TempDir(PathBuf);

impl TempDir {
    fn new() -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-codegen-{}-{n}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        TempDir(dir)
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn build_and_run(src: &str, opt: OptLevel) -> ExitStatus {
    let (tokens, _) = lexer::lex(src).expect("lexes");
    let (program, _) = parser::parse(&tokens).expect("parses");
    let (checked, _) = check::check(&program).unwrap_or_else(|e| panic!("{src}: {e:?}"));
    let dir = TempDir::new();
    let (object, exe) = (dir.0.join("prog.o"), dir.0.join("prog"));
    codegen::emit_object(
        &program,
        &checked,
        &codegen::SourceInfo {
            name: "test.la",
            text: src,
        },
        opt,
        &object,
    )
    .unwrap_or_else(|e| panic!("{src}: {e}"));
    link::link(&[&object], &exe).unwrap_or_else(|e| panic!("{src}: {e}"));
    Command::new(&exe).status().expect("built program runs")
}

/// Runs `src` at -O0 and -O2, asserts both agree, and returns the status.
fn run(src: &str) -> ExitStatus {
    let o0 = build_and_run(src, OptLevel::O0);
    let o2 = build_and_run(src, OptLevel::O2);
    assert_eq!(
        (o0.code(), o0.signal()),
        (o2.code(), o2.signal()),
        "{src}: -O0 vs -O2"
    );
    o0
}

fn exit_code(body: &str) -> Option<i32> {
    run(&format!("fun main(): i32 {{ {body} }}")).code()
}

#[test]
fn milestone_1_program_exits_with_14() {
    assert_eq!(exit_code("2 + 3 * 4"), Some(14));
}

#[test]
fn division_truncates_and_remainder_takes_the_dividends_sign() {
    // -3 as the low 8 bits of an exit status is 253.
    assert_eq!(exit_code("-7 / 2"), Some(253));
    assert_eq!(exit_code("7 % -3"), Some(1));
    assert_eq!(exit_code("-(3 - 10)"), Some(7));
}

#[test]
fn arithmetic_wraps_until_milestone_4() {
    // i64::MAX + 1 wraps to MIN; MIN / 2^62 = -2, i.e. exit 254.
    assert_eq!(
        exit_code("let x: i64 = 9223372036854775807; ((x + 1) / 4611686018427387904) as i32"),
        Some(254)
    );
}

#[test]
fn bad_divisions_trap() {
    for body in [
        "1 / 0",
        "1 % 0",
        "let m: i64 = -9223372036854775808; (m / -1) as i32",
    ] {
        let status = run(&format!("fun main(): i32 {{ {body} }}"));
        assert_eq!(status.signal(), Some(SIGILL), "{body}: {status:?}");
    }
}

#[test]
fn void_main_exits_with_zero() {
    assert_eq!(run("fun main() { }").code(), Some(0));
    assert_eq!(run("fun main() { 5 }").code(), Some(0));
}

#[test]
fn link_failure_reports_cc_stderr() {
    let dir = TempDir::new();
    let missing = Path::new("/nonexistent/lugha/prog.o");
    match link::link(&[missing], &dir.0.join("prog")) {
        Err(LinkError::Failed { stderr, .. }) => assert!(!stderr.is_empty()),
        other => panic!("expected LinkError::Failed, got {other:?}"),
    }
}

#[test]
fn run_programs_agree_at_o0_and_o2() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/programs");
    let mut checked = 0;
    for dir in ["m2", "m3", "m4"] {
        for entry in std::fs::read_dir(root.join(dir)).expect("program dir exists") {
            let path = entry.expect("entry").path();
            let exit = path.with_extension("exit");
            if path.extension().is_some_and(|e| e == "la") && exit.exists() {
                let src = std::fs::read_to_string(&path).expect("program is UTF-8");
                let expected: i32 = std::fs::read_to_string(&exit)
                    .expect(".exit")
                    .trim()
                    .parse()
                    .expect("integer");
                // `run` asserts -O0 and -O2 agree; a trap shows as 128 + signal, as in `lughac run`.
                let status = run(&src);
                let code = status.code().or(status.signal().map(|s| 128 + s));
                assert_eq!(code, Some(expected), "{}", path.display());
                checked += 1;
            }
        }
    }
    assert!(checked >= 25, "only {checked} run programs found");
}

#[test]
fn let_initialiser_reads_the_outer_binding() {
    assert_eq!(exit_code("let x: i32 = 2; let x = x + 1; x"), Some(3));
}

#[test]
fn runtime_compiles_without_warnings() {
    let dir = TempDir::new();
    let source = dir.0.join("lugha_rt.c");
    std::fs::write(&source, lugha::link::RUNTIME_SOURCE).expect("temp dir is writable");
    let out = Command::new("cc")
        .args(["-std=c11", "-Wall", "-Wextra", "-Werror", "-c"])
        .arg(&source)
        .arg("-o")
        .arg(dir.0.join("lugha_rt.o"))
        .output()
        .expect("cc runs");
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
