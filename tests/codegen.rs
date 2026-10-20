//! Codegen and linking end to end through the library (PRP-004): real
//! executables are built, run, and judged by their exit status.

use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::sync::atomic::{AtomicUsize, Ordering};

use lugha::codegen::{self, OptLevel};
use lugha::link::{self, LinkError};
use lugha::{check, lexer, parser};

/// The exit status of a panic (spec §5).
const PANIC: i32 = 101;

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
fn integer_overflow_panics() {
    for body in [
        "let x: i64 = 9223372036854775807; (x + 1) as i32",
        "let x: i32 = -2147483648; let y = -x; y",
        "let b: u8 = 16; (b * b) as i32",
    ] {
        assert_eq!(exit_code(body), Some(PANIC), "{body}");
    }
}

#[test]
fn bad_divisions_panic() {
    for body in [
        "1 / 0",
        "1 % 0",
        "let m: i64 = -9223372036854775808; (m / -1) as i32",
    ] {
        let status = run(&format!("fun main(): i32 {{ {body} }}"));
        assert_eq!(status.code(), Some(PANIC), "{body}: {status:?}");
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
    for dir in [
        "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "v1_1", "v1_2",
    ] {
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

/// Struct, enum, `Option`, tuple and function results through every kind of
/// call return through a hidden pointer (v17, milestone 125): the program's
/// exit code is a checksum of them all, the same at -O0 and -O2.
#[test]
fn aggregate_results_survive_every_kind_of_call_v17() {
    let src = "struct P { x: i64, y: i64 }
enum E { A(i64), B(P), C }
interface Make { fun make(self, n: i64): P; }
struct M { k: i64 }
impl M { fun make(self, n: i64): P = P { x: n * self.k, y: 1 }; }
impl P {
    fun flip(self): P = P { x: self.y, y: self.x };
    fun add(self, o: P): P = P { x: self.x + o.x, y: self.y + o.y };
}
fun direct(n: i64): P = P { x: n, y: n + 1 };
fun e(n: i64): E = if n > 0 { E.B(direct(n)) } else { E.C };
fun o(n: i64): Option<P> = if n > 2 { Some(direct(n)) } else { None };
fun t(n: i64): (P, i64) = (direct(n), n);
fun adder(k: i64): fun(i64): P = fun(n: i64): P = P { x: n + k, y: 0 };
fun through(m: Make, n: i64): P = m.make(n);
fun early(n: i64): P {
    if n == 0 { return P { x: 100, y: 0 }; }
    direct(n).flip()
}
fun main(): i32 {
    let a = direct(1);
    let b = a.flip();
    let c = a + b;
    let d = match e(2) { B(p) => p.x, A(n) => n, C => 0 };
    let f = match o(3) { Some(p) => p.y, None => 0 };
    let (g, h) = t(4);
    let add5 = adder(5);
    let i = add5(1);
    let j = through(M { k: 3 }, 2);
    let k = early(0);
    let l = early(7);
    let sum = a.x + b.x + c.x + c.y + d + f + g.x + h + i.x + j.x + k.x + l.x;
    sum as i32
}
";
    // 1 + 2 + 3 + 3 + 2 + 4 + 4 + 4 + 6 + 6 + 100 + 8
    assert_eq!(run(src).code(), Some(143));
}
