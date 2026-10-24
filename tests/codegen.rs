//! Codegen and linking end to end through the library (PRP-004): real
//! executables are built, run, and judged by their exit status.

use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::sync::atomic::{AtomicUsize, Ordering};

use lugha::codegen::{self, Cpu, OptLevel};
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
        Cpu::default(),
        &object,
    )
    .unwrap_or_else(|e| panic!("{src}: {e}"));
    link::link(&[&object], &exe).unwrap_or_else(|e| panic!("{src}: {e}"));
    Command::new(&exe).status().expect("built program runs")
}

/// The object file `src` makes at -O2 for `cpu`, and the exit code of the
/// program linked from it.
fn object_and_exit(src: &str, cpu: Cpu) -> (Vec<u8>, Option<i32>) {
    let (tokens, _) = lexer::lex(src).expect("lexes");
    let (program, _) = parser::parse(&tokens).expect("parses");
    let (checked, _) = check::check(&program).unwrap_or_else(|e| panic!("{src}: {e:?}"));
    let dir = TempDir::new();
    let (object, exe) = (dir.0.join("prog.o"), dir.0.join("prog"));
    let source = codegen::SourceInfo {
        name: "test.la",
        text: src,
    };
    codegen::emit_object(&program, &checked, &source, OptLevel::O2, cpu, &object)
        .unwrap_or_else(|e| panic!("{src}: {e}"));
    link::link(&[&object], &exe).unwrap_or_else(|e| panic!("{src}: {e}"));
    let status = Command::new(&exe).status().expect("built program runs");
    (
        std::fs::read(&object).expect("object is readable"),
        status.code(),
    )
}

/// True if `code` holds a `popcnt`: `F3`, an optional REX prefix, `0F B8`.
fn has_popcnt(code: &[u8]) -> bool {
    let plain = code.windows(3).any(|w| w == [0xF3, 0x0F, 0xB8]);
    let rex = code
        .windows(4)
        .any(|w| w[0] == 0xF3 && (0x40..=0x4F).contains(&w[1]) && w[2..] == [0x0F, 0xB8]);
    plain || rex
}

#[test]
fn the_default_cpu_is_baseline_x86_64_and_cpu_levels_add_instructions_v17() {
    // `args()` keeps the value unknown until the program runs, so LLVM
    // can't fold the count away: 12345 has six bits set.
    let src = "fun main(): i32 { let x = args().len as u64 * 12345; x.count_ones() as i32 }\n";
    let (baseline, code) = object_and_exit(src, Cpu::default());
    assert_eq!(code, Some(6));
    assert!(!has_popcnt(&baseline), "baseline x86-64 has no popcnt");
    let (v2, code) = object_and_exit(src, Cpu::V2);
    assert_eq!(code, Some(6));
    assert!(has_popcnt(&v2), "x86-64-v2 counts bits with popcnt");
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

/// Struct values split into fields (v17, milestone 126): joins from `if`
/// and `match`, a loop carrying a struct, nested structs, enum payloads,
/// and struct, function and interface values passed as arguments, all
/// agreeing at -O0 and -O2.
#[test]
fn split_struct_values_behave_as_whole_ones_v17() {
    let src = "struct In { a: i64, b: bool }
struct Out { i: In, n: i64 }
enum Shape { Dot, Box(i64, i64), Ring(In) }
interface Area { fun area(self): i64; }
struct Sq { s: i64 }
impl Sq { fun area(self): i64 = self.s * self.s; }
fun pick(c: bool, x: Out, y: Out): Out = if c { x } else { y };
fun size(s: Shape): i64 = match s { Dot => 1, Box(w, h) => w * h, Ring(i) => if i.b { i.a } else { 0 } };
fun twice(f: fun(i64): i64, n: i64): i64 = f(f(n));
fun area_of(a: Area): i64 = a.area();
fun same(p: Out, q: Out): bool = p == q;
fun main(): i32 {
    let x = Out { i: In { a: 2, b: true }, n: 3 };
    let y = Out { i: In { a: 5, b: false }, n: 7 };
    let mut acc = Out { i: In { a: 0, b: false }, n: 0 };
    for k in 0..4 {
        let p = pick(k % 2 == 0, x, y);
        acc = Out { i: In { a: acc.i.a + p.i.a, b: p.i.b }, n: acc.n + p.n };
    }
    let shapes = [Shape.Dot, Shape.Box(3, 4), Shape.Ring(In { a: 9, b: true })];
    let mut total = 0;
    for s of shapes { total += size(s); }
    let add3 = fun(n: i64): i64 = n + 3;
    let eq = if same(x, x) && !same(x, y) { 1 } else { 0 };
    let r = acc.i.a + acc.n + total + twice(add3, 1) + area_of(Sq { s: 4 }) + eq;
    r as i32
}
";
    // acc: a = 2+5+2+5 = 14, n = 3+7+3+7 = 20; total = 1 + 12 + 9 = 22;
    // twice = 7; area = 16; eq = 1.
    assert_eq!(run(src).code(), Some(14 + 20 + 22 + 7 + 16 + 1));
}
