//! Linux on ARM64 (v17, milestone 133, PRP-165): programs from
//! `tests/programs/` built with `--target aarch64-unknown-linux-gnu` and run
//! under `qemu-aarch64` match their expectation files, green threads,
//! actors, panics and the collector included; extern structs by value are
//! E0449 there. Without `aarch64-linux-gnu-gcc` and `qemu-aarch64` each test
//! prints a note and passes, as on a machine that can't build for ARM.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

const TARGET: &str = "aarch64-unknown-linux-gnu";

/// A cross-section of the language: each a `tests/programs/` case stem.
const CASES: [&str; 18] = [
    "m1/arith",
    "m1/div_zero",
    "m7/methods",
    "m8/stack_overflow",
    "m9/patterns",
    "m13/interp",
    "m29/hashing",
    "m44/format",
    "m101/maps",
    "m101/missing_key",
    "m108/float_bits",
    "m122/concat",
    "m91/fan_out",
    "m91/gc",
    "m91/deadlock",
    "m92/bank",
    "m92/many_actors",
    "m131/spawned_alloc",
];

/// An empty temporary folder, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn new() -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-arm-{}-{n}", std::process::id()));
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

/// Whether this machine has the ARM cross compiler and qemu; prints why not.
fn can_build_for_arm(test: &str) -> bool {
    let runs = |program: &str| {
        Command::new(program)
            .arg("--version")
            .output()
            .is_ok_and(|o| o.status.success())
    };
    let ok = runs("aarch64-linux-gnu-gcc") && runs("qemu-aarch64");
    if !ok {
        eprintln!("{test}: skipped, needs aarch64-linux-gnu-gcc and qemu-aarch64");
    }
    ok
}

fn root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

fn lughac(cwd: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(cwd)
        .env_remove("LUGHA_CC")
        .output()
        .expect("lughac runs")
}

fn qemu(exe: &Path) -> Output {
    Command::new("qemu-aarch64")
        .args(["-L", "/usr/aarch64-linux-gnu"])
        .arg(exe)
        .output()
        .expect("qemu-aarch64 runs")
}

/// The case's expectation file with `extension`, if it has one.
fn expected(stem: &str, extension: &str) -> Option<String> {
    std::fs::read_to_string(root().join(format!("tests/programs/{stem}.{extension}"))).ok()
}

#[test]
fn programs_run_on_arm_as_they_do_on_x86_64_v17() {
    if !can_build_for_arm("programs_run_on_arm") {
        return;
    }
    let t = Temp::new();
    for stem in CASES {
        let exe = t.0.join(stem.replace('/', "_"));
        let source = format!("tests/programs/{stem}.la");
        let built = lughac(
            root(),
            &[
                "build",
                &source,
                "--target",
                TARGET,
                "-o",
                exe.to_str().unwrap(),
            ],
        );
        assert_eq!(
            built.status.code(),
            Some(0),
            "{stem}: {}",
            text(&built.stderr)
        );
        let elf = std::fs::read(&exe).expect("built");
        // ELF `e_machine`, little-endian at offset 18: 183 is AArch64.
        assert_eq!(u16::from_le_bytes([elf[18], elf[19]]), 183, "{stem}");
        let out = qemu(&exe);
        let code: i32 = expected(stem, "exit").map_or(0, |e| e.trim().parse().expect("a code"));
        assert_eq!(
            out.status.code(),
            Some(code),
            "{stem}: {}",
            text(&out.stderr)
        );
        if let Some(stdout) = expected(stem, "stdout") {
            assert_eq!(text(&out.stdout), stdout, "{stem}");
        }
        if let Some(stderr) = expected(stem, "stderr") {
            assert_eq!(text(&out.stderr), stderr, "{stem}");
        }
    }
}

const BY_VALUE: &str = "extern struct P { x: i64, y: i64 }
extern fun sum(p: P): i64;
extern fun make(x: i64): P;
fun main() { println(sum(make(1))); }
";

const BY_POINTER: &str = "extern struct P { x: i64, y: i64 }
extern fun sum(ptr p: P): i64;
fun main() { let p = P { x: 1, y: 2 }; println(sum(ptr p)); }
";

#[test]
fn extern_structs_by_value_are_e0449_on_arm_only_v17() {
    let t = Temp::new();
    std::fs::write(t.0.join("value.la"), BY_VALUE).unwrap();
    std::fs::write(t.0.join("pointer.la"), BY_POINTER).unwrap();
    let arm = lughac(&t.0, &["build", "value.la", "--target", TARGET]);
    assert_eq!(arm.status.code(), Some(1), "{}", text(&arm.stderr));
    let stderr = text(&arm.stderr);
    assert_eq!(stderr.matches("error[E0449]").count(), 2, "{stderr}");
    // Checked before anything is compiled, so it needs no cross compiler.
    let check = lughac(&t.0, &["check", "value.la"]);
    assert_eq!(check.status.code(), Some(0), "{}", text(&check.stderr));
    if !can_build_for_arm("extern_structs_by_value") {
        return;
    }
    // By pointer it links on ARM; the missing C function is the only problem.
    let pointer = lughac(&t.0, &["build", "pointer.la", "--target", TARGET]);
    let stderr = text(&pointer.stderr);
    assert!(!stderr.contains("E0449"), "{stderr}");
    assert!(
        stderr.contains("sum"),
        "an undefined `sum`, nothing else: {stderr}"
    );
}
