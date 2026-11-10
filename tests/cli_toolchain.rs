//! The `lughac` binary's choice of machine and toolchain: CPU levels, targets, the collector
//! linked into every program, and `$LUGHA_CC`.

use std::process::Command;

#[path = "common/cli.rs"]
mod cli;

use cli::{ARITH, TempDir, lughac, text};

#[test]
fn cpu_chooses_the_level_and_an_unknown_one_is_exit_2() {
    let dir = TempDir::with(&[("arith.la", ARITH.as_bytes())]);
    let native = lughac(&dir.0, &["build", "arith.la", "--cpu=native", "-O2"]);
    assert_eq!(native.status.code(), Some(0), "{}", text(&native.stderr));
    let status = Command::new(dir.0.join("arith"))
        .status()
        .expect("binary runs");
    assert_eq!(status.code(), Some(14));
    // x86-64-v3 needs AVX2; a host without it runs x86-64-v2 instead.
    let cpuinfo = std::fs::read_to_string("/proc/cpuinfo").unwrap_or_default();
    let level = if cpuinfo.contains(" avx2") {
        "x86-64-v3"
    } else {
        "x86-64-v2"
    };
    let run = lughac(&dir.0, &["run", "arith.la", "--cpu", level]);
    assert_eq!(run.status.code(), Some(14), "{}", text(&run.stderr));
    let base = lughac(&dir.0, &["build", "arith.la", "--cpu=x86-64", "-o", "base"]);
    assert_eq!(base.status.code(), Some(0), "{}", text(&base.stderr));
    let unknown = lughac(&dir.0, &["build", "arith.la", "--cpu=zen4", "-o", "zen"]);
    assert_eq!(unknown.status.code(), Some(2));
    let stderr = text(&unknown.stderr);
    for name in ["x86-64", "x86-64-v2", "x86-64-v3", "x86-64-v4", "native"] {
        assert!(stderr.contains(name), "{stderr}");
    }
    assert!(!dir.0.join("zen").exists());
}

#[test]
fn programs_carry_the_collector_and_need_no_libgc() {
    let dir = TempDir::with(&[("arith.la", ARITH.as_bytes())]);
    let out = lughac(&dir.0, &["build", "arith.la"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    // A shared library the program needs is named in its dynamic section.
    let binary = std::fs::read(dir.0.join("arith")).expect("built");
    assert!(
        !binary.windows(8).any(|w| w == b"libgc.so"),
        "the program links libgc.so"
    );
    assert!(
        binary.windows(8).any(|w| w == b"GC_init\0"),
        "the collector is linked in"
    );
}

#[test]
fn a_missing_lugha_cc_is_exit_2_and_never_reaches_a_shell() {
    let dir = TempDir::with(&[("arith.la", ARITH.as_bytes())]);
    let build = |cc: &str| {
        Command::new(env!("CARGO_BIN_EXE_lughac"))
            .args(["build", "arith.la"])
            .current_dir(&dir.0)
            .env("LUGHA_CC", cc)
            .output()
            .expect("lughac runs")
    };
    let out = build("no-such-compiler -O1");
    assert_eq!(out.status.code(), Some(2));
    let stderr = text(&out.stderr);
    assert!(
        stderr.contains("`no-such-compiler -O1` (from $LUGHA_CC) not found"),
        "{stderr}"
    );
    // `true;` is a program name like any other: there is no shell to run `touch`.
    let out = build("true; touch hacked");
    assert_eq!(out.status.code(), Some(2), "{}", text(&out.stderr));
    assert!(!dir.0.join("hacked").exists());
    // Empty means `cc`, as when unset.
    let out = build("   ");
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
}

#[test]
fn target_takes_two_triples_and_only_build_takes_it() {
    let dir = TempDir::with(&[("arith.la", ARITH.as_bytes())]);
    let unknown = lughac(&dir.0, &["build", "arith.la", "--target=sparc-sun-solaris"]);
    assert_eq!(unknown.status.code(), Some(2));
    let stderr = text(&unknown.stderr);
    for name in ["x86_64-unknown-linux-gnu", "aarch64-unknown-linux-gnu"] {
        assert!(stderr.contains(name), "{stderr}");
    }
    let host = lughac(
        &dir.0,
        &["build", "arith.la", "--target", "x86_64-unknown-linux-gnu"],
    );
    assert_eq!(host.status.code(), Some(0), "{}", text(&host.stderr));
    let cpu = lughac(
        &dir.0,
        &[
            "build",
            "arith.la",
            "--target",
            "aarch64-unknown-linux-gnu",
            "--cpu=native",
        ],
    );
    assert_eq!(cpu.status.code(), Some(2));
    assert!(text(&cpu.stderr).contains("--cpu"), "{}", text(&cpu.stderr));
    let run = lughac(
        &dir.0,
        &["run", "arith.la", "--target", "aarch64-unknown-linux-gnu"],
    );
    assert_eq!(run.status.code(), Some(2), "run takes no --target yet");
}
