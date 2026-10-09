//! Spec §10 acceptance (PRP-016): every example program, taken from the
//! spec `lughac spec` prints, is built and run, and must produce exactly the
//! output its sentence states (CLAUDE.md rule 10).

use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

#[path = "common/spec_programs.rs"]
mod spec_programs;

/// A temporary directory removed on drop.
struct TempDir(PathBuf);

impl TempDir {
    fn with(name: &str, source: &str) -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-spec-{}-{n}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        std::fs::write(dir.join(name), source).expect("temp dir is writable");
        TempDir(dir)
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn lughac(dir: &TempDir, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(&dir.0)
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

#[test]
fn the_extractor_finds_every_section_10_program() {
    let titles: Vec<String> = spec_programs::section_10()
        .into_iter()
        .map(|p| p.title)
        .collect();
    for expected in [
        "Hello world",
        "Recursion",
        "Arrays, repeat literals and mutability",
        "Structs, for-of and casts",
        "Calling C",
        "A rejected program",
    ] {
        assert!(
            titles.iter().any(|t| t == expected),
            "missing `{expected}` in {titles:?}"
        );
    }
    // Spot-check the sentence parsing, so a broken extractor can't pass vacuously.
    let find = |title: &str| {
        spec_programs::section_10()
            .into_iter()
            .find(|p| p.title == title)
            .expect("found above")
    };
    assert_eq!(find("Hello world").stdout, "Hello, world!\n");
    assert_eq!(
        find("Calling C").stdout,
        "hello from libc\n1.4142135623730951\n"
    );
    let rejected = find("A rejected program");
    assert_eq!(rejected.exit, 1);
    assert!(
        rejected
            .stderr
            .expect("has diagnostics")
            .starts_with("error[E0401]")
    );
    assert_eq!(spec_programs::milestone_2().exit, 55);
}

#[test]
fn every_section_10_program_runs_as_stated() {
    for program in spec_programs::section_10()
        .into_iter()
        .filter(|p| p.stderr.is_none())
    {
        let dir = TempDir::with("prog.la", &program.source);
        for opt in ["-O0", "-O2"] {
            let out = lughac(&dir, &["run", opt, "prog.la"]);
            assert_eq!(
                text(&out.stdout),
                program.stdout,
                "{} {opt}: {}",
                program.title,
                text(&out.stderr)
            );
            assert_eq!(
                out.status.code(),
                Some(program.exit),
                "{} {opt}",
                program.title
            );
        }
    }
}

#[test]
fn the_rejected_program_reports_e0401_in_both_formats() {
    let program = spec_programs::section_10()
        .into_iter()
        .find(|p| p.stderr.is_some())
        .expect("§10 has a rejected program");
    // Its expected output names the file `main.la`.
    let dir = TempDir::with("main.la", &program.source);
    let human = lughac(&dir, &["check", "main.la"]);
    assert_eq!(human.status.code(), Some(program.exit));
    let expected = program.stderr.expect("filtered above");
    assert_eq!(text(&human.stderr).trim_end(), expected.trim_end());
    let json = lughac(&dir, &["check", "--diagnostics=json", "main.la"]);
    assert_eq!(json.status.code(), Some(program.exit));
    assert_eq!(text(&json.stderr), spec_programs::json_diagnostic());
}

#[test]
fn the_milestone_2_program_exits_as_stated() {
    let program = spec_programs::milestone_2();
    let dir = TempDir::with("prog.la", &program.source);
    let out = lughac(&dir, &["run", "prog.la"]);
    assert_eq!(
        out.status.code(),
        Some(program.exit),
        "{}",
        text(&out.stderr)
    );
}
