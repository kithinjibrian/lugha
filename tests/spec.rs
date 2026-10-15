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

/// `lughac` with `stdin` fed to the program it runs (PRP-044).
fn lughac_with_stdin(dir: &TempDir, args: &[&str], stdin: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(&dir.0)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("lughac runs");
    let mut pipe = child.stdin.take().expect("stdin is piped");
    std::io::Write::write_all(&mut pipe, stdin.as_bytes()).expect("the program reads its stdin");
    drop(pipe);
    child.wait_with_output().expect("lughac finishes")
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
        "Enums and match",
        "A linked list",
        "Methods",
        "A stack",
        "A rejected enum program",
        "Patterns and equality",
        "Two modules",
        "Generics",
        "Interfaces",
        "Arguments and interpolation",
        "Functions as values",
        "Closures",
        "Array helpers",
        "Results and `?`",
        "Text helpers",
        "Reading input",
        "Interface values",
        "Type patterns",
        "Errors as interfaces",
        "Generic interfaces",
        "Iterators",
        "`Self`",
        "Converting between interfaces",
        "More numbers",
        "Bits",
        "Hashing",
        "Packages",
        "Or-patterns",
        "Unwrapping",
        "Conditional patterns",
        "`Self` in `impl` blocks",
        "More integers",
        "Iterator helpers",
        "Package versions",
        "Re-exports",
        "Printing values",
        "Printing anything",
        "`Self` in declarations",
        "`for … of mut`",
        "More iterator helpers",
        "Format specifiers",
        "Bytes and indexes",
        "Unicode text",
        "Tuples",
        "Format",
        "Import aliases",
        "Operator overloading",
        "The standard library",
        "Random numbers",
        "Copy-on-write",
        "Nested calls",
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
    assert_eq!(find("Arguments and interpolation").args, ["21"]);
    assert_eq!(find("Reading input").stdin, "5\n7\nx\n");
    assert_eq!(find("Interface values").stdout, "16.0\n1.0\ncircle\n");
    assert_eq!(
        find("Type patterns").stdout,
        "big circle\ncircle of radius 1.5\na square\n"
    );
    assert_eq!(
        find("Errors as interfaces").stdout,
        "42\nerror: not a number: x\nrange error: 500 is out of range\n"
    );
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
        // A multi-file program runs from `main.la` with its modules beside it.
        let main = if program.modules.is_empty() {
            "prog.la"
        } else {
            "main.la"
        };
        let dir = TempDir::with(main, &program.source);
        for (name, src) in &program.modules {
            // A package's files sit in folders (v8).
            let path = dir.0.join(name);
            let parent = path.parent().expect("a file has a folder");
            std::fs::create_dir_all(parent).expect("temp dir is writable");
            std::fs::write(path, src).expect("temp dir is writable");
        }
        for opt in ["-O0", "-O2"] {
            let mut command = vec!["run", opt, main];
            if !program.args.is_empty() {
                command.push("--");
                command.extend(program.args.iter().map(String::as_str));
            }
            let out = lughac_with_stdin(&dir, &command, &program.stdin);
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
fn every_rejected_program_reports_exactly_its_shown_errors() {
    let rejected: Vec<_> = spec_programs::section_10()
        .into_iter()
        .filter(|p| p.stderr.is_some())
        .collect();
    assert!(
        rejected.len() >= 2,
        "§10 shows the v0 and the enum rejected programs"
    );
    for program in rejected {
        // Their expected output names the file `main.la`.
        let dir = TempDir::with("main.la", &program.source);
        let human = lughac(&dir, &["check", "main.la"]);
        assert_eq!(human.status.code(), Some(program.exit), "{}", program.title);
        let expected = program.stderr.as_deref().expect("filtered above");
        assert_eq!(
            text(&human.stderr).trim_end(),
            expected.trim_end(),
            "{}",
            program.title
        );
        // §9 shows the JSON form of the v0 program's error.
        if program.title == "A rejected program" {
            let json = lughac(&dir, &["check", "--diagnostics=json", "main.la"]);
            assert_eq!(json.status.code(), Some(program.exit));
            assert_eq!(text(&json.stderr), spec_programs::json_diagnostic());
        }
    }
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
