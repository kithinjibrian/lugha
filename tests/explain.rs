//! `lughac explain` (PRP-116, milestone 86): every code of spec §9's table
//! is explained, every example in the explanations is checked against the
//! compiler, and diagnostics end with the hint.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

use lugha::explain::{EXPLANATIONS, explain, title};

/// An empty temporary folder, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn new() -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-explain-{}-{n}", std::process::id()));
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

fn lughac(cwd: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(cwd)
        .env("HOME", cwd)
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

/// Spec §9's table: each code and its title, without a trailing `(vN)` note.
fn table() -> Vec<(String, String)> {
    lugha::SPEC
        .lines()
        .filter_map(|line| {
            let cells: Vec<&str> = line.split(" | ").collect();
            let code = cells.first()?.strip_prefix("| ")?;
            let is_code = code.len() == 5
                && (code.starts_with('E') || code.starts_with('W'))
                && code[1..].chars().all(|c| c.is_ascii_digit());
            if !is_code || cells.len() < 2 {
                return None;
            }
            let title = cells[1].trim();
            let title = match title.rfind(" (v") {
                Some(i) if title.ends_with(')') => &title[..i],
                _ => title,
            };
            Some((code.to_string(), title.to_string()))
        })
        .collect()
}

/// The fenced blocks of `page` tagged `tag`.
fn blocks<'a>(page: &'a str, tag: &str) -> Vec<&'a str> {
    let open = format!("```{tag}\n");
    let mut out = Vec::new();
    let mut rest = page;
    while let Some(i) = rest.find(&open) {
        let body = &rest[i + open.len()..];
        let Some(end) = body.find("\n```") else { break };
        out.push(&body[..end + 1]);
        rest = &body[end + 4..];
    }
    out
}

/// The codes `lughac check` reports for `program`, errors and warnings, in
/// order; `lughac build` for E0303, which `check` leaves out since v17.
fn codes(program: &str) -> Vec<String> {
    let t = Temp::new();
    std::fs::write(t.0.join("main.la"), program).expect("writable");
    let command = match program.contains("// error: E0303") {
        true => "build",
        false => "check",
    };
    let out = lughac(&t.0, &[command, "--diagnostics=json", "main.la"]);
    text(&out.stderr)
        .lines()
        .filter_map(|l| {
            let at = l.find("\"code\":\"")? + 8;
            Some(l[at..at + 5].to_string())
        })
        .collect()
}

#[test]
fn every_code_in_the_spec_is_explained_with_its_title() {
    let table = table();
    assert_eq!(
        table.len(),
        EXPLANATIONS.len(),
        "codes in the spec vs explanations"
    );
    for (code, spec_title) in &table {
        let page = explain(code).unwrap_or_else(|| panic!("{code} has no explanation"));
        assert!(
            page.starts_with(&format!("# {code}: ")),
            "{code}: starts with its heading"
        );
        let page_title = title(page);
        assert!(
            !page_title.is_empty() && spec_title.starts_with(page_title),
            "{code}: title {page_title:?} vs the spec's {spec_title:?}"
        );
        assert!(!page.contains("TODO"), "{code} isn't written yet");
    }
}

#[test]
fn every_example_reports_its_code_and_every_fix_checks() {
    let mut failures = Vec::new();
    for (code, page) in EXPLANATIONS {
        let checked = blocks(page, "lugha");
        let shown = blocks(page, "text");
        let retired = page.contains("never reported");
        let marked = checked.iter().any(|b| {
            b.contains(&format!("// error: {code}")) || b.contains(&format!("// warning: {code}"))
        });
        if !retired && !marked && shown.is_empty() {
            failures.push(format!("{code}: no example that reports it"));
        }
        for block in checked {
            let found = codes(block);
            let errors: Vec<&String> = found.iter().filter(|c| c.starts_with('E')).collect();
            let marks: Vec<String> = block
                .lines()
                .filter_map(|l| {
                    l.split_once("// error: ")
                        .or_else(|| l.split_once("// warning: "))
                })
                .map(|(_, c)| c.trim().to_string())
                .collect();
            let also: Vec<String> = block
                .lines()
                .filter_map(|l| l.split_once("// also: "))
                .map(|(_, c)| c.trim().to_string())
                .collect();
            if marks.is_empty() {
                if !errors.is_empty() || found.iter().any(|c| c == code) {
                    failures.push(format!("{code}: a fix reports {found:?}:\n{block}"));
                }
                continue;
            }
            for mark in &marks {
                if !found.contains(mark) {
                    failures.push(format!("{code}: expected {mark}, got {found:?}:\n{block}"));
                }
            }
            for e in errors {
                if !marks.contains(e) && !also.contains(e) {
                    failures.push(format!("{code}: unexpected {e} in:\n{block}"));
                }
            }
        }
    }
    assert!(
        failures.is_empty(),
        "{} problems:\n\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

#[test]
fn explain_prints_a_page_lists_every_code_and_refuses_unknown_ones() {
    let t = Temp::new();
    let out = lughac(&t.0, &["explain", "e0501"]);
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(text(&out.stdout), explain("E0501").expect("explained"));
    let list = lughac(&t.0, &["explain"]);
    assert_eq!(list.status.code(), Some(0));
    let stdout = text(&list.stdout);
    assert_eq!(stdout.lines().count(), EXPLANATIONS.len());
    assert!(
        stdout.contains("E0501  Assignment to an immutable binding\n"),
        "{stdout}"
    );
    let bad = lughac(&t.0, &["explain", "E9999"]);
    assert_eq!(bad.status.code(), Some(2));
    assert!(text(&bad.stderr).contains("no code E9999"));
}

#[test]
fn diagnostics_end_with_a_hint_naming_their_codes() {
    let t = Temp::new();
    let one = "fun main() {\n    let total = 0;\n    total = 5;\n}\n";
    std::fs::write(t.0.join("one.la"), one).unwrap();
    let out = lughac(&t.0, &["check", "one.la"]);
    assert!(
        text(&out.stderr).ends_with("\nFor more about E0501, run `lughac explain E0501`.\n"),
        "{}",
        text(&out.stderr)
    );
    let two = "fun main() {\n    let s: string = 1;\n    let n = 0;\n    n = 2;\n    let m = 0;\n    m = 3;\n}\n";
    std::fs::write(t.0.join("two.la"), two).unwrap();
    let out = lughac(&t.0, &["check", "two.la"]);
    assert!(
        text(&out.stderr)
            .ends_with("\nFor more about E0401 and E0501, run `lughac explain <code>`.\n"),
        "{}",
        text(&out.stderr)
    );
    let json = lughac(&t.0, &["check", "--diagnostics=json", "one.la"]);
    assert!(!text(&json.stderr).contains("lughac explain"));
    let fine = lughac(&t.0, &["explain", "E0501"]);
    assert!(!text(&fine.stderr).contains("For more"));
}
