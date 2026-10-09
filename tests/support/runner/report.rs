//! Compares outcomes with expectations and formats the failure report.

use super::{Case, Malformed, Mismatch, Mode, Outcome, lossy};

/// Longest stdout/stderr excerpt shown in a report, per side.
const SHOW_BYTES: usize = 2_000;

/// Lists every way `outcome` differs from `case`'s expectations, exit code first.
pub fn compare(case: &Case, outcome: &Outcome) -> Vec<Mismatch> {
    let (stdout, stderr, code) = match outcome {
        Outcome::Exited {
            stdout,
            stderr,
            code,
        } => (stdout, stderr, code),
        Outcome::TimedOut(limit) => return vec![problem(format!("timed out after {limit:?}"))],
        Outcome::SpawnFailed(e) => return vec![problem(format!("could not run lughac: {e}"))],
    };
    let (want_exit, want_stdout, want_stderr) = match &case.mode {
        Mode::Run {
            stdout,
            stderr,
            exit,
        } => (*exit, Some(stdout), stderr.as_ref()),
        Mode::Reject { stderr } => (1, None, Some(stderr)),
    };

    let mut mismatches = Vec::new();
    if *code != Some(want_exit) {
        let actual = code.map_or("terminated by signal".to_string(), |c| c.to_string());
        mismatches.push(differ("exit code", want_exit.to_string(), actual));
    }
    if let Some(want) = want_stdout.filter(|want| *want != stdout) {
        mismatches.push(differ("stdout", show(want), show(stdout)));
    }
    if let Some(want) = want_stderr.filter(|want| *want != stderr) {
        mismatches.push(differ("stderr", show(want), show(stderr)));
    }
    mismatches
}

fn problem(what: String) -> Mismatch {
    Mismatch { what, values: None }
}

fn differ(what: &str, expected: String, actual: String) -> Mismatch {
    Mismatch {
        what: what.to_string(),
        values: Some((expected, actual)),
    }
}

/// Quotes and escapes output so whitespace differences are visible.
fn show(bytes: &[u8]) -> String {
    if bytes.len() <= SHOW_BYTES {
        return format!("{:?}", lossy(bytes));
    }
    let rest = bytes.len() - SHOW_BYTES;
    format!("{:?} … ({rest} more bytes)", lossy(&bytes[..SHOW_BYTES]))
}

/// Formats failed cases as `N of M programs failed:` followed by one block per mismatch.
pub fn format_report(failures: &[(String, Vec<Mismatch>)], total: usize) -> String {
    let mut report = format!("{} of {total} programs failed:\n", failures.len());
    for (name, mismatches) in failures {
        for m in mismatches {
            report += &format!("\n--- {name}: {}\n", m.what);
            if let Some((expected, actual)) = &m.values {
                report += &format!("  expected: {expected}\n  actual:   {actual}\n");
            }
        }
    }
    report
}

/// Formats malformed entries as `N malformed test files:` followed by one line each.
pub fn format_malformed(malformed: &[Malformed]) -> String {
    let mut report = format!("{} malformed test files:\n\n", malformed.len());
    for m in malformed {
        report += &format!("--- {}: {}\n", m.name, m.reason);
    }
    report
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::time::Duration;

    use super::*;
    use crate::support::fixture::exited;

    fn case(name: &str, mode: Mode) -> Case {
        Case {
            name: name.to_string(),
            path: PathBuf::from(name),
            mode,
        }
    }

    fn run(stdout: &str, exit: i32) -> Mode {
        Mode::Run {
            stdout: stdout.as_bytes().to_vec(),
            stderr: None,
            exit,
        }
    }

    fn whats(mismatches: Vec<Mismatch>) -> Vec<String> {
        mismatches.into_iter().map(|m| m.what).collect()
    }

    #[test]
    fn matching_outcome_has_no_mismatches() {
        assert!(compare(&case("a.la", run("55\n", 0)), &exited("55\n", "ignored", 0)).is_empty());
    }

    #[test]
    fn missing_trailing_newline_is_a_mismatch() {
        let got = compare(&case("a.la", run("55\n", 0)), &exited("55", "", 0));
        assert_eq!(whats(got), ["stdout"]);
    }

    #[test]
    fn each_difference_is_reported() {
        let got = compare(&case("a.la", run("55\n", 14)), &exited("", "", 0));
        assert_eq!(whats(got), ["exit code", "stdout"]);
    }

    #[test]
    fn reject_mode_expects_lughac_exit_1() {
        let c = case(
            "bad.la",
            Mode::Reject {
                stderr: b"err\n".to_vec(),
            },
        );
        assert!(compare(&c, &exited("", "err\n", 1)).is_empty());
        assert_eq!(whats(compare(&c, &exited("", "err\n", 0))), ["exit code"]);
    }

    #[test]
    fn timeout_is_a_mismatch() {
        let got = compare(
            &case("loop.la", run("", 0)),
            &Outcome::TimedOut(Duration::from_secs(10)),
        );
        assert_eq!(whats(got), ["timed out after 10s"]);
    }

    #[test]
    fn report_escapes_and_truncates() {
        let long = "x".repeat(2_005);
        let mismatches = compare(&case("m1/a.la", run("55\n", 0)), &exited(&long, "", 0));
        let report = format_report(&[("m1/a.la".into(), mismatches)], 3);
        assert!(report.starts_with("1 of 3 programs failed:\n"), "{report}");
        assert!(report.contains("--- m1/a.la: stdout\n"), "{report}");
        assert!(report.contains("expected: \"55\\n\""), "{report}");
        assert!(report.contains("… (5 more bytes)"), "{report}");
    }
}
