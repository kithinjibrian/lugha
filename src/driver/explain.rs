//! `lughac explain [CODE]` (v16 draft decision 93, milestone 86), and the
//! hint after diagnostics that points to it (decision 95).

use std::io::Write;

use crate::diagnostic::Diagnostic;
use crate::explain::{EXPLANATIONS, explain, title};

/// Prints `code`'s explanation, or every code with its title; returns the
/// exit code: 0, or 2 for an unknown code (bad usage).
pub(super) fn run(code: Option<&str>, stdout: &mut dyn Write, stderr: &mut dyn Write) -> u8 {
    // Write errors (a closed pipe) are the reader's choice, not a failure.
    match code {
        None => {
            for (code, text) in EXPLANATIONS {
                let _ = writeln!(stdout, "{code}  {}", title(text));
            }
            0
        }
        Some(code) => match explain(code) {
            Some(text) => {
                let _ = stdout.write_all(text.as_bytes());
                0
            }
            None => {
                let _ = writeln!(
                    stderr,
                    "error: no code {code}; run `lughac explain` for the list"
                );
                2
            }
        },
    }
}

/// The line after a batch of diagnostics naming their codes, in order of
/// first appearance; `None` when none has a code.
pub(super) fn hint(diagnostics: &[Diagnostic]) -> Option<String> {
    let mut codes: Vec<&str> = Vec::new();
    for d in diagnostics {
        if !codes.contains(&d.code) {
            codes.push(d.code);
        }
    }
    match codes.as_slice() {
        [] => None,
        [one] => Some(format!(
            "For more about {one}, run `lughac explain {one}`.\n"
        )),
        [rest @ .., last] => Some(format!(
            "For more about {} and {last}, run `lughac explain <code>`.\n",
            rest.join(", ")
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::Span;

    fn d(code: &'static str) -> Diagnostic {
        Diagnostic::error(code, "m", Span::new(0, 0))
    }

    #[test]
    fn the_hint_names_each_code_once_in_order() {
        assert_eq!(hint(&[]), None);
        assert_eq!(
            hint(&[d("E0501"), d("E0501")]).as_deref(),
            Some("For more about E0501, run `lughac explain E0501`.\n")
        );
        assert_eq!(
            hint(&[d("E0403"), d("E0501"), d("E0403"), d("W0101")]).as_deref(),
            Some("For more about E0403, E0501 and W0101, run `lughac explain <code>`.\n")
        );
    }

    #[test]
    fn unknown_codes_are_bad_usage() {
        let (mut out, mut err) = (Vec::new(), Vec::new());
        assert_eq!(run(Some("E9999"), &mut out, &mut err), 2);
        assert!(String::from_utf8_lossy(&err).contains("no code E9999"));
        assert_eq!(run(Some("w0103"), &mut out, &mut err), 0);
    }
}
