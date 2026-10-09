//! Human-readable diagnostics via codespan-reporting (DECISION-011).
//!
//! Program diagnostics and internal errors are both turned into a `Report`,
//! so human and JSON output always come from the same record (spec §9).

use codespan_reporting::diagnostic::{
    Diagnostic as CsDiagnostic, Label as CsLabel, Severity as CsSeverity,
};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::NoColor;
use codespan_reporting::term::{self, Chars, Config};

use super::source::Source;
use crate::diagnostic::{Diagnostic, Label, Severity};
use crate::span::Span;

/// Anything printed as an error or warning: a program diagnostic, or an
/// internal error (no code, maybe no span).
pub(super) struct Report {
    pub severity: Severity,
    pub code: Option<&'static str>,
    pub message: String,
    pub span: Option<Span>,
    pub label: Option<String>,
    pub labels: Vec<Label>,
    pub help: Option<String>,
}

impl From<&Diagnostic> for Report {
    fn from(d: &Diagnostic) -> Self {
        Report {
            severity: d.severity,
            code: Some(d.code),
            message: d.message.clone(),
            span: Some(d.span),
            label: d.label.clone(),
            labels: d.labels.clone(),
            help: d.help.clone(),
        }
    }
}

impl Report {
    /// An internal error: no code, and a span only if it points into the source.
    pub(super) fn internal(message: impl Into<String>, span: Option<Span>) -> Self {
        Report {
            severity: Severity::Error,
            code: None,
            message: message.into(),
            span,
            label: None,
            labels: Vec::new(),
            help: None,
        }
    }
}

/// Renders `report` against `source` in the human format, ending in a blank line.
pub(super) fn human(report: &Report, source: &Source) -> String {
    let severity = match report.severity {
        Severity::Error => CsSeverity::Error,
        Severity::Warning => CsSeverity::Warning,
    };
    let mut diagnostic = CsDiagnostic::new(severity).with_message(&report.message);
    if let Some(code) = report.code {
        diagnostic = diagnostic.with_code(code);
    }
    let mut labels = Vec::new();
    if let Some(span) = report.span {
        let text = report.label.clone().unwrap_or_default();
        labels.push(CsLabel::primary((), span.start..span.end).with_message(text));
    }
    for label in &report.labels {
        let range = label.span.start..label.span.end;
        labels.push(CsLabel::secondary((), range).with_message(&label.message));
    }
    diagnostic = diagnostic.with_labels(labels);
    if let Some(help) = &report.help {
        diagnostic = diagnostic.with_notes(vec![format!("help: {help}")]);
    }

    let file = SimpleFile::new(&source.name, &source.text);
    let config = Config {
        chars: Chars::ascii(),
        ..Config::default()
    };
    let mut out = NoColor::new(Vec::new());
    if term::emit_to_write_style(&mut out, &config, &file, &diagnostic).is_err() {
        // Only reachable if a span lies outside the source — still say what went wrong.
        return format!("error: {}\n\n", report.message);
    }
    let text = String::from_utf8_lossy(&out.into_inner()).into_owned();
    // codespan pads some lines with spaces; strip them (DECISION-011).
    text.split('\n')
        .map(str::trim_end)
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
pub(super) mod tests {
    use super::*;
    use crate::diagnostic::Diagnostic;

    /// The spec §10 rejected program and its E0401 diagnostic, built by hand
    /// until the checker exists.
    pub(in crate::driver) fn e0401() -> (Source, Diagnostic) {
        let text = "fun main() {\n    let x: i32 = 5;\n    let y = x + 2.5;\n}\n";
        let source = Source {
            name: "main.la".into(),
            text: text.into(),
        };
        let d = Diagnostic::error(
            "E0401",
            "float literal where i32 expected",
            Span::new(49, 52),
        )
        .with_primary_label("expected i32")
        .with_label(Span::new(45, 46), "this operand is i32");
        (source, d)
    }

    #[test]
    fn e0401_matches_the_spec_example_exactly() {
        let (source, d) = e0401();
        let expected = "error[E0401]: float literal where i32 expected\n  --> main.la:3:17\n  |\n3 |     let y = x + 2.5;\n  |             -   ^^^ expected i32\n  |             |\n  |             this operand is i32\n\n";
        assert_eq!(human(&Report::from(&d), &source), expected);
    }

    #[test]
    fn help_is_a_note_and_internal_errors_have_no_code() {
        let source = Source {
            name: "a.la".into(),
            text: "2e5\n".into(),
        };
        let d = Diagnostic::error(
            "E0107",
            "float literal needs a fractional part",
            Span::new(0, 3),
        )
        .with_help("write 2.0e5");
        let text = human(&Report::from(&d), &source);
        assert!(text.contains("= help: write 2.0e5"), "{text}");
        let internal = human(&Report::internal("cannot read `x.la`: gone", None), &source);
        assert!(
            internal.starts_with("error: cannot read `x.la`: gone\n"),
            "{internal}"
        );
        assert!(
            internal.lines().all(|l| l == l.trim_end()),
            "trailing whitespace: {internal:?}"
        );
    }
}
