//! Diagnostic records shared by every compiler stage.
//!
//! A `Diagnostic` holds exactly the fields of the spec §9 JSON format. Stages
//! only create them; rendering (human or JSON) belongs to the driver, so both
//! formats always come from the same record.
//!
//! Depends on: span.

use crate::span::Span;

/// How serious a diagnostic is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// The program is invalid; the pipeline stops after this stage.
    Error,
    /// Worth reporting, but compilation continues (W01xx codes).
    Warning,
}

/// A secondary span with its own message, e.g. "this operand is i32".
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    /// The highlighted source range.
    pub span: Span,
    /// Text shown next to the highlight.
    pub message: String,
}

/// One problem found in a user's program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    /// Error or warning.
    pub severity: Severity,
    /// Stable code such as `E0102`; never reused or renumbered (spec §9).
    pub code: &'static str,
    /// One-line description of the problem.
    pub message: String,
    /// The primary location.
    pub span: Span,
    /// Text shown under the primary span, e.g. `expected i32`.
    pub label: Option<String>,
    /// Secondary locations, in the order they should be shown.
    pub labels: Vec<Label>,
    /// Optional suggestion, e.g. `write 2.0e5`.
    pub help: Option<String>,
}

impl Diagnostic {
    /// Creates an error with no labels or help.
    ///
    /// # Examples
    ///
    /// ```
    /// use lugha::diagnostic::{Diagnostic, Severity};
    /// use lugha::span::Span;
    ///
    /// let d = Diagnostic::error("E0102", "unterminated string", Span::new(8, 12));
    /// assert_eq!(d.severity, Severity::Error);
    /// ```
    pub fn error(code: &'static str, message: impl Into<String>, span: Span) -> Self {
        Self::new(Severity::Error, code, message.into(), span)
    }

    /// Creates a warning with no labels or help.
    pub fn warning(code: &'static str, message: impl Into<String>, span: Span) -> Self {
        Self::new(Severity::Warning, code, message.into(), span)
    }

    fn new(severity: Severity, code: &'static str, message: String, span: Span) -> Self {
        Diagnostic {
            severity,
            code,
            message,
            span,
            label: None,
            labels: Vec::new(),
            help: None,
        }
    }

    /// Sets the text shown under the primary span.
    pub fn with_primary_label(mut self, text: impl Into<String>) -> Self {
        self.label = Some(text.into());
        self
    }

    /// Adds a secondary label.
    pub fn with_label(mut self, span: Span, message: impl Into<String>) -> Self {
        self.labels.push(Label {
            span,
            message: message.into(),
        });
        self
    }

    /// Sets the help text.
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builders_set_severity_labels_and_help() {
        let d = Diagnostic::warning("W0101", "unreachable code", Span::new(3, 9))
            .with_label(Span::new(0, 2), "returns here")
            .with_help("remove it")
            .with_primary_label("never runs");
        assert_eq!(d.severity, Severity::Warning);
        assert_eq!(
            d.labels,
            [Label {
                span: Span::new(0, 2),
                message: "returns here".into()
            }]
        );
        assert_eq!(d.help.as_deref(), Some("remove it"));
        assert_eq!(d.label.as_deref(), Some("never runs"));
        assert_eq!(
            Diagnostic::error("E0101", "x", Span::new(0, 1)).severity,
            Severity::Error
        );
    }
}
