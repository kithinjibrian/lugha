//! Compiler reports as LSP diagnostics, and which check reported what.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString,
    Position, Range, Uri,
};

use super::documents::{normalize, path_to_uri};
use crate::diagnostic::Severity;
use crate::driver::render::Report;
use crate::driver::source::{Source, SourceFile};
use crate::span::Span;

/// The `source` every diagnostic carries, as the extension showed before.
const SOURCE: &str = "lughac";

/// Converts `report` from a check whose main file sits in `root`. Returns
/// the file it belongs to and the diagnostic, or `None` for a report
/// without a span or inside `<std>` or `<prelude>`, which have no file.
pub(super) fn convert(
    report: &Report,
    source: &Source,
    root: &Path,
) -> Option<(PathBuf, Diagnostic)> {
    let span = report.span?;
    let (file, range) = locate(source, root, span)?;
    let related = report
        .labels
        .iter()
        .filter_map(|label| {
            let (at, range) = locate(source, root, label.span)?;
            Some(DiagnosticRelatedInformation {
                location: Location {
                    uri: uri(&at)?,
                    range,
                },
                message: label.message.clone(),
            })
        })
        .collect::<Vec<_>>();
    let mut diagnostic = top(report);
    diagnostic.range = range;
    diagnostic.related_information = (!related.is_empty()).then_some(related);
    Some((file, diagnostic))
}

/// `report` as a diagnostic at the top of a file, for reports without a
/// span: internal errors and caught panics.
pub(super) fn top(report: &Report) -> Diagnostic {
    let mut message = report.message.clone();
    for line in [
        report.label.clone(),
        report.help.as_ref().map(|h| format!("help: {h}")),
    ]
    .into_iter()
    .flatten()
    {
        message.push('\n');
        message.push_str(&line);
    }
    Diagnostic {
        range: Range::default(),
        severity: Some(match report.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        code: report.code.map(|c| NumberOrString::String(c.to_string())),
        source: Some(SOURCE.to_string()),
        message,
        ..Diagnostic::default()
    }
}

/// The URI of `path`; `None` only if the URI parser rejects what
/// `path_to_uri` built, which percent-encoding prevents.
pub(super) fn uri(path: &Path) -> Option<Uri> {
    Uri::from_str(&path_to_uri(path)).ok()
}

/// A program file of the check: its path and the span's UTF-16 range.
fn locate(source: &Source, root: &Path, span: Span) -> Option<(PathBuf, Range)> {
    let file = source.file_at(span.start);
    let path = file_path(file, root)?;
    let start = position(&file.text, span.start - file.base);
    let end = position(&file.text, span.end.saturating_sub(file.base));
    Some((path, Range { start, end }))
}

/// The path a source file names: absolute as given, else relative to the
/// main file's folder (`Source::of`); `None` for `<std>` and `<prelude>`.
pub(super) fn file_path(file: &SourceFile, root: &Path) -> Option<PathBuf> {
    if file.name.starts_with('<') {
        return None;
    }
    Some(normalize(&root.join(&file.name)))
}

/// The zero-based line and UTF-16 column of byte `offset` in `text`.
pub(super) fn position(text: &str, offset: usize) -> Position {
    let mut offset = offset.min(text.len());
    // Spans end on character boundaries; this only guards a bad one.
    while !text.is_char_boundary(offset) {
        offset -= 1;
    }
    let before = &text[..offset];
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let line = before.matches('\n').count();
    let character = before[line_start..].encode_utf16().count();
    // Lines and columns past u32::MAX can't occur in a file an editor holds.
    Position {
        line: u32::try_from(line).unwrap_or(u32::MAX),
        character: u32::try_from(character).unwrap_or(u32::MAX),
    }
}

/// Diagnostics by the check (main file) that reported them, so a new result
/// replaces only that check's, even in a module several mains import.
#[derive(Default)]
pub(super) struct Store {
    by_check: HashMap<PathBuf, HashMap<PathBuf, Vec<Diagnostic>>>,
}

impl Store {
    /// Replaces `main`'s diagnostics, returning every file whose shown
    /// diagnostics may have changed: those it reported before and now.
    pub(super) fn update(
        &mut self,
        main: &Path,
        files: HashMap<PathBuf, Vec<Diagnostic>>,
    ) -> Vec<PathBuf> {
        let mut changed: Vec<PathBuf> = self
            .by_check
            .get(main)
            .map(|old| old.keys().cloned().collect())
            .unwrap_or_default();
        changed.extend(files.keys().cloned());
        changed.sort();
        changed.dedup();
        self.by_check.insert(main.to_path_buf(), files);
        changed
    }

    /// `file`'s diagnostics from every check, each once.
    pub(super) fn for_file(&self, file: &Path) -> Vec<Diagnostic> {
        let mut out: Vec<Diagnostic> = Vec::new();
        let mut mains: Vec<&PathBuf> = self.by_check.keys().collect();
        // A fixed order, so the same state always publishes the same list.
        mains.sort();
        for main in mains {
            for d in self.by_check[main].get(file).into_iter().flatten() {
                if !out.contains(d) {
                    out.push(d.clone());
                }
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Label;

    #[test]
    fn positions_count_utf16_units() {
        let text = "ab\nd é👋 x";
        assert_eq!(position(text, 0), Position::new(0, 0));
        assert_eq!(position(text, 3), Position::new(1, 0));
        // `x` is byte 12: `d`, ` `, `é` (2 bytes, 1 unit), `👋` (4 bytes, 2 units), ` `.
        assert_eq!(position(text, 12), Position::new(1, 6));
        assert_eq!(position(text, 99), Position::new(1, 7));
        // Inside `é`: moved back to its start.
        assert_eq!(position(text, 6), Position::new(1, 2));
    }

    fn source() -> Source {
        let mut source = Source::single("/w/main.la", "fun main() {}\n");
        source.add("util.la", 100, "fun f(): i32 { return \"x\"; }\n");
        source.add("<std>/math.la", 200, "x");
        source
    }

    fn report(span: Span) -> Report {
        Report {
            severity: Severity::Error,
            code: Some("E0403"),
            message: "expected i32, found string".to_string(),
            span: Some(span),
            label: Some("expected i32".to_string()),
            labels: vec![Label {
                span: Span::new(0, 3),
                message: "here".to_string(),
            }],
            help: Some("return a number".to_string()),
        }
    }

    #[test]
    fn a_report_lands_in_its_file_with_labels_and_help() {
        let (file, d) = convert(&report(Span::new(122, 125)), &source(), Path::new("/w")).unwrap();
        assert_eq!(file, Path::new("/w/util.la"));
        assert_eq!(
            d.range,
            Range::new(Position::new(0, 22), Position::new(0, 25))
        );
        assert_eq!(
            d.message,
            "expected i32, found string\nexpected i32\nhelp: return a number"
        );
        assert_eq!(d.code, Some(NumberOrString::String("E0403".to_string())));
        assert_eq!(d.source.as_deref(), Some("lughac"));
        let related = d.related_information.unwrap();
        assert_eq!(related[0].location.uri.as_str(), "file:///w/main.la");
        assert_eq!(related[0].message, "here");
    }

    #[test]
    fn std_reports_and_spanless_reports_have_no_file() {
        assert!(convert(&report(Span::new(200, 201)), &source(), Path::new("/w")).is_none());
        let mut internal = report(Span::new(0, 1));
        internal.span = None;
        assert!(convert(&internal, &source(), Path::new("/w")).is_none());
        assert_eq!(top(&internal).range, Range::default());
    }

    #[test]
    fn the_store_replaces_one_checks_results_and_merges_the_rest() {
        let d = |m: &str| Diagnostic {
            message: m.to_string(),
            ..Diagnostic::default()
        };
        let (main, demo, util) = (
            Path::new("/main.la"),
            Path::new("/demo.la"),
            PathBuf::from("/util.la"),
        );
        let mut store = Store::default();
        let changed = store.update(main, HashMap::from([(util.clone(), vec![d("x")])]));
        assert_eq!(changed, vec![util.clone()]);
        store.update(demo, HashMap::from([(util.clone(), vec![d("x"), d("y")])]));
        assert_eq!(store.for_file(&util), [d("x"), d("y")]);
        assert_eq!(store.update(demo, HashMap::new()), vec![util.clone()]);
        assert_eq!(store.for_file(&util), [d("x")]);
        store.update(main, HashMap::new());
        assert_eq!(store.for_file(&util), []);
    }
}
