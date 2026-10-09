//! JSON diagnostics: one object per line (spec §9), written by hand so no
//! serialisation crate is needed.

use super::render::Report;
use super::source::{Source, line_col};
use crate::diagnostic::Severity;
use crate::span::Span;

/// Serialises `report` as one JSON line, newline included.
pub(super) fn line(report: &Report, source: &Source) -> String {
    let severity = match report.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
    };
    let optional = |text: Option<&str>| text.map_or_else(|| "null".to_string(), string);
    let span = report
        .span
        .map_or_else(|| "null".to_string(), |s| span(source, s));
    let labels: Vec<_> = report
        .labels
        .iter()
        .map(|l| {
            format!(
                "{{\"span\":{},\"message\":{}}}",
                self::span(source, l.span),
                string(&l.message)
            )
        })
        .collect();
    format!(
        "{{\"severity\":\"{severity}\",\"code\":{},\"message\":{},\"file\":{},\"span\":{span},\"label\":{},\"labels\":[{}],\"help\":{}}}\n",
        optional(report.code),
        string(&report.message),
        string(&source.name),
        optional(report.label.as_deref()),
        labels.join(","),
        optional(report.help.as_deref()),
    )
}

fn span(source: &Source, span: Span) -> String {
    format!(
        "{{\"start\":{},\"end\":{}}}",
        position(source, span.start),
        position(source, span.end)
    )
}

fn position(source: &Source, offset: usize) -> String {
    let (line, col) = line_col(&source.text, offset);
    format!("{{\"line\":{line},\"col\":{col},\"offset\":{offset}}}")
}

/// Escapes `text` as a JSON string, quotes included.
fn string(text: &str) -> String {
    let mut out = String::with_capacity(text.len() + 2);
    out.push('"');
    for c in text.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if u32::from(c) < 0x20 => out.push_str(&format!("\\u{:04x}", u32::from(c))),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::render::tests::e0401;

    #[test]
    fn e0401_matches_the_spec_example_exactly() {
        let (source, d) = e0401();
        let expected = concat!(
            r#"{"severity":"error","code":"E0401","message":"float literal where i32 expected","file":"main.la","#,
            r#""span":{"start":{"line":3,"col":17,"offset":49},"end":{"line":3,"col":20,"offset":52}},"#,
            r#""label":"expected i32","#,
            r#""labels":[{"span":{"start":{"line":3,"col":13,"offset":45},"end":{"line":3,"col":14,"offset":46}},"message":"this operand is i32"}],"#,
            r#""help":null}"#,
            "\n"
        );
        assert_eq!(line(&Report::from(&d), &source), expected);
    }

    #[test]
    fn internal_errors_have_null_code_and_span() {
        let source = Source {
            name: "x.la".into(),
            text: String::new(),
        };
        let line = line(&Report::internal("cannot read `x.la`", None), &source);
        assert_eq!(
            line,
            "{\"severity\":\"error\",\"code\":null,\"message\":\"cannot read `x.la`\",\"file\":\"x.la\",\"span\":null,\"label\":null,\"labels\":[],\"help\":null}\n"
        );
    }

    #[test]
    fn strings_are_escaped() {
        assert_eq!(string("a\"b\\c\nd\te\u{1}"), r#""a\"b\\c\nd\te\u0001""#);
        assert_eq!(string("é👋"), "\"é👋\"");
    }
}
