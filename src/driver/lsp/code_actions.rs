//! Code actions (v16 draft decisions 56–58): on a non-exhaustive `match`
//! (E0417), add every missing arm, or a `_` arm, each with the body
//! `panic("not handled")`; and the fixes of `fixes.rs` for E0501, W0103
//! and E0301 (milestone 82).

use std::collections::HashMap;
use std::path::Path;

use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, Diagnostic, NumberOrString, Range, TextEdit,
    WorkspaceEdit,
};

use super::fixes;
use super::hover::{Snapshot, byte_offset};
use super::publish::{position, uri};

/// What a new arm's body is: it fits any match's type (decision 57).
const BODY: &str = "panic(\"not handled\")";

/// The quick fixes for the E0417s among `diagnostics`, in `file`.
pub(super) fn answer(
    snapshot: &Snapshot,
    file: &Path,
    diagnostics: &[Diagnostic],
) -> Vec<CodeActionOrCommand> {
    let Some((base, text)) = snapshot.base(file) else {
        return Vec::new();
    };
    let Some(uri) = uri(file) else {
        return Vec::new();
    };
    let mut actions = Vec::new();
    for d in diagnostics {
        let Some(NumberOrString::String(code)) = &d.code else {
            continue;
        };
        let Some(start) = byte_offset(text, d.range.start) else {
            continue;
        };
        if code != "E0417" {
            let found = byte_offset(text, d.range.end)
                .and_then(|end| fixes::fix(&snapshot.partial, code, base, text, (start, end)));
            if let Some(fix) = found {
                let edit = TextEdit {
                    range: Range {
                        start: position(text, fix.from),
                        end: position(text, fix.to),
                    },
                    new_text: fix.new_text,
                };
                actions.push(action(&fix.title, d, &uri, edit, true));
            }
            continue;
        }
        let Some((span, missing)) = snapshot
            .partial
            .missing_arms
            .iter()
            .find(|(s, _)| s.start == base + start)
        else {
            continue;
        };
        let scrutinee_end = span.end - base;
        // An interface's only missing case is `_`: the `_` fix says it.
        let named: Vec<String> = missing.iter().filter(|p| *p != "_").cloned().collect();
        let mut fixes = Vec::new();
        if !named.is_empty() {
            fixes.push(("Add missing arms", named, true));
        }
        fixes.push(("Add a `_` arm", vec!["_".to_string()], false));
        for (title, arms, preferred) in fixes {
            let Some((from, to, new_text)) = arms_edit(text, start, scrutinee_end, &arms) else {
                continue;
            };
            let edit = TextEdit {
                range: Range {
                    start: position(text, from),
                    end: position(text, to),
                },
                new_text,
            };
            actions.push(action(title, d, &uri, edit, preferred));
        }
    }
    actions
}

/// A quick fix titled `title` for `d`: one edit in the file at `uri`.
fn action(
    title: &str,
    d: &Diagnostic,
    uri: &lsp_types::Uri,
    edit: TextEdit,
    preferred: bool,
) -> CodeActionOrCommand {
    CodeActionOrCommand::CodeAction(CodeAction {
        title: title.to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![d.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(HashMap::from([(uri.clone(), vec![edit])])),
            ..WorkspaceEdit::default()
        }),
        is_preferred: Some(preferred),
        ..CodeAction::default()
    })
}

/// The edit adding `arms` to the match at `match_start` (its scrutinee ends
/// at `scrutinee_end`): the bytes `from..to` to replace, between the last
/// arm and the closing `}`, and their new text. `None` if the braces can't
/// be found.
fn arms_edit(
    text: &str,
    match_start: usize,
    scrutinee_end: usize,
    arms: &[String],
) -> Option<(usize, usize, String)> {
    let open = scrutinee_end + text[scrutinee_end..].find('{')?;
    let close = matching_close(text, open)?;
    let line_of = |at: usize| text[..at].rfind('\n').map_or(0, |i| i + 1);
    let indent_of = |at: usize| -> String {
        let line = &text[line_of(at)..];
        line[..line.len() - line.trim_start_matches([' ', '\t']).len()].to_string()
    };
    let match_indent = indent_of(match_start);
    // The first arm's indentation if it starts its own line, else four
    // spaces more than the `match`.
    let first = open
        + 1
        + text[open + 1..close]
            .find(|c: char| !c.is_whitespace())
            .unwrap_or(close - open - 1);
    let arm_indent = if first < close && text[open + 1..first].contains('\n') {
        indent_of(first)
    } else {
        format!("{match_indent}    ")
    };
    let end_of_arms = text[..close].trim_end().len();
    let needs_comma = !matches!(text[..end_of_arms].chars().last(), Some(',' | '{'));
    // The `}` keeps its own line's indentation when alone on it.
    let closing = if text[line_of(close)..close].trim().is_empty() {
        text[line_of(close)..close].to_string()
    } else {
        match_indent
    };
    let mut new_text = String::new();
    if needs_comma {
        new_text.push(',');
    }
    new_text.push('\n');
    for arm in arms {
        new_text.push_str(&format!("{arm_indent}{arm} => {BODY},\n"));
    }
    new_text.push_str(&closing);
    Some((end_of_arms, close, new_text))
}

/// The `}` closing the `{` at `open`, skipping strings, `char`s and comments.
fn matching_close(text: &str, open: usize) -> Option<usize> {
    let bytes = text.as_bytes();
    let (mut depth, mut i) = (0usize, open);
    while i < bytes.len() {
        match bytes[i] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            b'"' | b'\'' => {
                let quote = bytes[i];
                i += 1;
                while i < bytes.len() && bytes[i] != quote && bytes[i] != b'\n' {
                    i += if bytes[i] == b'\\' { 2 } else { 1 };
                }
            }
            b'/' if bytes.get(i + 1) == Some(&b'/') => {
                while i < bytes.len() && bytes[i] != b'\n' {
                    i += 1;
                }
            }
            b'/' if bytes.get(i + 1) == Some(&b'*') => {
                // Nested, as the lexer reads them (milestone 76).
                let mut nest = 0usize;
                while i < bytes.len() {
                    if bytes[i..].starts_with(b"/*") {
                        nest += 1;
                        i += 2;
                    } else if bytes[i..].starts_with(b"*/") {
                        nest -= 1;
                        i += 2;
                        if nest == 0 {
                            break;
                        }
                    } else {
                        i += 1;
                    }
                }
                continue;
            }
            _ => {}
        }
        i += 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `text` after adding `arms` to its first `match`.
    fn applied(text: &str, arms: &[&str]) -> String {
        let start = text.find("match").unwrap();
        let scrutinee_end = start + "match s".len();
        let arms: Vec<String> = arms.iter().map(|a| a.to_string()).collect();
        let (from, to, new) = arms_edit(text, start, scrutinee_end, &arms).expect("an edit");
        format!("{}{new}{}", &text[..from], &text[to..])
    }

    #[test]
    fn arms_go_after_the_last_at_its_indentation() {
        let text = "    match s {\n        A(r) => r,\n    }\n";
        assert_eq!(
            applied(text, &["B(_, _)", "C"]),
            "    match s {\n        A(r) => r,\n        B(_, _) => panic(\"not handled\"),\n        C => panic(\"not handled\"),\n    }\n"
        );
    }

    #[test]
    fn a_last_arm_without_a_comma_gets_one() {
        let text = "    match s {\n        A(r) => { r }\n    }\n";
        assert_eq!(
            applied(text, &["C"]),
            "    match s {\n        A(r) => { r },\n        C => panic(\"not handled\"),\n    }\n"
        );
    }

    #[test]
    fn a_one_line_match_is_opened_up() {
        let text = "    match s { A => 1 }\n";
        assert_eq!(
            applied(text, &["_"]),
            "    match s { A => 1,\n        _ => panic(\"not handled\"),\n    }\n"
        );
    }

    #[test]
    fn an_empty_match_gets_its_first_arms() {
        let text = "match s {\n}\n";
        assert_eq!(
            applied(text, &["A"]),
            "match s {\n    A => panic(\"not handled\"),\n}\n"
        );
    }

    #[test]
    fn braces_in_strings_and_comments_are_skipped() {
        let text = "match s {\n    A => \"}\", // }\n    /* } /* } */ */\n}\n";
        assert_eq!(
            matching_close(text, text.find('{').unwrap()),
            Some(text.rfind('}').unwrap())
        );
    }
}
