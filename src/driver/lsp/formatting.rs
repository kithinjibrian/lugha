//! Format Document (v16 draft decisions 66–69, milestone 81): the
//! editor's text through `lughac fmt`'s formatter, as one edit the client
//! applies. The server writes nothing.

use std::panic::{self, AssertUnwindSafe};

use lsp_server::{Message, Notification, RequestId, Response};
use lsp_types::{MessageType, Position, Range, ShowMessageParams, TextEdit};

use super::publish::position;
use crate::driver::guard;
use crate::formatter::{self, FormatError};

/// What formatting a document gave.
#[derive(Debug, PartialEq)]
pub(super) enum Formatted {
    /// One edit replacing the whole document, or none when it is formatted.
    Edits(Vec<TextEdit>),
    /// Syntax errors: left alone, quietly; its diagnostics show them.
    Unchanged,
    /// The formatter failed its own check (or panicked): a bug.
    Failed(String),
}

/// `text` formatted.
pub(super) fn format(text: &str) -> Formatted {
    format_with(text, formatter::format)
}

/// `text` through `run`, the formatter (a parameter so tests can make it fail).
fn format_with(text: &str, run: impl FnOnce(&str) -> Result<String, FormatError>) -> Formatted {
    // A formatter bug must not take the whole server down.
    let result = match panic::catch_unwind(AssertUnwindSafe(|| run(text))) {
        Ok(result) => result,
        Err(payload) => return Formatted::Failed(guard::message(&*payload)),
    };
    match result {
        Ok(out) if out == text => Formatted::Edits(Vec::new()),
        Ok(out) => Formatted::Edits(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: position(text, text.len()),
            },
            new_text: out,
        }]),
        Err(FormatError::Syntax(_)) => Formatted::Unchanged,
        Err(FormatError::Unsafe(reason)) => Formatted::Failed(reason),
    }
}

/// The response to request `id` for the document `name`, and for a
/// failure one error message (decision 68).
pub(super) fn reply(id: RequestId, name: &str, formatted: Option<Formatted>) -> Vec<Message> {
    match formatted {
        Some(Formatted::Edits(edits)) => vec![Response::new_ok(id, edits).into()],
        Some(Formatted::Failed(reason)) => {
            let message = format!(
                "Lugha: `{name}` was left as written — formatting failed its own check ({reason}). Please report this."
            );
            let shown = ShowMessageParams {
                typ: MessageType::ERROR,
                message,
            };
            vec![
                Response::new_ok(id, ()).into(),
                Notification::new("window/showMessage".to_string(), shown).into(),
            ]
        }
        Some(Formatted::Unchanged) | None => vec![Response::new_ok(id, ()).into()],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_messy_document_is_one_edit_over_all_of_it() {
        let text = "fun  main(){}\n";
        let Formatted::Edits(edits) = format(text) else {
            panic!("edits expected");
        };
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "fun main() {}\n");
        assert_eq!(edits[0].range.start, Position::new(0, 0));
        assert_eq!(edits[0].range.end, Position::new(1, 0));
    }

    #[test]
    fn the_end_counts_utf16_units_and_needs_no_final_newline() {
        let text = "fun main() {\n    println(\"é😀\");}";
        let Formatted::Edits(edits) = format(text) else {
            panic!("edits expected");
        };
        // `    println("é😀");}`: 4 + 9 + 1 + 2 + 3 units.
        let last = "    println(\"é😀\");}".encode_utf16().count() as u32;
        assert_eq!(edits[0].range.end, Position::new(1, last));
    }

    #[test]
    fn a_formatted_document_has_no_edits_and_a_broken_one_is_unchanged() {
        assert_eq!(format("fun main() {}\n"), Formatted::Edits(Vec::new()));
        assert_eq!(format("fun main( {"), Formatted::Unchanged);
    }

    #[test]
    fn a_verification_failure_or_a_panic_is_failed() {
        let unsafe_ = format_with("x", |_| Err(FormatError::Unsafe("the tree differs".into())));
        assert_eq!(unsafe_, Formatted::Failed("the tree differs".into()));
        let panicked = format_with("x", |_| panic!("boom"));
        assert!(matches!(panicked, Formatted::Failed(m) if m.contains("boom")));
    }

    #[test]
    fn a_failure_replies_null_and_shows_one_error() {
        let out = reply(
            RequestId::from(7),
            "main.la",
            Some(Formatted::Failed("why".into())),
        );
        assert_eq!(out.len(), 2);
        let Message::Response(response) = &out[0] else {
            panic!("a response first");
        };
        assert_eq!(
            serde_json::to_value(response).unwrap()["result"],
            serde_json::Value::Null
        );
        let Message::Notification(n) = &out[1] else {
            panic!("then a notification");
        };
        assert_eq!(n.method, "window/showMessage");
        assert_eq!(n.params["type"], 1);
        let text = n.params["message"].as_str().unwrap();
        assert!(
            text.contains("`main.la` was left as written") && text.contains("(why)"),
            "{text}"
        );
    }

    #[test]
    fn edits_and_quiet_cases_are_one_response() {
        let out = reply(RequestId::from(1), "a.la", Some(Formatted::Unchanged));
        assert_eq!(out.len(), 1);
        let none = reply(RequestId::from(2), "a.la", None);
        let Message::Response(response) = &none[0] else {
            panic!("a response");
        };
        assert_eq!(
            serde_json::to_value(response).unwrap()["result"],
            serde_json::Value::Null
        );
    }
}
