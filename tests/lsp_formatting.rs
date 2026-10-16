//! Format Document in `lughac lsp` end to end (PRP-111, milestone 81):
//! the editor's text formatted as `lughac fmt` would, quietly nothing for a
//! syntax error, and no file written.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MESSY: &str = "fun  main(){println(\"é😀\");let x=1+2;}";
const TIDY: &str = "fun main() {\n    println(\"é😀\");\n    let x = 1 + 2;\n}\n";

/// The reply to formatting `uri`, and whether a `window/showMessage` came
/// before it.
fn format(s: &mut Server, uri: &str) -> (Value, bool) {
    let id = s.next_id;
    s.next_id += 1;
    s.send(json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": "textDocument/formatting",
        "params": {
            "textDocument": { "uri": uri },
            "options": { "tabSize": 2, "insertSpaces": false },
        },
    }));
    let mut shown = false;
    let reply = s
        .wait(|m| {
            shown |= m["method"] == "window/showMessage";
            m.get("id") == Some(&json!(id)) && m.get("method").is_none()
        })
        .expect("a reply to formatting");
    (reply, shown)
}

#[test]
fn the_server_declares_document_formatting() {
    let t = Temp::with(&[]);
    let s = Server::start(&t, json!({}));
    assert_eq!(
        s.init["result"]["capabilities"]["documentFormattingProvider"],
        true
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn unsaved_text_is_formatted_as_lughac_fmt_would_and_no_file_is_written() {
    let t = Temp::with(&[("main.la", "fun main() {}\n")]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, MESSY);
    let (reply, shown) = format(&mut s, &uri);
    assert!(!shown);
    let edits = reply["result"].as_array().expect("edits");
    assert_eq!(edits.len(), 1, "{reply}");
    assert_eq!(edits[0]["newText"], TIDY);
    // The whole document: to its end, counted in UTF-16 units.
    assert_eq!(
        edits[0]["range"]["start"],
        json!({ "line": 0, "character": 0 })
    );
    let end = MESSY.encode_utf16().count();
    assert_eq!(
        edits[0]["range"]["end"],
        json!({ "line": 0, "character": end })
    );
    assert_eq!(s.finish(), Some(0));
    let on_disk = std::fs::read_to_string(t.path("main.la")).unwrap();
    assert_eq!(on_disk, "fun main() {}\n");
}

#[test]
fn a_formatted_document_gets_no_edits() {
    let t = Temp::with(&[]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, TIDY);
    let (reply, _) = format(&mut s, &uri);
    assert_eq!(reply["result"], json!([]));
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_syntax_error_gets_nothing_and_no_message() {
    let t = Temp::with(&[]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, "fun main( {\n");
    let (reply, shown) = format(&mut s, &uri);
    assert_eq!(reply["result"], Value::Null, "{reply}");
    assert!(!shown);
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_document_the_server_does_not_hold_gets_nothing() {
    let t = Temp::with(&[("main.la", "fun  main(){}\n")]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    let (reply, _) = format(&mut s, &uri);
    assert_eq!(reply["result"], Value::Null, "{reply}");
    assert_eq!(s.finish(), Some(0));
}
