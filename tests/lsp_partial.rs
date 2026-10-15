//! Partial syntax trees in `lughac lsp` end to end (PRP-105, milestone 75):
//! hover, definitions, completion and signature help in files with syntax
//! errors, and only the syntax errors shown.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

/// A program with a syntax error (the `;` after `sum(1)` is missing; the
/// parser skips to the next `;`, dropping the line after it too) and, below
/// it, a use of `total` that the dropped statement would have declared.
const BROKEN: &str = "import geo;

struct Point { x: f64 }
impl Point { fun scale(self, k: f64): Point = Point { x: self.x * k }; }
fun sum(n: i64): i64 = n;
fun main() {
    let p = Point { x: 1.0 };
    println(p.scale(2.0).x);
    let total = sum(1)
    println(total);
}
";

const GEO: &str = "fun double(n: i64): i64 = n * 2;\nfun broken() { let = ; }\n";

/// The zero-based line and UTF-16 character of `word` inside the first
/// `context` in `text`.
fn pos(text: &str, context: &str, word: &str) -> (u32, u32) {
    let start = text
        .find(context)
        .unwrap_or_else(|| panic!("no {context:?}"));
    let offset = start + context.find(word).unwrap_or_else(|| panic!("no {word:?}"));
    let before = &text[..offset];
    let line = before.matches('\n').count() as u32;
    let col = before[before.rfind('\n').map_or(0, |i| i + 1)..]
        .encode_utf16()
        .count() as u32;
    (line, col)
}

/// A server with `text` opened as `main.la` (never valid) beside `GEO`,
/// once its diagnostics are in; and its codes.
fn opened(t: &Temp, text: &str) -> (Server, String, Vec<String>) {
    let uri = t.uri("main.la");
    let mut server = Server::start(t, json!({}));
    server.open(&uri, text);
    let published = server
        .wait(|m| m["method"] == "textDocument/publishDiagnostics" && m["params"]["uri"] == uri)
        .expect("diagnostics");
    let codes = published["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["code"].as_str().unwrap_or_default().to_string())
        .collect();
    (server, uri, codes)
}

fn files(text: &str) -> Temp {
    Temp::with(&[("main.la", text), ("geo.la", GEO)])
}

#[test]
fn only_the_syntax_error_is_shown() {
    let t = files(BROKEN);
    let (s, _, codes) = opened(&t, BROKEN);
    assert!(!codes.is_empty(), "the missing `;`");
    assert!(
        codes
            .iter()
            .all(|c| c.starts_with("E01") || c.starts_with("E02")),
        "{codes:?}"
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn hover_and_definitions_work_around_the_broken_statement() {
    let t = files(BROKEN);
    let (mut s, uri, _) = opened(&t, BROKEN);
    let (line, col) = pos(BROKEN, "let p", "p");
    let hover = s.hover(&uri, line, col);
    assert_eq!(
        hover["contents"]["value"], "```lugha\nlet p: Point\n```",
        "{hover}"
    );
    let (line, col) = pos(BROKEN, "p.scale", "scale");
    assert_eq!(
        s.hover(&uri, line, col)["contents"]["value"],
        "```lugha\nfun Point.scale(self, k: f64): Point\n```"
    );
    let (line, col) = pos(BROKEN, "= Point {", "Point");
    let def = s.definition(&uri, line, col);
    assert_eq!(
        def["range"]["start"],
        json!({ "line": 2, "character": 7 }),
        "{def}"
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn completion_works_in_a_file_that_never_parsed() {
    let text = BROKEN.replace("    println(p.scale(2.0).x);\n", "    p.\n");
    let t = files(&text);
    let (mut s, uri, _) = opened(&t, &text);
    let (line, col) = pos(&text, "    p.\n", "\n");
    let labels: Vec<String> = s
        .completion(&uri, line, col)
        .into_iter()
        .map(|(l, ..)| l)
        .collect();
    assert!(labels.contains(&"x".to_string()), "{labels:?}");
    assert!(labels.contains(&"scale".to_string()), "{labels:?}");
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn signature_help_works_in_a_file_that_never_parsed() {
    let text = BROKEN.replace("    println(total);\n", "    sum(\n");
    let t = files(&text);
    let (mut s, uri, _) = opened(&t, &text);
    let (line, col) = pos(&text, "    sum(\n", "\n");
    let help = s.signature_help(&uri, line, col).expect("signature help");
    assert_eq!(help.0, "fun sum(n: i64): i64");
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_module_with_a_syntax_error_still_serves_its_other_functions() {
    let text = "import geo;\nfun main() { println(geo.double(2)); }\n";
    let t = files(text);
    let (mut s, uri, codes) = opened(&t, text);
    assert!(codes.is_empty(), "main.la's own diagnostics: {codes:?}");
    let (line, col) = pos(text, "geo.double", "double");
    let hover: Value = s.hover(&uri, line, col);
    assert_eq!(
        hover["contents"]["value"], "```lugha\nfun double(n: i64): i64\n```",
        "{hover}"
    );
    let def = s.definition(&uri, line, col);
    assert_eq!(def["uri"], t.uri("geo.la"), "{def}");
    assert_eq!(s.finish(), Some(0));
}
