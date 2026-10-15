//! The language server's snippets end to end (PRP-108, milestone 78): call
//! placeholders in completion, and the postfix `.match`.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MAIN: &str = "struct Point { x: f64 }
impl Point { fun scale(self, k: f64): Point = Point { x: self.x * k }; }
enum Shape { Circle(f64), Rect(f64, f64), Dot }
fun twice(n: i64): i64 = n * 2;
fun max<T: Ord>(a: T, b: T): T { if a > b { a } else { b } }
fun main() {
    let p = Point { x: 1.0 };
    let s = Shape.Dot;
    let xs = [1, 2];
    let o: Option<i64> = Some(1);
    let r: Result<i64, string> = Ok(1);
    println(p.x);
}
";

const SNIPPETS: &str =
    r#"{ "textDocument": { "completion": { "completionItem": { "snippetSupport": true } } } }"#;

/// The completion items at the end of `typed`, written on a new line before
/// `println(p.x);` with `after` following it on that line.
fn items(typed: &str, after: &str, snippets: bool) -> Vec<Value> {
    let t = Temp::with(&[("main.la", MAIN)]);
    let text = MAIN.replacen(
        "    println(p.x);",
        &format!("    {typed}{after}\n    println(p.x);"),
        1,
    );
    let before = &text[..text.find(&format!("    {typed}{after}\n")).unwrap() + 4 + typed.len()];
    let line = before.matches('\n').count() as u32;
    let col = before[before.rfind('\n').unwrap() + 1..]
        .encode_utf16()
        .count() as u32;
    let caps: Value = if snippets {
        serde_json::from_str(SNIPPETS).unwrap()
    } else {
        json!({})
    };
    let uri = t.uri("main.la");
    let mut s = Server::start_with(&t, json!({}), caps);
    s.open(&uri, MAIN);
    s.expect(&uri, &[]);
    s.change(&uri, 2, &text);
    let reply = s.request(
        "textDocument/completion",
        json!({ "textDocument": { "uri": uri }, "position": { "line": line, "character": col } }),
    );
    assert_eq!(s.finish(), Some(0));
    reply["result"].as_array().cloned().unwrap_or_default()
}

/// The snippet an item inserts, if it inserts one.
fn snippet(items: &[Value], label: &str) -> Option<String> {
    let item = items
        .iter()
        .find(|i| i["label"] == label)
        .unwrap_or_else(|| panic!("no {label}"));
    (item["insertTextFormat"] == 2)
        .then(|| item["insertText"].as_str().unwrap_or_default().to_string())
}

#[test]
fn calls_get_a_placeholder_per_parameter() {
    assert_eq!(
        snippet(&items("twi", "", true), "twice").as_deref(),
        Some("twice(${1:n})")
    );
    assert_eq!(
        snippet(&items("ma", "", true), "max").as_deref(),
        Some("max(${1:a}, ${2:b})")
    );
    assert_eq!(
        snippet(&items("p.sc", "", true), "scale").as_deref(),
        Some("scale(${1:k})")
    );
    assert_eq!(
        snippet(&items("xs.pu", "", true), "push").as_deref(),
        Some("push(${1:i64})")
    );
    let variants = items("Shape.", "", true);
    assert_eq!(
        snippet(&variants, "Circle").as_deref(),
        Some("Circle(${1:f64})")
    );
    assert_eq!(
        snippet(&variants, "Rect").as_deref(),
        Some("Rect(${1:f64}, ${2:f64})")
    );
    assert_eq!(snippet(&variants, "Dot"), None);
}

#[test]
fn no_placeholders_before_a_paren_or_without_snippet_support() {
    assert_eq!(snippet(&items("twi", "(3);", true), "twice"), None);
    assert_eq!(snippet(&items("twi", "", false), "twice"), None);
}

/// The postfix `match` item's edit: its new text and the range's start column.
fn postfix(items: &[Value]) -> Option<(String, u64)> {
    let item = items
        .iter()
        .find(|i| i["label"] == "match" && i["kind"] == 15)?;
    let edit = &item["textEdit"];
    Some((
        edit["newText"].as_str()?.to_string(),
        edit["range"]["start"]["character"].as_u64()?,
    ))
}

#[test]
fn postfix_match_writes_every_variant() {
    let (text, start) = postfix(&items("s.", "", true)).expect("a .match item");
    assert_eq!(
        text,
        "match s {\n\tCircle(${1:_}) => ${2},\n\tRect(${3:_}, ${4:_}) => ${5},\n\tDot => ${6},\n}$0"
    );
    assert_eq!(start, 4, "it replaces `s.` from the start of `s`");
    let (text, _) = postfix(&items("o.ma", "", true)).expect("on an Option");
    assert_eq!(
        text,
        "match o {\n\tSome(${1:_}) => ${2},\n\tNone => ${3},\n}$0"
    );
    let (text, _) = postfix(&items("r.", "", true)).expect("on a Result");
    assert_eq!(
        text,
        "match r {\n\tOk(${1:_}) => ${2},\n\tErr(${3:_}) => ${4},\n}$0"
    );
}

#[test]
fn postfix_match_is_only_for_enums_options_and_results_with_snippets() {
    assert_eq!(postfix(&items("p.", "", true)), None);
    assert_eq!(postfix(&items("s.", "", false)), None);
}
