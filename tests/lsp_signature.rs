//! Signature help in `lughac lsp` end to end (PRP-104, milestone 74): the
//! call being typed, with its active parameter, while the text doesn't parse.

use serde_json::json;

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MAIN: &str = "import geo;

struct Point { x: f64 }
impl Point { fun scale(self, k: f64): Point = Point { x: self.x * k }; }
enum Shape { Circle(f64), Dot }
fun twice(n: i64): i64 = n * 2;
fun max<T: Ord>(a: T, b: T): T { if a > b { a } else { b } }
fun main() {
    let p = Point { x: 1.0 };
    let xs = [1, 2];
    println(max(1.5, 2.5));
    println(p.x);
    println(xs.len);
}
";

const GEO: &str = "fun double(n: i64): i64 = n * 2;\n";

/// `MAIN` with `cut` replaced by `typed`, and the zero-based line and UTF-16
/// character at the end of `typed`.
fn edited(cut: &str, typed: &str) -> (String, (u32, u32)) {
    let at = MAIN.find(cut).unwrap_or_else(|| panic!("no {cut:?}"));
    let text = format!("{}{typed}{}", &MAIN[..at], &MAIN[at + cut.len()..]);
    let end = &text[..at + typed.len()];
    let line = end.matches('\n').count() as u32;
    let col = end[end.rfind('\n').map_or(0, |i| i + 1)..]
        .encode_utf16()
        .count() as u32;
    (text, (line, col))
}

/// `typed` on a new line before `println(p.x);`.
fn new_line(typed: &str) -> (String, (u32, u32)) {
    edited("    println(p.x);", &format!("    {typed}"))
}

/// The signature help at `at` after `MAIN` was checked and changed to `text`.
fn help(text: &str, (line, col): (u32, u32)) -> Option<(String, u64, String, String)> {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let uri = t.uri("main.la");
    let mut server = Server::start(&t, json!({}));
    server.open(&uri, MAIN);
    server.expect(&uri, &[]);
    server.change(&uri, 2, text);
    let found = server.signature_help(&uri, line, col);
    assert_eq!(server.finish(), Some(0));
    found
}

fn shown(label: &str, active: u64, param: &str) -> Option<(String, u64, String, String)> {
    Some((label.to_string(), active, param.to_string(), String::new()))
}

#[test]
fn the_server_offers_signature_help_on_open_paren_and_comma() {
    let t = Temp::with(&[]);
    let server = Server::start(&t, json!({}));
    let provider = &server.init["result"]["capabilities"]["signatureHelpProvider"];
    assert_eq!(
        provider["triggerCharacters"],
        json!(["(", ","]),
        "{}",
        server.init
    );
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn a_function_shows_its_header_with_the_first_parameter() {
    let (text, at) = new_line("twice(");
    assert_eq!(
        help(&text, at),
        shown("fun twice(n: i64): i64", 0, "n: i64")
    );
}

#[test]
fn a_known_generic_call_is_filled_in_with_the_written_header_as_documentation() {
    let (text, at) = edited("max(1.5, 2.5));", "max(1.5, ");
    assert_eq!(
        help(&text, at),
        Some((
            "fun max(a: f64, b: f64): f64".to_string(),
            1,
            "b: f64".to_string(),
            "fun max<T: Ord>(a: T, b: T): T".to_string()
        ))
    );
}

#[test]
fn a_new_generic_call_shows_the_written_header() {
    let (text, at) = new_line("max(1, ");
    assert_eq!(
        help(&text, at),
        shown("fun max<T: Ord>(a: T, b: T): T", 1, "b: T")
    );
}

#[test]
fn methods_modules_variants_and_built_ins_have_signatures() {
    let (text, at) = new_line("p.scale(");
    assert_eq!(
        help(&text, at),
        shown("fun Point.scale(self, k: f64): Point", 0, "k: f64")
    );
    let (text, at) = new_line("geo.double(");
    assert_eq!(
        help(&text, at),
        shown("fun double(n: i64): i64", 0, "n: i64")
    );
    let (text, at) = new_line("Shape.Circle(");
    assert_eq!(help(&text, at), shown("Circle(f64)", 0, "f64"));
    let (text, at) = new_line("xs.push(");
    assert_eq!(help(&text, at), shown("fun push(i64)", 0, "i64"));
}

#[test]
fn intrinsics_show_the_specs_call_form() {
    let (text, at) = new_line("read_file(");
    assert_eq!(help(&text, at), shown("read_file(path)", 0, "path"));
    let (text, at) = new_line("println(");
    assert_eq!(help(&text, at), shown("println(x)", 0, "x"));
}

#[test]
fn nested_calls_answer_for_the_innermost() {
    let (text, at) = new_line("max(twice(");
    assert_eq!(
        help(&text, at),
        shown("fun twice(n: i64): i64", 0, "n: i64")
    );
    let (text, at) = new_line("max(twice(2), ");
    assert_eq!(
        help(&text, at),
        shown("fun max<T: Ord>(a: T, b: T): T", 1, "b: T")
    );
}

#[test]
fn outside_a_call_there_is_none() {
    let (text, at) = new_line("let y = (1 + ");
    assert_eq!(help(&text, at), None);
    let (text, at) = new_line("let y = 1");
    assert_eq!(help(&text, at), None);
    let (text, at) = new_line("twice(\"a, (b\" ");
    assert_eq!(
        help(&text, at),
        shown("fun twice(n: i64): i64", 0, "n: i64")
    );
}

#[test]
fn a_file_never_opened_has_none() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let mut server = Server::start(&t, json!({}));
    assert_eq!(server.signature_help(&t.uri("never.la"), 0, 0), None);
    assert_eq!(server.finish(), Some(0));
}
