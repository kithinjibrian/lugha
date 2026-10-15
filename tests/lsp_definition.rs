//! Go to Definition in `lughac lsp` end to end (PRP-102, milestone 72):
//! from uses to declarations, across files, from a real server process.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MAIN: &str = "import geo;
import std.math;

struct Circle { r: f64 }
enum Shape { Round(f64), Dot }
interface Area { fun area(self): f64; }
impl Circle { fun area(self): f64 = 3.0 * self.r * self.r; }
fun measure(a: Area): f64 = a.area();
// A comment.
fun main() {
    let c = Circle { r: 1.0 };
    println(c.area());
    println(measure(c));
    let s = Shape.Round(2.0);
    match s { Round(x) => println(x), Dot => {} }
    println(geo.double(2));
    println(math.sqrt(2.0));
}
";

const GEO: &str = "// Doubles.\nfun double(n: i64): i64 = n * 2;\n";

/// The zero-based line and UTF-16 character of `word` inside the first
/// `context` in `text`; `word` must not occur earlier in `context`.
fn pos(text: &str, context: &str, word: &str) -> (u32, u32) {
    let start = text
        .find(context)
        .unwrap_or_else(|| panic!("no {context:?}"));
    let offset = start
        + context
            .find(word)
            .unwrap_or_else(|| panic!("no {word:?} in {context:?}"));
    let before = &text[..offset];
    let line = before.matches('\n').count();
    let col = before[before.rfind('\n').map_or(0, |i| i + 1)..]
        .encode_utf16()
        .count();
    (line as u32, col as u32)
}

/// The LSP range of `word` inside `context` in `text`.
fn range(text: &str, context: &str, word: &str) -> Value {
    let (line, col) = pos(text, context, word);
    let end = col + word.encode_utf16().count() as u32;
    json!({ "start": { "line": line, "character": col }, "end": { "line": line, "character": end } })
}

fn location(uri: &str, range: Value) -> Value {
    json!({ "uri": uri, "range": range })
}

/// A server with `MAIN` and `GEO` on disk and `MAIN` open, checked once.
fn opened(t: &Temp) -> (Server, String) {
    let uri = t.uri("main.la");
    let mut server = Server::start(t, json!({}));
    server.open(&uri, MAIN);
    server.expect(&uri, &[]);
    (server, uri)
}

fn jump(server: &mut Server, uri: &str, (line, col): (u32, u32)) -> Value {
    server.definition(uri, line, col)
}

#[test]
fn the_server_offers_definitions() {
    let t = Temp::with(&[]);
    let server = Server::start(&t, json!({}));
    assert_eq!(
        server.init["result"]["capabilities"]["definitionProvider"], true,
        "{}",
        server.init
    );
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn names_jump_to_their_declarations() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let here = |ctx, word| location(&uri, range(MAIN, ctx, word));
    let at = |ctx, word| pos(MAIN, ctx, word);
    // Variables, functions and methods.
    assert_eq!(
        jump(&mut s, &uri, at("println(c.", "c")),
        here("let c", "c")
    );
    assert_eq!(
        jump(&mut s, &uri, at("c.area()", "area")),
        here("{ fun area(self): f64 =", "area")
    );
    assert_eq!(
        jump(&mut s, &uri, at("println(measure", "measure")),
        here("fun measure", "measure")
    );
    assert_eq!(
        jump(&mut s, &uri, at("a.area()", "area")),
        here("{ fun area(self): f64; }", "area")
    );
    assert_eq!(
        jump(&mut s, &uri, at("=> println(x)", "x)")),
        here("Round(x)", "x")
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn types_fields_and_variants_jump_to_their_declarations() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let here = |ctx, word| location(&uri, range(MAIN, ctx, word));
    let at = |ctx, word| pos(MAIN, ctx, word);
    assert_eq!(
        jump(&mut s, &uri, at("= Circle {", "Circle")),
        here("struct Circle", "Circle")
    );
    assert_eq!(
        jump(&mut s, &uri, at("impl Circle", "Circle")),
        here("struct Circle", "Circle")
    );
    assert_eq!(
        jump(&mut s, &uri, at("(a: Area)", "Area")),
        here("interface Area", "Area")
    );
    assert_eq!(
        jump(&mut s, &uri, at("Circle { r: 1.0", "r:")),
        here("{ r: f64", "r")
    );
    assert_eq!(
        jump(&mut s, &uri, at("self.r *", "r *")),
        here("{ r: f64", "r")
    );
    assert_eq!(
        jump(&mut s, &uri, at("= Shape.Round", "Shape")),
        here("enum Shape", "Shape")
    );
    assert_eq!(
        jump(&mut s, &uri, at("Shape.Round(2.0)", "Round")),
        here("{ Round(f64)", "Round")
    );
    assert_eq!(
        jump(&mut s, &uri, at("Dot =>", "Dot")),
        here("Dot }", "Dot")
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn modules_and_their_functions_jump_to_the_module_file() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let geo = t.uri("geo.la");
    let at = |ctx, word| pos(MAIN, ctx, word);
    let top =
        json!({ "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 0 } });
    assert_eq!(
        jump(&mut s, &uri, at("import geo", "geo")),
        location(&geo, top.clone())
    );
    assert_eq!(
        jump(&mut s, &uri, at("geo.double", "geo")),
        location(&geo, top)
    );
    assert_eq!(
        jump(&mut s, &uri, at("geo.double", "double")),
        location(&geo, range(GEO, "fun double", "double"))
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn bundled_definitions_and_empty_places_have_none() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let at = |ctx, word| pos(MAIN, ctx, word);
    assert_eq!(jump(&mut s, &uri, at("math.sqrt", "sqrt")), json!(null));
    assert_eq!(jump(&mut s, &uri, at("std.math", "math")), json!(null));
    assert_eq!(
        jump(&mut s, &uri, at("// A comment", "comment")),
        json!(null)
    );
    assert_eq!(jump(&mut s, &uri, at("fun main", "fun")), json!(null));
    assert_eq!(jump(&mut s, &uri, (99, 0)), json!(null));
    assert_eq!(jump(&mut s, &t.uri("never.la"), (0, 0)), json!(null));
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_declaration_is_its_own_definition() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let here = location(&uri, range(MAIN, "struct Circle", "Circle"));
    assert_eq!(
        jump(&mut s, &uri, pos(MAIN, "struct Circle", "Circle")),
        here
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn definitions_work_while_the_program_has_errors() {
    let text = "fun main() {\n    let fine = 2.5;\n    let bad = nope;\n    println(fine);\n}\n";
    let t = Temp::with(&[("main.la", text)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, text);
    s.expect(&uri, &["E0301"]);
    assert_eq!(
        jump(&mut s, &uri, pos(text, "println(fine)", "fine")),
        location(&uri, range(text, "let fine", "fine"))
    );
    assert_eq!(jump(&mut s, &uri, pos(text, "= nope", "nope")), json!(null));
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_jump_right_after_typing_sees_the_new_text() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let changed = MAIN.replace(
        "    let c = Circle { r: 1.0 };",
        "    let other = 1;\n    let c = Circle { r: 1.0 };",
    );
    s.change(&uri, 2, &changed);
    // No wait for the diagnostics: the request runs the pending check itself.
    assert_eq!(
        jump(&mut s, &uri, pos(&changed, "println(c.", "c")),
        location(&uri, range(&changed, "let c", "c"))
    );
    assert_eq!(s.finish(), Some(0));
}
