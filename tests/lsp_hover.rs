//! Hover in `lughac lsp` end to end (PRP-101, milestone 71): types of
//! expressions and declared names from a real server process.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const PROGRAM: &str = "struct Circle { r: f64 }
impl Circle { fun area(self): f64 = 3.0 * self.r * self.r; }
interface Shape { fun area(self): f64; }
fun show(s: Shape): f64 = s.area();
fun max<T: Ord>(a: T, b: T): T { if a > b { a } else { b } }
fun total(xs: i64[]): i64 { let mut sum = 0; for x of xs { sum += x; } return sum; }
// A comment.
fun main() {
    let c = Circle { r: 1.0 };
    println(total([1, 2]));
    println(c.area());
    println(max(1.5, 2.5));
    println(show(c));
}
";

/// The zero-based line and UTF-16 character of `word` inside the first
/// `context` in `text`.
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

/// The markdown a hover shows, or `None` for no hover.
fn shown(result: &Value) -> Option<String> {
    if result.is_null() {
        return None;
    }
    assert_eq!(result["contents"]["kind"], "markdown", "{result}");
    Some(result["contents"]["value"].as_str().unwrap().to_string())
}

fn code(line: &str, notes: &[&str]) -> Option<String> {
    let mut text = format!("```lugha\n{line}\n```");
    for note in notes {
        text.push_str("\n\n");
        text.push_str(note);
    }
    Some(text)
}

/// A server with `PROGRAM` open as `main.la`, checked once.
fn opened(t: &Temp) -> (Server, String) {
    let uri = t.uri("main.la");
    let mut server = Server::start(t, json!({}));
    server.open(&uri, PROGRAM);
    server.expect(&uri, &[]);
    (server, uri)
}

fn hover_at(server: &mut Server, uri: &str, (line, col): (u32, u32)) -> Option<String> {
    shown(&server.hover(uri, line, col))
}

#[test]
fn the_server_offers_hover() {
    let t = Temp::with(&[]);
    let server = Server::start(&t, json!({}));
    assert_eq!(
        server.init["result"]["capabilities"]["hoverProvider"], true,
        "{}",
        server.init
    );
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn declared_names_show_their_declarations() {
    let t = Temp::with(&[("main.la", PROGRAM)]);
    let (mut s, uri) = opened(&t);
    let at = |ctx, word| pos(PROGRAM, ctx, word);
    assert_eq!(
        hover_at(&mut s, &uri, at("let c", "c")),
        code("let c: Circle", &[])
    );
    assert_eq!(
        hover_at(&mut s, &uri, at("let mut sum", "sum")),
        code("let mut sum: i64", &[])
    );
    assert_eq!(
        hover_at(&mut s, &uri, at("(xs: i64[])", "xs")),
        code("let xs: i64[]", &[])
    );
    assert_eq!(
        hover_at(&mut s, &uri, at("for x of", "x")),
        code("let x: i64", &[])
    );
    assert_eq!(
        hover_at(&mut s, &uri, at("{ r: f64 }", "r")),
        code("r: f64", &[])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn functions_and_methods_show_their_signatures_where_declared_and_called() {
    let t = Temp::with(&[("main.la", PROGRAM)]);
    let (mut s, uri) = opened(&t);
    let at = |ctx, word| pos(PROGRAM, ctx, word);
    let area = code("fun Circle.area(self): f64", &[]);
    assert_eq!(
        hover_at(&mut s, &uri, at("fun area(self): f64 =", "area")),
        area
    );
    assert_eq!(hover_at(&mut s, &uri, at("c.area()", "area")), area);
    assert_eq!(
        hover_at(&mut s, &uri, at("fun total", "total")),
        code("fun total(xs: i64[]): i64", &[])
    );
    assert_eq!(
        hover_at(&mut s, &uri, at("max(1.5", "max")),
        code("fun max<T: Ord>(a: T, b: T): T", &["instance: max<f64>"])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn expressions_show_their_types_and_notes() {
    let t = Temp::with(&[("main.la", PROGRAM)]);
    let (mut s, uri) = opened(&t);
    let at = |ctx, word| pos(PROGRAM, ctx, word);
    // The call itself, between its name and its arguments.
    assert_eq!(
        hover_at(&mut s, &uri, at("max(1.5", "(")),
        code("f64", &["instance: max<f64>"])
    );
    assert_eq!(
        hover_at(&mut s, &uri, at("max(1.5", "1.5")),
        code("f64", &[])
    );
    assert_eq!(
        hover_at(&mut s, &uri, at("show(c)", "c")),
        code("Circle", &["converted to Shape"])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn an_interface_method_call_shows_the_call() {
    let t = Temp::with(&[("main.la", PROGRAM)]);
    let (mut s, uri) = opened(&t);
    assert_eq!(
        hover_at(&mut s, &uri, pos(PROGRAM, "s.area()", "area")),
        code("f64", &[])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn nothing_under_the_cursor_is_null() {
    let t = Temp::with(&[("main.la", PROGRAM)]);
    let (mut s, uri) = opened(&t);
    let at = |ctx, word| pos(PROGRAM, ctx, word);
    assert_eq!(hover_at(&mut s, &uri, at("// A comment", "comment")), None);
    assert_eq!(hover_at(&mut s, &uri, at("fun main", "fun")), None);
    assert_eq!(hover_at(&mut s, &uri, (99, 0)), None);
    assert_eq!(hover_at(&mut s, &t.uri("never.la"), (0, 0)), None);
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn hover_works_while_the_program_has_errors() {
    let text = "fun main() {\n    let fine = 2.5;\n    let bad = nope;\n    println(fine);\n}\n";
    let t = Temp::with(&[("main.la", text)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, text);
    s.expect(&uri, &["E0301"]);
    assert_eq!(
        hover_at(&mut s, &uri, pos(text, "let fine", "fine")),
        code("let fine: f64", &[])
    );
    assert_eq!(
        hover_at(&mut s, &uri, pos(text, "println(fine)", "fine")),
        code("f64", &[])
    );
    assert_eq!(hover_at(&mut s, &uri, pos(text, "= nope", "nope")), None);
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_hover_right_after_typing_sees_the_new_text() {
    let t = Temp::with(&[("main.la", PROGRAM)]);
    let (mut s, uri) = opened(&t);
    let changed = PROGRAM
        .replace(
            "let c = Circle { r: 1.0 };",
            "let c = 7;\n    let d = Circle { r: 1.0 };",
        )
        .replace("c.area()", "d.area()")
        .replace("show(c)", "show(d)");
    s.change(&uri, 2, &changed);
    // No wait for the diagnostics: the hover runs the pending check itself.
    assert_eq!(
        hover_at(&mut s, &uri, pos(&changed, "let c", "c")),
        code("let c: i64", &[])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_module_hovers_through_a_main_already_seen() {
    let main = "import geo;\nfun main() { println(geo.double(2)); }\n";
    let geo = "fun double(n: i64): i64 = n * 2;\n";
    let t = Temp::with(&[("main.la", main), ("geo.la", geo)]);
    let (main_uri, geo_uri) = (t.uri("main.la"), t.uri("geo.la"));
    let mut s = Server::start(&t, json!({}));
    s.open(&main_uri, main);
    s.expect(&main_uri, &[]);
    s.open(&geo_uri, geo);
    s.expect(&geo_uri, &[]);
    assert_eq!(
        hover_at(&mut s, &geo_uri, pos(geo, "(n: i64)", "n")),
        code("let n: i64", &[])
    );
    assert_eq!(
        hover_at(&mut s, &main_uri, pos(main, "geo.double", "double")),
        code("fun double(n: i64): i64", &[])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn positions_count_utf16_units() {
    let text = "fun main() { let s = \"é👋\"; let n = 3; println(s); println(n); }\n";
    let t = Temp::with(&[("main.la", text)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, text);
    s.expect(&uri, &[]);
    assert_eq!(
        hover_at(&mut s, &uri, pos(text, "let n", "n")),
        code("let n: i64", &[])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn handles_show_their_extern_type_v16() {
    const HANDLES: &str = "extern type Win;\nextern fun open_win(): Win;\nfun keep(w: Win): Win = w;\nfun main() { let w = keep(open_win()); println(w); }\n";
    let t = Temp::with(&[("main.la", HANDLES)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, HANDLES);
    s.expect(&uri, &[]);
    assert_eq!(
        hover_at(&mut s, &uri, pos(HANDLES, "(w: Win)", "Win")),
        code("extern type Win", &[])
    );
    assert_eq!(
        hover_at(&mut s, &uri, pos(HANDLES, "let w", "w")),
        code("let w: Win", &[])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_receiver_typed_by_its_calls_expected_type_hovers_with_it_v17() {
    let main = "enum Node<M> { Leaf(string), Wrapped(Node<M>, i64) }\n\
                impl Node<M> { fun pad(self, n: i64): Node<M> = Node.Wrapped(self, n); }\n\
                fun leaf<M>(s: string): Node<M> = Node.Leaf(s);\n\
                enum Msg { Go }\n\
                fun a(): Node<Msg> = leaf(\"x\").pad(8);\n\
                fun main() {}\n";
    let t = Temp::with(&[("main.la", main)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, main);
    s.expect(&uri, &[]);
    // `leaf` in `a`: the call's instance; the `(` after it: the receiver's type.
    let line = 4;
    let col = main.lines().nth(4).unwrap().find("leaf").unwrap() as u32;
    let shown = s.hover(&uri, line, col)["contents"]["value"].to_string();
    assert!(shown.contains("instance: leaf<Msg>"), "{shown}");
    let paren = main.lines().nth(4).unwrap().find("\")").unwrap() as u32 + 1;
    let shown = s.hover(&uri, line, paren)["contents"]["value"].to_string();
    assert!(shown.contains("Node<Msg>"), "{shown}");
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn prelude_types_show_as_written_v17() {
    let main = "fun main() {\n    let m = #{\"a\": 1};\n    let it = [1].iter();\n    println(m.len);\n}\n";
    let t = Temp::with(&[("main.la", main)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, main);
    s.expect(&uri, &[]);
    let m = shown(&s.hover(&uri, 1, 8)).unwrap_or_default();
    assert!(m.contains("let m: Map<string, i64>"), "{m}");
    let it = shown(&s.hover(&uri, 2, 8)).unwrap_or_default();
    assert!(it.contains("let it: ArrayIter<i64>"), "{it}");
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_unit_binding_shows_its_type_v17() {
    let program =
        "fun log() {}\n\nfun main() {\n    let done: () = log();\n    println(done);\n}\n";
    let t = Temp::with(&[("main.la", program)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, program);
    s.expect(&uri, &[]);
    assert_eq!(
        hover_at(&mut s, &uri, pos(program, "let done", "done")),
        code("let done: ()", &[])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_constant_shows_its_declaration_v17() {
    let program = "const LIMIT: i64 = 8;\n\nfun main() {\n    println(LIMIT);\n}\n";
    let t = Temp::with(&[("main.la", program)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, program);
    s.expect(&uri, &[]);
    let want = code("const LIMIT: i64", &[]);
    assert_eq!(
        hover_at(&mut s, &uri, pos(program, "(LIMIT)", "LIMIT")),
        want
    );
    assert_eq!(
        hover_at(&mut s, &uri, pos(program, "const LIMIT", "LIMIT")),
        want
    );
    let found = s.definition(&uri, 3, 13);
    assert_eq!(found["range"]["start"]["line"], 0, "{found}");
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn an_included_file_shows_its_size_v17() {
    let program = "const LOGO: u8[] = include_bytes(\"art/logo.svg\");\n\nfun main() {\n    println(LOGO.len);\n}\n";
    let t = Temp::with(&[("main.la", program), ("art/logo.svg", "<svg/>")]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, program);
    s.expect(&uri, &[]);
    assert_eq!(
        hover_at(&mut s, &uri, pos(program, "\"art/logo.svg\"", "logo")),
        code("\"art/logo.svg\"", &["included: 6 bytes"])
    );
    // The call itself is its type.
    assert_eq!(
        hover_at(&mut s, &uri, pos(program, "include_bytes", "bytes")),
        code("u8[]", &[])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_derive_blocks_items_show_their_signatures() {
    let program = "interface Show { fun show(self): string; }\n\
        derive Show for i64 { fun show(self): string = \"{self}\"; }\n\
        fun show_it<T: Show>(x: T): string = x.show();\n\
        fun main() { println(show_it(1)); }\n";
    let t = Temp::with(&[("main.la", program)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, program);
    s.expect(&uri, &[]);
    let at = pos(program, "fun show(self): string =", "show");
    assert_eq!(
        hover_at(&mut s, &uri, at),
        code("fun Show.show(self): string", &[])
    );
    assert_eq!(s.finish(), Some(0));
}
