//! Semantic tokens in `lughac lsp` end to end (PRP-187, milestone 154):
//! every token classified, names by what the check resolved them to.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MAIN: &str = "import geo;

/** A point. */
struct Point { x: f64 }
enum Shape { Round(f64), Dot }
const LIMIT: i64 = 10;
fun same<T>(v: T): T = v;
fun main() {
    let mut p = Point { x: 1.0 };
    p.x = 2.0;
    let s = Shape.Round(p.x);
    match s { Round(r) => println(r), Dot => {} }
    println(\"n = {geo.double(LIMIT)}\");
    println(same(3));
}
";

const GEO: &str = "fun double(n: i64): i64 = n * 2;\n";

/// One decoded token: its type and modifiers, by name.
#[derive(Debug, PartialEq)]
struct Token {
    line: u32,
    col: u32,
    len: u32,
    kind: String,
    mods: Vec<String>,
}

/// The tokens of `uri`, decoded against the legend the server declared.
fn tokens(server: &mut Server, uri: &str) -> Vec<Token> {
    let legend = server.init["result"]["capabilities"]["semanticTokensProvider"]["legend"].clone();
    let names = |key: &str| -> Vec<String> {
        legend[key]
            .as_array()
            .expect("a legend list")
            .iter()
            .map(|v| v.as_str().expect("names").to_string())
            .collect()
    };
    let (types, modifiers) = (names("tokenTypes"), names("tokenModifiers"));
    let reply = server.request(
        "textDocument/semanticTokens/full",
        json!({ "textDocument": { "uri": uri } }),
    );
    assert!(reply.get("error").is_none(), "{reply}");
    let data: Vec<u32> = reply["result"]["data"]
        .as_array()
        .unwrap_or_else(|| panic!("no data: {reply}"))
        .iter()
        .map(|v| v.as_u64().expect("integers") as u32)
        .collect();
    let (mut line, mut col) = (0, 0);
    data.chunks(5)
        .map(|c| {
            if c[0] > 0 {
                col = 0;
            }
            line += c[0];
            col += c[1];
            Token {
                line,
                col,
                len: c[2],
                kind: types[c[3] as usize].clone(),
                mods: (0..modifiers.len())
                    .filter(|b| c[4] & (1 << b) != 0)
                    .map(|b| modifiers[b].clone())
                    .collect(),
            }
        })
        .collect()
}

/// The zero-based line and UTF-16 column of `word` inside the first
/// `context` in `text`.
fn pos(text: &str, context: &str, word: &str) -> (u32, u32) {
    let start = text
        .find(context)
        .unwrap_or_else(|| panic!("no {context:?}"));
    let offset = start + context.find(word).unwrap_or_else(|| panic!("no {word:?}"));
    let before = &text[..offset];
    let line = before.matches('\n').count();
    let col = before[before.rfind('\n').map_or(0, |i| i + 1)..]
        .encode_utf16()
        .count();
    (line as u32, col as u32)
}

/// The kind and modifiers of the token starting at `word` in `context`.
fn at(tokens: &[Token], context: &str, word: &str) -> Option<(String, Vec<String>)> {
    let (line, col) = pos(MAIN, context, word);
    tokens
        .iter()
        .find(|t| t.line == line && t.col == col)
        .map(|t| (t.kind.clone(), t.mods.clone()))
}

fn is(kind: &str, mods: &[&str]) -> Option<(String, Vec<String>)> {
    Some((
        kind.to_string(),
        mods.iter().map(|m| m.to_string()).collect(),
    ))
}

fn opened(t: &Temp) -> (Server, String) {
    let uri = t.uri("main.la");
    let mut server = Server::start(t, json!({}));
    server.open(&uri, MAIN);
    server.expect(&uri, &[]);
    (server, uri)
}

#[test]
fn the_server_offers_full_semantic_tokens_with_a_standard_legend() {
    let t = Temp::with(&[]);
    let server = Server::start(&t, json!({}));
    let provider = &server.init["result"]["capabilities"]["semanticTokensProvider"];
    assert_eq!(provider["full"], true, "{provider}");
    assert!(
        provider.get("range").is_none_or(Value::is_null),
        "{provider}"
    );
    assert_eq!(
        provider["legend"]["tokenModifiers"],
        json!(["declaration", "mutable", "defaultLibrary", "documentation"])
    );
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn lexical_tokens_are_classified() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let all = tokens(&mut s, &uri);
    assert_eq!(at(&all, "import geo", "import"), is("keyword", &[]));
    assert_eq!(
        at(&all, "/** A point", "/**"),
        is("comment", &["documentation"])
    );
    assert_eq!(
        at(&all, "{ x: f64 }", "f64"),
        is("type", &["defaultLibrary"])
    );
    assert_eq!(at(&all, "= 10;", "10"), is("number", &[]));
    assert_eq!(at(&all, "= 10;", "="), is("operator", &[]));
    assert_eq!(at(&all, "\"n = {", "\"n = {"), is("string", &[]));
    // Punctuation has no token.
    assert_eq!(at(&all, "{ x: f64 }", "{"), None);
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn names_are_classified_by_what_they_resolve_to() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let all = tokens(&mut s, &uri);
    let check = |context: &str, word: &str, kind: &str, mods: &[&str]| {
        assert_eq!(
            at(&all, context, word),
            is(kind, mods),
            "{word} in {context:?}"
        );
    };
    check("import geo", "geo", "namespace", &[]);
    check("struct Point", "Point", "struct", &["declaration"]);
    check("= Point {", "Point", "struct", &[]);
    check("{ x: f64 }", "x", "property", &["declaration"]);
    check("p.x = 2.0", "x", "property", &[]);
    check("enum Shape", "Shape", "enum", &["declaration"]);
    check("{ Round(f64)", "Round", "enumMember", &["declaration"]);
    check("{ Round(r)", "Round", "enumMember", &[]);
    check("= Shape.Round", "Shape", "enum", &[]);
    check("= Shape.Round", "Round", "enumMember", &[]);
    check("const LIMIT", "LIMIT", "variable", &["declaration"]);
    check("double(LIMIT)", "LIMIT", "variable", &[]);
    check("same<T>", "same", "function", &["declaration"]);
    check("same<T>", "T", "typeParameter", &["declaration"]);
    check("(v: T)", "v", "parameter", &["declaration"]);
    check("let mut p", "p", "variable", &["declaration", "mutable"]);
    check("p.x = 2.0", "p", "variable", &["mutable"]);
    check("let s", "s", "variable", &["declaration"]);
    check("geo.double", "double", "function", &[]);
    check("println(same", "println", "function", &["defaultLibrary"]);
    check("println(same", "same", "function", &[]);
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn an_unresolved_name_has_no_token() {
    let text = "fun main() {\n    println(nope);\n}\n";
    let t = Temp::with(&[("main.la", text)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, text);
    s.expect(&uri, &["E0301"]);
    let all = tokens(&mut s, &uri);
    assert!(!all.iter().any(|t| t.line == 1 && t.col == 12), "{all:?}");
    assert!(
        all.iter()
            .any(|t| t.line == 1 && t.col == 4 && t.kind == "function")
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn an_unopened_document_has_none() {
    let t = Temp::with(&[]);
    let mut s = Server::start(&t, json!({}));
    let reply = s.request(
        "textDocument/semanticTokens/full",
        json!({ "textDocument": { "uri": t.uri("missing.la") } }),
    );
    assert_eq!(reply["result"], Value::Null, "{reply}");
    assert_eq!(s.finish(), Some(0));
}
