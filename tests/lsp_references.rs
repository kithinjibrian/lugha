//! Find References in `lughac lsp` end to end (PRP-113, milestone 83):
//! every place a name resolves to the same declaration, across the files
//! the server has checked.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MAIN: &str = "import geo;
import std.math;

struct Circle { r: f64 }
enum Shape { Round(f64), Dot }
impl Circle { fun area(self): f64 = 3.0 * self.r * self.r; }
fun main() {
    let c = Circle { r: 1.0 };
    println(c.area());
    println(c.r);
    let s = Shape.Round(2.0);
    match s { Round(x) => println(x), Dot => {} }
    println(geo.double(2));
    println(math.sqrt(4.0) + math.sqrt(9.0));
}
";

const GEO: &str = "fun double(n: i64): i64 = n * 2;\nfun quad(n: i64): i64 = double(double(n));\n";

/// The zero-based line and UTF-16 column of `word` inside the first
/// `context` in `text`.
fn pos(text: &str, context: &str, word: &str) -> (u32, u32) {
    let offset = text
        .find(context)
        .unwrap_or_else(|| panic!("no {context:?}"))
        + context.find(word).unwrap_or_else(|| panic!("no {word:?}"));
    let before = &text[..offset];
    let line = before.matches('\n').count();
    let col = before[before.rfind('\n').map_or(0, |i| i + 1)..]
        .encode_utf16()
        .count();
    (line as u32, col as u32)
}

/// A server with both files on disk and `main.la` open, checked once.
fn opened(t: &Temp) -> (Server, String) {
    let uri = t.uri("main.la");
    let mut server = Server::start(t, json!({}));
    server.open(&uri, MAIN);
    server.expect(&uri, &[]);
    (server, uri)
}

/// The references at `at` in `uri`: (file name, line, column) of each
/// start, sorted; `None` for a `null` result.
fn refs(
    s: &mut Server,
    uri: &str,
    at: (u32, u32),
    declaration: bool,
) -> Option<Vec<(String, u32, u32)>> {
    let reply = s.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": at.0, "character": at.1 },
            "context": { "includeDeclaration": declaration },
        }),
    );
    assert!(reply.get("error").is_none(), "{reply}");
    let found = reply["result"].as_array()?;
    let mut out: Vec<(String, u32, u32)> = found
        .iter()
        .map(|l: &Value| {
            let file = l["uri"]
                .as_str()
                .unwrap()
                .rsplit('/')
                .next()
                .unwrap()
                .to_string();
            let start = &l["range"]["start"];
            (
                file,
                start["line"].as_u64().unwrap() as u32,
                start["character"].as_u64().unwrap() as u32,
            )
        })
        .collect();
    out.sort();
    Some(out)
}

fn at(file: &str, text: &str, context: &str, word: &str) -> (String, u32, u32) {
    let (l, c) = pos(text, context, word);
    (file.to_string(), l, c)
}

#[test]
fn the_server_offers_references() {
    let t = Temp::with(&[]);
    let s = Server::start(&t, json!({}));
    assert_eq!(s.init["result"]["capabilities"]["referencesProvider"], true);
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_local_from_its_declaration_or_a_use_with_or_without_it() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let uses = vec![
        at("main.la", MAIN, "c.area", "c"),
        at("main.la", MAIN, "c.r)", "c"),
    ];
    let mut all = uses.clone();
    all.push(at("main.la", MAIN, "let c", "c"));
    all.sort();
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "c.area", "c"), true),
        Some(all.clone())
    );
    assert_eq!(refs(&mut s, &uri, pos(MAIN, "let c", "c"), true), Some(all));
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "let c", "c"), false),
        Some(uses)
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_function_in_another_file_its_uses_in_both() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let mut want = vec![
        at("geo.la", GEO, "fun double", "double"),
        at("geo.la", GEO, "double(double", "double"),
        at("geo.la", GEO, "(double(n)", "double"),
        at("main.la", MAIN, "geo.double", "double"),
    ];
    want.sort();
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "geo.double", "double"), true),
        Some(want)
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn types_fields_variants_methods_and_modules() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let sorted = |mut v: Vec<(String, u32, u32)>| {
        v.sort();
        Some(v)
    };
    let m = |context: &str, word: &str| at("main.la", MAIN, context, word);
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "struct Circle", "Circle"), true),
        sorted(vec![
            m("struct Circle", "Circle"),
            m("impl Circle", "Circle"),
            m("= Circle {", "Circle")
        ])
    );
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "{ r: f64", "r"), true),
        sorted(vec![
            m("{ r: f64", "r"),
            m("self.r *", "r"),
            m("* self.r;", "r"),
            m("{ r: 1.0", "r"),
            m("c.r)", "r")
        ])
    );
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "Round(x)", "Round"), true),
        sorted(vec![
            m("{ Round(f64)", "Round"),
            m("Shape.Round", "Round"),
            m("Round(x)", "Round")
        ])
    );
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "c.area", "area"), true),
        sorted(vec![m("fun area", "area"), m("c.area", "area")])
    );
    // A module: its import and its uses; it has no declaration to add.
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "geo.double", "geo"), true),
        sorted(vec![m("import geo", "geo"), m("geo.double", "geo")])
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_std_function_lists_its_uses_and_nothing_under_the_cursor_is_null() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t);
    let found = refs(&mut s, &uri, pos(MAIN, "math.sqrt(4", "sqrt"), true).expect("uses");
    let mut want = vec![
        at("main.la", MAIN, "math.sqrt(4", "sqrt"),
        at("main.la", MAIN, "math.sqrt(9", "sqrt"),
    ];
    want.sort();
    assert_eq!(found, want);
    // An intrinsic has no declaration; a blank line has no name.
    assert_eq!(
        refs(&mut s, &uri, pos(MAIN, "println(c.area", "println"), true),
        None
    );
    assert_eq!(refs(&mut s, &uri, (1, 0), true), None);
    assert_eq!(s.finish(), Some(0));
}

const SHARED: &str = "struct P { name: string }
impl P { fun name(self): string = \"Dr. {self.name}\"; }
enum Shape { Circle(f64), Dot }
impl Shape { fun Circle(self): bool = match self { Circle(_) => true, Dot => false }; }
fun main() {
    let p = P { name: \"Ada\" };
    println(p.name);
    println(p.name());
    let s = Shape.Circle(1.0);
    println(s.Circle());
}
";

#[test]
fn a_field_and_a_method_of_one_name_are_two_members_v17() {
    let t = Temp::with(&[("main.la", SHARED)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, SHARED);
    s.expect(&uri, &[]);
    let m = |context: &str, word: &str| at("main.la", SHARED, context, word);
    let sorted = |mut v: Vec<(String, u32, u32)>| {
        v.sort();
        Some(v)
    };
    let field = sorted(vec![
        m("{ name: string", "name"),
        m("self.name}", "name"),
        m("{ name: \"Ada", "name"),
        m("p.name);", "name"),
    ]);
    assert_eq!(
        refs(&mut s, &uri, pos(SHARED, "p.name);", "name"), true),
        field
    );
    let method = sorted(vec![m("fun name", "name"), m("p.name()", "name")]);
    assert_eq!(
        refs(&mut s, &uri, pos(SHARED, "p.name()", "name"), true),
        method
    );
    let variant = sorted(vec![
        m("{ Circle(f64)", "Circle"),
        m("{ Circle(_)", "Circle"),
        m("Shape.Circle(1.0)", "Circle"),
    ]);
    assert_eq!(
        refs(
            &mut s,
            &uri,
            pos(SHARED, "Shape.Circle(1.0)", "Circle"),
            true
        ),
        variant
    );
    let self_method = sorted(vec![m("fun Circle", "Circle"), m("s.Circle()", "Circle")]);
    assert_eq!(
        refs(&mut s, &uri, pos(SHARED, "s.Circle()", "Circle"), true),
        self_method
    );
    assert_eq!(s.finish(), Some(0));
}
