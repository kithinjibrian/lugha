//! Doc comments in `lughac lsp` end to end (PRP-107, milestone 77): `/** */`
//! text and tags in hover, completion and signature help, and the library's.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MAIN: &str = "import std.math;

/**
 * A point on the plane.
 */
struct Point {
    /** Across. */
    x: f64,
}

/** Shapes we draw. */
enum Shape {
    /** A round one. */
    Circle(f64),
    Dot,
}

impl Point {
    /**
     * Scales the point.
     * @param k the factor
     * @returns a new point
     */
    fun scale(self, k: f64): Point = Point { x: self.x * k };
}

/**
 * Returns the larger of `a` and `b`.
 * @param a the first
 * @param b the second
 * @returns the larger
 */
fun max<T: Ord>(a: T, b: T): T { if a > b { a } else { b } }

fun plain(): i64 = 1;

fun main() {
    let p = Point { x: 1.0 };
    let s = Shape.Circle(1.0);
    let xs = [1, 2];
    let word = \"a b\";
    println(max(1, 2));
    println(p.scale(2.0).x);
    println(plain());
    println(math.sqrt(4.0));
    println(xs.map(fun(n: i64): i64 = n * 2));
    println(word.split(\" \"));
}
";

const MAX_DOC: &str = "Returns the larger of `a` and `b`.\n\n*a* — the first  \n*b* — the second  \n*returns* — the larger";

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

fn opened(t: &Temp) -> (Server, String) {
    let uri = t.uri("main.la");
    let mut server = Server::start(t, json!({}));
    server.open(&uri, MAIN);
    server.expect(&uri, &[]);
    (server, uri)
}

fn hover(s: &mut Server, uri: &str, context: &str, word: &str) -> String {
    let (line, col) = pos(MAIN, context, word);
    s.hover(uri, line, col)["contents"]["value"]
        .as_str()
        .unwrap_or_default()
        .to_string()
}

fn code(line: &str) -> String {
    format!("```lugha\n{line}\n```")
}

#[test]
fn hover_shows_the_doc_under_the_header() {
    let t = Temp::with(&[("main.la", MAIN)]);
    let (mut s, uri) = opened(&t);
    assert_eq!(
        hover(&mut s, &uri, "fun max<", "max"),
        format!("{}\n\n{MAX_DOC}", code("fun max<T: Ord>(a: T, b: T): T"))
    );
    assert_eq!(
        hover(&mut s, &uri, "println(max(", "max"),
        format!(
            "{}\n\ninstance: max<i64>\n\n{MAX_DOC}",
            code("fun max<T: Ord>(a: T, b: T): T")
        )
    );
    assert_eq!(
        hover(&mut s, &uri, "p.scale(", "scale"),
        format!(
            "{}\n\nScales the point.\n\n*k* — the factor  \n*returns* — a new point",
            code("fun Point.scale(self, k: f64): Point")
        )
    );
    assert_eq!(
        hover(&mut s, &uri, "    x: f64", "x"),
        format!("{}\n\nAcross.", code("x: f64"))
    );
    assert_eq!(
        hover(&mut s, &uri, "println(plain", "plain"),
        code("fun plain(): i64")
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn hover_answers_on_type_and_variant_names() {
    let t = Temp::with(&[("main.la", MAIN)]);
    let (mut s, uri) = opened(&t);
    assert_eq!(
        hover(&mut s, &uri, "= Point {", "Point"),
        format!("{}\n\nA point on the plane.", code("struct Point"))
    );
    assert_eq!(
        hover(&mut s, &uri, "struct Point", "Point"),
        format!("{}\n\nA point on the plane.", code("struct Point"))
    );
    assert_eq!(
        hover(&mut s, &uri, "= Shape.Circle", "Shape"),
        format!("{}\n\nShapes we draw.", code("enum Shape"))
    );
    assert_eq!(
        hover(&mut s, &uri, "Shape.Circle(1.0)", "Circle"),
        format!("{}\n\nA round one.", code("Circle(f64)"))
    );
    assert_eq!(hover(&mut s, &uri, "    Dot,", "Dot"), code("Dot"));
    assert_eq!(s.finish(), Some(0));
}

/// The completion items at the end of `typed`, put on a new line before
/// `println(plain());`, as JSON.
fn items(t: &Temp, typed: &str) -> Vec<Value> {
    let text = MAIN.replacen(
        "    println(plain());",
        &format!("    {typed}\n    println(plain());"),
        1,
    );
    let (line, col) = pos(&text, &format!("    {typed}\n"), "\n");
    let uri = t.uri("main.la");
    let mut s = Server::start(t, json!({}));
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

fn documentation(items: &[Value], label: &str) -> Option<String> {
    let item = items.iter().find(|i| i["label"] == label)?;
    item["documentation"]["value"].as_str().map(String::from)
}

#[test]
fn completion_items_carry_their_docs() {
    let t = Temp::with(&[("main.la", MAIN)]);
    let members = items(&t, "p.");
    assert_eq!(documentation(&members, "x").as_deref(), Some("Across."));
    assert_eq!(
        documentation(&members, "scale").as_deref(),
        Some("Scales the point.\n\n*k* — the factor  \n*returns* — a new point")
    );
    let names = items(&t, "ma");
    assert_eq!(documentation(&names, "max").as_deref(), Some(MAX_DOC));
    assert_eq!(documentation(&names, "plain"), None);
    let variants = items(&t, "Shape.");
    assert_eq!(
        documentation(&variants, "Circle").as_deref(),
        Some("A round one.")
    );
}

#[test]
fn signature_help_shows_the_summary_and_the_active_parameters_doc() {
    let t = Temp::with(&[("main.la", MAIN)]);
    let text = MAIN.replacen(
        "    println(plain());",
        "    max(1, \n    println(plain());",
        1,
    );
    let (line, col) = pos(&text, "    max(1, \n", "\n");
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, MAIN);
    s.expect(&uri, &[]);
    s.change(&uri, 2, &text);
    let reply = s.request(
        "textDocument/signatureHelp",
        json!({ "textDocument": { "uri": uri }, "position": { "line": line, "character": col } }),
    );
    let sig = &reply["result"]["signatures"][0];
    assert_eq!(
        sig["documentation"]["value"],
        "Returns the larger of `a` and `b`.\n\n*returns* — the larger",
        "{reply}"
    );
    assert_eq!(reply["result"]["activeParameter"], 1);
    assert_eq!(
        sig["parameters"][1]["documentation"], "the second",
        "{reply}"
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn the_library_is_documented() {
    let t = Temp::with(&[("main.la", MAIN)]);
    let (mut s, uri) = opened(&t);
    for (context, word) in [
        ("math.sqrt", "sqrt"),
        ("xs.map", "map"),
        ("word.split", "split"),
    ] {
        let shown = hover(&mut s, &uri, context, word);
        let doc = shown
            .split_once("```\n\n")
            .map(|(_, d)| d)
            .unwrap_or_default();
        assert!(!doc.trim().is_empty(), "{word} has no doc: {shown:?}");
    }
    assert_eq!(s.finish(), Some(0));
}

const MODULES: &str = "import geo.shapes;
import geo.plain;
import std.math;

fun main() {
    println(shapes.area() + plain.one());
    println(math.sqrt(4.0));
}
";

const SHAPES: &str = "/**
 * Shapes and their areas.
 */

/** The unit area. */
public fun area(): i64 = 1;
";

#[test]
fn hover_on_a_module_shows_its_module_doc_v17() {
    let t = Temp::with(&[
        ("main.la", MODULES),
        ("geo/shapes.la", SHAPES),
        ("geo/plain.la", "public fun one(): i64 = 1;\n"),
    ]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, MODULES);
    s.expect(&uri, &[]);
    let mut at = |context: &str, word: &str| {
        let (line, col) = pos(MODULES, context, word);
        s.hover(&uri, line, col)["contents"]["value"]
            .as_str()
            .unwrap_or_default()
            .to_string()
    };
    let shapes = format!("{}\n\nShapes and their areas.", code("module geo.shapes"));
    assert_eq!(at("import geo.shapes", "shapes"), shapes);
    assert_eq!(at("shapes.area()", "shapes"), shapes);
    // A module without a doc, and the first item keeping its own.
    assert_eq!(at("import geo.plain", "plain"), code("module geo.plain"));
    assert_eq!(
        at("shapes.area()", "area"),
        format!("{}\n\nThe unit area.", code("fun area(): i64"))
    );
    let math = at("import std.math", "math");
    assert!(
        math.starts_with(&format!("{}\n\n", code("module std.math"))) && math.len() > 40,
        "{math:?}"
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_packages_entry_hovers_and_opens_as_its_module_v17() {
    let main = "import geo;\nfun main() { println(geo.one()); }\n";
    let t = Temp::with(&[
        ("main.la", main),
        ("lugha.toml", "[packages]\ngeo = \"geo\"\n"),
        (
            "geo/lib.la",
            "/**\n * Geometry.\n */\n\npublic fun one(): i64 = 1;\n",
        ),
    ]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, main);
    s.expect(&uri, &[]);
    let shown = s.hover(&uri, 0, 8)["contents"]["value"]
        .as_str()
        .unwrap_or_default()
        .to_string();
    assert_eq!(shown, format!("{}\n\nGeometry.", code("module geo")));
    let found = s.definition(&uri, 1, 22);
    assert!(
        found["uri"]
            .as_str()
            .unwrap_or_default()
            .ends_with("geo/lib.la"),
        "{found}"
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn extern_types_and_actors_carry_their_docs_v17() {
    let main = "/** A C window. */\npublic extern type Win;\n\n\
                /** Counts. */\nactor Counter {\n    n: i64,\n    fun get(self): i64 = self.n;\n}\n\n\
                fun keep(w: Win) {}\n\
                fun main() {\n    let c = spawn Counter { n: 0 };\n    println(c.get());\n}\n";
    let t = Temp::with(&[("main.la", main)]);
    let uri = t.uri("main.la");
    let mut s = Server::start(&t, json!({}));
    s.open(&uri, main);
    s.expect(&uri, &[]);
    let mut at = |context: &str, word: &str| {
        let start = main.find(context).unwrap() + context.find(word).unwrap();
        let before = &main[..start];
        let line = before.matches('\n').count() as u32;
        let col = (start - before.rfind('\n').map_or(0, |i| i + 1)) as u32;
        s.hover(&uri, line, col)["contents"]["value"].to_string()
    };
    for (context, word, doc) in [
        ("type Win", "Win", "A C window."),
        ("w: Win", "Win", "A C window."),
        ("actor Counter", "Counter", "Counts."),
    ] {
        let shown = at(context, word);
        assert!(shown.contains(doc), "{context}: {shown}");
    }
    assert_eq!(s.finish(), Some(0));
}
