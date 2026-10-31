//! Completion in `lughac lsp` end to end (PRP-103, milestone 73): members
//! after `.` and names in scope, while the text being typed doesn't parse.

use serde_json::json;

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

// LSP `CompletionItemKind` numbers.
const METHOD: u64 = 2;
const FUNCTION: u64 = 3;
const FIELD: u64 = 5;
const VARIABLE: u64 = 6;
const MODULE: u64 = 9;
const KEYWORD: u64 = 14;
const ENUM_MEMBER: u64 = 20;
const STRUCT: u64 = 22;

const MAIN: &str = "import geo;

struct P { x: f64, n: i64 }
impl P { fun len(self): i64 = self.n; }
enum Shape { Circle(f64), Dot }
impl Shape { fun unit(): Shape = Shape.Dot; }
fun twice(n: i64): i64 = n * 2;
fun other() { let hidden = 1; println(hidden); }
fun main() {
    let p = P { x: 1.0, n: 2 };
    let xs = [1, 2, 3];
    let word = \"hi\";
    println(p.n);
    println(xs.len);
    println(word);
    let later = 0;
    println(later);
}
";

const GEO: &str =
    "fun double(n: i64): i64 = n * 2;\nstruct Point { x: i64 }\nprivate fun secret(): i64 = 1;\n";

/// `MAIN` with `insert` placed right after the first `anchor`, and the
/// zero-based line and UTF-16 character at the end of the insertion.
fn typed(anchor: &str, insert: &str) -> (String, (u32, u32)) {
    let at = MAIN.find(anchor).unwrap_or_else(|| panic!("no {anchor:?}")) + anchor.len();
    let text = format!("{}{insert}{}", &MAIN[..at], &MAIN[at..]);
    let end = &text[..at + insert.len()];
    let line = end.matches('\n').count() as u32;
    let col = end[end.rfind('\n').map_or(0, |i| i + 1)..]
        .encode_utf16()
        .count() as u32;
    (text, (line, col))
}

/// A server with `MAIN` and `GEO` checked once, then `MAIN` changed to
/// `text` (no wait), and the completions at `(line, col)`.
fn complete(t: &Temp, text: &str, (line, col): (u32, u32)) -> Vec<(String, u64, String)> {
    let uri = t.uri("main.la");
    let mut server = Server::start(t, json!({}));
    server.open(&uri, MAIN);
    server.expect(&uri, &[]);
    server.change(&uri, 2, text);
    let items = server.completion(&uri, line, col);
    assert_eq!(server.finish(), Some(0));
    items
}

fn find<'a>(items: &'a [(String, u64, String)], label: &str) -> Option<&'a (String, u64, String)> {
    items.iter().find(|(l, _, _)| l == label)
}

fn files() -> Temp {
    Temp::with(&[("main.la", MAIN), ("geo.la", GEO)])
}

#[test]
fn the_server_offers_completion_on_dot() {
    let t = Temp::with(&[]);
    let server = Server::start(&t, json!({}));
    let provider = &server.init["result"]["capabilities"]["completionProvider"];
    assert_eq!(
        provider["triggerCharacters"],
        json!(["."]),
        "{}",
        server.init
    );
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn a_struct_value_offers_its_fields_and_methods() {
    let t = files();
    let (text, at) = typed("println(word);\n", "    p.");
    let items = complete(&t, &text, at);
    assert_eq!(
        find(&items, "x"),
        Some(&("x".into(), FIELD, "x: f64".into()))
    );
    assert_eq!(
        find(&items, "len"),
        Some(&("len".into(), METHOD, "fun P.len(self): i64".into()))
    );
    assert!(find(&items, "Circle").is_none(), "{items:?}");
}

#[test]
fn a_partly_typed_member_still_completes() {
    let t = files();
    let (text, at) = typed("println(word);\n", "    p.le");
    let items = complete(&t, &text, at);
    assert!(find(&items, "len").is_some(), "{items:?}");
}

#[test]
fn arrays_and_strings_offer_built_ins_and_helpers() {
    let t = files();
    let (text, at) = typed("println(word);\n", "    xs.");
    let items = complete(&t, &text, at);
    for name in ["len", "push", "map", "filter"] {
        assert!(find(&items, name).is_some(), "{name} in {items:?}");
    }
    let (text, at) = typed("println(word);\n", "    word.");
    let items = complete(&t, &text, at);
    for name in ["len", "split", "trim", "chars"] {
        assert!(find(&items, name).is_some(), "{name} in {items:?}");
    }
}

#[test]
fn a_module_offers_its_items_but_not_private_ones() {
    let t = files();
    let (text, at) = typed("println(word);\n", "    geo.");
    let items = complete(&t, &text, at);
    assert_eq!(
        find(&items, "double"),
        Some(&("double".into(), FUNCTION, "fun double(n: i64): i64".into()))
    );
    assert_eq!(find(&items, "Point").map(|i| i.1), Some(STRUCT));
    assert!(find(&items, "secret").is_none(), "{items:?}");
}

#[test]
fn an_enum_offers_its_variants_and_associated_functions() {
    let t = files();
    let (text, at) = typed("println(word);\n", "    Shape.");
    let items = complete(&t, &text, at);
    assert_eq!(
        find(&items, "Circle"),
        Some(&("Circle".into(), ENUM_MEMBER, "Circle(f64)".into()))
    );
    assert_eq!(find(&items, "Dot").map(|i| i.1), Some(ENUM_MEMBER));
    assert_eq!(find(&items, "unit").map(|i| i.1), Some(FUNCTION));
}

#[test]
fn a_plain_word_offers_names_in_scope() {
    let t = files();
    let (text, at) = typed("println(word);\n", "    wo");
    let items = complete(&t, &text, at);
    assert_eq!(
        find(&items, "word"),
        Some(&("word".into(), VARIABLE, "let word: string".into()))
    );
    assert_eq!(find(&items, "p").map(|i| i.1), Some(VARIABLE));
    assert_eq!(
        find(&items, "twice"),
        Some(&("twice".into(), FUNCTION, "fun twice(n: i64): i64".into()))
    );
    assert_eq!(find(&items, "P").map(|i| i.1), Some(STRUCT));
    assert_eq!(find(&items, "geo").map(|i| i.1), Some(MODULE));
    assert!(find(&items, "println").is_some(), "{items:?}");
    assert!(find(&items, "Some").is_some(), "{items:?}");
    assert_eq!(find(&items, "while").map(|i| i.1), Some(KEYWORD));
    assert_eq!(find(&items, "defer").map(|i| i.1), Some(KEYWORD));
    // Not declared yet at the cursor, or in another function.
    assert!(find(&items, "later").is_none(), "{items:?}");
    assert!(find(&items, "hidden").is_none(), "{items:?}");
}

#[test]
fn an_edit_earlier_in_the_file_no_longer_matters() {
    let t = files();
    // An edit near the top, then `p.` below it: the current text is checked
    // (milestone 75), so nothing depends on the last good check's offsets.
    let (text, _) = typed("println(word);\n", "    p.");
    let text = text.replacen("import geo;", "import geo;\n// added", 1);
    let line = text[..text.find("    p.").unwrap()].matches('\n').count() as u32;
    let items = complete(&t, &text, (line, 6));
    assert!(find(&items, "x").is_some(), "{items:?}");
    let (text, _) = typed("println(word);\n", "    wo");
    let text = text.replacen("import geo;", "import geo;\n// added", 1);
    let line = text[..text.find("    wo").unwrap()].matches('\n').count() as u32;
    let items = complete(&t, &text, (line, 6));
    assert!(find(&items, "word").is_some(), "{items:?}");
}

#[test]
fn a_lex_error_earlier_falls_back_and_stops_member_completion() {
    let t = files();
    // An unclosed string leaves no tokens at all: completion falls back to
    // the last good check, whose offsets no longer line up below the edit.
    let (text, _) = typed("println(word);\n", "    p.");
    let text = text.replacen("import geo;", "import geo;\n\"oops", 1);
    let line = text[..text.find("    p.").unwrap()].matches('\n').count() as u32;
    let items = complete(&t, &text, (line, 6));
    assert!(find(&items, "x").is_none(), "{items:?}");
    // Plain names still come, without locals.
    let (text, _) = typed("println(word);\n", "    wo");
    let text = text.replacen("import geo;", "import geo;\n\"oops", 1);
    let line = text[..text.find("    wo").unwrap()].matches('\n').count() as u32;
    let items = complete(&t, &text, (line, 6));
    assert!(find(&items, "twice").is_some(), "{items:?}");
    assert!(find(&items, "word").is_none(), "{items:?}");
}

#[test]
fn a_file_never_opened_has_no_completions() {
    let t = files();
    let uri = t.uri("main.la");
    let mut server = Server::start(&t, json!({}));
    assert_eq!(server.completion(&t.uri("never.la"), 0, 0), Vec::new());
    server.open(&uri, MAIN);
    server.expect(&uri, &[]);
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn nothing_completes_inside_a_string_or_comment() {
    let t = files();
    let (text, at) = typed("println(word);\n", "    // p.");
    assert_eq!(complete(&t, &text, at), Vec::new());
    let (text, at) = typed("println(word);\n", "    let s = \"p.");
    assert_eq!(complete(&t, &text, at), Vec::new());
}

#[test]
fn a_maps_members_complete_without_its_internals_v17() {
    let main = "fun main() {\n    let m = #{\"a\": 1};\n    println(m.len);\n}\n";
    let t = Temp::with(&[("main.la", main)]);
    let uri = t.uri("main.la");
    let mut server = Server::start(&t, json!({}));
    server.open(&uri, main);
    server.expect(&uri, &[]);
    let text = main.replace("println(m.len);", "println(m.len);\n    m.");
    server.change(&uri, 2, &text);
    let items = server.completion(&uri, 3, 6);
    assert_eq!(server.finish(), Some(0));
    for (label, kind) in [
        ("get", METHOD),
        ("has", METHOD),
        ("keys", METHOD),
        ("remove", METHOD),
        ("len", FIELD),
    ] {
        assert!(
            items.iter().any(|(l, k, _)| l == label && *k == kind),
            "{label}: {items:?}"
        );
    }
    assert!(
        !items.iter().any(|(l, _, _)| l.starts_with('$')),
        "{items:?}"
    );
}

#[test]
fn derive_dot_in_a_derive_block_offers_the_field_intrinsics() {
    let before = "interface S { fun s(self): i64; }\nderive S for struct {\n    fun s(self): i64 = 1;\n}\nfun main() {}\n";
    let t = Temp::with(&[("main.la", before)]);
    let uri = t.uri("main.la");
    let mut server = Server::start(&t, json!({}));
    server.open(&uri, before);
    server.expect(&uri, &[]);
    let text = before.replace("= 1;", "= derive.");
    server.change(&uri, 2, &text);
    let col = "    fun s(self): i64 = derive.".len() as u32;
    let items = server.completion(&uri, 2, col);
    assert_eq!(server.finish(), Some(0));
    let fields = find(&items, "fields").expect("offered");
    assert_eq!(fields.1, FUNCTION);
    assert!(fields.2.starts_with("derive.fields("), "{fields:?}");
    assert!(find(&items, "build_variant").is_some(), "{items:?}");
}
