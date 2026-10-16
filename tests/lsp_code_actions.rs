//! Code actions in `lughac lsp` end to end (PRP-109, milestone 79): the
//! quick fixes on a non-exhaustive `match`.

use std::process::Command;

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MAIN: &str = "enum Shape { Circle(f64), Rect(f64, f64), Dot }

fun area(s: Shape): f64 = match s {
    Circle(r) => 3.0 * r * r,
};

fun main() {
    let o: Option<Shape> = None;
    match o {
        Some(Dot) => println(\"dot\"),
        None => {}
    }
    let n = 3;
    match n { 1 => {} }
    println(area(Shape.Dot));
}
";

/// The server's diagnostics for `MAIN`, and the code actions for them.
fn actions(t: &Temp, text: &str) -> (Vec<Value>, Vec<Value>) {
    let uri = t.uri("main.la");
    let mut s = Server::start(t, json!({}));
    s.open(&uri, text);
    let published = s
        .wait(|m| m["method"] == "textDocument/publishDiagnostics" && m["params"]["uri"] == uri)
        .expect("diagnostics");
    let diagnostics = published["params"]["diagnostics"]
        .as_array()
        .cloned()
        .unwrap_or_default();
    let reply = s.request(
        "textDocument/codeAction",
        json!({
            "textDocument": { "uri": uri },
            "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 99, "character": 0 } },
            "context": { "diagnostics": diagnostics },
        }),
    );
    assert_eq!(s.finish(), Some(0));
    (
        diagnostics,
        reply["result"].as_array().cloned().unwrap_or_default(),
    )
}

/// `text` with `action`'s single edit applied.
fn apply(text: &str, action: &Value) -> String {
    let edits = action["edit"]["changes"]
        .as_object()
        .expect("changes")
        .values()
        .next()
        .unwrap();
    let edit = &edits[0];
    let offset = |p: &Value| {
        let (line, col) = (
            p["line"].as_u64().unwrap() as usize,
            p["character"].as_u64().unwrap() as usize,
        );
        let start: usize = text.split_inclusive('\n').take(line).map(str::len).sum();
        let line_text = &text[start..];
        start
            + line_text
                .char_indices()
                .nth(col)
                .map_or(line_text.len(), |(i, _)| i)
    };
    let (from, to) = (
        offset(&edit["range"]["start"]),
        offset(&edit["range"]["end"]),
    );
    format!(
        "{}{}{}",
        &text[..from],
        edit["newText"].as_str().unwrap(),
        &text[to..]
    )
}

fn titled<'a>(actions: &'a [Value], title: &str) -> Vec<&'a Value> {
    actions.iter().filter(|a| a["title"] == title).collect()
}

#[test]
fn the_server_offers_quick_fixes() {
    let t = Temp::with(&[]);
    let server = Server::start(&t, json!({}));
    let provider = &server.init["result"]["capabilities"]["codeActionProvider"];
    assert_eq!(
        provider["codeActionKinds"],
        json!(["quickfix"]),
        "{}",
        server.init
    );
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn missing_arms_are_added_in_full_and_the_result_checks() {
    let t = Temp::with(&[("main.la", MAIN)]);
    let (diagnostics, actions) = actions(&t, MAIN);
    assert_eq!(
        diagnostics.iter().filter(|d| d["code"] == "E0417").count(),
        3
    );
    let add = titled(&actions, "Add missing arms");
    assert_eq!(
        add.len(),
        2,
        "the enum match and the Option match: {actions:?}"
    );
    assert_eq!(add[0]["kind"], "quickfix");
    assert_eq!(add[0]["isPreferred"], true);
    // Applying every "Add missing arms", then the integer's `_`, checks.
    let mut text = MAIN.to_string();
    let mut fixes: Vec<&Value> = add.clone();
    fixes.extend(titled(&actions, "Add a `_` arm").into_iter().filter(|a| {
        a["diagnostics"][0]["message"]
            .as_str()
            .unwrap_or_default()
            .contains("other values")
    }));
    // From the bottom up, so earlier edits keep their positions.
    fixes.sort_by_key(|a| {
        std::cmp::Reverse(
            a["edit"]["changes"]
                .as_object()
                .unwrap()
                .values()
                .next()
                .unwrap()[0]["range"]["start"]["line"]
                .as_u64()
                .unwrap(),
        )
    });
    for fix in fixes {
        text = apply(&text, fix);
    }
    assert!(
        text.contains(
            "    Rect(_, _) => panic(\"not handled\"),\n    Dot => panic(\"not handled\"),\n"
        ),
        "{text}"
    );
    assert!(
        text.contains("Some(Circle(_)) => panic(\"not handled\"),"),
        "{text}"
    );
    assert!(
        text.contains("Some(Rect(_, _)) => panic(\"not handled\"),"),
        "{text}"
    );
    std::fs::write(t.path("main.la"), &text).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(["check", "main.la"])
        .current_dir(&t.0)
        .env("HOME", &t.0)
        .output()
        .unwrap();
    assert_eq!(
        out.status.code(),
        Some(0),
        "{text}\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn an_integer_match_gets_only_the_wildcard_fix() {
    let t = Temp::with(&[("main.la", MAIN)]);
    let (_, actions) = actions(&t, MAIN);
    let for_int: Vec<&Value> = actions
        .iter()
        .filter(|a| {
            a["diagnostics"][0]["message"]
                .as_str()
                .unwrap_or_default()
                .contains("other values")
        })
        .collect();
    assert_eq!(for_int.len(), 1);
    assert_eq!(for_int[0]["title"], "Add a `_` arm");
}

#[test]
fn no_actions_without_an_e0417_or_with_a_syntax_error() {
    let fine = "fun main() { println(1); }\n";
    let t = Temp::with(&[("main.la", fine)]);
    assert_eq!(actions(&t, fine).1, Vec::<Value>::new());
    let broken = MAIN.replace("let n = 3;", "let n = 3");
    let t = Temp::with(&[("main.la", &broken)]);
    let (diagnostics, actions) = actions(&t, &broken);
    assert!(diagnostics.iter().all(|d| d["code"] != "E0417"));
    assert_eq!(actions, Vec::<Value>::new());
}

/// The codes `lughac check` reports for `text` (milestone 82: a fixed
/// program must no longer report the fixed code).
fn checked_codes(t: &Temp, text: &str) -> Vec<String> {
    std::fs::write(t.path("main.la"), text).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(["check", "main.la", "--diagnostics=json"])
        .current_dir(&t.0)
        .env("HOME", &t.0)
        .output()
        .unwrap();
    String::from_utf8_lossy(&out.stderr)
        .lines()
        .filter_map(|l| serde_json::from_str::<Value>(l).ok())
        .filter_map(|d| d["code"].as_str().map(str::to_string))
        .collect()
}

/// The single fix titled `title` for `text`, applied; panics if there
/// isn't exactly one.
fn fixed(text: &str, title: &str) -> String {
    let t = Temp::with(&[("main.la", text)]);
    let (_, actions) = actions(&t, text);
    let found = titled(&actions, title);
    assert_eq!(found.len(), 1, "{title}: {actions:?}");
    assert_eq!(found[0]["isPreferred"], true);
    apply(text, found[0])
}

/// The titles of every fix offered for `text`, which must report `code`.
fn titles(text: &str, code: &str) -> Vec<String> {
    let t = Temp::with(&[("main.la", text)]);
    let (diagnostics, actions) = actions(&t, text);
    assert!(
        diagnostics.iter().any(|d| d["code"] == code),
        "{diagnostics:?}"
    );
    actions
        .iter()
        .map(|a| a["title"].as_str().unwrap().to_string())
        .collect()
}

const MUT: &str = "struct P { x: i64 }
fun main() {
    let total = 0;
    total = 5;
    let p = P { x: 1 };
    p.x = 2;
    let xs = [1, 2];
    xs[0] = 3;
    let ys = [4];
    for y of mut ys { y += 1; }
    println(total + p.x + xs[0] + ys[0]);
}
";

#[test]
fn an_immutable_let_is_made_mutable_and_the_result_checks() {
    let mut text = MUT.to_string();
    for name in ["total", "p", "xs", "ys"] {
        text = fixed(&text, &format!("Make `{name}` mutable"));
    }
    assert!(
        text.contains("let mut total = 0;") && text.contains("let mut ys = [4];"),
        "{text}"
    );
    let t = Temp::with(&[]);
    assert_eq!(checked_codes(&t, &text), Vec::<String>::new(), "{text}");
}

#[test]
fn parameters_and_loop_variables_get_no_mutable_fix() {
    let text = "fun f(n: i64) { n = 1; }
fun main() {
    for i in 0..3 { i = 2; }
    f(1);
}
";
    assert_eq!(titles(text, "E0501"), Vec::<String>::new());
}

const VARIANT: &str = "enum Shape { Circle(f64), Dot, Empty }
fun main() {
    let o: Option<Shape> = None;
    let n = match o {
        Some(Emtpy) => 0,
        Some(dot) => 1,
        Some(Circle(_)) => 2,
        None => 3,
    };
    println(n);
}
";

#[test]
fn a_misspelled_variant_is_corrected_and_the_result_checks() {
    let text = fixed(VARIANT, "Change to `Empty`");
    let text = fixed(&text, "Change to `Dot`");
    assert!(
        text.contains("Some(Empty) => 0") && text.contains("Some(Dot) => 1"),
        "{text}"
    );
    let t = Temp::with(&[]);
    assert!(
        !checked_codes(&t, &text).contains(&"W0103".to_string()),
        "{text}"
    );
}

#[test]
fn a_binding_far_from_every_variant_or_tied_gets_no_fix() {
    let text = "enum E { Aa, Bb, Zebra }
fun main() {
    let e: Option<E> = None;
    match e {
        Some(value) => println(1),
        None => {}
    }
    match e {
        Some(Ab) => println(2),
        None => {}
    }
}
";
    assert_eq!(titles(text, "W0103"), Vec::<String>::new());
}

#[test]
fn a_std_module_is_imported_after_the_last_import_or_first() {
    let alone = "fun main() {\n    println(math.sqrt(4.0));\n}\n";
    let text = fixed(alone, "Add `import std.math;`");
    assert_eq!(text, format!("import std.math;\n\n{alone}"));
    let t = Temp::with(&[]);
    assert_eq!(checked_codes(&t, &text), Vec::<String>::new(), "{text}");
    let with = "import std.env;\n\nfun main() {\n    println(env.current_dir());\n    println(math.pi());\n}\n";
    let text = fixed(with, "Add `import std.math;`");
    assert!(
        text.starts_with("import std.env;\nimport std.math;\n\nfun main"),
        "{text}"
    );
}

#[test]
fn an_unknown_module_or_a_bare_name_gets_no_import() {
    let text = "fun main() {\n    println(nope.x());\n    let m = math;\n}\n";
    assert_eq!(titles(text, "E0301"), Vec::<String>::new());
}
