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
