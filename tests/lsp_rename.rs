//! Rename in `lughac lsp` end to end (PRP-113, milestone 83): every place
//! renamed in one edit, self-checked, or refused with the reason.

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp};

const MAIN: &str = "import geo;
import std.math;

struct P { x: i64 }
fun main() {
    let a = 1;
    let b = 2;
    let p = P { x: a };
    println(p.x + b);
    println(a);
    println(geo.double(b));
    println(math.sqrt(4.0));
}
";

const GEO: &str = "fun double(n: i64): i64 = n * 2;\nfun quad(n: i64): i64 = double(double(n));\n";

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

/// A server with `files` on disk, the first one open and checked.
fn opened(t: &Temp, files: &[(&str, &str)]) -> (Server, String) {
    let uri = t.uri(files[0].0);
    let mut server = Server::start(t, json!({}));
    server.open(&uri, files[0].1);
    server.wait(|m| m["method"] == "textDocument/publishDiagnostics" && m["params"]["uri"] == uri);
    (server, uri)
}

fn rename(s: &mut Server, uri: &str, at: (u32, u32), name: &str) -> Value {
    s.request(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": uri },
            "position": { "line": at.0, "character": at.1 },
            "newName": name,
        }),
    )
}

/// The texts after applying a rename's edits: by file name.
fn applied(t: &Temp, reply: &Value) -> Vec<(String, String)> {
    let changes = reply["result"]["changes"]
        .as_object()
        .unwrap_or_else(|| panic!("{reply}"));
    let mut out = Vec::new();
    for (uri, edits) in changes {
        let name = uri.rsplit('/').next().unwrap().to_string();
        let mut text = std::fs::read_to_string(t.path(&name)).unwrap();
        let mut edits: Vec<&Value> = edits.as_array().unwrap().iter().collect();
        // From the end, so earlier offsets stay valid; one line each.
        edits.sort_by_key(|e| {
            std::cmp::Reverse((
                e["range"]["start"]["line"].as_u64(),
                e["range"]["start"]["character"].as_u64(),
            ))
        });
        for e in edits {
            let offset = |p: &Value| {
                let line = p["line"].as_u64().unwrap() as usize;
                let col = p["character"].as_u64().unwrap() as usize;
                text.split_inclusive('\n')
                    .take(line)
                    .map(str::len)
                    .sum::<usize>()
                    + col
            };
            let (from, to) = (offset(&e["range"]["start"]), offset(&e["range"]["end"]));
            text.replace_range(from..to, e["newText"].as_str().unwrap());
        }
        out.push((name, text));
    }
    out.sort();
    out
}

/// The refusal message of a rename, which must be an error.
fn refused(reply: &Value) -> String {
    assert!(reply.get("result").is_none_or(Value::is_null), "{reply}");
    reply["error"]["message"]
        .as_str()
        .unwrap_or_else(|| panic!("{reply}"))
        .to_string()
}

fn checks_clean(t: &Temp, texts: &[(String, String)]) {
    for (name, text) in texts {
        std::fs::write(t.path(name), text).unwrap();
    }
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(["check", "main.la"])
        .current_dir(&t.0)
        .env("HOME", &t.0)
        .output()
        .unwrap();
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn the_server_offers_rename_with_prepare() {
    let t = Temp::with(&[]);
    let s = Server::start(&t, json!({}));
    assert_eq!(
        s.init["result"]["capabilities"]["renameProvider"],
        json!({ "prepareProvider": true })
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_local_is_renamed_everywhere_and_nothing_is_written() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t, &[("main.la", MAIN), ("geo.la", GEO)]);
    let reply = rename(&mut s, &uri, pos(MAIN, "let a", "a"), "first");
    let texts = applied(&t, &reply);
    assert_eq!(s.finish(), Some(0));
    assert_eq!(
        std::fs::read_to_string(t.path("main.la")).unwrap(),
        MAIN,
        "the server wrote"
    );
    assert_eq!(texts.len(), 1);
    let text = &texts[0].1;
    assert!(
        text.contains("let first = 1;")
            && text.contains("P { x: first }")
            && text.contains("println(first);"),
        "{text}"
    );
    checks_clean(&t, &texts);
}

#[test]
fn a_function_and_a_field_are_renamed_across_files() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t, &[("main.la", MAIN), ("geo.la", GEO)]);
    let reply = rename(&mut s, &uri, pos(MAIN, "geo.double", "double"), "twice");
    let texts = applied(&t, &reply);
    assert_eq!(texts.len(), 2, "{reply}");
    assert_eq!(
        texts[0].1,
        "fun twice(n: i64): i64 = n * 2;\nfun quad(n: i64): i64 = twice(twice(n));\n"
    );
    assert!(texts[1].1.contains("geo.twice(b)"));
    let reply = rename(&mut s, &uri, pos(MAIN, "{ x: i64", "x"), "y");
    let fields = applied(&t, &reply);
    assert!(
        fields[0].1.contains("struct P { y: i64 }")
            && fields[0].1.contains("P { y: a }")
            && fields[0].1.contains("p.y + b")
    );
    assert_eq!(s.finish(), Some(0));
    checks_clean(&t, &texts);
}

#[test]
fn bad_names_the_prelude_and_modules_are_refused() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t, &[("main.la", MAIN), ("geo.la", GEO)]);
    let a = pos(MAIN, "let a", "a");
    assert!(refused(&rename(&mut s, &uri, a, "while")).contains("keyword"));
    assert!(refused(&rename(&mut s, &uri, a, "1abc")).contains("isn't a name"));
    let sqrt = pos(MAIN, "math.sqrt", "sqrt");
    assert!(refused(&rename(&mut s, &uri, sqrt, "root")).contains("std or the prelude"));
    // An intrinsic has no declaration to rename.
    let println = pos(MAIN, "println(a)", "println");
    assert!(refused(&rename(&mut s, &uri, println, "say")).contains("nothing to rename"));
    let module = pos(MAIN, "geo.double", "geo");
    assert!(refused(&rename(&mut s, &uri, module, "g")).contains("module"));
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_clash_or_a_capture_is_refused_by_the_self_check() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t, &[("main.la", MAIN), ("geo.la", GEO)]);
    // `b` → `a`: the later uses of `a` would now mean the new `a`.
    let reply = rename(&mut s, &uri, pos(MAIN, "let b", "b"), "a");
    assert!(
        refused(&reply).contains("would make a use point elsewhere"),
        "{reply}"
    );
    // `double` → `quad`: two functions named alike in geo.la.
    let reply = rename(&mut s, &uri, pos(MAIN, "geo.double", "double"), "quad");
    assert!(
        refused(&reply).contains("would add an error in geo.la"),
        "{reply}"
    );
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn a_public_function_of_a_module_no_checked_main_imports_is_refused() {
    let lib = "fun helper(): i64 = 1;\nfun other(): i64 = helper();\n";
    let t = Temp::with(&[("lib.la", lib)]);
    let (mut s, uri) = opened(&t, &[("lib.la", lib)]);
    let reply = rename(&mut s, &uri, pos(lib, "fun helper", "helper"), "assist");
    assert!(refused(&reply).contains("main file"), "{reply}");
    // A `private` one is the file's own: renamed.
    let private = "private fun helper(): i64 = 1;\nfun other(): i64 = helper();\n";
    s.change(&uri, 2, private);
    s.wait(|m| m["method"] == "textDocument/publishDiagnostics");
    let reply = rename(&mut s, &uri, pos(private, "fun helper", "helper"), "assist");
    assert!(reply["result"]["changes"].is_object(), "{reply}");
    assert_eq!(s.finish(), Some(0));
}

#[test]
fn prepare_answers_the_names_range_or_the_refusal() {
    let t = Temp::with(&[("main.la", MAIN), ("geo.la", GEO)]);
    let (mut s, uri) = opened(&t, &[("main.la", MAIN), ("geo.la", GEO)]);
    let (line, col) = pos(MAIN, "println(a)", "a)");
    let reply = s.request(
        "textDocument/prepareRename",
        json!({ "textDocument": { "uri": uri }, "position": { "line": line, "character": col } }),
    );
    assert_eq!(
        reply["result"],
        json!({ "start": { "line": line, "character": col }, "end": { "line": line, "character": col + 1 } })
    );
    let (line, col) = pos(MAIN, "geo.double", "geo");
    let reply = s.request(
        "textDocument/prepareRename",
        json!({ "textDocument": { "uri": uri }, "position": { "line": line, "character": col } }),
    );
    assert!(refused(&reply).contains("module"));
    let reply = s.request(
        "textDocument/prepareRename",
        json!({ "textDocument": { "uri": uri }, "position": { "line": 1, "character": 0 } }),
    );
    assert!(refused(&reply).contains("nothing to rename"));
    assert_eq!(s.finish(), Some(0));
}
