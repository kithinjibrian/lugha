//! `lughac lsp` end to end (PRP-100, milestone 70): live diagnostics from
//! a real server process (the client is `common/lsp_client.rs`).

use std::io::{BufReader, Write};
use std::process::{Command, Stdio};

use serde_json::{Value, json};

#[path = "common/lsp_client.rs"]
mod lsp_client;

use lsp_client::{Server, Temp, read_frame};

const BAD_MAIN: &str = "fun main(): i32 { let s = \"é\"; return s; }\n";
const GOOD_MAIN: &str = "fun main(): i32 { return 0; }\n";
// The code `other.la`'s own error gets: adding a bool to an i32.
const OTHER_CODE: &str = "E0404";

#[test]
fn initialize_offers_full_sync_and_utf16_and_shuts_down_cleanly() {
    let t = Temp::with(&[]);
    let mut server = Server::start(&t, json!({}));
    let reply = server.request("shutdown", Value::Null);
    assert_eq!(reply["result"], Value::Null);
    server.notify("exit", Value::Null);
    assert_eq!(server.exit_code(), Some(0));
}

#[test]
fn a_second_initialize_is_an_error_and_the_server_carries_on() {
    let t = Temp::with(&[]);
    let mut server = Server::start(&t, json!({}));
    let reply = server.request(
        "initialize",
        json!({ "processId": null, "capabilities": {} }),
    );
    assert!(reply.get("error").is_some(), "{reply}");
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn capabilities_are_full_text_sync_in_utf16() {
    let t = Temp::with(&[]);
    let mut child = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .arg("lsp")
        .current_dir(&t.0)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let body = json!({ "jsonrpc": "2.0", "id": 1, "method": "initialize",
        "params": { "processId": null, "rootUri": null, "capabilities": {} } })
    .to_string();
    let mut stdin = child.stdin.take().unwrap();
    write!(stdin, "Content-Length: {}\r\n\r\n{body}", body.len()).unwrap();
    let mut reader = BufReader::new(child.stdout.take().unwrap());
    let reply = read_frame(&mut reader).expect("a reply");
    let caps = &reply["result"]["capabilities"];
    assert_eq!(caps["textDocumentSync"]["change"], 1, "{reply}");
    assert_eq!(caps["textDocumentSync"]["openClose"], true, "{reply}");
    assert_eq!(caps["positionEncoding"], "utf-16", "{reply}");
    assert_eq!(reply["result"]["serverInfo"]["name"], "lughac", "{reply}");
    drop(stdin);
    // Done with it: the server isn't asked to exit cleanly here.
    let _ = child.kill();
    let _ = child.wait();
}

#[test]
fn exit_without_shutdown_is_exit_1() {
    let t = Temp::with(&[]);
    let mut server = Server::start(&t, json!({}));
    server.notify("exit", Value::Null);
    assert_eq!(server.exit_code(), Some(1));
}

#[test]
fn errors_appear_as_you_type_and_clear_when_fixed() {
    let t = Temp::with(&[("main.la", GOOD_MAIN)]);
    let uri = t.uri("main.la");
    let mut server = Server::start(&t, json!({}));
    server.open(&uri, GOOD_MAIN);
    server.expect(&uri, &[]);
    server.change(&uri, 2, BAD_MAIN);
    let ds = server.expect(&uri, &["E0403"]);
    let d = &ds[0];
    assert_eq!(d["severity"], 1);
    assert_eq!(d["source"], "lughac");
    assert_eq!(d["message"], "expected i32, found string\nexpected i32");
    // `return s;`: `s` is byte 39 but UTF-16 character 38, after the `é`.
    assert_eq!(d["range"]["start"], json!({ "line": 0, "character": 38 }));
    assert_eq!(d["range"]["end"], json!({ "line": 0, "character": 39 }));
    server.change(&uri, 3, GOOD_MAIN);
    server.expect(&uri, &[]);
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn help_and_related_labels_are_kept() {
    let text = "fun main() { let x: i32 = 1; let y: i32 = x + 2.5; }\n";
    let t = Temp::with(&[("main.la", text)]);
    let uri = t.uri("main.la");
    let mut server = Server::start(&t, json!({}));
    server.open(&uri, text);
    let ds = server.expect(&uri, &["E0401"]);
    let related = ds[0]["relatedInformation"].as_array().unwrap();
    assert_eq!(related.len(), 1, "{ds:?}");
    assert_eq!(related[0]["location"]["uri"], uri);
    assert_eq!(related[0]["message"], "this operand is i32");
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn unsaved_text_wins_over_the_disk() {
    let t = Temp::with(&[
        (
            "main.la",
            "import geo;\nfun main(): i32 { return geo.f(); }\n",
        ),
        ("geo.la", "fun f(): i32 { return \"x\"; }\n"),
    ]);
    let (main, geo) = (t.uri("main.la"), t.uri("geo.la"));
    let mut server = Server::start(&t, json!({}));
    // The disk version of main.la is fine; geo.la on disk has the error.
    server.open(&main, "import geo;\nfun main(): i32 { return geo.f(); }\n");
    server.expect(&geo, &["E0403"]);
    // Fixing geo.la without saving fixes the program.
    server.open(&geo, "fun f(): i32 { return \"x\"; }\n");
    server.change(&geo, 2, "fun f(): i32 { return 1; }\n");
    server.expect(&geo, &[]);
    // And an unsaved main.la with an error shows it, though the disk has none.
    server.change(&main, 2, "import geo;\nfun main(): i32 { return true; }\n");
    server.expect(&main, &["E0403"]);
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn a_module_is_checked_through_a_main_already_seen() {
    let main_text = "import util;\nfun main(): i32 { return util.f(); }\n";
    let t = Temp::with(&[
        ("app/main.la", main_text),
        ("app/util.la", "fun f(): i32 { return 1; }\n"),
    ]);
    let (main, util) = (t.uri("app/main.la"), t.uri("app/util.la"));
    let mut server = Server::start(&t, json!({}));
    server.open(&main, main_text);
    server.expect(&main, &[]);
    server.open(&util, "fun f(): i32 { return 1; }\n");
    server.change(&util, 2, "fun f(): i32 { return \"x\"; }\n");
    // Through main.la: the type error, and no E0303.
    server.expect(&util, &["E0403"]);
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn a_module_alone_hides_e0303() {
    let text = "fun f(): i32 { return \"x\"; }\n";
    let t = Temp::with(&[("lib/util.la", text)]);
    let uri = t.uri("lib/util.la");
    let mut server = Server::start(&t, json!({}));
    server.open(&uri, text);
    server.expect(&uri, &["E0403"]);
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn entries_come_from_initialization_options_and_configuration() {
    let t = Temp::with(&[
        (
            "src/main.la",
            "import geo;\nfun main(): i32 { return geo.f(); }\n",
        ),
        (
            "src/other.la",
            "import geo;\nfun main() { println(geo.f() + true); }\n",
        ),
        ("src/geo.la", "fun f(): i32 { return 1; }\n"),
    ]);
    let geo = t.uri("src/geo.la");
    let other = t.uri("src/other.la");
    let mut server = Server::start(&t, json!({ "entries": ["src/other.la"] }));
    server.open(&geo, "fun f(): i32 { return 1; }\n");
    // Checked through the entry: its own error shows in other.la.
    server.expect(&other, &[OTHER_CODE]);
    // Replacing the entries drops other.la's check on the next round.
    server.notify(
        "workspace/didChangeConfiguration",
        json!({ "settings": { "lugha": { "entries": ["src/main.la"] } } }),
    );
    server.change(&geo, 2, "fun f(): i32 { return \"x\"; }\n");
    server.expect(&geo, &["E0403"]);
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn checks_are_offline_and_write_nothing() {
    let text = "import geo;\nfun main() { println(geo.name()); }\n";
    let t = Temp::with(&[
        ("main.la", text),
        (
            "lugha.toml",
            "[packages]\ngeo = { git = \"https://example.com/geo.git\", version = \"1\" }\n",
        ),
    ]);
    let uri = t.uri("main.la");
    let mut server = Server::start(&t, json!({}));
    server.open(&uri, text);
    // Reported at the manifest's entry, as `lughac check` reports it.
    server.expect(&t.uri("lugha.toml"), &["E0325"]);
    assert_eq!(server.finish(), Some(0));
    assert!(!t.path(".git-ran").exists(), "git ran");
    assert!(!t.path("lugha.lock").exists(), "a lock was written");
}

#[test]
fn an_unknown_request_is_method_not_found_and_the_server_carries_on() {
    let t = Temp::with(&[]);
    let mut server = Server::start(&t, json!({}));
    let reply = server.request("textDocument/frobnicate", json!({}));
    assert_eq!(reply["error"]["code"], -32601, "{reply}");
    // Still answering.
    let again = server.request("textDocument/frobnicate", json!({}));
    assert_eq!(again["error"]["code"], -32601);
    assert_eq!(server.finish(), Some(0));
}

#[test]
fn garbage_on_stdin_ends_the_server_with_exit_2() {
    let t = Temp::with(&[]);
    let mut server = Server::start(&t, json!({}));
    let stdin = server.stdin.as_mut().unwrap();
    write!(stdin, "Content-Length: 5\r\n\r\nnope!").unwrap();
    stdin.flush().unwrap();
    assert_eq!(server.exit_code(), Some(2));
}

#[test]
fn documents_that_arent_files_are_ignored() {
    let t = Temp::with(&[("main.la", BAD_MAIN)]);
    let uri = t.uri("main.la");
    let mut server = Server::start(&t, json!({}));
    server.open("untitled:Untitled-1", BAD_MAIN);
    server.open(&uri, BAD_MAIN);
    // The first publish is for the file: nothing came for the untitled one.
    let first = server
        .wait(|m| m["method"] == "textDocument/publishDiagnostics")
        .expect("a publish");
    assert_eq!(first["params"]["uri"], uri, "{first}");
    assert_eq!(server.finish(), Some(0));
}
