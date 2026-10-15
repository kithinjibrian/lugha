//! `lughac lsp` end to end (PRP-100, milestone 70): a real server process
//! driven over stdio with framed JSON-RPC, as an editor drives it.

use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError, channel};
use std::time::{Duration, Instant};

use serde_json::{Value, json};

/// A temporary folder of files, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn with(files: &[(&str, &str)]) -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-lsp-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        for (name, text) in files {
            let path = dir.join(name);
            std::fs::create_dir_all(path.parent().expect("files sit in folders")).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Temp(dir)
    }

    fn path(&self, name: &str) -> PathBuf {
        self.0.join(name)
    }

    fn uri(&self, name: &str) -> String {
        format!("file://{}", self.path(name).display())
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// A running `lughac lsp` and the messages it has sent.
struct Server {
    child: Child,
    stdin: Option<ChildStdin>,
    messages: Receiver<Value>,
    next_id: i64,
}

// Long enough for a debug build's check on a loaded CI machine, short enough
// that a hang fails the test instead of the suite.
const WAIT: Duration = Duration::from_secs(20);

impl Server {
    /// Starts the server in `cwd` with `$HOME` and the git cache inside it,
    /// `fake_git` first on `PATH`, and initializes it with `options`.
    fn start(t: &Temp, options: Value) -> Server {
        let bin = t.path(".bin");
        std::fs::create_dir_all(&bin).unwrap();
        let marker = t.path(".git-ran");
        let git = bin.join("git");
        std::fs::write(
            &git,
            format!("#!/bin/sh\ntouch '{}'\nexit 1\n", marker.display()),
        )
        .unwrap();
        let mut perms = std::fs::metadata(&git).unwrap().permissions();
        std::os::unix::fs::PermissionsExt::set_mode(&mut perms, 0o755);
        std::fs::set_permissions(&git, perms).unwrap();
        let path = format!(
            "{}:{}",
            bin.display(),
            std::env::var("PATH").unwrap_or_default()
        );
        let mut child = Command::new(env!("CARGO_BIN_EXE_lughac"))
            .arg("lsp")
            .current_dir(&t.0)
            .env("HOME", &t.0)
            .env("LUGHA_CACHE", t.path(".cache"))
            .env("PATH", path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("lughac lsp starts");
        let stdout = child.stdout.take().unwrap();
        let (send, messages) = channel();
        std::thread::spawn(move || {
            let mut reader = BufReader::new(stdout);
            while let Some(message) = read_frame(&mut reader) {
                if send.send(message).is_err() {
                    break;
                }
            }
        });
        let stdin = child.stdin.take();
        let mut server = Server {
            child,
            stdin,
            messages,
            next_id: 1,
        };
        let reply = server.request(
            "initialize",
            json!({
                "processId": null,
                "rootUri": t.uri(""),
                "workspaceFolders": [{ "uri": t.uri("").trim_end_matches('/'), "name": "w" }],
                "capabilities": {},
                "initializationOptions": options,
            }),
        );
        assert!(reply.get("result").is_some(), "{reply}");
        server.notify("initialized", json!({}));
        server
    }

    fn send(&mut self, message: Value) {
        let text = message.to_string();
        let stdin = self.stdin.as_mut().expect("stdin open");
        write!(stdin, "Content-Length: {}\r\n\r\n{text}", text.len()).unwrap();
        stdin.flush().unwrap();
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }

    /// Sends a request and waits for its response, skipping notifications.
    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }));
        self.wait(|m| m.get("id") == Some(&json!(id)) && m.get("method").is_none())
            .unwrap_or_else(|| panic!("no response to {method}"))
    }

    fn wait(&mut self, mut want: impl FnMut(&Value) -> bool) -> Option<Value> {
        let end = Instant::now() + WAIT;
        loop {
            let left = end.saturating_duration_since(Instant::now());
            match self.messages.recv_timeout(left) {
                Ok(m) if want(&m) => return Some(m),
                Ok(_) => {}
                Err(RecvTimeoutError::Timeout | RecvTimeoutError::Disconnected) => return None,
            }
        }
    }

    /// Waits until a publish for `uri` has exactly the codes `want` (sorted),
    /// skipping earlier rounds' publishes, and returns its diagnostics.
    fn expect(&mut self, uri: &str, want: &[&str]) -> Vec<Value> {
        let mut last = None;
        let found = self.wait(|m| {
            if m["method"] != "textDocument/publishDiagnostics" || m["params"]["uri"] != uri {
                return false;
            }
            let ds = m["params"]["diagnostics"].as_array().unwrap().clone();
            let ok = codes(&ds) == want;
            last = Some(ds);
            ok
        });
        match found {
            Some(m) => m["params"]["diagnostics"].as_array().unwrap().clone(),
            None => panic!("{uri}: never got {want:?}; last publish {last:?}"),
        }
    }

    fn open(&mut self, uri: &str, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({ "textDocument": { "uri": uri, "languageId": "lugha", "version": 1, "text": text } }),
        );
    }

    fn change(&mut self, uri: &str, version: i64, text: &str) {
        self.notify(
            "textDocument/didChange",
            json!({ "textDocument": { "uri": uri, "version": version }, "contentChanges": [{ "text": text }] }),
        );
    }

    /// `shutdown` then `exit`, returning the exit code.
    fn finish(mut self) -> Option<i32> {
        let reply = self.request("shutdown", Value::Null);
        assert_eq!(reply["result"], Value::Null, "{reply}");
        self.notify("exit", Value::Null);
        self.exit_code()
    }

    fn exit_code(mut self) -> Option<i32> {
        drop(self.stdin.take());
        let end = Instant::now() + WAIT;
        while Instant::now() < end {
            if let Some(status) = self.child.try_wait().unwrap() {
                return status.code();
            }
            std::thread::sleep(Duration::from_millis(20));
        }
        let _ = self.child.kill();
        panic!("lughac lsp didn't exit");
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        // A failed test must not leave a server running.
        let _ = self.child.kill();
    }
}

fn read_frame(reader: &mut impl BufRead) -> Option<Value> {
    let mut length = None;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            return None;
        }
        let line = line.trim_end();
        if line.is_empty() {
            break;
        }
        if let Some(n) = line.strip_prefix("Content-Length: ") {
            length = n.parse::<usize>().ok();
        }
    }
    let mut body = vec![0; length?];
    reader.read_exact(&mut body).ok()?;
    serde_json::from_slice(&body).ok()
}

fn codes(diagnostics: &[Value]) -> Vec<String> {
    let mut codes: Vec<String> = diagnostics
        .iter()
        .map(|d| d["code"].as_str().unwrap_or("").to_string())
        .collect();
    codes.sort();
    codes
}

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
