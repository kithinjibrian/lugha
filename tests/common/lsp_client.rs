//! A `lughac lsp` client for the tests: a real server process driven
//! over stdio with framed JSON-RPC, as an editor drives it (PRP-100; moved
//! here from `tests/lsp.rs` by PRP-101).

// Shared by several test crates; each uses a different subset.
#![allow(dead_code)]

use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError, channel};
use std::time::{Duration, Instant};

use serde_json::{Value, json};

/// A temporary folder of files, removed on drop.
pub struct Temp(pub PathBuf);

impl Temp {
    pub fn with(files: &[(&str, &str)]) -> Temp {
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

    pub fn path(&self, name: &str) -> PathBuf {
        self.0.join(name)
    }

    pub fn uri(&self, name: &str) -> String {
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
pub struct Server {
    pub child: Child,
    pub stdin: Option<ChildStdin>,
    pub messages: Receiver<Value>,
    pub next_id: i64,
    /// The reply to `initialize`.
    pub init: Value,
}

// Long enough for a debug build's check on a loaded CI machine, short enough
// that a hang fails the test instead of the suite.
pub const WAIT: Duration = Duration::from_secs(20);

impl Server {
    /// Starts the server in `cwd` with `$HOME` and the git cache inside it,
    /// `fake_git` first on `PATH`, and initializes it with `options`.
    pub fn start(t: &Temp, options: Value) -> Server {
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
            init: Value::Null,
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
        server.init = reply;
        server.notify("initialized", json!({}));
        server
    }

    pub fn send(&mut self, message: Value) {
        let text = message.to_string();
        let stdin = self.stdin.as_mut().expect("stdin open");
        write!(stdin, "Content-Length: {}\r\n\r\n{text}", text.len()).unwrap();
        stdin.flush().unwrap();
    }

    pub fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }

    /// Sends a request and waits for its response, skipping notifications.
    pub fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params }));
        self.wait(|m| m.get("id") == Some(&json!(id)) && m.get("method").is_none())
            .unwrap_or_else(|| panic!("no response to {method}"))
    }

    pub fn wait(&mut self, mut want: impl FnMut(&Value) -> bool) -> Option<Value> {
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
    pub fn expect(&mut self, uri: &str, want: &[&str]) -> Vec<Value> {
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

    pub fn open(&mut self, uri: &str, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({ "textDocument": { "uri": uri, "languageId": "lugha", "version": 1, "text": text } }),
        );
    }

    pub fn change(&mut self, uri: &str, version: i64, text: &str) {
        self.notify(
            "textDocument/didChange",
            json!({ "textDocument": { "uri": uri, "version": version }, "contentChanges": [{ "text": text }] }),
        );
    }

    /// Hovers at a zero-based `line` and UTF-16 `character` of `uri`;
    /// returns the result (`null` for nothing).
    pub fn hover(&mut self, uri: &str, line: u32, character: u32) -> Value {
        let reply = self.request(
            "textDocument/hover",
            json!({ "textDocument": { "uri": uri }, "position": { "line": line, "character": character } }),
        );
        assert!(reply.get("error").is_none(), "{reply}");
        reply["result"].clone()
    }

    /// Asks for the definition at a zero-based `line` and UTF-16
    /// `character` of `uri`; returns the result (`null` for none).
    pub fn definition(&mut self, uri: &str, line: u32, character: u32) -> Value {
        let reply = self.request(
            "textDocument/definition",
            json!({ "textDocument": { "uri": uri }, "position": { "line": line, "character": character } }),
        );
        assert!(reply.get("error").is_none(), "{reply}");
        reply["result"].clone()
    }

    /// `shutdown` then `exit`, returning the exit code.
    pub fn finish(mut self) -> Option<i32> {
        let reply = self.request("shutdown", Value::Null);
        assert_eq!(reply["result"], Value::Null, "{reply}");
        self.notify("exit", Value::Null);
        self.exit_code()
    }

    pub fn exit_code(mut self) -> Option<i32> {
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

pub fn read_frame(reader: &mut impl BufRead) -> Option<Value> {
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

pub fn codes(diagnostics: &[Value]) -> Vec<String> {
    let mut codes: Vec<String> = diagnostics
        .iter()
        .map(|d| d["code"].as_str().unwrap_or("").to_string())
        .collect();
    codes.sort();
    codes
}
