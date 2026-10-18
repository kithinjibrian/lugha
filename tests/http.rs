//! `std.http` end to end (v17, milestone 111, PRP-143): a Lugha server
//! sent raw requests — good, malformed and hostile — from Rust, and the Lugha
//! client against a raw Rust server. Loopback only.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

/// A temporary folder holding one program, removed on drop.
struct Case(PathBuf);

impl Case {
    fn new(main: &str) -> Case {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-http-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("main.la"), main).unwrap();
        Case(dir)
    }

    fn command(&self) -> Command {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_lughac"));
        cmd.args(["run", "main.la"])
            .current_dir(&self.0)
            .env("HOME", &self.0);
        cmd
    }
}

impl Drop for Case {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// A port nothing listens on right now.
fn free_port() -> u16 {
    TcpListener::bind("127.0.0.1:0")
        .unwrap()
        .local_addr()
        .unwrap()
        .port()
}

const HANDLER: &str = "import std.http;

fun handle(req: http.Request): http.Response {
    match (req.method, req.path) {
        (\"GET\", \"/\") => http.text(200, \"hello {req.query}\"),
        (\"POST\", \"/echo\") => http.bytes(200, req.body),
        (\"GET\", \"/inject\") => http.Response {
            status: 200,
            headers: #{\"x-bad\": \"a\\r\\nset-cookie: x\", \"x-good\": \"ok\"},
            body: [],
        },
        _ => http.text(404, \"not found\"),
    }
}
";

/// A Lugha server on `port` (timeouts of 500 ms), killed on drop.
struct Server(Child, #[allow(dead_code)] Case, u16);

impl Server {
    fn start() -> Server {
        let port = free_port();
        let case = Case::new(&format!(
            "{HANDLER}
fun main() {{
    let mut limits = http.limits();
    limits.timeout_ms = 500;
    http.serve_with(\"127.0.0.1:{port}\", limits, handle).unwrap();
}}
"
        ));
        let child = case
            .command()
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        // The program compiles first: wait until it listens.
        let start = Instant::now();
        while TcpStream::connect(("127.0.0.1", port)).is_err() {
            assert!(
                start.elapsed() < Duration::from_secs(60),
                "the server never listened"
            );
            std::thread::sleep(Duration::from_millis(50));
        }
        Server(child, case, port)
    }

    /// Sends `raw` and reads until the server closes.
    fn exchange(&self, raw: &[u8]) -> String {
        let mut s = TcpStream::connect(("127.0.0.1", self.2)).unwrap();
        s.set_read_timeout(Some(Duration::from_secs(10))).unwrap();
        s.write_all(raw).unwrap();
        let mut out = Vec::new();
        // A reset after the reply is fine: what arrived is what counts.
        let _ = s.read_to_end(&mut out);
        String::from_utf8_lossy(&out).into_owned()
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        // Best effort: the server runs forever otherwise.
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn status(reply: &str) -> &str {
    reply.get(9..12).unwrap_or("")
}

#[test]
fn the_server_answers_keeps_alive_and_reads_every_framing() {
    let server = Server::start();
    let two = server.exchange(
        b"GET /?a=1 HTTP/1.1\r\nHost: x\r\n\r\nPOST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\nConnection: close\r\n\r\nping",
    );
    assert!(two.starts_with("HTTP/1.1 200 OK\r\n"), "{two}");
    assert!(two.contains("hello a=1"), "{two}");
    assert!(two.ends_with("connection: close\r\n\r\nping"), "{two}");
    let chunked = server.exchange(
        b"POST /echo HTTP/1.1\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n3;x=y\r\nabc\r\n2\r\nde\r\n0\r\nTrailer: t\r\n\r\n",
    );
    assert!(chunked.ends_with("abcde"), "{chunked}");
    let old = server.exchange(b"GET / HTTP/1.0\r\n\r\n");
    assert!(old.contains("connection: close"), "{old}");
    let expect = server.exchange(
        b"POST /echo HTTP/1.1\r\nExpect: 100-continue\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok",
    );
    assert!(
        expect.starts_with("HTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK"),
        "{expect}"
    );
    let inject = server.exchange(b"GET /inject HTTP/1.1\r\nConnection: close\r\n\r\n");
    assert!(
        inject.contains("x-good: ok") && !inject.contains("set-cookie"),
        "{inject}"
    );
    let missing = server.exchange(b"GET /nope HTTP/1.1\r\nConnection: close\r\n\r\n");
    assert_eq!(status(&missing), "404");
}

#[test]
fn hostile_requests_get_their_status_and_a_closed_connection() {
    let server = Server::start();
    let long_line = format!("GET /{} HTTP/1.1\r\n\r\n", "a".repeat(9000));
    let big_header = format!("GET / HTTP/1.1\r\nX: {}\r\n\r\n", "a".repeat(9000));
    let many: String = (0..101).map(|i| format!("X{i}: y\r\n")).collect();
    let many = format!("GET / HTTP/1.1\r\n{many}\r\n");
    for (raw, want) in [
        (long_line.as_bytes(), "414"),
        (big_header.as_bytes(), "431"),
        (many.as_bytes(), "431"),
        (&b"POST /echo HTTP/1.1\r\nContent-Length: 2000000\r\n\r\n"[..], "413"),
        (&b"GET\r\n\r\n"[..], "400"),
        (&b"GET / HTTP/1.1\r\nX: a\rb\r\n\r\n"[..], "400"),
        (&b"POST /echo HTTP/1.1\r\nContent-Length: 1\r\nContent-Length: 2\r\n\r\nab"[..], "400"),
        (
            &b"POST /echo HTTP/1.1\r\nContent-Length: 3\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n"[..],
            "400",
        ),
        (&b"POST /echo HTTP/1.1\r\nTransfer-Encoding: gzip\r\n\r\n"[..], "501"),
        (&b"GET / HTTP/2.0\r\n\r\n"[..], "505"),
        (&b"GET / HTTP/1.1\r\nHost: slow"[..], "408"),
    ] {
        let reply = server.exchange(raw);
        assert_eq!(status(&reply), want, "{reply}");
        assert!(reply.contains("connection: close"), "{reply}");
    }
}

#[test]
fn the_client_reads_every_framing_and_reports_failures() {
    let raw = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = raw.local_addr().unwrap().port();
    let replies: [&[u8]; 3] = [
        b"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nwiki\r\n5\r\npedia\r\n0\r\n\r\n",
        b"HTTP/1.1 201 Created\r\nX-Thing: yes\r\n\r\nto the end",
        b"HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\n",
    ];
    let serving = std::thread::spawn(move || {
        for reply in replies {
            let (mut s, _) = raw.accept().unwrap();
            let request = read_request(&mut s);
            assert!(request.contains("host: 127.0.0.1"), "{request}");
            s.write_all(reply).unwrap();
        }
    });
    let refused = free_port();
    let case = Case::new(&format!(
        "import std.http;

fun main() {{
    let a = http.get(\"http://127.0.0.1:{port}/x\").unwrap();
    println(\"{{a.status}} {{a.text().unwrap()}}\");
    let b = http.post(\"http://127.0.0.1:{port}/\", \"text/plain\", \"hi\".to_bytes()).unwrap();
    println(\"{{b.status}} {{b.text().unwrap()}} {{b.header(\"x-thing\")}}\");
    let mut small = http.limits();
    small.body = 10;
    println(http.request_with(\"GET\", \"http://127.0.0.1:{port}/\", #{{}}, [], small).is_err());
    println(http.get(\"https://127.0.0.1:{refused}/\").is_err());
    println(http.get(\"ftp://x\"));
    println(http.get(\"http:///\"));
    match http.get(\"http://127.0.0.1:{refused}/\") {{
        Ok(_) => println(\"ok?\"),
        Err(e) => println(e),
    }}
}}
"
    ));
    let out = case.command().output().unwrap();
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "200 wikipedia\n201 to the end Some(\"yes\")\ntrue\ntrue\n\
         Err(\"bad URL\")\nErr(\"bad URL\")\nConnection refused\n",
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    serving.join().unwrap();
}

#[test]
fn an_address_is_connected_to_with_the_url_host() {
    let raw = TcpListener::bind("127.0.0.1:0").unwrap();
    let port = raw.local_addr().unwrap().port();
    let serving = std::thread::spawn(move || {
        let (mut s, _) = raw.accept().unwrap();
        let request = read_request(&mut s);
        s.write_all(b"HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok")
            .unwrap();
        request
    });
    let case = Case::new(&format!(
        "import std.http;
import std.tls;

fun main() {{
    let opts = tls.Options {{ address: \"127.0.0.1\", ..tls.options() }};
    let res = http.request_tls_with(\"GET\", \"http://wrong.example:{port}/x\", #{{}}, [], http.limits(), opts).unwrap();
    println(\"{{res.status}} {{res.text().unwrap()}}\");
}}
"
    ));
    let out = case.command().output().unwrap();
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "200 ok\n",
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let request = serving.join().unwrap();
    assert!(
        request.contains(&format!("host: wrong.example:{port}\r\n")),
        "{request}"
    );
}

/// A whole request: its head, and the body its `content-length` gives, so
/// closing afterwards never resets the connection under unread bytes.
fn read_request(s: &mut TcpStream) -> String {
    let mut got = Vec::new();
    let mut buf = [0u8; 4096];
    loop {
        let text = String::from_utf8_lossy(&got).into_owned();
        if let Some(end) = text.find("\r\n\r\n") {
            let length = text
                .lines()
                .find_map(|l| l.strip_prefix("content-length: "))
                .and_then(|n| n.trim().parse::<usize>().ok())
                .unwrap_or(0);
            if got.len() >= end + 4 + length {
                return text;
            }
        }
        let n = s.read(&mut buf).unwrap();
        if n == 0 {
            return String::from_utf8_lossy(&got).into_owned();
        }
        got.extend_from_slice(&buf[..n]);
    }
}
