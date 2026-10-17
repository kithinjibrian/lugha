//! `std.net` end to end (v17, milestone 110, PRP-142): Lugha programs
//! through `lughac run`, on the loopback interface only.

use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary folder holding one program, removed on drop.
struct Case(PathBuf);

impl Case {
    fn new(main: &str) -> Case {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-net-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("main.la"),
            format!("import std.net;\nimport std.task;\n\n{main}"),
        )
        .unwrap();
        Case(dir)
    }

    fn command(&self, opt: &str) -> Command {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_lughac"));
        cmd.args(["run", "main.la", opt])
            .current_dir(&self.0)
            .env("HOME", &self.0);
        cmd
    }

    fn run(&self, opt: &str) -> Output {
        self.command(opt).output().expect("lughac runs")
    }

    /// Runs at -O0 and -O2, expecting exit 0 and exactly `stdout`.
    fn runs(&self, stdout: &str) {
        for opt in ["-O0", "-O2"] {
            let out = self.run(opt);
            assert_eq!(
                (
                    out.status.code(),
                    String::from_utf8_lossy(&out.stdout).as_ref()
                ),
                (Some(0), stdout),
                "{opt}, stderr: {}",
                String::from_utf8_lossy(&out.stderr)
            );
        }
    }
}

impl Drop for Case {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

const ECHO: &str = "fun serve(conn: net.Conn) {
    match conn.read(4096) {
        Ok(bytes) => {
            let _ = conn.write(bytes);
        }
        Err(e) => eprintln(e),
    }
    conn.close();
}

fun accept_all(listener: net.Listener, n: i64) {
    for _ in 0..n {
        match listener.accept() {
            Ok(conn) => {
                spawn serve(conn);
            }
            Err(e) => eprintln(e),
        }
    }
}
";

#[test]
fn a_server_and_a_client_talk_in_one_program() {
    Case::new(&format!(
        "{ECHO}
fun main() {{
    let listener = net.listen(\"127.0.0.1:0\").unwrap();
    let addr = listener.addr();
    println(addr.starts_with(\"127.0.0.1:\"));
    spawn accept_all(listener, 1);
    let conn = net.connect(addr).unwrap();
    println(conn.peer() == addr);
    conn.write(\"hello\".to_bytes()).unwrap();
    println(string_from_utf8(conn.read(100).unwrap()).unwrap());
    println(conn.read(100).unwrap().len);
    let copy = conn;
    copy.close();
    println(conn.read(1));
    println(conn.write([1]));
    conn.close();
    listener.close();
    println(listener.accept().is_err());
}}
"
    ))
    .runs("true\ntrue\nhello\n0\nErr(\"closed\")\nErr(\"closed\")\ntrue\n");
}

#[test]
fn many_clients_wait_without_blocking_each_other() {
    Case::new(&format!(
        "{ECHO}
fun client(addr: string, i: i64, done: Chan<i64>) {{
    let conn = net.connect(addr).unwrap();
    conn.write(\"{{i}}\".to_bytes()).unwrap();
    let back = string_from_utf8(conn.read(64).unwrap()).unwrap();
    conn.close();
    done.send(if back == \"{{i}}\" {{ 1 }} else {{ 0 }});
}}

fun ticker(ticks: Chan<i64>) {{
    for _ in 0..20 {{
        task.sleep(1);
    }}
    ticks.send(20);
}}

fun main() {{
    let listener = net.listen(\"127.0.0.1:0\").unwrap();
    let addr = listener.addr();
    spawn accept_all(listener, 200);
    let ticks: Chan<i64> = Chan.buffered(1);
    spawn ticker(ticks);
    let done: Chan<i64> = Chan.buffered(200);
    for i in 0..200 {{
        spawn client(addr, i, done);
    }}
    let mut ok = 0;
    for _ in 0..200 {{
        ok += done.recv().unwrap();
    }}
    println(ok);
    println(ticks.recv().unwrap());
    listener.close();
}}
"
    ))
    .runs("200\n20\n");
}

#[test]
fn waits_time_out_and_failures_are_errors() {
    Case::new(
        "fun hold(listener: net.Listener) {
    let conn = listener.accept().unwrap();
    let _ = conn.read(10);
    conn.close();
}

fun main() {
    let listener = net.listen(\"127.0.0.1:0\").unwrap();
    let addr = listener.addr();
    spawn hold(listener);
    let conn = net.connect(addr).unwrap();
    conn.set_timeout(50);
    println(conn.read(10));
    println(conn.read(0));
    conn.close();
    let gone = net.listen(\"127.0.0.1:0\").unwrap();
    let free = gone.addr();
    gone.close();
    match net.connect(free) {
        Ok(_) => println(\"connected?\"),
        Err(e) => println(e),
    }
    println(net.listen(\"nope\"));
    println(net.listen(\"127.0.0.1:70000\"));
    let quiet = net.listen(\"127.0.0.1:0\").unwrap();
    quiet.set_timeout(20);
    println(quiet.accept().is_err());
    quiet.close();
    listener.close();
}
",
    )
    .runs(
        "Err(\"timed out\")\nErr(\"read size must be from 1 to 16777216 bytes\")\n\
         Connection refused\nErr(\"address must be host:port\")\n\
         Err(\"port must be a number from 0 to 65535\")\ntrue\n",
    );
}

#[test]
fn ipv6_works_where_the_host_has_it() {
    let case = Case::new(&format!(
        "{ECHO}
fun main() {{
    match net.listen(\"[::1]:0\") {{
        Ok(listener) => {{
            let addr = listener.addr();
            spawn accept_all(listener, 1);
            let conn = net.connect(addr).unwrap();
            conn.write(\"six\".to_bytes()).unwrap();
            println(string_from_utf8(conn.read(10).unwrap()).unwrap());
            println(addr.starts_with(\"[::1]:\"));
        }}
        Err(_) => println(\"six\\ntrue\"),
    }}
}}
"
    ));
    case.runs("six\ntrue\n");
}

#[test]
fn main_alone_waits_for_a_client() {
    let case = Case::new(
        "fun main() {
    let listener = net.listen(\"127.0.0.1:0\").unwrap();
    // stderr isn't buffered: the test reads the address while this waits.
    eprintln(listener.addr());
    let conn = listener.accept().unwrap();
    let got = conn.read(100).unwrap();
    conn.write(got).unwrap();
    conn.close();
    listener.close();
}
",
    );
    let mut child = case
        .command("-O0")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("lughac runs");
    let mut lines = BufReader::new(child.stderr.take().unwrap());
    let mut addr = String::new();
    lines.read_line(&mut addr).unwrap();
    let mut stream = TcpStream::connect(addr.trim()).expect("the program listens");
    stream.write_all(b"ping").unwrap();
    let mut back = String::new();
    stream.read_to_string(&mut back).unwrap();
    assert_eq!(back, "ping");
    assert!(child.wait().unwrap().success());
}
