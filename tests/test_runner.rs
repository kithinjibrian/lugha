//! `lughac test` end to end (PRP-114, milestone 84): tests found in files
//! and folders, each run in its own process, failures isolated, timeouts,
//! `--filter`, captured output, compile errors and exit codes.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// An empty temporary folder, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn with(files: &[(&str, &str)]) -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-test-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        for (name, text) in files {
            let path = dir.join(name);
            std::fs::create_dir_all(path.parent().expect("a parent")).expect("writable");
            std::fs::write(path, text).expect("writable");
        }
        std::fs::create_dir_all(&dir).expect("writable");
        Temp(dir)
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn lughac(cwd: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(cwd)
        .env("HOME", cwd)
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

const GEO: &str = "fun area(r: f64): f64 = 3.0 * r * r;
private fun double(n: i64): i64 = n * 2;

@test
fun area_of_unit() {
    assert(area(1.0) > 2.0);
    assert_eq(area(0.0), 0.0);
}

@test
fun doubles() {
    println(\"doubling 2\");
    assert_eq(double(2), 5);
}
";

#[test]
fn passing_and_failing_tests_report_in_order_with_captured_output() {
    let t = Temp::with(&[("geo.la", GEO)]);
    let out = lughac(&t.0, &["test"]);
    assert_eq!(out.status.code(), Some(1), "{}", text(&out.stderr));
    let stdout = text(&out.stdout);
    assert!(
        stdout.starts_with("test geo.area_of_unit ... ok\ntest geo.doubles ... FAILED\n"),
        "{stdout}"
    );
    assert!(
        stdout.contains("panic: assert_eq failed: left 4, right 5 at geo.la:13:5"),
        "{stdout}"
    );
    assert!(
        stdout.contains("--- output ---\n    doubling 2"),
        "{stdout}"
    );
    assert!(
        stdout.ends_with("2 tests: 1 passed, 1 failed\n"),
        "{stdout}"
    );
}

#[test]
fn each_failure_stays_in_its_test_and_an_endless_loop_times_out() {
    let src = "@test fun overflows() { let x: i64 = 9223372036854775807; let y = x + 1; }
@test fun recurses() { recurse(1); }
fun recurse(n: i64): i64 = recurse(n + 1) + 1;
@test fun out_of_bounds() { let xs = [1]; let i = 3; println(xs[i]); }
@test fun fine() { assert_ne(\"a\", \"b\"); }
";
    // Only the endless loop gets a short limit: a loaded machine may be slow.
    let looping = "@test fun loops() { while true {} }\n";
    let t = Temp::with(&[("many.la", src), ("looping.la", looping)]);
    let out = lughac(&t.0, &["test", "many.la"]);
    assert_eq!(out.status.code(), Some(1));
    let stdout = text(&out.stdout);
    for name in ["overflows", "recurses", "out_of_bounds"] {
        assert!(
            stdout.contains(&format!("test many.{name} ... FAILED\n")),
            "{stdout}"
        );
    }
    assert!(stdout.contains("test many.fine ... ok"), "{stdout}");
    assert!(
        stdout.contains("integer overflow") && stdout.contains("index out of bounds"),
        "{stdout}"
    );
    assert!(
        stdout.ends_with("4 tests: 1 passed, 3 failed\n"),
        "{stdout}"
    );
    let out = lughac(&t.0, &["test", "--timeout", "1", "looping.la"]);
    assert_eq!(out.status.code(), Some(1));
    assert!(text(&out.stdout).contains("test looping.loops ... FAILED (timed out after 1 s)"));
}

#[test]
fn filter_runs_matching_tests_and_folders_include_imports() {
    let t = Temp::with(&[
        ("geo.la", GEO),
        (
            "use_geo.la",
            "import geo;\n@test fun area_from_outside() { assert(geo.area(2.0) == 12.0); }\n",
        ),
        ("notes/readme.txt", "@test"),
    ]);
    let out = lughac(&t.0, &["test", "--filter", "area"]);
    let stdout = text(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}{}", text(&out.stderr));
    assert_eq!(
        stdout,
        "test geo.area_of_unit ... ok\ntest use_geo.area_from_outside ... ok\n\n2 tests: 2 passed, 0 failed\n"
    );
}

#[test]
fn a_file_that_does_not_compile_is_reported_and_the_rest_still_run() {
    let t = Temp::with(&[
        ("bad.la", "@test fun broken() { let x: i64 = \"no\"; }\n"),
        ("good.la", "@test fun fine() {}\n"),
    ]);
    let out = lughac(&t.0, &["test"]);
    assert_eq!(out.status.code(), Some(1));
    assert!(
        text(&out.stderr).contains("error[E0403]"),
        "{}",
        text(&out.stderr)
    );
    assert!(text(&out.stdout).contains("test good.fine ... ok"));
    assert!(
        text(&out.stdout).ends_with("1 test: 1 passed, 0 failed\n"),
        "{}",
        text(&out.stdout)
    );
}

#[test]
fn no_tests_is_success_and_build_leaves_tests_out() {
    let t = Temp::with(&[(
        "main.la",
        "fun main() { println(1); }\n@test fun t() { assert(false); }\n",
    )]);
    let run = lughac(&t.0, &["run", "main.la"]);
    assert_eq!(
        (run.status.code(), text(&run.stdout)),
        (Some(0), "1\n".to_string())
    );
    let none = Temp::with(&[("main.la", "fun main() {}\n")]);
    let out = lughac(&none.0, &["test"]);
    assert_eq!(out.status.code(), Some(0));
    assert_eq!(text(&out.stdout), "0 tests\n");
}

#[test]
fn list_shows_every_test_and_where_without_building() {
    // A type error doesn't stop listing: nothing is checked or built.
    let broken = "@test\nfun wrong() { let x: i64 = \"no\"; }\n";
    let t = Temp::with(&[("geo.la", GEO), ("broken.la", broken)]);
    let out = lughac(&t.0, &["test", "--list"]);
    assert_eq!(out.status.code(), Some(0), "{}", text(&out.stderr));
    assert_eq!(
        text(&out.stdout),
        "test broken.wrong broken.la:2:5\ntest geo.area_of_unit geo.la:5:5\ntest geo.doubles geo.la:11:5\n"
    );
    let out = lughac(&t.0, &["test", "--list", "--json", "geo.la"]);
    assert_eq!(
        text(&out.stdout),
        "{\"test\":\"geo.area_of_unit\",\"file\":\"geo.la\",\"line\":5,\"col\":5}\n\
         {\"test\":\"geo.doubles\",\"file\":\"geo.la\",\"line\":11,\"col\":5}\n"
    );
}

#[test]
fn json_results_carry_status_message_location_and_output() {
    let looping = "@test fun loops() { while true {} }\n";
    let t = Temp::with(&[("geo.la", GEO), ("looping.la", looping)]);
    let out = lughac(&t.0, &["test", "--json", "geo.la"]);
    assert_eq!(out.status.code(), Some(1));
    let stdout = text(&out.stdout);
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines.len(), 3, "{stdout}");
    let passed = lines
        .iter()
        .find(|l| l.contains("\"geo.area_of_unit\""))
        .expect("a result");
    assert!(
        passed.starts_with(
            "{\"test\":\"geo.area_of_unit\",\"file\":\"geo.la\",\"line\":5,\"status\":\"passed\""
        ),
        "{passed}"
    );
    let failed = lines
        .iter()
        .find(|l| l.contains("\"geo.doubles\""))
        .expect("a result");
    assert!(failed.contains("\"status\":\"failed\""), "{failed}");
    assert!(
        failed.contains("\"message\":\"assert_eq failed: left 4, right 5\""),
        "{failed}"
    );
    assert!(
        failed.contains("\"at\":{\"file\":\"geo.la\",\"line\":13,\"col\":5}"),
        "{failed}"
    );
    assert!(failed.contains("\"output\":\"doubling 2\\n\""), "{failed}");
    assert!(failed.contains("\"ms\":"), "{failed}");
    assert_eq!(lines[2], "{\"summary\":{\"passed\":1,\"failed\":1}}");
    let out = lughac(&t.0, &["test", "--json", "--timeout", "1", "looping.la"]);
    assert!(
        text(&out.stdout).contains("\"status\":\"timed_out\""),
        "{}",
        text(&out.stdout)
    );
}

#[test]
fn exact_filters_match_whole_names_and_filters_repeat() {
    let src = "@test fun a() {}\n@test fun ab() {}\n@test fun b() {}\n";
    let t = Temp::with(&[("m.la", src)]);
    let names = |args: &[&str]| -> Vec<String> {
        let mut all = vec!["test"];
        all.extend_from_slice(args);
        let out = lughac(&t.0, &all);
        text(&out.stdout)
            .lines()
            .filter_map(|l| {
                l.strip_prefix("test ")
                    .and_then(|l| l.split(" ...").next())
                    .map(str::to_string)
            })
            .collect()
    };
    assert_eq!(names(&["--filter", "m.a"]), ["m.a", "m.ab"]);
    assert_eq!(names(&["--exact", "--filter", "m.a"]), ["m.a"]);
    assert_eq!(
        names(&["--exact", "--filter", "m.a", "--filter", "m.b"]),
        ["m.a", "m.b"]
    );
}
