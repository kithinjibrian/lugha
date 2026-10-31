//! `std.json`'s parser against JSONTestSuite (v17, milestone 137, PRP-169):
//! every parsing case (vendored in `tests/fixtures/JSONTestSuite`, MIT)
//! accepted, refused or decided as PRP-169 pinned, and what parses writes
//! back to the same value. Split from `tests/json.rs` for the 500-line limit.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

use serde_json::Value as Json;

/// A temporary folder, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn new() -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-json-suite-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        Temp(dir)
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// Prints a parsed file written back compactly, after checking that both
/// `stringify` and `pretty` read back as the same value; or `ERROR <kind>
/// <message>` and exit 1.
const DECODER: &str = r#"import std.json;

fun main() {
    let path = args()[1];
    let text = match string_from_utf8(read_bytes(path).unwrap()) {
        Ok(t) => t,
        Err(_) => {
            println("ERROR Utf8 not UTF-8");
            exit(1);
        }
    };
    match json.parse(text) {
        Ok(v) => {
            let compact = json.stringify(v).unwrap();
            let pretty = json.pretty(v).unwrap();
            if json.parse(compact).unwrap() != v || json.parse(pretty).unwrap() != v {
                println("ROUNDTRIP {compact}");
                exit(1);
            }
            println(compact);
        }
        Err(e) => {
            println("ERROR {e.kind} {e.message()}");
            exit(1);
        }
    }
}
"#;

fn fixtures() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/JSONTestSuite/test_parsing")
}

/// Builds `source` into `dir/prog` (or emits its IR) and returns the output.
fn lughac(dir: &Path, source: &str, args: &[&str]) -> std::process::Output {
    std::fs::write(dir.join("main.la"), source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(dir)
        .env("HOME", dir)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    out
}

fn build(dir: &Path, source: &str) -> PathBuf {
    lughac(dir, source, &["build", "main.la", "-o", "prog"]);
    dir.join("prog")
}

/// Whether two JSON documents mean the same, numbers compared as `f64`
/// (`-0`, `1E2` and `100.0` are spelled differently on each side).
fn same(a: &Json, b: &Json) -> bool {
    match (a, b) {
        (Json::Number(x), Json::Number(y)) => x.as_f64() == y.as_f64(),
        (Json::Array(x), Json::Array(y)) => {
            x.len() == y.len() && x.iter().zip(y).all(|(p, q)| same(p, q))
        }
        (Json::Object(x), Json::Object(y)) => {
            x.len() == y.len() && y.iter().all(|(k, v)| x.get(k).is_some_and(|g| same(g, v)))
        }
        _ => a == b,
    }
}

/// The `i_` cases' outcomes, pinned by PRP-169's decisions: `Ok`, or the
/// `ERROR` kind (`Utf8` = refused before `parse`, `Rejected` = any error but
/// `Limit`).
fn pinned(name: &str) -> &'static str {
    match name {
        "i_number_double_huge_neg_exp"
        | "i_number_real_underflow"
        | "i_number_too_big_neg_int"
        | "i_number_too_big_pos_int"
        | "i_number_very_big_negative_int" => "Ok",
        n if n.starts_with("i_number_") => "Overflow",
        "i_structure_500_nested_arrays" => "Limit",
        "i_structure_UTF-8_BOM_empty_object" => "Syntax",
        n if n.contains("surrogate") && !n.contains("UTF8_surrogate") => "BadValue",
        _ => "Rejected",
    }
}

#[test]
fn json_test_suite_cases_behave_as_decided() {
    let temp = Temp::new();
    let decoder = build(&temp.0, DECODER);
    let mut failures = Vec::new();
    let mut counts = [0; 3];
    let mut names: Vec<_> = std::fs::read_dir(fixtures())
        .unwrap()
        .map(|e| e.unwrap().file_name().into_string().unwrap())
        .filter(|n| n.ends_with(".json"))
        .collect();
    names.sort();
    for name in names {
        let path = fixtures().join(&name);
        let case = name.trim_end_matches(".json");
        let out = Command::new(&decoder).arg(&path).output().unwrap();
        let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&out.stderr);
        let error = stdout
            .strip_prefix("ERROR ")
            .filter(|_| out.status.code() == Some(1))
            .map(|rest| rest.split(' ').next().unwrap_or(""));
        let ok = match &case[..2] {
            // Duplicate keys are an error by decision 332, though RFC 8259
            // only says their meaning is unpredictable.
            "y_" if case.starts_with("y_object_duplicated_key") => error == Some("Duplicate"),
            "y_" => {
                counts[0] += 1;
                let want: Json = serde_json::from_slice(&std::fs::read(&path).unwrap()).unwrap();
                out.status.success()
                    && serde_json::from_str::<Json>(stdout.trim()).is_ok_and(|g| same(&g, &want))
            }
            "n_" => {
                counts[1] += 1;
                // Only the two cases nested past depth 100 may stop at the limit.
                let deep = case.starts_with("n_structure_100000_opening_arrays")
                    || case.starts_with("n_structure_open_array_object");
                error.is_some_and(|k| k != "Limit" || deep)
            }
            _ => {
                counts[2] += 1;
                match pinned(case) {
                    "Ok" => out.status.success(),
                    "Rejected" => error.is_some_and(|k| k != "Limit"),
                    kind => error == Some(kind),
                }
            }
        };
        if !ok {
            failures.push(format!(
                "{case} ({:?}): {stdout}{stderr}",
                out.status.code()
            ));
        }
    }
    assert!(
        counts[0] > 90 && counts[1] > 180 && counts[2] > 30,
        "{counts:?}"
    );
    assert!(
        failures.is_empty(),
        "{} cases failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
