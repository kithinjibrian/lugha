//! `std.json` end to end (v17, milestone 137, PRP-169): JSONTestSuite's
//! parsing cases (vendored in `tests/fixtures/JSONTestSuite`, MIT), values,
//! errors and their positions, limits, hostile input, writing and the
//! helpers, through Lugha programs.

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
        let dir = std::env::temp_dir().join(format!("lugha-json-{}-{n}", std::process::id()));
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

/// Runs a Lugha program (with `std.json` imported and `HELPERS`) and returns its stdout.
fn run(main: &str) -> String {
    let temp = Temp::new();
    let prog = build(&temp.0, &format!("import std.json;\n{HELPERS}\n{main}"));
    let out = Command::new(prog).output().unwrap();
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

const HELPERS: &str = r#"
fun repeat(s: string, n: i64): string {
    let unit = s.to_bytes();
    let mut out: u8[] = [];
    for _ in 0..n {
        for b of unit {
            out.push(b);
        }
    }
    string_from_utf8(out).unwrap()
}

fun show(r: Result<json.Value, json.JsonError>) {
    match r {
        Ok(v) => println("ok {v}"),
        Err(e) => println("{e.kind}: {e.message()}"),
    }
}

fun written(r: Result<string, json.JsonError>) {
    match r {
        Ok(s) => println(s),
        Err(e) => println("{e.kind}: {e.message()}"),
    }
}
"#;

#[test]
fn the_objective_program_runs() {
    let out = run(r#"
fun go(): Result<(), json.JsonError> {
    let v = json.parse("{{\"user\": {{\"name\": \"ada\", \"id\": 9007199254740993}}}}")?;
    println(json.get(v, "user").and_then(fun(u) = json.get(u, "name")).and_then(json.as_text));
    println(json.get(v, "user").and_then(fun(u) = json.get(u, "id")).and_then(json.as_i64));
    let reply = json.Value.Object(#{
        "ok": json.Value.Bool(true),
        "tags": json.Value.Array([json.Value.Text("a"), json.Value.Null]),
    });
    println(json.stringify(reply)?);
    println(json.pretty(reply)?);
    match json.parse("{{\"a\": 1, \"a\": 2}}") {
        Err(e) => println(e.message()),
        Ok(_) => {}
    }
    Ok(())
}

fun main() {
    match go() { Err(e) => println(e.message()), Ok(_) => {} }
}
"#);
    assert_eq!(
        out,
        "Some(\"ada\")\nSome(9007199254740993)\n{\"ok\":true,\"tags\":[\"a\",null]}\n\
         {\n  \"ok\": true,\n  \"tags\": [\n    \"a\",\n    null\n  ]\n}\n\
         a key defined twice: a (line 1, column 10)\n"
    );
}

#[test]
fn values_keep_numbers_exact_and_keys_in_order() {
    let out = run(r#"
fun main() {
    show(json.parse("null"));
    show(json.parse(" [true, false] "));
    show(json.parse("9223372036854775807"));
    show(json.parse("9223372036854775808"));
    show(json.parse("-9223372036854775808"));
    show(json.parse("-9223372036854775809"));
    show(json.parse("1.0"));
    show(json.parse("1e2"));
    show(json.parse("-0"));
    show(json.parse("-0.0"));
    show(json.parse("1e-400"));
    show(json.parse("{{\"b\": 1, \"a\": \"x\", \"c\": {{}}, \"d\": []}}"));
    let text = json.as_text(json.parse("\"\\ud834\\udd1e\\u00e9\\\"\\\\\\/\\b\\f\\n\\r\\t\"").unwrap()).unwrap();
    println(text.to_bytes());
}
"#);
    assert_eq!(
        out,
        "ok Null\nok Array([Bool(true), Bool(false)])\nok Integer(9223372036854775807)\n\
         ok Float(9.223372036854776e18)\nok Integer(-9223372036854775808)\n\
         ok Float(-9.223372036854776e18)\nok Float(1.0)\nok Float(100.0)\nok Integer(0)\n\
         ok Float(-0.0)\nok Float(0.0)\n\
         ok Object(#{\"b\": Integer(1), \"a\": Text(\"x\"), \"c\": Object(#{}), \"d\": Array([])})\n\
         [240, 157, 132, 158, 195, 169, 34, 92, 47, 8, 12, 10, 13, 9]\n"
    );
}

#[test]
fn errors_name_the_problem_and_where() {
    let out = run(r#"
fun main() {
    show(json.parse("{{\"a\": 1, \"a\": 2}}"));
    show(json.parse("[1,]"));
    show(json.parse("{{\"a\":1,}}"));
    show(json.parse("{{\"a\" 1}}"));
    show(json.parse("\"\\ud800\""));
    show(json.parse("\"\\udc00\\ud800\""));
    show(json.parse("\"a\tb\""));
    show(json.parse("1e400"));
    show(json.parse("1 2"));
    show(json.parse(string_from_utf8([239, 187, 191, 123, 125]).unwrap()));
    show(json.parse("[1,\r\n2,\r\n]"));
    show(json.parse("[\"é\", x]"));
    show(json.parse(""));
    show(json.parse("\"\\q\""));
    show(json.parse("01"));
    show(json.parse("\"abc"));
    show(json.parse("[1 2]"));
    show(json.parse("-"));
    show(json.parse("1."));
    show(json.parse("tru"));
}
"#);
    assert_eq!(
        out,
        "Duplicate: a key defined twice: a (line 1, column 10)\n\
         Syntax: expected a value (line 1, column 4)\n\
         Syntax: expected a key (line 1, column 8)\n\
         Syntax: expected ':' after a key (line 1, column 6)\n\
         BadValue: a lone surrogate: \\ud800 (line 1, column 2)\n\
         BadValue: a lone surrogate: \\udc00 (line 1, column 2)\n\
         Syntax: a control character in a string (line 1, column 3)\n\
         Overflow: a number too large for f64 (line 1, column 1)\n\
         Syntax: expected the end of the text (line 1, column 3)\n\
         Syntax: a byte-order mark (line 1, column 1)\n\
         Syntax: expected a value (line 3, column 1)\n\
         Syntax: expected a value (line 1, column 7)\n\
         Syntax: expected a value (line 1, column 1)\n\
         BadValue: an unknown escape (line 1, column 2)\n\
         Syntax: a leading zero in a number (line 1, column 1)\n\
         Syntax: an unterminated string (line 1, column 1)\n\
         Syntax: expected ',' or ']' (line 1, column 4)\n\
         Syntax: not a number (line 1, column 1)\n\
         Syntax: not a number (line 1, column 1)\n\
         Syntax: expected a value (line 1, column 1)\n"
    );
}

#[test]
fn limits_hold_against_hostile_input() {
    let out = run(r#"
fun main() {
    show(json.parse("\"" + repeat("x", 1048575) + "\"").map(fun(x) = json.Value.Null));
    show(json.parse("\"" + repeat("x", 1048574) + "\"").map(fun(x) = json.Value.Null));
    show(json.parse(repeat("[", 100) + repeat("]", 100)).map(fun(x) = json.Value.Null));
    show(json.parse(repeat("[", 101) + repeat("]", 101)));
    show(json.parse(repeat("{{\"a\":", 101) + "1" + repeat("}}", 101)));
    show(json.parse(repeat("[", 100000)));
    let mut small = json.limits();
    small.values = 5;
    show(json.parse_with("[1,2,3,4]", small));
    show(json.parse_with("[1,2,3,4,5]", small));
    let mut shallow = json.limits();
    shallow.depth = 2;
    show(json.parse_with("[[1]]", shallow));
    show(json.parse_with("[[[1]]]", shallow));
    let mut tiny = json.limits();
    tiny.bytes = 3;
    show(json.parse_with("[1]", tiny));
    show(json.parse_with("[10]", tiny));
    show(json.parse("1" + repeat("0", 1000000)));
}
"#);
    assert_eq!(
        out,
        "Limit: a text larger than 1048576 bytes (line 1, column 1)\nok Null\nok Null\n\
         Limit: nesting deeper than 100 (line 1, column 101)\n\
         Limit: nesting deeper than 100 (line 1, column 501)\n\
         Limit: nesting deeper than 100 (line 1, column 101)\n\
         ok Array([Integer(1), Integer(2), Integer(3), Integer(4)])\n\
         Limit: more than 5 values (line 1, column 10)\n\
         ok Array([Array([Integer(1)])])\n\
         Limit: nesting deeper than 2 (line 1, column 3)\n\
         ok Array([Integer(1)])\n\
         Limit: a text larger than 3 bytes (line 1, column 1)\n\
         Overflow: a number too large for f64 (line 1, column 1)\n"
    );
}

#[test]
fn big_documents_parse_in_linear_time() {
    let start = std::time::Instant::now();
    let out = run(r#"
fun main() {
    let mut members: string[] = [];
    let mut items: string[] = [];
    for i in 0..60000 {
        members.push("\"k{i}\": {i}");
    }
    for i in 0..100000 {
        items.push("{i}");
    }
    println(json.as_object(json.parse("{{" + members.join(", ") + "}}").unwrap()).unwrap().len);
    println(json.as_array(json.parse("[" + items.join(",") + "]").unwrap()).unwrap().len);
    println(json.as_text(json.parse("\"" + repeat("x", 1048000) + "\"").unwrap()).unwrap().len);
    show(json.parse("{{" + repeat("\"a\":1,", 60000) + "\"a\":1}}"));
}
"#);
    assert_eq!(
        out,
        "60000\n100000\n1048000\nDuplicate: a key defined twice: a (line 1, column 8)\n"
    );
    assert!(start.elapsed().as_secs() < 20, "took {:?}", start.elapsed());
}

#[test]
fn writing_escapes_only_what_it_must() {
    let out = run(r#"
fun main() {
    let nan = f64.from_bits(9221120237041090560);
    let inf = f64.from_bits(9218868437227405312);
    let text = string_from_utf8([104, 195, 169, 34, 92, 10, 9, 13, 8, 12, 1, 31, 47, 127]).unwrap();
    written(json.stringify(json.Value.Null));
    written(json.stringify(json.Value.Bool(false)));
    written(json.stringify(json.Value.Integer(-5)));
    written(json.stringify(json.Value.Text(text)));
    written(json.stringify(json.Value.Array([])));
    written(json.pretty(json.Value.Array([])));
    written(json.pretty(json.Value.Object(#{})));
    let nested = json.Value.Object(#{
        "a": json.Value.Array([json.Value.Integer(1), json.Value.Null]),
        "b": json.Value.Object(#{}),
    });
    written(json.stringify(nested));
    written(json.pretty(nested));
    let floats: f64[] = [1.0, 0.1, 1.0e300, 5.0e-324, -0.0, 1.0e21];
    for x of floats {
        let s = json.stringify(json.Value.Float(x)).unwrap();
        println("{s} {json.parse(s).unwrap() == json.Value.Float(x)}");
    }
    written(json.stringify(json.Value.Float(nan)));
    written(json.pretty(json.Value.Array([json.Value.Float(inf)])));
    written(json.stringify(json.Value.Object(#{"x": json.Value.Float(-inf)})));
}
"#);
    assert_eq!(
        out,
        "null\nfalse\n-5\n\"hé\\\"\\\\\\n\\t\\r\\b\\f\\u0001\\u001f/\u{7f}\"\n[]\n[]\n{}\n\
         {\"a\":[1,null],\"b\":{}}\n\
         {\n  \"a\": [\n    1,\n    null\n  ],\n  \"b\": {}\n}\n\
         1.0 true\n0.1 true\n1.0e300 true\n5.0e-324 true\n-0.0 true\n1.0e21 true\n\
         NotFinite: NaN or an infinity can't be written as JSON (line 0, column 0)\n\
         NotFinite: NaN or an infinity can't be written as JSON (line 0, column 0)\n\
         NotFinite: NaN or an infinity can't be written as JSON (line 0, column 0)\n"
    );
}

#[test]
fn helpers_reach_into_values() {
    let out = run(r#"
fun main() {
    let v = json.parse("{{\"user\": {{\"name\": \"ada\", \"id\": 9007199254740993, \"n\": 3, \"tags\": [\"x\", null], \"ok\": true, \"r\": 0.5}}}}").unwrap();
    let u = json.get(v, "user").unwrap();
    println(json.get(u, "name").and_then(json.as_text));
    println(json.get(u, "id").and_then(json.as_i64));
    println(json.get(u, "n").and_then(json.as_f64));
    println(json.get(u, "r").and_then(json.as_f64));
    println(json.get(u, "ok").and_then(json.as_bool));
    let tags = json.get(u, "tags").unwrap();
    println(json.as_array(tags).unwrap().len);
    println(json.at(tags, 0).and_then(json.as_text));
    println(json.is_null(json.at(tags, 1).unwrap()));
    println("{json.at(tags, 2).is_none()} {json.at(tags, -1).is_none()} {json.at(u, 0).is_none()}");
    println("{json.get(tags, "x").is_none()} {json.get(u, "missing").is_none()}");
    println(json.as_object(u).unwrap().len);
    println("{json.as_text(tags)} {json.as_i64(u)} {json.as_f64(json.Value.Text("1"))} {json.as_bool(json.Value.Null)} {json.as_array(u)} {json.as_object(tags)} {json.is_null(u)}");
}
"#);
    assert_eq!(
        out,
        "Some(\"ada\")\nSome(9007199254740993)\nSome(3.0)\nSome(0.5)\nSome(true)\n2\n\
         Some(\"x\")\ntrue\ntrue true true\ntrue true\n6\n\
         None None None None None None false\n"
    );
}

#[test]
fn a_program_that_only_parses_compiles_no_writer() {
    let temp = Temp::new();
    let out = lughac(
        &temp.0,
        "import std.json;\nfun main() { println(json.parse(\"1\").is_ok()); }\n",
        &["build", "--emit=ir", "main.la"],
    );
    let ir = String::from_utf8_lossy(&out.stdout);
    let defined: Vec<&str> = ir.lines().filter(|l| l.starts_with("define ")).collect();
    assert!(defined.iter().any(|l| l.contains("json/parse")), "no parse");
    assert!(
        !defined
            .iter()
            .any(|l| l.contains("json_write") || l.contains("json/stringify")),
        "{defined:?}"
    );
    // Nor any of `encode`'s machinery (milestone 139).
    assert!(
        !defined
            .iter()
            .any(|l| l.contains("lugha_json_") || l.contains("json_derive")),
        "{defined:?}"
    );
}
