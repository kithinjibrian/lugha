//! `std.toml` end to end (v17, milestone 114, PRP-146): toml-test v2.2.0's
//! TOML 1.0 cases (vendored in `tests/fixtures/toml-test`, MIT), the limits,
//! hostile input, error positions and the helpers, through Lugha programs.

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
        let dir = std::env::temp_dir().join(format!("lugha-toml-{}-{n}", std::process::id()));
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

/// Prints a document as toml-test's tagged JSON (floats as their bits), or
/// `ERROR <kind> <message>` and exit 1.
const DECODER: &str = r#"import std.toml;

fun quote(s: string): string {
    let mut out: u8[] = [34];
    let digits = "0123456789abcdef".to_bytes();
    for b of s.to_bytes() {
        if b == 34 || b == 92 {
            out.push(92);
            out.push(b);
        } else if b < 32 {
            for c of "\\u00".to_bytes() {
                out.push(c);
            }
            out.push(digits[(b / 16) as i64]);
            out.push(digits[(b % 16) as i64]);
        } else {
            out.push(b);
        }
    }
    out.push(34);
    string_from_utf8(out).unwrap_or("")
}

fun pad(n: i64, width: i64): string {
    let mut s = "{n}";
    while s.len < width {
        s = "0" + s;
    }
    s
}

fun date(d: toml.Date): string = "{pad(d.year, 4)}-{pad(d.month, 2)}-{pad(d.day, 2)}";

fun time(t: toml.Time): string {
    let base = "{pad(t.hour, 2)}:{pad(t.minute, 2)}:{pad(t.second, 2)}";
    if t.nanosecond == 0 { base } else { base + "." + pad(t.nanosecond, 9) }
}

fun offset(minutes: i64): string {
    if minutes == 0 {
        return "Z";
    }
    let sign = if minutes < 0 { "-" } else { "+" };
    let m = if minutes < 0 { -minutes } else { minutes };
    "{sign}{pad(m / 60, 2)}:{pad(m % 60, 2)}"
}

fun tagged(kind: string, value: string): string = "{{\"type\": {quote(kind)}, \"value\": {quote(value)}}}";

fun json(v: toml.Value): string {
    match v {
        Text(s) => tagged("string", s),
        Integer(n) => tagged("integer", "{n}"),
        Float(x) => tagged("float", "bits:{x.to_bits()}"),
        Boolean(b) => tagged("bool", "{b}"),
        OffsetDateTime(d, t, o) => tagged("datetime", date(d) + "T" + time(t) + offset(o)),
        LocalDateTime(d, t) => tagged("datetime-local", date(d) + "T" + time(t)),
        LocalDate(d) => tagged("date-local", date(d)),
        LocalTime(t) => tagged("time-local", time(t)),
        Array(xs) => "[" + xs.map(json).join(", ") + "]",
        Table(m) => table(m),
    }
}

fun table(m: Map<string, toml.Value>): string {
    let mut parts: string[] = [];
    for (k, v) of m {
        parts.push(quote(k) + ": " + json(v));
    }
    "{{" + parts.join(", ") + "}}"
}

fun main() {
    let path = args()[1];
    let text = match string_from_utf8(read_bytes(path).unwrap()) {
        Ok(t) => t,
        Err(_) => {
            println("ERROR Utf8 not UTF-8");
            exit(1);
        }
    };
    match toml.parse(text) {
        Ok(doc) => println(table(doc)),
        Err(e) => {
            println("ERROR {e.kind} {e.message()}");
            exit(1);
        }
    }
}
"#;

fn fixtures() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/toml-test")
}

/// Builds `source` into `dir/prog` and returns its path.
fn build(dir: &Path, source: &str) -> PathBuf {
    std::fs::write(dir.join("main.la"), source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(["build", "main.la", "-o", "prog"])
        .current_dir(dir)
        .env("HOME", dir)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    dir.join("prog")
}

/// A date-time as toml-test compares them: `T` between date and time, `Z`
/// for a zero offset, 9 fraction digits or none.
fn normal_datetime(s: &str) -> String {
    let s = s.replace(['t', ' '], "T").replace('z', "Z");
    let (main, zone) = match s.find(['Z', '+']) {
        Some(i) => (&s[..i], &s[i..]),
        None => match s.rfind('-').filter(|&i| i > 10 && s.contains(':')) {
            Some(i) => (&s[..i], &s[i..]),
            None => (&s[..], ""),
        },
    };
    let zone = if zone == "+00:00" || zone == "-00:00" {
        "Z"
    } else {
        zone
    };
    let main = match main.split_once('.') {
        Some((head, frac)) => {
            let mut digits: String = frac.chars().take(9).collect();
            while digits.len() < 9 {
                digits.push('0');
            }
            if digits.chars().all(|c| c == '0') {
                head.to_string()
            } else {
                format!("{head}.{digits}")
            }
        }
        None => main.to_string(),
    };
    format!("{main}{zone}")
}

fn float_of(text: &str) -> f64 {
    match text {
        "inf" | "+inf" => f64::INFINITY,
        "-inf" => f64::NEG_INFINITY,
        "nan" | "+nan" | "-nan" => f64::NAN,
        other => other.parse().unwrap(),
    }
}

/// Whether our tagged JSON `got` means the same as toml-test's `want`.
fn same(got: &Json, want: &Json) -> bool {
    match (got, want) {
        (Json::Array(a), Json::Array(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(x, y)| same(x, y))
        }
        (Json::Object(a), Json::Object(b))
            if b.contains_key("type") && b.len() == 2 && b.contains_key("value") =>
        {
            let (Some(kind), Some(gk)) = (b["type"].as_str(), a.get("type").and_then(Json::as_str))
            else {
                return false;
            };
            let (wv, gv) = (
                b["value"].as_str().unwrap_or(""),
                a["value"].as_str().unwrap_or(""),
            );
            kind == gk
                && match kind {
                    "float" => {
                        let bits: u64 = gv.trim_start_matches("bits:").parse().unwrap();
                        let (g, w) = (f64::from_bits(bits), float_of(wv));
                        (g.is_nan() && w.is_nan()) || g == w
                    }
                    "integer" => gv.parse::<i64>().ok() == wv.parse::<i64>().ok(),
                    "datetime" | "datetime-local" | "date-local" | "time-local" => {
                        normal_datetime(gv) == normal_datetime(wv)
                    }
                    _ => gv == wv,
                }
        }
        (Json::Object(a), Json::Object(b)) => {
            a.len() == b.len() && b.iter().all(|(k, v)| a.get(k).is_some_and(|g| same(g, v)))
        }
        _ => false,
    }
}

#[test]
fn the_toml_test_suite_passes() {
    let temp = Temp::new();
    let decoder = build(&temp.0, DECODER);
    let list = std::fs::read_to_string(fixtures().join("files-toml-1.0.0")).unwrap();
    let mut failures = Vec::new();
    let (mut valid, mut invalid) = (0, 0);
    for case in list.lines().filter(|l| l.ends_with(".toml")) {
        let path = fixtures().join(case);
        let out = Command::new(&decoder).arg(&path).output().unwrap();
        let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
        if case.starts_with("valid/") {
            valid += 1;
            let want: Json = serde_json::from_str(
                &std::fs::read_to_string(path.with_extension("json")).unwrap(),
            )
            .unwrap();
            match serde_json::from_str::<Json>(stdout.trim()) {
                Ok(got) if out.status.success() && same(&got, &want) => {}
                Ok(got) => failures.push(format!("{case}: got {got}\n  want {want}")),
                Err(_) => failures.push(format!(
                    "{case}: {stdout}{}",
                    String::from_utf8_lossy(&out.stderr)
                )),
            }
        } else {
            invalid += 1;
            let rejected = out.status.code() == Some(1)
                && stdout.starts_with("ERROR")
                && !stdout.starts_with("ERROR Limit");
            if !rejected {
                failures.push(format!(
                    "{case}: accepted or crashed ({:?}): {stdout}{}",
                    out.status.code(),
                    String::from_utf8_lossy(&out.stderr)
                ));
            }
        }
    }
    assert!(
        valid > 150 && invalid > 300,
        "{valid} valid, {invalid} invalid"
    );
    assert!(
        failures.is_empty(),
        "{} of {} cases failed:\n{}",
        failures.len(),
        valid + invalid,
        failures.join("\n")
    );
}

/// Runs a Lugha program (with `std.toml` imported) and returns its stdout.
fn run(main: &str) -> String {
    let temp = Temp::new();
    let prog = build(&temp.0, &format!("import std.toml;\n\n{main}"));
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

fun show(r: Result<Map<string, toml.Value>, toml.TomlError>) {
    match r {
        Ok(_) => println("ok"),
        Err(e) => println("{e.kind}: {e.message()}"),
    }
}
"#;

#[test]
fn limits_hold_against_hostile_input() {
    let out = run(&format!(
        "{HELPERS}
fun main() {{
    show(toml.parse(\"# \" + repeat(\"x\", 1048575)));
    show(toml.parse(\"# \" + repeat(\"x\", 1048574)));
    show(toml.parse(\"a = \" + repeat(\"[\", 100) + repeat(\"]\", 100)));
    show(toml.parse(\"a = \" + repeat(\"[\", 101) + repeat(\"]\", 101)));
    show(toml.parse(\"a = \" + repeat(\"{{{{b = \", 101) + \"1\" + repeat(\"}}}}\", 101)));
    show(toml.parse(repeat(\"k.\", 99) + \"k = 1\"));
    show(toml.parse(repeat(\"k.\", 100) + \"k = 1\"));
    show(toml.parse(\"a = \" + repeat(\"[\", 100000)));
    let mut small = toml.limits();
    small.values = 5;
    show(toml.parse_with(\"a = 1\\nb = 2\\nc = 3\\nd = 4\\ne = 5\\nf = 6\", small));
    show(toml.parse(\"n = 9223372036854775808\"));
    show(toml.parse(\"n = -9223372036854775809\"));
    show(toml.parse(\"n = 0x8000000000000000\"));
    println(toml.parse(\"n = -9223372036854775808\").unwrap());
    println(toml.parse(\"n = 0x7fffffffffffffff\").unwrap());
    let big = toml.parse(\"s = \\\"\" + repeat(\"x\", 1048000) + \"\\\"\").unwrap();
    println(toml.as_text(big[\"s\"]).unwrap().len);
}}
"
    ));
    assert_eq!(
        out,
        "Limit: a text larger than 1048576 bytes (line 1, column 1)\nok\nok\n\
         Limit: nesting deeper than 100 (line 1, column 105)\n\
         Limit: nesting deeper than 100 (line 1, column 505)\nok\n\
         Limit: a key with more than 100 parts (line 1, column 201)\n\
         Limit: nesting deeper than 100 (line 1, column 105)\n\
         Limit: more than 5 values (line 6, column 5)\n\
         Overflow: an integer too large for i64: 9223372036854775808 (line 1, column 5)\n\
         Overflow: an integer too large for i64: -9223372036854775809 (line 1, column 5)\n\
         Overflow: an integer too large for i64 (line 1, column 5)\n\
         #{\"n\": Integer(-9223372036854775808)}\n#{\"n\": Integer(9223372036854775807)}\n1048000\n"
    );
}

#[test]
fn big_documents_parse_in_linear_time() {
    let start = std::time::Instant::now();
    let out = run(&format!(
        "{HELPERS}
fun main() {{
    let mut keys: string[] = [];
    for i in 0..70000 {{
        keys.push(\"k{{i}} = {{i}}\");
    }}
    let doc = toml.parse(keys.join(\"\\n\")).unwrap();
    println(doc.len);
    let tables = toml.parse(repeat(\"[[a]]\\nx = 1\\n\", 80000)).unwrap();
    println(toml.as_array(tables[\"a\"]).unwrap().len);
}}
"
    ));
    assert_eq!(out, "70000\n80000\n");
    assert!(start.elapsed().as_secs() < 20, "took {:?}", start.elapsed());
}

#[test]
fn errors_name_the_problem_and_where() {
    let out = run(&format!(
        "{HELPERS}
fun main() {{
    show(toml.parse(\"[app]\\nname = 1\\n\\n\\n\\n\\nname = 2\"));
    show(toml.parse(\"[a]\\nx = 1\\n[a]\"));
    show(toml.parse(\"a = {{{{b = 1}}}}\\na.c = 2\"));
    show(toml.parse(\"a = []\\n[[a]]\"));
    show(toml.parse(\"a = \\\"\\\\q\\\"\"));
    show(toml.parse(\"d = 2023-02-30\"));
    show(toml.parse(\"a 1\"));
    show(toml.parse(\"a = 1\\r\\nb = 2\\r\\na = 3\"));
    show(toml.parse(\"k = \\\"é\\\" x\"));
}}
"
    ));
    assert_eq!(
        out,
        "Duplicate: a key defined twice: app.name (line 7, column 1)\n\
         Duplicate: a table defined twice: a (line 3, column 1)\n\
         Immutable: an inline table extended: a (line 2, column 1)\n\
         Immutable: an array that isn't an array of tables extended: a (line 2, column 1)\n\
         BadValue: an unknown escape (line 1, column 6)\n\
         BadValue: an invalid date or time (line 1, column 5)\n\
         Syntax: expected '=' after a key (line 1, column 3)\n\
         Duplicate: a key defined twice: a (line 3, column 1)\n\
         Syntax: expected the end of the line (line 1, column 9)\n"
    );
}

#[test]
fn helpers_reach_into_documents() {
    let out = run("fun name(doc: Map<string, toml.Value>): Option<string> = toml.as_text(toml.get(doc, \"app.name\")?);

fun main() {
    let doc = toml.parse(\"[app]\\nname = \\\"koi\\\"\\nport = 8080\\nratio = 0.5\\ndebug = true\\ntags = [\\\"a\\\"]\\nwhen = 1979-05-27T07:32:00Z\\n[app.inner]\\nx = 1\").unwrap();
    println(name(doc));
    println(toml.get(doc, \"app.missing\").is_none());
    println(toml.get(doc, \"app.name.deeper\").is_none());
    let app = toml.as_table(toml.get(doc, \"app\").unwrap()).unwrap();
    println(\"{toml.as_i64(app[\"port\"])} {toml.as_f64(app[\"port\"])} {toml.as_f64(app[\"ratio\"])} {toml.as_bool(app[\"debug\"])}\");
    println(\"{toml.as_array(app[\"tags\"]).unwrap().len} {toml.as_table(app[\"inner\"]).unwrap().len}\");
    println(\"{toml.as_text(app[\"port\"])} {toml.as_i64(app[\"name\"])} {toml.as_bool(app[\"ratio\"])} {toml.as_array(app[\"name\"])} {toml.as_table(app[\"tags\"])}\");
    match toml.as_date_time(app[\"when\"]) {
        Some((Some(d), Some(t), Some(o))) => println(\"{d.year}-{d.month}-{d.day} {t.hour}:{t.minute} {o}\"),
        _ => println(\"?\"),
    }
    println(toml.as_date_time(app[\"port\"]).is_none());
}
");
    assert_eq!(
        out,
        "Some(\"koi\")\ntrue\ntrue\nSome(8080) Some(8080.0) Some(0.5) Some(true)\n1 1\n\
         None None None None None\n1979-5-27 7:32 0\ntrue\n"
    );
}
