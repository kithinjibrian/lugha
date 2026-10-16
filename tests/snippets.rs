//! The VS Code extension's static snippets (PRP-108, milestone 78) stay
//! valid Lugha: each body, its placeholders filled with their defaults,
//! checks with `lughac check`.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

use serde_json::Value;

/// The prefixes the extension's snippet file must have, and where each
/// expands: at the top level of a file, or as a statement in `main`.
const DECLARATIONS: [&str; 13] = [
    "fun",
    "main",
    "struct",
    "enum",
    "impl",
    "interface",
    "import",
    "doc",
    "extern",
    "externtype",
    "externstruct",
    "externcb",
    "actor",
];
const STATEMENTS: [&str; 11] = [
    "if", "ifelse", "while", "forof", "forin", "iflet", "letelse", "match", "spawn", "chan",
    "select",
];

/// What a statement snippet's defaults may name.
const LOCALS: &str = "    let x = 1;\n    let xs = [1, 2, 3];\n    let cond = true;\n    let o: Option<i64> = Some(1);\n    let c: Chan<i64> = Chan.buffered(1);\n";

fn snippets() -> Vec<(String, String)> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("editors/vscode/snippets/lugha.json");
    let text = std::fs::read_to_string(&path).expect("the snippet file exists");
    let json: Value = serde_json::from_str(&text).expect("the snippet file is JSON");
    json.as_object()
        .expect("an object of snippets")
        .values()
        .map(|s| {
            let prefix = s["prefix"].as_str().expect("a prefix").to_string();
            let body = match &s["body"] {
                Value::Array(lines) => lines
                    .iter()
                    .map(|l| l.as_str().unwrap_or_default())
                    .collect::<Vec<_>>()
                    .join("\n"),
                Value::String(body) => body.clone(),
                other => panic!("{prefix}: body {other}"),
            };
            (prefix, body)
        })
        .collect()
}

/// `body` with each `${n:default}` as its default, each `${n}`/`$n` empty,
/// `\}`, `\$` and `\\` unescaped as VS Code does, and each tab as four spaces.
fn expand(body: &str) -> String {
    let mut out = String::new();
    let mut chars = body.chars().peekable();
    // Nesting of `${…}`: their closing `}` is dropped, their `n:` too.
    let mut open = 0;
    while let Some(c) = chars.next() {
        match c {
            '\\' => out.extend(chars.next()),
            '$' if chars.peek() == Some(&'{') => {
                chars.next();
                while chars.peek().is_some_and(char::is_ascii_digit) {
                    chars.next();
                }
                if chars.peek() == Some(&':') {
                    chars.next();
                }
                open += 1;
            }
            '$' => {
                while chars.peek().is_some_and(char::is_ascii_digit) {
                    chars.next();
                }
            }
            '}' if open > 0 => open -= 1,
            c => out.push(c),
        }
    }
    out.replace('\t', "    ")
}

struct Temp(PathBuf);

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// `lughac check` on `main` (beside a module `geo.la`): exit code and stderr.
fn check(main: &str) -> (Option<i32>, String) {
    static NEXT: AtomicUsize = AtomicUsize::new(0);
    let n = NEXT.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("lugha-snippets-{}-{n}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let t = Temp(dir);
    std::fs::write(t.0.join("main.la"), main).unwrap();
    std::fs::write(t.0.join("geo.la"), "fun double(n: i64): i64 = n * 2;\n").unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(["check", "main.la"])
        .current_dir(&t.0)
        .env("HOME", &t.0)
        .output()
        .expect("lughac runs");
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[test]
fn the_snippet_file_has_every_chosen_prefix() {
    let prefixes: Vec<String> = snippets().into_iter().map(|(p, _)| p).collect();
    for p in DECLARATIONS.iter().chain(&STATEMENTS) {
        assert!(
            prefixes.iter().any(|x| x == p),
            "no `{p}` snippet: {prefixes:?}"
        );
    }
}

#[test]
fn every_snippet_expands_to_code_that_checks() {
    let mut failures = Vec::new();
    for (prefix, body) in snippets() {
        let code = expand(&body);
        let program = if prefix == "main" {
            code.clone()
        } else if prefix == "doc" {
            format!("{code}\nfun f(name: i64) {{ }}\nfun main() {{ }}\n")
        } else if prefix == "impl" {
            format!("struct Type {{ x: i64 }}\n{code}\nfun main() {{ }}\n")
        } else if DECLARATIONS.contains(&prefix.as_str()) {
            format!("{code}\nfun main() {{ }}\n")
        } else {
            let indented: Vec<String> = code.lines().map(|l| format!("    {l}")).collect();
            format!("fun main() {{\n{LOCALS}{}\n}}\n", indented.join("\n"))
        };
        let (status, stderr) = check(&program);
        if status != Some(0) {
            failures.push(format!("`{prefix}`:\n{program}\n{stderr}"));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n---\n"));
}

#[test]
fn expansion_uses_defaults_and_drops_bare_stops() {
    assert_eq!(
        expand("fun ${1:name}(${2}) {\n\t$0\n}"),
        "fun name() {\n    \n}"
    );
    assert_eq!(expand("${3:{\\}}, $10"), "{}, ");
}
