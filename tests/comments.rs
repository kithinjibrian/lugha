//! Comments stand on their own: none names a language version, a milestone, a PRP, a decision
//! number, a spec section or a draft. The rules are in the code style guide; this test is what
//! keeps them.

use std::path::{Path, PathBuf};

/// The folders whose sources are ours to comment; `third_party/` and the vendored suites are not.
const ROOTS: [&str; 5] = [
    "src",
    "runtime",
    "tests",
    "editors/vscode/src",
    "editors/vscode/test",
];

/// Source extensions whose comments start with `//`, `/*` or `*`.
const EXTENSIONS: [&str; 5] = ["rs", "c", "h", "la", "ts"];

/// Every source file under `dir`, skipping vendored test suites.
fn sources(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            if path.ends_with("tests/fixtures") || path.ends_with("node_modules") {
                continue;
            }
            sources(&path, out);
        } else if path
            .extension()
            .and_then(|e| e.to_str())
            .is_some_and(|e| EXTENSIONS.contains(&e))
        {
            out.push(path);
        }
    }
}

/// The comment text on `line`, when the line is a comment or the inside of a block comment.
fn comment(line: &str) -> Option<&str> {
    let t = line.trim_start();
    ["///", "//!", "//", "/**", "/*", "*"]
        .iter()
        .find_map(|m| t.strip_prefix(m))
}

/// Whether `text` holds `v` and a number as a word of its own, as in `v17` but not `x86-64-v3`,
/// `v1.2.0` or `llvm21`.
fn names_version(text: &str) -> bool {
    let b = text.as_bytes();
    (0..b.len()).any(|i| {
        let before_ok =
            i == 0 || !(b[i - 1].is_ascii_alphanumeric() || b"-_.`\"".contains(&b[i - 1]));
        let digits = b[i + 1..].iter().take_while(|c| c.is_ascii_digit()).count();
        let end = i + 1 + digits;
        // A dot ends a sentence, unless a digit follows it, as in `v1.2.0`.
        let dotted = b.get(end) == Some(&b'.') && b.get(end + 1).is_some_and(u8::is_ascii_digit);
        let after_ok = end == b.len()
            || !(dotted || b[end].is_ascii_alphanumeric() || b"_-`\"".contains(&b[end]));
        b[i] == b'v' && before_ok && digits > 0 && after_ok
    })
}

/// What in `text` names history or a document, if anything.
fn violation(text: &str) -> Option<&'static str> {
    let lower = text.to_lowercase();
    let digit_after = |word: &str| {
        lower.match_indices(word).any(|(i, _)| {
            lower[i + word.len()..]
                .trim_start()
                .starts_with(|c: char| c.is_ascii_digit())
        })
    };
    if text.contains('§') {
        Some("a spec section")
    } else if digit_after("prp-") || digit_after("decision-") {
        Some("a PRP or decision")
    } else if digit_after("milestone") {
        Some("a milestone")
    } else if digit_after("draft decision") || lower.contains(" draft") && digit_after("decisions")
    {
        Some("a draft decision")
    } else if names_version(text) {
        Some("a language version")
    } else {
        None
    }
}

#[test]
fn no_comment_names_history_or_a_document() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    for dir in ROOTS {
        sources(&root.join(dir), &mut files);
    }
    let mut found = Vec::new();
    for path in files {
        let text = std::fs::read_to_string(&path).expect("sources are UTF-8");
        for (n, line) in text.lines().enumerate() {
            if let Some(what) = comment(line).and_then(violation) {
                let shown = path.strip_prefix(root).unwrap_or(&path).display();
                found.push(format!("{shown}:{}: names {what}: {}", n + 1, line.trim()));
            }
        }
    }
    assert!(
        found.is_empty(),
        "{} comments:\n{}",
        found.len(),
        found.join("\n")
    );
}

#[test]
fn a_version_is_found_only_as_a_word_of_its_own() {
    assert!(names_version("arrived in v17."));
    assert!(names_version("(v3 interpolation)"));
    assert!(!names_version("--cpu=x86-64-v3"));
    assert!(!names_version("tag `v1.2.0`"));
    assert!(!names_version("llvm21 and dev"));
}

#[test]
fn history_and_documents_are_violations() {
    assert_eq!(violation("rules (spec §4)"), Some("a spec section"));
    assert_eq!(violation("added by PRP-101"), Some("a PRP or decision"));
    assert_eq!(violation("since milestone 147"), Some("a milestone"));
    assert_eq!(
        violation("the v16 draft decisions 87–88"),
        Some("a draft decision")
    );
    assert_eq!(violation("Peek two tokens ahead."), None);
}
