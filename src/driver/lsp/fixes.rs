//! The quick fixes of milestone 82 (v16 draft decisions 71–74): `let mut`
//! for E0501, a misspelled variant for W0103, a std import for E0301. Each
//! turns one diagnostic into at most one edit; anything not found means no
//! fix.

use crate::check::{Partial, Target};
use crate::span::Span;

/// A fix: its title, and the bytes `from..to` of the file's text to replace
/// with `new_text`.
#[derive(Debug, PartialEq)]
pub(super) struct Fix {
    pub title: String,
    pub from: usize,
    pub to: usize,
    pub new_text: String,
}

/// The fix for diagnostic `code` at the bytes `start..end` of `text`, a
/// file starting at `base` in the check's offset space.
pub(super) fn fix(
    partial: &Partial,
    code: &str,
    base: usize,
    text: &str,
    (start, end): (usize, usize),
) -> Option<Fix> {
    let written = text.get(start..end)?;
    match code {
        "E0501" => mutable(partial, base, text, start, written),
        "W0103" => {
            let variants = partial.variants.get(&Span::new(base + start, base + end))?;
            let to = closest(written, variants)?;
            Some(Fix {
                title: format!("Change to `{to}`"),
                from: start,
                to: end,
                new_text: to.to_string(),
            })
        }
        "E0301" => import(text, end, written),
        _ => None,
    }
}

/// `mut ` after the `let` that declared the root of the place `written`.
fn mutable(partial: &Partial, base: usize, text: &str, start: usize, written: &str) -> Option<Fix> {
    let name = root(written);
    let at = Span::new(base + start, base + start + name.len());
    let Some(Target::Decl(decl)) = partial.definitions.get(&at) else {
        return None;
    };
    // Only a binding in this file, declared by a plain `let`.
    let local = decl.start.checked_sub(base).filter(|&d| d < text.len())?;
    if !after_let(text, local) {
        return None;
    }
    Some(Fix {
        title: format!("Make `{name}` mutable"),
        from: local,
        to: local,
        new_text: "mut ".to_string(),
    })
}

/// `import std.<m>;` for an undefined std module name used as `m.`.
fn import(text: &str, end: usize, written: &str) -> Option<Fix> {
    let known = crate::STD.iter().any(|(m, _)| *m == written);
    if !known || !text[end..].starts_with('.') {
        return None;
    }
    let line = format!("import std.{written};\n");
    let (at, new_text) = match after_imports(text) {
        Some(at) => (at, line),
        // The first import: a blank line between it and the rest.
        None => (0, format!("{line}\n")),
    };
    Some(Fix {
        title: format!("Add `import std.{written};`"),
        from: at,
        to: at,
        new_text,
    })
}

/// The name a place starts with: `p` in `p.x[0]`.
fn root(place: &str) -> &str {
    let end = place.find(['.', '[', ' ']).unwrap_or(place.len());
    &place[..end]
}

/// Whether the binding at `at` follows a `let` (so not `let mut`, a
/// parameter, a loop variable or a pattern).
fn after_let(text: &str, at: usize) -> bool {
    let before = &text[..at];
    let trimmed = before.trim_end();
    trimmed.len() < before.len()
        && trimmed.ends_with("let")
        && !trimmed[..trimmed.len() - 3]
            .chars()
            .last()
            .is_some_and(|c| c.is_alphanumeric() || c == '_')
}

/// Where a new import goes: the start of the line after the file's last
/// top-level import, or `None` when it has none.
fn after_imports(text: &str) -> Option<usize> {
    let mut last = None;
    let mut offset = 0;
    for line in text.split_inclusive('\n') {
        if line.starts_with("import ") || line.starts_with("public import ") {
            last = Some(offset);
        }
        offset += line.len();
    }
    let semicolon = last? + text[last?..].find(';')?;
    Some(
        text[semicolon..]
            .find('\n')
            .map_or(text.len(), |i| semicolon + i + 1),
    )
}

/// The variant `name` is most likely a misspelling of: one differing only
/// in case, else the only one within 2 edits of the fewest.
fn closest<'v>(name: &str, variants: &'v [String]) -> Option<&'v str> {
    let cased: Vec<&String> = variants
        .iter()
        .filter(|v| v.to_lowercase() == name.to_lowercase())
        .collect();
    if let [one] = cased.as_slice() {
        return Some(one.as_str());
    }
    let scored: Vec<(usize, &String)> = variants
        .iter()
        .map(|v| (distance(&name.to_lowercase(), &v.to_lowercase()), v))
        .filter(|(d, _)| *d <= 2)
        .collect();
    let best = scored.iter().map(|(d, _)| *d).min()?;
    match scored
        .iter()
        .filter(|(d, _)| *d == best)
        .collect::<Vec<_>>()
        .as_slice()
    {
        [(_, one)] => Some(one.as_str()),
        // A tie: no guess.
        _ => None,
    }
}

/// Levenshtein distance in characters.
fn distance(a: &str, b: &str) -> usize {
    let b: Vec<char> = b.chars().collect();
    let mut row: Vec<usize> = (0..=b.len()).collect();
    for (i, ca) in a.chars().enumerate() {
        let mut prev = row[0];
        row[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            let here = row[j + 1];
            row[j + 1] = (prev + usize::from(ca != *cb))
                .min(row[j] + 1)
                .min(here + 1);
            prev = here;
        }
    }
    row[b.len()]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn names(v: &[&str]) -> Vec<String> {
        v.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn distance_counts_edits() {
        assert_eq!(distance("Empty", "Empty"), 0);
        assert_eq!(distance("Emtpy", "Empty"), 2);
        assert_eq!(distance("Dto", "Dot"), 2);
        assert_eq!(distance("Circl", "Circle"), 1);
        assert_eq!(distance("", "abc"), 3);
    }

    #[test]
    fn closest_prefers_case_then_the_unique_nearest() {
        let vs = names(&["Circle", "Dot", "Empty"]);
        assert_eq!(closest("empty", &vs), Some("Empty"));
        assert_eq!(closest("Emtpy", &vs), Some("Empty"));
        assert_eq!(closest("Circl", &vs), Some("Circle"));
        assert_eq!(closest("value", &vs), None);
        // `Ab` is one edit from both: a tie.
        assert_eq!(closest("Ab", &names(&["Aa", "Bb"])), None);
    }

    #[test]
    fn the_root_of_a_place() {
        assert_eq!(root("x"), "x");
        assert_eq!(root("p.x.y"), "p");
        assert_eq!(root("xs[0]"), "xs");
    }

    #[test]
    fn only_a_plain_let_binding_can_be_made_mutable() {
        let at = |text: &str, name: &str| after_let(text, text.find(name).unwrap());
        assert!(at("    let total = 0;", "total"));
        assert!(!at("    let mut total = 0;", "total"));
        assert!(!at("fun f(total: i64) {}", "total"));
        assert!(!at("for total of xs {}", "total"));
        assert!(!at("let (a, total) = t;", "total"));
        assert!(!at("outlet total", "total"));
    }

    #[test]
    fn imports_go_after_the_last_one_or_first() {
        let text = "import geo;\npublic import shapes {\n    Circle,\n};\n\nfun main() {}\n";
        assert_eq!(after_imports(text), Some(text.find("\nfun").unwrap()));
        assert_eq!(after_imports("fun main() {}\n"), None);
        let fix = import("fun main() { math.pi(); }", 17, "math").expect("a fix");
        assert_eq!(
            (fix.from, fix.new_text.as_str()),
            (0, "import std.math;\n\n")
        );
        assert_eq!(import("fun main() { nope.pi(); }", 17, "nope"), None);
        assert_eq!(import("fun main() { math; }", 17, "math"), None);
    }
}
