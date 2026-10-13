//! `lugha.toml` (v8 draft §6, PRP-058): the small subset of TOML that lists
//! a program's or a package's packages. Blank lines, `#` comments, one
//! `[packages]` table, and `name = "path"` lines; anything else is E0321.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// The manifest's file name, in a program's or a package's folder.
pub const MANIFEST: &str = "lugha.toml";

/// One `name = "path"` line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    /// The package name, an identifier.
    pub name: String,
    /// The path as written, relative to the manifest's folder.
    pub path: String,
    /// The whole line, in the shared offset space.
    pub span: Span,
}

/// The entries of the manifest `text`, which starts at offset `base`, and
/// an E0321 for every line outside the subset.
pub fn parse(text: &str, base: usize) -> (Vec<Entry>, Vec<Diagnostic>) {
    let (mut entries, mut errors): (Vec<Entry>, Vec<Diagnostic>) = (Vec::new(), Vec::new());
    let mut in_table = false;
    let mut start = 0;
    for raw in text.split_inclusive('\n') {
        let line = raw.trim_end_matches(['\n', '\r']);
        let span = Span::new(base + start, base + start + line.len());
        start += raw.len();
        // A `#` outside a string starts a comment; paths can't hold `"`, so the
        // first `#` after the last quote is the comment.
        let code = strip_comment(line).trim();
        if code.is_empty() {
            continue;
        }
        if code == "[packages]" && !in_table {
            in_table = true;
            continue;
        }
        let entry = match (in_table, entry(code)) {
            (false, _) => Err("an entry needs the `[packages]` table first".to_string()),
            (true, Ok((name, path))) if path.is_empty() => {
                Err(format!("`{name}` has an empty path"))
            }
            (true, Ok((name, path))) => Ok((name, path)),
            (true, Err(why)) => Err(why),
        };
        match entry {
            Ok((name, path)) => {
                if entries.iter().any(|e| e.name == name) {
                    errors.push(malformed(format!("`{name}` is listed twice"), span));
                } else {
                    entries.push(Entry { name, path, span });
                }
            }
            Err(why) => errors.push(malformed(why, span)),
        }
    }
    (entries, errors)
}

/// `line` without its comment.
fn strip_comment(line: &str) -> &str {
    let after_quotes = line.rfind('"').map_or(0, |i| i + 1);
    match line[after_quotes..].find('#') {
        Some(i) => &line[..after_quotes + i],
        None => line,
    }
}

/// `name = "path"`, or why the line isn't one.
fn entry(code: &str) -> Result<(String, String), String> {
    const SHAPE: &str = "expected `name = \"path\"`";
    let (name, value) = code.split_once('=').ok_or(SHAPE)?;
    let name = name.trim();
    let first = name.chars().next().ok_or(SHAPE)?;
    let ident = (first.is_ascii_alphabetic() || first == '_')
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');
    if !ident {
        return Err(format!(
            "`{name}` is not a package name: use letters, digits and `_`"
        ));
    }
    let value = value.trim();
    let path = value
        .strip_prefix('"')
        .and_then(|v| v.strip_suffix('"'))
        .filter(|p| !p.contains(['"', '\\']))
        .ok_or_else(|| format!("the path of `{name}` must be one double-quoted string"))?;
    Ok((name.to_string(), path.to_string()))
}

fn malformed(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0321", format!("bad `{MANIFEST}` line: {why}"), span)
        .with_help("a manifest holds `[packages]` and lines like `geo = \"libs/geo\"`")
}

#[cfg(test)]
mod tests {
    use super::parse;

    fn errors(text: &str) -> Vec<&str> {
        let (_, errors) = parse(text, 0);
        errors
            .iter()
            .map(|d| &text[d.span.start..d.span.end])
            .collect()
    }

    #[test]
    fn entries_parse_with_comments_and_blank_lines() {
        let text = "# deps\n\n[packages]\ngeo = \"libs/geo\"  # shapes\n  json=\"vendor/json\"\n";
        let (entries, errors) = parse(text, 10);
        assert!(errors.is_empty(), "{errors:?}");
        let pairs: Vec<(&str, &str)> = entries
            .iter()
            .map(|e| (e.name.as_str(), e.path.as_str()))
            .collect();
        assert_eq!(pairs, [("geo", "libs/geo"), ("json", "vendor/json")]);
        let geo = &entries[0].span;
        assert_eq!(
            &text[geo.start - 10..geo.end - 10],
            "geo = \"libs/geo\"  # shapes"
        );
    }

    #[test]
    fn anything_else_is_e0321() {
        assert_eq!(errors("geo = \"a\""), ["geo = \"a\""]);
        assert_eq!(errors("[deps]"), ["[deps]"]);
        let table = "[packages]\n";
        for bad in [
            "geo = libs",
            "geo = \"\"",
            "1x = \"a\"",
            "geo = \"a\" junk",
            "geo",
            "[packages]",
            "geo = \"a\\b\"",
        ] {
            let text = format!("{table}{bad}");
            assert_eq!(errors(&text), [bad], "{bad}");
        }
        assert_eq!(
            errors("[packages]\ngeo = \"a\"\ngeo = \"b\""),
            ["geo = \"b\""]
        );
    }

    #[test]
    fn messages_say_what_is_wrong() {
        let (_, errors) = parse("[packages]\ngeo = \"a\"\ngeo = \"b\"", 0);
        assert_eq!(errors[0].code, "E0321");
        assert!(
            errors[0].message.contains("listed twice"),
            "{}",
            errors[0].message
        );
    }
}
