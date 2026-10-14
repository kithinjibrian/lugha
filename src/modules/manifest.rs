//! `lugha.toml` (v8 draft §6, PRP-058): the small subset of TOML that lists
//! a program's or a package's packages. Blank lines, `#` comments, one
//! `[packages]` table, and `name = "path"` lines; anything else is E0321.
//! v11 adds a `[package]` table with the package's `version`, and inline
//! entries `name = { path = "…", version = "…" }` with a requirement.

use super::inline;
use super::version::{Requirement, Version};
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
    /// The version it requires, if any (v11).
    pub requirement: Option<Requirement>,
    /// The whole line, in the shared offset space.
    pub span: Span,
}

/// What one manifest says.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Manifest {
    /// The `[packages]` entries, in order.
    pub entries: Vec<Entry>,
    /// The `[package]` table's version (v11).
    pub version: Option<Version>,
}

/// Which table the lines belong to.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Table {
    None,
    Packages,
    Package,
}

/// The manifest `text`, which starts at offset `base`, and an E0321 for
/// every line outside the subset.
pub fn parse(text: &str, base: usize) -> (Manifest, Vec<Diagnostic>) {
    let (mut entries, mut errors): (Vec<Entry>, Vec<Diagnostic>) = (Vec::new(), Vec::new());
    let mut version: Option<Version> = None;
    let (mut table, mut seen) = (Table::None, Vec::new());
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
        let header = match code {
            "[packages]" => Some(Table::Packages),
            "[package]" => Some(Table::Package),
            _ => None,
        };
        if let Some(next) = header {
            if seen.contains(&next) {
                errors.push(malformed(format!("`{code}` appears twice"), span));
            } else {
                seen.push(next);
                table = next;
            }
            continue;
        }
        if table == Table::Package {
            match package_line(code) {
                Ok(_) if version.is_some() => {
                    errors.push(malformed("`version` is given twice".to_string(), span));
                }
                Ok(v) => version = Some(v),
                Err(why) => errors.push(malformed(why, span)),
            }
            continue;
        }
        let entry = match (table, entry(code)) {
            (Table::Packages, Ok(parsed)) => Ok(parsed),
            (Table::Packages, Err(why)) => Err(why),
            _ => Err("an entry needs the `[packages]` table first".to_string()),
        };
        match entry {
            Ok((name, path, requirement)) => {
                if entries.iter().any(|e| e.name == name) {
                    errors.push(malformed(format!("`{name}` is listed twice"), span));
                } else {
                    entries.push(Entry {
                        name,
                        path,
                        requirement,
                        span,
                    });
                }
            }
            Err(why) => errors.push(malformed(why, span)),
        }
    }
    (Manifest { entries, version }, errors)
}

/// The `[package]` table's one line, `version = "1.4.0"` (v11).
fn package_line(code: &str) -> Result<Version, String> {
    let (key, value) = code
        .split_once('=')
        .ok_or("expected `version = \"1.4.0\"`")?;
    if key.trim() != "version" {
        return Err(format!(
            "`[package]` holds only `version`, not `{}`",
            key.trim()
        ));
    }
    let text = value
        .trim()
        .strip_prefix('"')
        .and_then(|v| v.strip_suffix('"'))
        .ok_or("the version must be one double-quoted string")?;
    Version::parse(text)
}

/// `line` without its comment.
fn strip_comment(line: &str) -> &str {
    let after_quotes = line.rfind('"').map_or(0, |i| i + 1);
    match line[after_quotes..].find('#') {
        Some(i) => &line[..after_quotes + i],
        None => line,
    }
}

/// `name = "path"` or `name = { path = "…", version = "…" }` (v11), or why
/// the line isn't one.
fn entry(code: &str) -> Result<(String, String, Option<Requirement>), String> {
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
    if value.starts_with('{') {
        let (path, requirement) = inline::parse(value)?;
        return Ok((name.to_string(), path, requirement));
    }
    let path = value
        .strip_prefix('"')
        .and_then(|v| v.strip_suffix('"'))
        .filter(|p| !p.contains(['"', '\\']))
        .ok_or_else(|| format!("the path of `{name}` must be one double-quoted string"))?;
    if path.is_empty() {
        return Err(format!("`{name}` has an empty path"));
    }
    Ok((name.to_string(), path.to_string(), None))
}

fn malformed(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0321", format!("bad `{MANIFEST}` line: {why}"), span)
        .with_help(
            "a manifest holds `[packages]` with lines like `geo = \"libs/geo\"` or \
             `geo = { path = \"libs/geo\", version = \"1.2\" }`, and `[package]` with `version = \"1.4.0\"`",
        )
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
        let (manifest, errors) = parse(text, 10);
        assert!(errors.is_empty(), "{errors:?}");
        let entries = manifest.entries;
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

    #[test]
    fn versions_and_requirements_v11() {
        let text = "[package]\nversion = \"1.4.0\"\n[packages]\ngeo = { path = \"g\", version = \"1.2\" } # c\n";
        let (manifest, found) = parse(text, 0);
        assert!(found.is_empty(), "{found:?}");
        assert_eq!(
            manifest.version.map(|v| v.to_string()),
            Some("1.4.0".into())
        );
        assert_eq!(manifest.entries[0].requirement.map(|r| r.parts), Some(2));
        let package = "[package]\n";
        for bad in ["name = \"geo\"", "version = 1.4.0", "version = \"1.4\""] {
            assert_eq!(errors(&format!("{package}{bad}")), [bad], "{bad}");
        }
        assert_eq!(errors("[package]\n[package]"), ["[package]"]);
        assert_eq!(
            errors("[package]\nversion = \"1.0.0\"\nversion = \"2.0.0\""),
            ["version = \"2.0.0\""]
        );
        let bad = "geo = { path = \"g\", version = \"x\" }";
        assert_eq!(errors(&format!("[packages]\n{bad}")), [bad]);
    }
}
