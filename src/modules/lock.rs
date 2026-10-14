//! `lugha.lock` (v14 draft §6, milestone 50): the exact commit of every git
//! package a build used, next to the main file, in the manifest's subset:
//! one `[locked]` table of `name = { git = "…", version = "…", commit = "…" }`.

use super::inline;
use super::version::Version;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// The lock file's name, next to the main file.
pub const LOCK: &str = "lugha.lock";

/// One locked package.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Locked {
    /// The package name, as the manifests list it.
    pub name: String,
    /// The repository's URL.
    pub url: String,
    /// The tag's version, `v` dropped.
    pub version: Version,
    /// The commit the tag gave: 40 lower-case hex digits.
    pub commit: String,
}

/// The entries of the lock `text`, which starts at offset `base`, and an
/// E0321 for every line outside the subset.
pub fn parse(text: &str, base: usize) -> (Vec<Locked>, Vec<Diagnostic>) {
    let (mut entries, mut errors): (Vec<Locked>, Vec<Diagnostic>) = (Vec::new(), Vec::new());
    let (mut table, mut start) = (false, 0);
    for raw in text.split_inclusive('\n') {
        let line = raw.trim_end_matches(['\n', '\r']);
        let span = Span::new(base + start, base + start + line.len());
        start += raw.len();
        let code = line.trim();
        if code.is_empty() || code.starts_with('#') {
            continue;
        }
        if code == "[locked]" && !table {
            table = true;
            continue;
        }
        match entry(code).filter(|_| table) {
            Some(Ok(locked)) if entries.iter().all(|e: &Locked| e.name != locked.name) => {
                entries.push(locked);
            }
            Some(Ok(locked)) => errors.push(malformed(
                format!("`{}` is locked twice", locked.name),
                span,
            )),
            Some(Err(why)) => errors.push(malformed(why, span)),
            None => errors.push(malformed(
                "expected `[locked]` then its entries".into(),
                span,
            )),
        }
    }
    (entries, errors)
}

/// `name = { git = "…", version = "…", commit = "…" }`, or why not.
fn entry(code: &str) -> Option<Result<Locked, String>> {
    let (name, value) = code.split_once('=')?;
    let name = name.trim();
    if name.is_empty() || !name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return Some(Err(format!("`{name}` is not a package name")));
    }
    Some(locked(name, value.trim()))
}

fn locked(name: &str, value: &str) -> Result<Locked, String> {
    let pairs = inline::pairs(value, &["git", "version", "commit"])?;
    let field = |key: &str| inline::get(&pairs, key).ok_or(format!("`{name}` needs `{key}`"));
    let url = field("git")?;
    super::git::check_url(&url)?;
    let version = Version::parse(&field("version")?)?;
    let commit = field("commit")?;
    if !super::cache::is_commit(&commit) {
        return Err(format!(
            "`{name}`'s commit must be 40 lower-case hex digits"
        ));
    }
    Ok(Locked {
        name: name.to_string(),
        url,
        version,
        commit,
    })
}

/// The lock file holding `entries`, sorted by name so it is stable.
pub fn write(entries: &[Locked]) -> String {
    let mut sorted = entries.to_vec();
    sorted.sort_by(|a, b| a.name.cmp(&b.name));
    let mut text = String::from("# Written by lughac; `lughac update` refreshes it.\n[locked]\n");
    for e in sorted {
        text.push_str(&format!(
            "{} = {{ git = \"{}\", version = \"{}\", commit = \"{}\" }}\n",
            e.name, e.url, e.version, e.commit
        ));
    }
    text
}

fn malformed(why: String, span: Span) -> Diagnostic {
    Diagnostic::error("E0321", format!("bad `{LOCK}` line: {why}"), span)
        .with_help("run `lughac update` to write it again")
}

#[cfg(test)]
mod tests {
    use super::{Locked, parse, write};
    use crate::modules::version::Version;

    const C: &str = "0123456789abcdef0123456789abcdef01234567";

    #[test]
    fn locks_round_trip() {
        let entries = vec![
            Locked {
                name: "zed".into(),
                url: "file:///srv/zed".into(),
                version: Version::parse("2.0.1").unwrap(),
                commit: C.into(),
            },
            Locked {
                name: "geo".into(),
                url: "https://x.org/geo.git".into(),
                version: Version::parse("1.4.2").unwrap(),
                commit: C.into(),
            },
        ];
        let text = write(&entries);
        assert!(text.find("geo =").unwrap() < text.find("zed =").unwrap());
        let (read, errors) = parse(&text, 0);
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(read.len(), 2);
        assert!(entries.iter().all(|e| read.contains(e)));
    }

    #[test]
    fn bad_lines_are_e0321() {
        let line = |l: &str| format!("[locked]\n{l}\n");
        let bad = [
            "geo = { git = \"https://x.org/g\", version = \"1.0.0\", commit = \"abc\" }"
                .to_string(),
            format!("geo = {{ git = \"ssh://x/g\", version = \"1.0.0\", commit = \"{C}\" }}"),
            format!("geo = {{ git = \"https://x.org/g\", version = \"1.0\", commit = \"{C}\" }}"),
            format!("geo = {{ git = \"https://x.org/g\", commit = \"{C}\" }}"),
            "junk".to_string(),
        ];
        for b in bad {
            let (_, errors) = parse(&line(&b), 0);
            assert_eq!(errors.len(), 1, "{b}");
            assert_eq!(errors[0].code, "E0321");
        }
        let (_, errors) = parse(
            &format!("geo = {{ git = \"file:///g\", version = \"1.0.0\", commit = \"{C}\" }}\n"),
            0,
        );
        assert_eq!(errors.len(), 1, "an entry before `[locked]`");
    }
}
