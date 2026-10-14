//! A manifest's inline entry (v11 draft §6, milestone 37):
//! `{ path = "libs/geo", version = "1.2" }`, the keys in either order, and
//! (v14, milestone 50) `{ git = "https://…", version = "1.2" }`. The same
//! `{ key = "value", … }` form holds a lock file's entries (`lock.rs`).

use super::git;
use super::version::Requirement;

/// Where an entry's package comes from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Place {
    /// A folder, relative to the manifest's.
    Path(String),
    /// A git repository's URL, checked by `git::check_url` (v14).
    Git(String),
}

/// The keys and values of `{ key = "value", … }`, in order, or why the
/// text isn't one. Only `allowed` keys, each at most once.
pub(super) fn pairs(value: &str, allowed: &[&str]) -> Result<Vec<(String, String)>, String> {
    let shape = || format!("expected `{{ {} = \"…\", … }}`", allowed[0]);
    let inner = value
        .strip_prefix('{')
        .and_then(|v| v.strip_suffix('}'))
        .ok_or_else(shape)?;
    let mut found: Vec<(String, String)> = Vec::new();
    let mut rest = inner.trim();
    while !rest.is_empty() {
        let (key, after) = rest.split_once('=').ok_or_else(shape)?;
        let key = key.trim();
        let (text, after) = quoted(after.trim_start())
            .ok_or_else(|| format!("the value of `{key}` must be one double-quoted string"))?;
        if !allowed.contains(&key) {
            let keys: Vec<String> = allowed.iter().map(|k| format!("`{k}`")).collect();
            return Err(format!("unknown key `{key}`: only {}", keys.join(", ")));
        }
        if found.iter().any(|(k, _)| k == key) {
            return Err(format!("`{key}` is given twice"));
        }
        found.push((key.to_string(), text));
        rest = after.trim_start();
        // Another key follows a comma; a trailing comma is fine.
        if let Some(next) = rest.strip_prefix(',') {
            rest = next.trim_start();
        } else if !rest.is_empty() {
            return Err(shape());
        }
    }
    Ok(found)
}

/// The value of `key` in `pairs`, if given.
pub(super) fn get(pairs: &[(String, String)], key: &str) -> Option<String> {
    pairs.iter().find(|(k, _)| k == key).map(|(_, v)| v.clone())
}

/// The place and the requirement of an inline entry, or why it isn't one.
pub(super) fn parse(value: &str) -> Result<(Place, Option<Requirement>), String> {
    let found = pairs(value, &["path", "version", "git"])?;
    let version = get(&found, "version");
    let requirement = version.map(|v| Requirement::parse(&v)).transpose()?;
    let place = match (get(&found, "path"), get(&found, "git")) {
        (Some(_), Some(_)) => return Err("an entry has `path` or `git`, not both".into()),
        (Some(path), None) if !path.is_empty() => Place::Path(path),
        (None, Some(url)) => {
            git::check_url(&url)?;
            if requirement.is_none() {
                return Err("a `git` entry needs a `version`, such as `\"1.2\"`".into());
            }
            Place::Git(url)
        }
        _ => return Err("an entry needs a non-empty `path`, or a `git` URL".into()),
    };
    Ok((place, requirement))
}

/// The string at the start of `text`, between double quotes and without
/// `\`, and what follows it.
fn quoted(text: &str) -> Option<(String, &str)> {
    let body = text.strip_prefix('"')?;
    let end = body.find('"')?;
    let value = &body[..end];
    (!value.contains('\\')).then(|| (value.to_string(), &body[end + 1..]))
}

#[cfg(test)]
mod tests {
    use super::{Place, parse};

    #[test]
    fn keys_come_in_either_order() {
        let (place, req) = parse("{ path = \"libs/geo\", version = \"1.2\" }").unwrap();
        assert_eq!(place, Place::Path("libs/geo".into()));
        assert_eq!(req.map(|r| r.parts), Some(2));
        let (place, req) = parse("{version=\"1\",path=\"a,b\",}").unwrap();
        assert_eq!(
            (place, req.map(|r| r.parts)),
            (Place::Path("a,b".into()), Some(1))
        );
        assert_eq!(
            parse("{ path = \"a\" }").unwrap(),
            (Place::Path("a".into()), None)
        );
    }

    #[test]
    fn git_entries_need_a_version_and_a_safe_url_v14() {
        let (place, req) = parse("{ git = \"https://x.org/geo.git\", version = \"1.4\" }").unwrap();
        assert_eq!(place, Place::Git("https://x.org/geo.git".into()));
        assert_eq!(req.map(|r| r.parts), Some(2));
        assert!(parse("{ git = \"file:///srv/geo\", version = \"1\" }").is_ok());
        for bad in [
            "{ git = \"https://x.org/geo.git\" }",
            "{ git = \"https://x.org/g\", path = \"a\", version = \"1\" }",
            "{ git = \"ssh://x.org/g\", version = \"1\" }",
            "{ git = \"git@x.org:g\", version = \"1\" }",
            "{ git = \"ext::sh -c x\", version = \"1\" }",
            "{ git = \"https://me:token@x.org/g\", version = \"1\" }",
        ] {
            assert!(parse(bad).is_err(), "{bad}");
        }
    }

    #[test]
    fn anything_else_is_refused() {
        for bad in [
            "{ version = \"1.2\" }",
            "{ path = \"a\", url = \"b\" }",
            "{ path = \"a\", path = \"b\" }",
            "{ path = \"a\", version = \"^1\" }",
            "{ path = \"a\"",
            "{ path = a }",
            "{ path = \"a\" version = \"1\" }",
            "{ path = \"\" }",
        ] {
            assert!(parse(bad).is_err(), "{bad}");
        }
    }
}
