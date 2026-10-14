//! A manifest's inline entry (v11 draft §6, milestone 37):
//! `{ path = "libs/geo", version = "1.2" }`, the keys in either order,
//! `path` required and nothing but `path` and `version` allowed.

use super::version::Requirement;

/// The path and the requirement of an inline entry, or why it isn't one.
pub(super) fn parse(value: &str) -> Result<(String, Option<Requirement>), String> {
    const SHAPE: &str = "expected `{ path = \"…\", version = \"…\" }`";
    let inner = value
        .strip_prefix('{')
        .and_then(|v| v.strip_suffix('}'))
        .ok_or(SHAPE)?;
    let (mut path, mut version): (Option<String>, Option<String>) = (None, None);
    let mut rest = inner.trim();
    while !rest.is_empty() {
        let (key, after) = rest.split_once('=').ok_or(SHAPE)?;
        let key = key.trim();
        let (text, after) = quoted(after.trim_start())
            .ok_or_else(|| format!("the value of `{key}` must be one double-quoted string"))?;
        let slot = match key {
            "path" => &mut path,
            "version" => &mut version,
            _ => return Err(format!("unknown key `{key}`: only `path` and `version`")),
        };
        if slot.replace(text).is_some() {
            return Err(format!("`{key}` is given twice"));
        }
        rest = after.trim_start();
        // Another key follows a comma; a trailing comma is fine.
        if let Some(next) = rest.strip_prefix(',') {
            rest = next.trim_start();
        } else if !rest.is_empty() {
            return Err(SHAPE.to_string());
        }
    }
    let path = path
        .filter(|p| !p.is_empty())
        .ok_or("an entry needs a non-empty `path`")?;
    let requirement = version.map(|v| Requirement::parse(&v)).transpose()?;
    Ok((path, requirement))
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
    use super::parse;

    #[test]
    fn keys_come_in_either_order() {
        let (path, req) = parse("{ path = \"libs/geo\", version = \"1.2\" }").unwrap();
        assert_eq!(path, "libs/geo");
        assert_eq!(req.map(|r| r.parts), Some(2));
        let (path, req) = parse("{version=\"1\",path=\"a,b\",}").unwrap();
        assert_eq!((path.as_str(), req.map(|r| r.parts)), ("a,b", Some(1)));
        assert_eq!(parse("{ path = \"a\" }").unwrap(), ("a".to_string(), None));
    }

    #[test]
    fn anything_else_is_refused() {
        for bad in [
            "{ version = \"1.2\" }",
            "{ path = \"a\", git = \"b\" }",
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
