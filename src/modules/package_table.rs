//! A manifest's `[package]` table: `version` (v11), and (v15, milestone 51)
//! `name`, the package's own name, and `source`, the folder its modules
//! start in. Each at most once.

use super::version::Version;

/// What a `[package]` table says.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PackageTable {
    /// `version = "1.4.0"` (v11).
    pub version: Option<Version>,
    /// `name = "glowworm"` (v15): a Lugha identifier.
    pub name: Option<String>,
    /// `source = "src"` (v15): relative, with `/`, inside the package.
    pub source: Option<String>,
}

impl PackageTable {
    /// Adds one line of the table, or says why it isn't one.
    pub(super) fn line(&mut self, code: &str) -> Result<(), String> {
        let (key, value) = code
            .split_once('=')
            .ok_or("expected `key = \"value\"`, such as `version = \"1.4.0\"`")?;
        let key = key.trim();
        let text = value
            .trim()
            .strip_prefix('"')
            .and_then(|v| v.strip_suffix('"'))
            .filter(|v| !v.contains(['"', '\\']))
            .ok_or_else(|| format!("the value of `{key}` must be one double-quoted string"))?;
        let twice = || format!("`{key}` is given twice");
        match key {
            "version" if self.version.is_some() => return Err(twice()),
            "version" => self.version = Some(Version::parse(text)?),
            "name" if self.name.is_some() => return Err(twice()),
            "name" => self.name = Some(identifier(text)?),
            "source" if self.source.is_some() => return Err(twice()),
            "source" => self.source = Some(relative(text)?),
            _ => {
                return Err(format!(
                    "`[package]` holds `version`, `name` and `source`, not `{key}`"
                ));
            }
        }
        Ok(())
    }
}

/// `text` if it is a Lugha identifier, as package names are.
fn identifier(text: &str) -> Result<String, String> {
    let mut chars = text.chars();
    let first = chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_');
    if text == "std" {
        return Err("`std` is the standard library's name".to_string());
    }
    if first && chars.all(|c| c.is_ascii_alphanumeric() || c == '_') {
        return Ok(text.to_string());
    }
    Err(format!(
        "`{text}` is not a package name: use letters, digits and `_`"
    ))
}

/// `text` if it is a folder inside the package: relative, not empty, and
/// without `..`, so a package's modules can never come from outside it.
fn relative(text: &str) -> Result<String, String> {
    let inside =
        !text.is_empty() && !text.starts_with('/') && text.split('/').all(|part| part != "..");
    if inside {
        return Ok(text.to_string());
    }
    Err(format!(
        "`source = \"{text}\"` must be a folder inside the package, such as \"src\""
    ))
}

#[cfg(test)]
mod tests {
    use super::PackageTable;

    #[test]
    fn name_and_source_v15() {
        let mut table = PackageTable::default();
        for line in [
            "name = \"glowworm\"",
            "source = \"src/lib\"",
            "version = \"0.1.0\"",
        ] {
            table.line(line).unwrap();
        }
        assert_eq!(table.name.as_deref(), Some("glowworm"));
        assert_eq!(table.source.as_deref(), Some("src/lib"));
        assert_eq!(table.version.map(|v| v.to_string()), Some("0.1.0".into()));
        assert!(table.line("name = \"again\"").is_err());
    }

    #[test]
    fn bad_lines_are_refused() {
        for bad in [
            "name = \"1x\"",
            "name = \"glow-worm\"",
            "name = glowworm",
            "source = \"\"",
            "source = \"/abs\"",
            "source = \"../up\"",
            "source = \"src/../..\"",
            "author = \"me\"",
            "version = \"1.4\"",
        ] {
            assert!(PackageTable::default().line(bad).is_err(), "{bad}");
        }
    }
}
