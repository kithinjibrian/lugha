//! A manifest's `[link]` table (v16, milestone 87, PRP-117): the C libraries,
//! search folders, header folders, C files and pkg-config packages a package
//! needs at link time. Each line is `key = ["…", …]`, one key at most once.
//!
//! Only the shape is checked here (E0321 through the manifest); whether the
//! folders and files exist is the loader's (E0322), and nothing is run.

use std::path::PathBuf;

use crate::span::Span;

/// One package's `[link]` table and the folder its paths are relative to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageLink {
    /// The package; `None` for the program's own (the root package).
    pub package: Option<String>,
    /// The folder holding the package's `lugha.toml`.
    pub folder: PathBuf,
    /// What the table lists.
    pub table: LinkTable,
}

/// What one `[link]` line lists.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkKey {
    /// `libs`: libraries, linked as `-l<name>`.
    Libs,
    /// `paths`: folders searched for libraries, `-L`.
    Paths,
    /// `include`: folders searched for this package's C files' headers, `-I`.
    Include,
    /// `c`: C files compiled into the program.
    C,
    /// `pkg-config`: package names given to the system `pkg-config`.
    PkgConfig,
}

impl LinkKey {
    /// The key as written in `lugha.toml`.
    pub fn name(self) -> &'static str {
        match self {
            LinkKey::Libs => "libs",
            LinkKey::Paths => "paths",
            LinkKey::Include => "include",
            LinkKey::C => "c",
            LinkKey::PkgConfig => "pkg-config",
        }
    }

    fn parse(text: &str) -> Option<LinkKey> {
        [
            LinkKey::Libs,
            LinkKey::Paths,
            LinkKey::Include,
            LinkKey::C,
            LinkKey::PkgConfig,
        ]
        .into_iter()
        .find(|k| k.name() == text)
    }
}

/// One `key = [...]` line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkLine {
    /// Which list it is.
    pub key: LinkKey,
    /// The strings, in order, as written.
    pub values: Vec<String>,
    /// The whole line, in the shared offset space.
    pub span: Span,
}

/// What a `[link]` table says, its lines in the order written.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LinkTable {
    /// One per key given.
    pub lines: Vec<LinkLine>,
}

impl LinkTable {
    /// Adds the line `code`, at `span`, or says why it isn't one.
    pub(super) fn line(&mut self, code: &str, span: Span) -> Result<(), String> {
        let (key, value) = code
            .split_once('=')
            .ok_or("expected `key = [\"…\"]`, such as `libs = [\"raylib\"]`")?;
        let key = key.trim();
        let key = LinkKey::parse(key).ok_or_else(|| {
            format!("`[link]` holds `libs`, `paths`, `include`, `c` and `pkg-config`, not `{key}`")
        })?;
        if self.line_of(key).is_some() {
            return Err(format!("`{}` is given twice", key.name()));
        }
        let values = strings(value.trim(), key)?;
        for value in &values {
            check(key, value)?;
        }
        self.lines.push(LinkLine { key, values, span });
        Ok(())
    }

    /// The line for `key`, if the table has one.
    pub fn line_of(&self, key: LinkKey) -> Option<&LinkLine> {
        self.lines.iter().find(|l| l.key == key)
    }

    /// The values listed under `key`; empty when it isn't given.
    pub fn values(&self, key: LinkKey) -> &[String] {
        self.line_of(key).map_or(&[], |l| &l.values)
    }

    /// True when no line lists anything: nothing changes the link.
    pub fn is_empty(&self) -> bool {
        self.lines.iter().all(|l| l.values.is_empty())
    }
}

/// The strings of a one-line array `["a", "b"]` (a trailing comma allowed).
fn strings(text: &str, key: LinkKey) -> Result<Vec<String>, String> {
    let list = || {
        format!(
            "the value of `{}` must be a list, such as `[\"a\", \"b\"]`",
            key.name()
        )
    };
    let inner = text
        .strip_prefix('[')
        .and_then(|t| t.strip_suffix(']'))
        .ok_or_else(list)?;
    let quoted = || {
        format!(
            "the items of `{}` must be double-quoted strings without `\"` or `\\`",
            key.name()
        )
    };
    let mut values = Vec::new();
    let mut rest = inner.trim_start();
    while !rest.is_empty() {
        let body = rest.strip_prefix('"').ok_or_else(quoted)?;
        let end = body.find('"').ok_or_else(quoted)?;
        let value = &body[..end];
        if value.contains('\\') {
            return Err(quoted());
        }
        values.push(value.to_string());
        rest = body[end + 1..].trim_start();
        match rest.strip_prefix(',') {
            Some(after) => rest = after.trim_start(),
            None if rest.is_empty() => {}
            None => return Err(format!("put a comma between the items of `{}`", key.name())),
        }
    }
    Ok(values)
}

/// Whether `value` may be listed under `key`. Names never start with `-`,
/// so none can be read as an option by `cc` or `pkg-config`; paths stay
/// inside the package, so a package can't point the link outside itself.
fn check(key: LinkKey, value: &str) -> Result<(), String> {
    let name = !value.is_empty()
        && !value.starts_with('-')
        && value
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || "_+.-".contains(c));
    let inside =
        !value.is_empty() && !value.starts_with('/') && value.split('/').all(|part| part != "..");
    match key {
        LinkKey::Libs if !name => Err(format!(
            "`{value}` is not a library name: write `raylib` for `libraylib.so`, using letters, digits and `_+.-`"
        )),
        LinkKey::PkgConfig if !name => Err(format!(
            "`{value}` is not a package name for pkg-config: use letters, digits and `_+.-`"
        )),
        LinkKey::Paths | LinkKey::Include | LinkKey::C if !inside => Err(format!(
            "`{value}` must be a relative path inside the package, without `..`"
        )),
        LinkKey::C if !value.ends_with(".c") => Err(format!(
            "`{value}` is not a `.c` file: `c` lists C source files"
        )),
        _ => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::{LinkKey, LinkTable};
    use crate::span::Span;

    fn table(lines: &[&str]) -> Result<LinkTable, String> {
        let mut table = LinkTable::default();
        for (i, line) in lines.iter().enumerate() {
            table.line(line, Span::new(i, i + 1))?;
        }
        Ok(table)
    }

    #[test]
    fn every_key_parses_in_order() {
        let t = table(&[
            "libs = [\"raylib\", \"m\"]",
            "paths = [\"vendor/lib\"]",
            "include=[\"vendor/include\",]",
            "c = [ \"shim/colors.c\" ]",
            "pkg-config = [\"gtk4\", \"gl\"]",
        ])
        .unwrap();
        assert_eq!(t.values(LinkKey::Libs), ["raylib", "m"]);
        assert_eq!(t.values(LinkKey::Paths), ["vendor/lib"]);
        assert_eq!(t.values(LinkKey::Include), ["vendor/include"]);
        assert_eq!(t.values(LinkKey::C), ["shim/colors.c"]);
        assert_eq!(t.values(LinkKey::PkgConfig), ["gtk4", "gl"]);
        assert_eq!(t.line_of(LinkKey::C).unwrap().span, Span::new(3, 4));
        assert!(!t.is_empty());
    }

    #[test]
    fn empty_arrays_and_missing_keys_change_nothing() {
        let t = table(&["libs = []"]).unwrap();
        assert!(t.values(LinkKey::Libs).is_empty());
        assert!(t.values(LinkKey::C).is_empty());
        assert!(t.is_empty());
        assert!(LinkTable::default().is_empty());
    }

    #[test]
    fn library_names_allow_what_real_libraries_use() {
        let t = table(&["libs = [\"gtk-4\", \"pangocairo-1.0\", \"stdc++\", \"SDL2_image\"]"]);
        assert_eq!(
            t.unwrap().values(LinkKey::Libs),
            ["gtk-4", "pangocairo-1.0", "stdc++", "SDL2_image"]
        );
    }

    #[test]
    fn bad_lines_are_refused_with_a_reason() {
        for (bad, why) in [
            ("flags = [\"-Wl,-x\"]", "`[link]` holds"),
            ("libs = \"raylib\"", "a list"),
            ("libs = [raylib]", "double-quoted"),
            ("libs = [\"raylib\"", "a list"),
            ("libs = [\"a\" \"b\"]", "comma"),
            ("libs = [\"\"]", "not a library name"),
            ("libs = [\"-lraylib\"]", "not a library name"),
            ("libs = [\"ray lib\"]", "not a library name"),
            ("libs = [\"a/b\"]", "not a library name"),
            ("pkg-config = [\"--libs\"]", "not a package name"),
            ("pkg-config = [\"gtk4 x\"]", "not a package name"),
            ("paths = [\"/usr/lib\"]", "inside the package"),
            ("paths = [\"../lib\"]", "inside the package"),
            ("include = [\"a/../../b\"]", "inside the package"),
            ("paths = [\"\"]", "inside the package"),
            ("paths = [\"a\\\\b\"]", "double-quoted"),
            ("c = [\"shim.h\"]", "`.c` file"),
            ("c = [\"/x/shim.c\"]", "inside the package"),
            ("libs", "expected"),
        ] {
            let err = table(&[bad]).expect_err(bad);
            assert!(err.contains(why), "{bad}: {err}");
        }
    }

    #[test]
    fn a_key_given_twice_is_refused() {
        let err = table(&["libs = [\"a\"]", "libs = [\"b\"]"]).unwrap_err();
        assert!(err.contains("twice"), "{err}");
    }
}
