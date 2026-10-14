//! Package versions (v11 draft §6, milestone 37): a package's
//! `MAJOR.MINOR.PATCH`, a manifest's compatible requirement (`"1.2"` is
//! 1.2.0 or later, below 2.0.0), and E0324 when one doesn't meet the other.

use std::fmt;

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// A package's declared version.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

/// A compatible requirement: `parts` (1 to 3) numbers were written, and
/// the missing ones are 0 in `base`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Requirement {
    pub base: Version,
    pub parts: usize,
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// The decimal parts of `text`, split at `.`: digits only, no leading
/// zeros, each fitting a `u64`.
fn numbers(text: &str) -> Result<Vec<u64>, String> {
    text.split('.')
        .map(|part| {
            let digits = !part.is_empty() && part.bytes().all(|b| b.is_ascii_digit());
            if !digits || (part.len() > 1 && part.starts_with('0')) {
                return Err(format!(
                    "`{text}` is not a version: use numbers like `1.4.0`"
                ));
            }
            part.parse::<u64>()
                .map_err(|_| format!("`{part}` is too large for a version number"))
        })
        .collect()
}

impl Version {
    /// `MAJOR.MINOR.PATCH`, or why `text` isn't one.
    pub fn parse(text: &str) -> Result<Version, String> {
        match numbers(text)?[..] {
            [major, minor, patch] => Ok(Version {
                major,
                minor,
                patch,
            }),
            _ => Err(format!(
                "`{text}` is not a version: write `MAJOR.MINOR.PATCH`"
            )),
        }
    }
}

impl Requirement {
    /// `MAJOR`, `MAJOR.MINOR` or `MAJOR.MINOR.PATCH`, or why `text` isn't one.
    pub fn parse(text: &str) -> Result<Requirement, String> {
        let parts = numbers(text)?;
        if parts.len() > 3 {
            return Err(format!("`{text}` has more than three parts"));
        }
        let at = |i: usize| parts.get(i).copied().unwrap_or(0);
        Ok(Requirement {
            base: Version {
                major: at(0),
                minor: at(1),
                patch: at(2),
            },
            parts: parts.len(),
        })
    }

    /// The first version past the compatible range: the first non-zero
    /// written part goes up by one (or the last written part, when all
    /// are 0), and the parts after it become 0.
    fn ceiling(&self) -> Version {
        let written = [self.base.major, self.base.minor, self.base.patch];
        let index = written[..self.parts]
            .iter()
            .position(|&n| n != 0)
            .unwrap_or(self.parts - 1);
        let mut next = [0u64; 3];
        next[..index].copy_from_slice(&written[..index]);
        // Saturating: a version can't reach `u64::MAX + 1` anyway.
        next[index] = written[index].saturating_add(1);
        Version {
            major: next[0],
            minor: next[1],
            patch: next[2],
        }
    }

    /// True if `version` is compatible with this requirement.
    pub fn matches(&self, version: &Version) -> bool {
        self.base <= *version && *version < self.ceiling()
    }

    /// The range in words: `>= 1.2.0, < 2.0.0`.
    pub fn range(&self) -> String {
        format!(">= {}, < {}", self.base, self.ceiling())
    }
}

/// E0324 at `span`: package `name` is at `found` (or declares no version),
/// which doesn't meet `required`.
pub fn unmet(
    name: &str,
    required: &Requirement,
    found: Option<&Version>,
    span: Span,
) -> Diagnostic {
    let message = match found {
        Some(v) => format!(
            "package `{name}` is version {v}, which doesn't meet {}",
            required.range()
        ),
        None => format!(
            "package `{name}` declares no version to check against {}",
            required.range()
        ),
    };
    let help = match found {
        Some(_) => "update the package's folder, or the requirement in `lugha.toml`".to_string(),
        None => format!("add `[package]` with `version = \"…\"` to `{name}`'s `lugha.toml`"),
    };
    Diagnostic::error("E0324", message, span).with_help(help)
}

#[cfg(test)]
mod tests {
    use super::{Requirement, Version};

    fn v(text: &str) -> Version {
        Version::parse(text).unwrap_or_else(|e| panic!("{e}"))
    }

    #[test]
    fn versions_have_three_plain_numbers() {
        assert_eq!(
            v("1.4.0"),
            Version {
                major: 1,
                minor: 4,
                patch: 0
            }
        );
        assert_eq!(v("0.0.0").to_string(), "0.0.0");
        for bad in [
            "1.4",
            "01.2.3",
            "1.2.3-beta",
            "1..2",
            "",
            "1.2.3.4",
            "99999999999999999999.0.0",
        ] {
            assert!(Version::parse(bad).is_err(), "{bad}");
        }
    }

    #[test]
    fn requirements_match_compatible_versions() {
        let r = |text: &str| Requirement::parse(text).unwrap_or_else(|e| panic!("{e}"));
        let cases = [
            ("1.2", "1.2.0", true),
            ("1.2", "1.9.9", true),
            ("1.2", "1.1.9", false),
            ("1.2", "2.0.0", false),
            ("1.2.3", "1.2.2", false),
            ("0.3", "0.3.5", true),
            ("0.3", "0.4.0", false),
            ("0.0.4", "0.0.4", true),
            ("0.0.4", "0.0.5", false),
            ("0", "0.9.0", true),
            ("0", "1.0.0", false),
            ("0.0", "0.0.7", true),
            ("0.0", "0.1.0", false),
        ];
        for (req, version, expected) in cases {
            assert_eq!(r(req).matches(&v(version)), expected, "{req} vs {version}");
        }
        assert_eq!(r("1.2").range(), ">= 1.2.0, < 2.0.0");
        for bad in ["", "1.2.3.4", "x", "^1.2", ">=1"] {
            assert!(Requirement::parse(bad).is_err(), "{bad}");
        }
    }
}
