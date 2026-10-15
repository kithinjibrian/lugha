//! Which main files to check when a document changes (v16 draft decision 14).
//!
//! `lughac` has no default entry point (spec §11), so a module reaches its
//! main files through the `lugha.entries` setting or through a main file
//! the server has already checked that loaded it.

use std::path::{Path, PathBuf};

use super::documents::normalize;
use crate::lexer::{self, token::TokenKind};

/// A main file to check, and whether E0303 is dropped from its results.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct Target {
    pub main: PathBuf,
    /// A module checked alone has no `main` by design.
    pub alone: bool,
}

/// The checks for one changed file, and entries that couldn't be used.
#[derive(Debug, PartialEq)]
pub(super) struct Plan {
    pub targets: Vec<Target>,
    pub rejected: Vec<(String, &'static str)>,
}

/// Whether `text` declares `fun main` at the top level.
///
/// Read from tokens, so comments and strings don't count. Text the lexer
/// rejects falls back to a line starting with `fun main(`.
pub(super) fn has_main(text: &str) -> bool {
    let Ok((tokens, _)) = lexer::lex(text) else {
        return text.lines().any(main_line);
    };
    let mut depth = 0usize;
    tokens.windows(3).any(|w| {
        match w[0].kind {
            TokenKind::LBrace => depth += 1,
            TokenKind::RBrace => depth = depth.saturating_sub(1),
            _ => {}
        }
        depth == 0
            && w[0].kind == TokenKind::Fun
            && w[1].kind == TokenKind::Ident("main".to_string())
            && w[2].kind == TokenKind::LParen
    })
}

/// A line starting `fun main(` or `public fun main(`.
fn main_line(line: &str) -> bool {
    let line = line.trim_start();
    let line = line.strip_prefix("public").map_or(line, str::trim_start);
    let Some(rest) = line.strip_prefix("fun") else {
        return false;
    };
    rest.starts_with(char::is_whitespace)
        && rest
            .trim_start()
            .strip_prefix("main")
            .is_some_and(|r| r.trim_start().starts_with('('))
}

/// Plans the checks for `file` with contents `text`.
///
/// A file with `main` checks itself. A module checks every valid entry of
/// its workspace folder (the longest of `folders` holding it) and every
/// main in `seen` (mains whose last check loaded it); with none, itself
/// alone.
pub(super) fn plan(
    file: &Path,
    text: &str,
    folders: &[PathBuf],
    entries: &[String],
    seen: &[PathBuf],
    exists: impl Fn(&Path) -> bool,
) -> Plan {
    let file = normalize(file);
    if has_main(text) {
        return Plan {
            targets: vec![Target {
                main: file,
                alone: false,
            }],
            rejected: Vec::new(),
        };
    }
    let mut targets: Vec<Target> = Vec::new();
    let mut add = |t: Target| {
        if !targets.contains(&t) {
            targets.push(t);
        }
    };
    let mut rejected = Vec::new();
    let folder = folders
        .iter()
        .filter(|f| file.starts_with(f))
        .max_by_key(|f| f.components().count());
    if let Some(folder) = folder {
        for entry in entries {
            match invalid(entry, folder, &exists) {
                Some(reason) => rejected.push((entry.clone(), reason)),
                None => add(Target {
                    main: normalize(&folder.join(entry)),
                    alone: false,
                }),
            }
        }
    }
    for main in seen {
        add(Target {
            main: normalize(main),
            alone: false,
        });
    }
    if targets.is_empty() {
        targets.push(Target {
            main: file,
            alone: true,
        });
    }
    Plan { targets, rejected }
}

fn invalid(entry: &str, folder: &Path, exists: impl Fn(&Path) -> bool) -> Option<&'static str> {
    if Path::new(entry).is_absolute() {
        return Some("not a relative path");
    }
    let resolved = normalize(&folder.join(entry));
    // The setting may come from a cloned repository's settings, so it can't
    // name files outside the folder it belongs to.
    if !resolved.starts_with(folder) {
        return Some("outside the workspace folder");
    }
    if !entry.ends_with(".la") {
        return Some("not a .la file");
    }
    if !exists(&resolved) {
        return Some("not found");
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn main_is_found_in_tokens_only() {
        assert!(has_main("fun main() {}"));
        assert!(has_main("import x;\npublic fun main(): i32 { return 0; }"));
        assert!(!has_main("// fun main() {}\nfun f() {}"));
        assert!(!has_main("fun f() { let s = \"fun main()\"; }"));
        assert!(!has_main("fun mainly() {}"));
        assert!(!has_main("impl T { fun main(self) {} }"));
    }

    #[test]
    fn text_the_lexer_rejects_falls_back_to_lines() {
        assert!(has_main("fun main() { let s = \"open\n}"));
        assert!(!has_main("fun f() { @ }"));
    }

    fn present(p: &Path) -> bool {
        [Path::new("/w/src/main.la"), Path::new("/w/ex/demo.la")].contains(&p)
    }

    fn target(main: &str, alone: bool) -> Target {
        Target {
            main: PathBuf::from(main),
            alone,
        }
    }

    #[test]
    fn a_file_with_main_checks_itself() {
        let folders = [PathBuf::from("/w")];
        let plan = plan(
            Path::new("/w/src/main.la"),
            "fun main() {}",
            &folders,
            &["ex/demo.la".into()],
            &[],
            present,
        );
        assert_eq!(plan.targets, [target("/w/src/main.la", false)]);
    }

    #[test]
    fn a_module_checks_entries_and_seen_mains_once_each() {
        let folders = [PathBuf::from("/"), PathBuf::from("/w")];
        let seen = [
            PathBuf::from("/w/src/main.la"),
            PathBuf::from("/w/other/main.la"),
        ];
        let entries = ["src/main.la".to_string(), "ex/demo.la".to_string()];
        let plan = plan(
            Path::new("/w/src/util.la"),
            "fun f() {}",
            &folders,
            &entries,
            &seen,
            present,
        );
        assert_eq!(
            plan.targets,
            [
                target("/w/src/main.la", false),
                target("/w/ex/demo.la", false),
                target("/w/other/main.la", false)
            ]
        );
    }

    #[test]
    fn a_module_with_nothing_checks_alone() {
        let plan = plan(
            Path::new("/w/src/util.la"),
            "fun f() {}",
            &[],
            &["src/main.la".into()],
            &[],
            present,
        );
        assert_eq!(plan.targets, [target("/w/src/util.la", true)]);
    }

    #[test]
    fn bad_entries_are_rejected_with_reasons() {
        let folders = [PathBuf::from("/w")];
        let entries: Vec<String> = [
            "/abs.la",
            "../out.la",
            "src/main.txt",
            "src/gone.la",
            "src/main.la",
        ]
        .iter()
        .map(|e| e.to_string())
        .collect();
        let plan = plan(
            Path::new("/w/src/util.la"),
            "",
            &folders,
            &entries,
            &[],
            present,
        );
        assert_eq!(plan.targets, [target("/w/src/main.la", false)]);
        assert_eq!(
            plan.rejected,
            [
                ("/abs.la".to_string(), "not a relative path"),
                ("../out.la".to_string(), "outside the workspace folder"),
                ("src/main.txt".to_string(), "not a .la file"),
                ("src/gone.la".to_string(), "not found"),
            ]
        );
    }
}
