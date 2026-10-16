//! Package layout (v15 draft §6, milestone 51): where a program's manifest
//! is when none sits beside its main file (a bounded upward lookup), the
//! name check (E0327), and one file, one module (E0328).

use std::path::{Path, PathBuf};

use super::fetch::Fetcher;
use super::manifest::{self, MANIFEST};
use super::packages::Packages;
use super::paths::{exists, normalized};
use super::{Loaded, Read};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// What loading needs from outside the program's files.
pub struct LoadEnv {
    /// Fetches git packages (v14).
    pub fetcher: Fetcher,
    /// The user's home folder, where the upward lookup stops (v15).
    pub home: Option<PathBuf>,
    /// The folder relative paths start from, absolute (v15).
    pub cwd: PathBuf,
    /// Keep a module's recovered program when it has syntax errors, and
    /// follow its imports: for the language server only (v16, milestone 75).
    pub partial: bool,
    /// Load a main file inside its package's `source` folder as that
    /// package's module, for `lughac test` and `lughac check` (v17, milestone 97).
    pub as_module: bool,
}

impl LoadEnv {
    /// No git packages and no home folder: the lookup stops at a `.git`,
    /// a manifest, or the top of `cwd` (`/` here).
    pub fn plain() -> LoadEnv {
        LoadEnv {
            fetcher: Fetcher::none(),
            home: None,
            cwd: PathBuf::from("/"),
            partial: false,
            as_module: false,
        }
    }
}

/// The package name and module path of `main` when it lies inside the
/// `source` folder its manifest names (`glow/src/diff.la` → `glow`,
/// `[glow, diff]`; `glow/src/lib.la` → `[glow]`): the module a hidden entry would import (v17 draft
/// decision 43). `None` for a file outside one, or a path that can't be
/// a module's.
pub fn module_path(
    main: &Path,
    read: &mut Read<'_>,
    env: &LoadEnv,
) -> Option<(String, Vec<String>)> {
    let root = main.parent().unwrap_or(Path::new(""));
    let folder = manifest_folder(root, read, env);
    // An unreadable manifest is reported when the program loads.
    let text = read(&folder.join(MANIFEST)).ok()?;
    let (manifest, _) = manifest::parse(&String::from_utf8_lossy(&text), 0);
    let name = manifest.package.name?;
    // Compared as absolute paths: `live.la` run from `src/` is inside `../src`.
    let source = normalized(&env.cwd.join(&folder).join(manifest.package.source?));
    let inside = normalized(&env.cwd.join(main))
        .strip_prefix(&source)
        .ok()?
        .with_extension("");
    let mut path = vec![name.clone()];
    for part in inside.components() {
        match part {
            std::path::Component::Normal(p) => path.push(p.to_str()?.to_string()),
            _ => return None,
        }
    }
    // The entry, `lib.la` at the module root, is the module `N` (v17, milestone 99).
    if path.len() == 2 && path[1] == "lib" {
        path.pop();
        return Some((name, path));
    }
    (path.len() > 1).then_some((name, path))
}

/// The folder whose `lugha.toml` is the program's: `root` (the main file's
/// folder) if it has one, else the nearest parent that has one. The walk
/// stops without one at a folder holding `.git`, at `home`, or at the top,
/// so a manifest in a shared folder above a repository or a home folder is
/// never read. `root` itself when nothing is found.
pub(super) fn manifest_folder(root: &Path, read: &mut Read<'_>, env: &LoadEnv) -> PathBuf {
    let mut dir = root.to_path_buf();
    loop {
        if exists(read, &dir.join(MANIFEST)) {
            return dir;
        }
        let absolute = normalized(&env.cwd.join(&dir));
        let boundary = exists(read, &dir.join(".git")) || env.home.as_deref() == Some(&absolute);
        if boundary || absolute.parent().is_none() {
            return root.to_path_buf();
        }
        dir = normalized(&dir.join(".."));
    }
}

/// E0327 at `at`: the package listed as `key` declares `name`.
pub(super) fn misnamed(key: &str, name: &str, at: Span) -> Diagnostic {
    Diagnostic::error(
        "E0327",
        format!("this package is named `{name}`, not `{key}`"),
        at,
    )
    .with_help(format!(
        "list it as `{name} = …`, and for a shorter name write `import {name}.<module> as {key};`"
    ))
}

/// E0328 at `at`: `file` is already the module `first`, and this import
/// would make it `second` too.
pub(super) fn two_names(file: &Path, first: &[String], second: &[String], at: Span) -> Diagnostic {
    let shown = |p: &[String]| match p.is_empty() {
        true => "the main file".to_string(),
        false => format!("`{}`", p.join(".")),
    };
    Diagnostic::error(
        "E0328",
        format!(
            "`{}` is imported as both {} and {}",
            file.display(),
            shown(first),
            shown(second)
        ),
        at,
    )
    .with_help("a file is one module: import it by one name everywhere")
}

impl Packages {
    /// A manifest's `[package]` name and source (v15): E0327 when a
    /// listing's key differs from the name, the module root from
    /// `source` (E0322 if missing), and for the root's manifest, the
    /// implicit package it names, listed first so its imports resolve.
    /// `at` is the manifest's start, for what has no line of its own.
    pub(super) fn layout(
        &mut self,
        package: &Option<String>,
        (manifest, at): (&mut manifest::Manifest, Span),
        read: &mut Read<'_>,
        loaded: &mut Loaded,
    ) {
        let folder = self.folder(package);
        let table = &manifest.package;
        if let (Some(key), Some(name)) = (package, &table.name)
            && key != name
            && let Some(at) = self.listed_at.get(key)
        {
            loaded.diagnostics.push(misnamed(key, name, *at));
        }
        let root = table.source.as_ref().map(|s| normalized(&folder.join(s)));
        if let Some(root) = &root
            && !exists(read, root)
        {
            // At the listing for a package; at the manifest's start for the root's.
            let at = package
                .as_ref()
                .and_then(|p| self.listed_at.get(p).copied())
                .unwrap_or(at);
            let d = Diagnostic::error("E0322", "the `source` folder doesn't exist", at)
                .with_help(format!("there is no folder `{}`", root.display()));
            loaded.diagnostics.push(d);
        }
        match (package, &table.name) {
            (Some(key), _) => {
                if let Some(root) = root {
                    self.sources.insert(key.clone(), root);
                }
            }
            // The root's manifest names a package: the program may import it (v15).
            (None, Some(name)) => {
                let entry = manifest::Entry {
                    name: name.clone(),
                    path: ".".to_string(),
                    git: None,
                    requirement: None,
                    span: at,
                };
                if let Some(root) = root {
                    self.sources.insert(name.clone(), root);
                }
                manifest.entries.insert(0, entry);
                self.own = Some(name.clone());
            }
            (None, None) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{LoadEnv, manifest_folder};
    use std::io;
    use std::path::{Path, PathBuf};

    /// The folder `manifest_folder` picks for `root`, with these files
    /// existing and `/home/me` as home.
    fn found(root: &str, files: &[&str]) -> PathBuf {
        let env = LoadEnv {
            home: Some(PathBuf::from("/home/me")),
            cwd: PathBuf::from("/home/me/work"),
            ..LoadEnv::plain()
        };
        let files: Vec<PathBuf> = files.iter().map(PathBuf::from).collect();
        let mut read = |p: &Path| match files.contains(&super::normalized(&env.cwd.join(p))) {
            true => Ok(Vec::new()),
            false => Err(io::Error::from(io::ErrorKind::NotFound)),
        };
        manifest_folder(Path::new(root), &mut read, &env)
    }

    #[test]
    fn the_nearest_manifest_wins() {
        let lib = "/home/me/work/glow/lugha.toml";
        assert_eq!(found("glow/examples", &[lib]), PathBuf::from("glow"));
        assert_eq!(found("glow/examples/deep", &[lib]), PathBuf::from("glow"));
        let own = "/home/me/work/glow/examples/lugha.toml";
        assert_eq!(
            found("glow/examples", &[lib, own]),
            PathBuf::from("glow/examples")
        );
        // Relative to the current folder, upward through `..`.
        assert_eq!(found("", &["/home/me/lugha.toml"]), PathBuf::from(".."));
    }

    #[test]
    fn the_walk_stops_at_git_and_home() {
        let above = "/home/me/work/lugha.toml";
        assert_eq!(
            found("glow/examples", &[above, "/home/me/work/glow/.git"]),
            PathBuf::from("glow/examples")
        );
        // A repository root with a manifest is used.
        let both = ["/home/me/work/glow/.git", "/home/me/work/glow/lugha.toml"];
        assert_eq!(found("glow/examples", &both), PathBuf::from("glow"));
        // Nothing above home is read.
        assert_eq!(found("glow", &["/home/lugha.toml"]), PathBuf::from("glow"));
        assert_eq!(found("glow", &["/lugha.toml"]), PathBuf::from("glow"));
    }
}

#[cfg(test)]
#[path = "layout_tests.rs"]
mod loader_tests;
