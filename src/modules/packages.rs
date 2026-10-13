//! Packages (v8 draft §6, PRP-058): the manifests of a build, which folder
//! each package name means, and where an import points from a module of a
//! given package (E0312, E0320, E0322, E0323).

use std::collections::HashMap;
use std::io;
use std::path::{Component, Path, PathBuf};

use super::manifest::{self, MANIFEST};
use super::{LoadError, Loaded, ManifestFile, Read, import_path_span, names};
use crate::ast::Import;
use crate::diagnostic::Diagnostic;

/// Where an import leads.
pub(super) struct Target {
    /// The module path: a package's modules start with the package name.
    pub path: Vec<String>,
    /// The file to read.
    pub file: PathBuf,
    /// The package the module belongs to; `None` for the root package.
    pub package: Option<String>,
}

/// Every package of a build, and the names each manifest lists.
pub(super) struct Packages {
    /// The main file's folder: the root package.
    root: PathBuf,
    /// The folder of every package named in any manifest, with the entry.
    folders: HashMap<String, PathBuf>,
    /// The names each package's manifest lists (`None` is the root's).
    listed: HashMap<Option<String>, Vec<String>>,
}

impl Packages {
    pub(super) fn new(root: PathBuf) -> Packages {
        Packages {
            root,
            folders: HashMap::new(),
            listed: HashMap::new(),
        }
    }

    /// The folder of `package`; the root's for `None`.
    fn folder(&self, package: &Option<String>) -> PathBuf {
        match package {
            None => self.root.clone(),
            Some(name) => self.folders[name].clone(),
        }
    }

    /// Reads `package`'s manifest once, if it has one: its text joins the
    /// offset space at `*base`, and each entry is checked (E0320, E0322, E0323).
    pub(super) fn read_manifest(
        &mut self,
        package: &Option<String>,
        read: &mut Read<'_>,
        base: &mut usize,
        loaded: &mut Loaded,
    ) -> Result<(), LoadError> {
        if self.listed.contains_key(package) {
            return Ok(());
        }
        self.listed.insert(package.clone(), Vec::new());
        let folder = self.folder(package);
        let file = folder.join(MANIFEST);
        let text = match read(&file) {
            Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(()),
            Err(source) => return Err(LoadError::Io { path: file, source }),
        };
        let (entries, errors) = manifest::parse(&text, *base);
        loaded.diagnostics.extend(errors);
        let start = *base;
        *base = start
            .checked_add(text.len() + 1)
            .ok_or(LoadError::TooLarge)?;
        loaded.manifests.push(ManifestFile {
            file,
            base: start,
            text,
        });
        for entry in entries {
            let target = normalized(&folder.join(&entry.path));
            if !exists(read, &target) {
                let d = Diagnostic::error(
                    "E0322",
                    format!("package `{}`'s folder doesn't exist", entry.name),
                    entry.span,
                )
                .with_help(format!("there is no folder `{}`", target.display()));
                loaded.diagnostics.push(d);
                continue;
            }
            if let Some(other) = self.folders.get(&entry.name).filter(|f| **f != target) {
                let d = Diagnostic::error(
                    "E0323",
                    format!("package `{}` names two folders", entry.name),
                    entry.span,
                )
                .with_help(format!(
                    "another manifest gives `{}` as `{}`; one name must mean one folder",
                    entry.name,
                    other.display()
                ));
                loaded.diagnostics.push(d);
                continue;
            }
            // The root's modules have no package prefix, so no package may
            // share a name with one of the root's own folders or files.
            // Still listed afterwards, so its imports don't also fail as missing.
            if self.local(read, &None, &entry.name) {
                loaded.diagnostics.push(clash(&entry.name, entry.span));
            }
            self.folders.insert(entry.name.clone(), target);
            let listed = self.listed.get_mut(package).expect("inserted above");
            listed.push(entry.name);
        }
        Ok(())
    }

    /// True if `package`'s own folder has a folder `name` or a file `name.la`.
    fn local(&self, read: &mut Read<'_>, package: &Option<String>, name: &str) -> bool {
        let folder = self.folder(package);
        exists(read, &folder.join(name)) || exists(read, &folder.join(format!("{name}.la")))
    }

    /// Where `import`, written in a module of `package`, leads, or the
    /// diagnostic that stops it: E0312 for a bare package, E0320 for a name
    /// that is both a listed package and a local folder.
    pub(super) fn resolve(
        &mut self,
        import: &Import,
        package: &Option<String>,
        read: &mut Read<'_>,
        base: &mut usize,
        loaded: &mut Loaded,
    ) -> Result<(Target, Option<Diagnostic>), LoadError> {
        let written = names(import);
        let first = &written[0];
        let listed = self
            .listed
            .get(package)
            .is_some_and(|names| names.contains(first));
        if !listed {
            let mut path: Vec<String> = package.iter().cloned().collect();
            path.extend(written.iter().cloned());
            let mut file = self.folder(package);
            file.extend(&written);
            file.set_extension("la");
            let package = package.clone();
            return Ok((
                Target {
                    path,
                    file,
                    package,
                },
                None,
            ));
        }
        let at = import_path_span(import);
        let mut file = self.folders[first].clone();
        file.extend(&written[1..]);
        file.set_extension("la");
        let target = Target {
            path: written.clone(),
            file,
            package: Some(first.clone()),
        };
        if written.len() == 1 {
            let d = Diagnostic::error("E0312", format!("`{first}` is a package, not a module"), at)
                .with_help(format!(
                    "import one of its modules: `import {first}.<module>;`"
                ));
            return Ok((target, Some(d)));
        }
        if package.is_some() && self.local(read, package, first) {
            return Ok((target, Some(clash(first, at))));
        }
        self.read_manifest(&Some(first.clone()), read, base, loaded)?;
        Ok((target, None))
    }
}

/// E0320 at `at`: `name` is a package and a local folder or file.
fn clash(name: &str, at: crate::span::Span) -> Diagnostic {
    Diagnostic::error(
        "E0320",
        format!("`{name}` is both a package and a local module"),
        at,
    )
    .with_help(format!(
        "rename the folder `{name}/` (or `{name}.la`) or the package in `lugha.toml`"
    ))
}

/// True if `path` exists. Reading a folder fails with an error other than
/// `NotFound`, as `std::fs::read` does, so one callback serves both.
fn exists(read: &mut Read<'_>, path: &Path) -> bool {
    match read(path) {
        Ok(_) => true,
        Err(e) => e.kind() != io::ErrorKind::NotFound,
    }
}

/// `path` with `.` and `..` resolved lexically, as written.
fn normalized(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for part in path.components() {
        match part {
            Component::CurDir => {}
            Component::ParentDir
                if matches!(out.components().next_back(), Some(Component::Normal(_))) =>
            {
                out.pop();
            }
            other => out.push(other),
        }
    }
    out
}
