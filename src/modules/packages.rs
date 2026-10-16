//! Packages (v8 draft §6, PRP-058): the manifests of a build, which folder
//! each package name means, and where an import points from a module of a
//! given package (E0312, E0320, E0322, E0323), and whether each package's
//! version meets what the manifests require (E0324, v11).

use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

use super::fetch::Fetcher;
use super::link_table::{LinkKey, LinkTable, PackageLink};
use super::manifest::{self, MANIFEST};
use super::paths::{clash, exists, normalized};
use super::version::{self, Requirement, Version};
use super::{LoadError, Loaded, ManifestFile, Read, import_path_span, names};
use crate::ast::Import;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

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
    /// Where the root's `lugha.toml` is: `root`, or a parent (v15).
    manifest_folder: PathBuf,
    /// The folder of every package named in any manifest, with the entry.
    folders: HashMap<String, PathBuf>,
    /// Each package's module root, its folder joined with `source` (v15),
    /// once its manifest is read; and where it was first listed.
    pub(super) sources: HashMap<String, PathBuf>,
    pub(super) listed_at: HashMap<String, Span>,
    /// The names each package's manifest lists (`None` is the root's).
    listed: HashMap<Option<String>, Vec<String>>,
    /// Each package's declared version, once its manifest is read (v11).
    versions: HashMap<Option<String>, Option<Version>>,
    /// Every requirement of every manifest read: the package, the
    /// requirement, and its entry's line (v11).
    required: Vec<(String, Requirement, Span)>,
    /// Fetches git entries (v14).
    pub(super) fetcher: Fetcher,
    /// Git packages that couldn't be fetched: their imports are skipped,
    /// since E0325 or E0326 already says why (v14).
    unfetched: Vec<String>,
    /// The package the root's manifest names, if any: the main file and
    /// its own modules belong to it for visibility (v17, milestone 97).
    pub(super) own: Option<String>,
}

impl Packages {
    pub(super) fn new(root: PathBuf, manifest_folder: PathBuf, fetcher: Fetcher) -> Packages {
        Packages {
            manifest_folder,
            sources: HashMap::new(),
            listed_at: HashMap::new(),
            fetcher,
            unfetched: Vec::new(),
            root,
            folders: HashMap::new(),
            listed: HashMap::new(),
            versions: HashMap::new(),
            required: Vec::new(),
            own: None,
        }
    }

    /// The folder of `package`'s manifest; the root's for `None`.
    pub(super) fn folder(&self, package: &Option<String>) -> PathBuf {
        match package {
            None => self.manifest_folder.clone(),
            Some(name) => self.folders[name].clone(),
        }
    }

    /// Where `package`'s modules start: the main file's folder for `None`,
    /// else the package's folder and its `source` (v15).
    fn module_root(&self, package: &Option<String>) -> PathBuf {
        match package {
            None => self.root.clone(),
            Some(name) => self
                .sources
                .get(name)
                .unwrap_or(&self.folders[name])
                .clone(),
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
        // The standard library has no manifest (v15).
        if self.listed.contains_key(package) || package.as_deref() == Some(STD) {
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
        let (mut manifest, errors) = manifest::parse(&text, *base);
        loaded.diagnostics.extend(errors);
        let at = Span::new(*base, *base);
        self.layout(package, (&mut manifest, at), read, loaded);
        self.versions
            .insert(package.clone(), manifest.package.version);
        let start = *base;
        *base = start
            .checked_add(text.len() + 1)
            .ok_or(LoadError::TooLarge)?;
        loaded.manifests.push(ManifestFile {
            file,
            base: start,
            text,
        });
        // The root's manifest is read again as its implicit package: its
        // table is the build's once (kivuli's second report, v17).
        let seen = loaded.links.iter().any(|l| l.folder == folder);
        if !manifest.link.is_empty() && !seen {
            missing_link_files(read, &folder, &manifest.link, loaded);
            loaded.links.push(PackageLink {
                package: package.clone(),
                folder: folder.clone(),
                table: manifest.link,
            });
        }
        // The root's lock (v14), before any git entry needs it: beside the
        // manifest, or once from beside the main file, an older build's (v17).
        if package.is_none()
            && !self
                .fetcher
                .read_lock(&self.manifest_folder, read, base, loaded)?
            && normalized(&self.root) != normalized(&self.manifest_folder)
        {
            self.fetcher.moved = self.fetcher.read_lock(&self.root, read, base, loaded)?;
        }
        for entry in manifest.entries {
            let target = match &entry.git {
                None => normalized(&folder.join(&entry.path)),
                Some(url) => match self.fetcher.folder(&entry, url, loaded)? {
                    Some(folder) => folder,
                    None => {
                        self.unfetched.push(entry.name);
                        continue;
                    }
                },
            };
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
            self.listed_at
                .entry(entry.name.clone())
                .or_insert(entry.span);
            // A git entry's tag already meets its requirement (v14).
            if let Some(requirement) = entry.requirement.filter(|_| entry.git.is_none()) {
                self.required
                    .push((entry.name.clone(), requirement, entry.span));
            }
            let listed = self.listed.get_mut(package).expect("inserted above");
            listed.push(entry.name);
        }
        Ok(())
    }

    /// E0324 for every requirement its package's version doesn't meet,
    /// after every module is loaded (v11). A required package that nothing
    /// imported has its manifest read now, which may add requirements.
    pub(super) fn check_versions(
        &mut self,
        read: &mut Read<'_>,
        base: &mut usize,
        loaded: &mut Loaded,
    ) -> Result<(), LoadError> {
        // A package read only for its version has no module in the
        // program, so its `[link]` table must not reach the link (v16).
        let used = loaded.links.len();
        let mut next = 0;
        while let Some((name, requirement, span)) = self.required.get(next).cloned() {
            next += 1;
            let package = Some(name.clone());
            self.read_manifest(&package, read, base, loaded)?;
            let found = self.versions.get(&package).copied().flatten();
            if !found.is_some_and(|v| requirement.matches(&v)) {
                let d = version::unmet(&name, &requirement, found.as_ref(), span);
                loaded.diagnostics.push(d);
            }
        }
        loaded.links.truncate(used);
        Ok(())
    }

    /// True if `import` names a git package that couldn't be fetched (v14).
    pub(super) fn unfetched(&self, import: &Import) -> bool {
        self.unfetched.contains(&import.path[0].name)
    }

    /// True if `package`'s own folder has a folder `name` or a file `name.la`.
    fn local(&self, read: &mut Read<'_>, package: &Option<String>, name: &str) -> bool {
        let folder = self.module_root(package);
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
        // `std.<module>` is the bundled standard library (v15).
        if first == STD {
            return Ok(self.std_target(import, package, read, written));
        }
        let listed = self
            .listed
            .get(package)
            .is_some_and(|names| names.contains(first));
        if !listed {
            let mut path: Vec<String> = package.iter().cloned().collect();
            path.extend(written.iter().cloned());
            let mut file = self.module_root(package);
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
        // The manifest first: its `source` decides where the file is (v15).
        let first_package = Some(first.clone());
        self.read_manifest(&first_package, read, base, loaded)?;
        let mut file = self.module_root(&first_package);
        file.extend(&written[1..]);
        file.set_extension("la");
        let target = Target {
            path: written.clone(),
            file,
            package: Some(first.clone()),
        };
        // The package's entry, `lib.la` at its module root (v17, milestone 99).
        let entry = self.module_root(&first_package).join(ENTRY);
        let has_entry = exists(read, &entry);
        if written.len() == 1 && has_entry {
            let target = Target {
                file: entry,
                ..target
            };
            let error =
                (package.is_some() && self.local(read, package, first)).then(|| clash(first, at));
            return Ok((target, error));
        }
        if written.len() == 1 {
            let d = Diagnostic::error("E0312", format!("`{first}` is a package, not a module"), at)
                .with_help(format!(
                    "import one of its modules, `import {first}.<module>;`, or give it an \
                     entry: a `{ENTRY}` in its source folder is `import {first};`"
                ));
            return Ok((target, Some(d)));
        }
        if written.len() == 2 && written[1] == "lib" && has_entry {
            let d = Diagnostic::error("E0312", format!("`{first}.lib` is `{first}`'s entry"), at)
                .with_help(format!("the entry is imported as `import {first};`"));
            return Ok((target, Some(d)));
        }
        if package.is_some() && self.local(read, package, first) {
            return Ok((target, Some(clash(first, at))));
        }
        Ok((target, None))
    }
}

/// A package's entry module, at its module root (v17, milestone 99).
pub(super) const ENTRY: &str = "lib.la";

/// The reserved name of the standard library's package (v15).
pub(super) const STD: &str = "std";

impl Packages {
    /// Where `import std.<module>` leads: the embedded module, with E0312
    /// for a name `lugha::STD` lacks and E0320 when the importing package
    /// has its own `std/` folder or `std.la` (v15).
    fn std_target(
        &self,
        import: &Import,
        package: &Option<String>,
        read: &mut Read<'_>,
        written: Vec<String>,
    ) -> (Target, Option<Diagnostic>) {
        let at = import_path_span(import);
        let file = PathBuf::from(format!("<std>/{}.la", written[1..].join("/")));
        let module = written[1..].join(".");
        let known = crate::STD.iter().any(|(name, _)| *name == module);
        let target = Target {
            path: written.clone(),
            file,
            package: Some(STD.to_string()),
        };
        if package.as_deref() != Some(STD) && self.local(read, package, STD) {
            return (target, Some(clash(STD, at)));
        }
        if written.len() == 1 || !known {
            let modules: Vec<String> = crate::STD
                .iter()
                .map(|(n, _)| format!("`std.{n}`"))
                .collect();
            let d = Diagnostic::error(
                "E0312",
                format!("module `{}` not found", written.join(".")),
                at,
            )
            .with_help(format!(
                "the standard library's modules are {}",
                modules.join(", ")
            ));
            return (target, Some(d));
        }
        (target, None)
    }
}

/// E0322 for each `paths` or `include` folder and `c` file of a `[link]`
/// table that doesn't exist, at its line (v16, milestone 87).
fn missing_link_files(read: &mut Read<'_>, folder: &Path, link: &LinkTable, loaded: &mut Loaded) {
    for key in [LinkKey::Paths, LinkKey::Include, LinkKey::C] {
        let Some(line) = link.line_of(key) else {
            continue;
        };
        for value in &line.values {
            let path = folder.join(value);
            if exists(read, &path) {
                continue;
            }
            let what = if key == LinkKey::C { "file" } else { "folder" };
            let d = Diagnostic::error(
                "E0322",
                format!("the `[link]` {what} `{value}` doesn't exist"),
                line.span,
            )
            .with_help(format!("there is no {what} `{}`", path.display()));
            loaded.diagnostics.push(d);
        }
    }
}
