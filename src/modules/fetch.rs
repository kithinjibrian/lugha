//! Resolving git entries (v14 draft §6, milestone 50): a locked entry uses
//! its commit, from the cache or a fresh clone that must still have it
//! (E0326); an unlocked one takes the highest `vX.Y.Z` tag meeting its
//! requirement. Every failure to fetch is E0325; `--offline` runs no `git`.

use std::io;
use std::path::{Path, PathBuf};

use super::git::{self, Git};
use super::lock::{self, LOCK, Locked};
use super::manifest::Entry;
use super::version::{Requirement, Version};
use super::{LoadError, Loaded, ManifestFile, Read, cache};
use crate::diagnostic::Diagnostic;

/// How a command may fetch.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Mode {
    /// Run no `git`: everything must be locked and cached (`--offline`).
    pub offline: bool,
    /// Ignore the lock and take the newest matching tags (`lughac update`).
    pub update: bool,
}

/// The loader's way to git packages: `git`, the cache, the lock.
pub struct Fetcher {
    git: Box<dyn Git>,
    root: Option<PathBuf>,
    mode: Mode,
    /// The lock file's entries and text, when there is one.
    lock: Vec<Locked>,
    lock_text: Option<String>,
    /// What this build used, and whether any entry failed.
    used: Vec<Locked>,
    failed: bool,
}

/// Why an entry has no folder.
enum Failure {
    /// E0325: it couldn't be fetched.
    Fetch(String),
    /// E0326: its tag no longer gives the locked commit.
    Moved(String),
    /// The cache couldn't be written, or has no root.
    Load(LoadError),
}

impl From<io::Error> for Failure {
    fn from(source: io::Error) -> Failure {
        Failure::Load(LoadError::Cache(source))
    }
}

impl Fetcher {
    /// A fetcher running `git`, with the cache at `root` (`cache::root`).
    pub fn new(git: Box<dyn Git>, root: Option<PathBuf>, mode: Mode) -> Fetcher {
        Fetcher {
            git,
            root,
            mode,
            lock: Vec::new(),
            lock_text: None,
            used: Vec::new(),
            failed: false,
        }
    }

    /// A fetcher that never fetches: git entries fail with E0325.
    pub fn none() -> Fetcher {
        let offline = Mode {
            offline: true,
            update: false,
        };
        Fetcher::new(Box::new(git::SystemGit), None, offline)
    }

    /// The new text for `lugha.lock`, if this build should write it: git
    /// packages were used, none failed, and the entries differ from the
    /// lock's (its comments and order don't count).
    pub fn lock_text(&self) -> Option<String> {
        if self.failed || (self.used.is_empty() && self.lock_text.is_none()) {
            return None;
        }
        let same = self.lock_text.is_some()
            && self.used.len() == self.lock.len()
            && self.used.iter().all(|u| self.lock.contains(u));
        (!same).then(|| lock::write(&self.used))
    }

    /// Reads `lugha.lock` from `root` into the offset space at `*base`.
    pub(super) fn read_lock(
        &mut self,
        root: &Path,
        read: &mut Read<'_>,
        base: &mut usize,
        loaded: &mut Loaded,
    ) -> Result<(), LoadError> {
        let file = root.join(LOCK);
        let text = match read(&file) {
            Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(()),
            Err(source) => return Err(LoadError::Io { path: file, source }),
        };
        let (entries, errors) = lock::parse(&text, *base);
        loaded.diagnostics.extend(errors);
        let start = *base;
        *base = start
            .checked_add(text.len() + 1)
            .ok_or(LoadError::TooLarge)?;
        loaded.manifests.push(ManifestFile {
            file,
            base: start,
            text: text.clone(),
        });
        self.lock = entries;
        self.lock_text = Some(text);
        Ok(())
    }

    /// The folder of git `entry` at `url`, or `None` with its E0325 or
    /// E0326 in `loaded`.
    pub(super) fn folder(
        &mut self,
        entry: &Entry,
        url: &str,
        loaded: &mut Loaded,
    ) -> Result<Option<PathBuf>, LoadError> {
        let requirement = entry.requirement.expect("a git entry has a version");
        let (code, why) = match self.resolve(&entry.name, url, &requirement) {
            Ok(folder) => return Ok(Some(folder)),
            Err(Failure::Load(error)) => return Err(error),
            Err(Failure::Fetch(why)) => ("E0325", why),
            Err(Failure::Moved(why)) => ("E0326", why),
        };
        self.failed = true;
        let message = format!("package `{}` can't be fetched", entry.name);
        loaded
            .diagnostics
            .push(Diagnostic::error(code, message, entry.span).with_help(why));
        Ok(None)
    }

    fn resolve(&mut self, name: &str, url: &str, req: &Requirement) -> Result<PathBuf, Failure> {
        let root = self.root.clone().ok_or(Failure::Load(LoadError::NoCache))?;
        let same = |l: &&Locked| l.name == name && l.url == url && req.matches(&l.version);
        if let Some(done) = self.used.iter().find(same) {
            return Ok(cache::folder(&root, url, &done.commit));
        }
        let locked = self
            .lock
            .iter()
            .find(same)
            .filter(|_| !self.mode.update)
            .cloned();
        let entry = match locked {
            Some(l) => {
                let folder = cache::folder(&root, url, &l.commit);
                if !folder.exists() {
                    self.offline(&format!("{name} {} isn't in the cache", l.version))?;
                    let (scratch, commit) = self.clone_at(&root, url, &l.version)?;
                    if commit != l.commit {
                        let _ = std::fs::remove_dir_all(&scratch); // a leftover is harmless
                        return Err(Failure::Moved(format!(
                            "`lugha.lock` has commit {} for v{}, but the tag now gives {commit}; \
                             run `lughac update` if the change is expected",
                            l.commit, l.version
                        )));
                    }
                    cache::install(&scratch, &folder)?;
                }
                l
            }
            None => {
                self.offline(&format!("{name} isn't in `lugha.lock`"))?;
                let tags = self.git.ls_remote(url).map_err(Failure::Fetch)?;
                let best = git::tags(&tags)
                    .into_iter()
                    .filter(|v| req.matches(v))
                    .max();
                let version = best.ok_or_else(|| {
                    Failure::Fetch(format!("no tag `vX.Y.Z` of {url} is {}", req.range()))
                })?;
                let (scratch, commit) = self.clone_at(&root, url, &version)?;
                cache::install(&scratch, &cache::folder(&root, url, &commit))?;
                Locked {
                    name: name.to_string(),
                    url: url.to_string(),
                    version,
                    commit,
                }
            }
        };
        let folder = cache::folder(&root, url, &entry.commit);
        self.used.push(entry);
        Ok(folder)
    }

    /// E0325 under `--offline`, which may not run `git`.
    fn offline(&self, why: &str) -> Result<(), Failure> {
        if self.mode.offline {
            return Err(Failure::Fetch(format!(
                "{why}, and `--offline` stops lughac fetching it; build once without `--offline`"
            )));
        }
        Ok(())
    }

    /// Clones `url` at tag `v<version>` into a scratch folder: the folder
    /// and the commit it holds.
    fn clone_at(
        &mut self,
        root: &Path,
        url: &str,
        version: &Version,
    ) -> Result<(PathBuf, String), Failure> {
        let scratch = cache::scratch(root)?;
        // The tag is rebuilt from the parsed version, never copied from `git`.
        let tag = format!("v{version}");
        let commit = self
            .git
            .clone_tag(url, &tag, &scratch)
            .and_then(|()| self.git.head(&scratch))
            .and_then(|head| match cache::is_commit(&head) {
                true => Ok(head),
                false => Err(format!("`git` gave `{head}` as the commit")),
            });
        commit.map(|c| (scratch.clone(), c)).map_err(|why| {
            let _ = std::fs::remove_dir_all(&scratch); // a leftover is harmless
            Failure::Fetch(why)
        })
    }
}

#[cfg(test)]
#[path = "fetch_tests.rs"]
mod tests;
