//! Compiled C objects kept between builds (v17, milestone 127, PRP-159):
//! the runtime and packages' `[link]` C files. An entry lives in
//! `<root>/objects/<key>/` (`<root>` as for git packages) and holds the
//! object with what made it — the source's bytes, its flags, the
//! compiler's `--version` text — a fingerprint of the object, and one of
//! every header the compile read (from `cc -MD`). A hit needs them all unchanged; anything
//! else compiles as before. The cache never fails a build: any problem
//! with it means compiling into the build's own temp folder, silently.
//! It is trusted as the git package cache beside it is (decision 313).

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsString;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use super::LinkError;
use super::compiler::Compiler;

/// The object cache of one build.
pub(super) struct Cache {
    /// `<root>/objects`, or `None` when nothing is cached.
    objects: Option<PathBuf>,
    /// The compiler's words and its `--version` output (v17, milestone 132).
    compiler: String,
    /// The C compiler this build runs (`$LUGHA_CC` or `cc`).
    cc: Compiler,
    /// Header fingerprints already taken in this build.
    prints: Fingerprints,
}

/// Each header's fingerprint, taken once per build: lugha-gtk's five C
/// files read the same ~300 GTK headers, which can't change mid-build.
#[derive(Default)]
pub(super) struct Fingerprints(RefCell<HashMap<PathBuf, Option<u64>>>);

impl Fingerprints {
    /// `path`'s contents' fingerprint; `None` when it can't be read.
    fn of(&self, path: &Path) -> Option<u64> {
        if let Some(found) = self.0.borrow().get(path) {
            return *found;
        }
        let print = std::fs::read(path).ok().map(|b| fnv1a(&b));
        self.0.borrow_mut().insert(path.to_path_buf(), print);
        print
    }
}

impl Cache {
    /// The cache under the user's cache root for objects `cc` compiles, or
    /// one that caches nothing when there is no root or `cc --version`
    /// doesn't run.
    pub(super) fn open(cc: Compiler) -> Cache {
        let objects =
            crate::modules::cache::root(|n| std::env::var_os(n)).map(|r| r.join("objects"));
        // The words count too: `zig cc` for two targets prints one version.
        let compiler = cc
            .command()
            .arg("--version")
            .output()
            .ok()
            .filter(|o| o.status.success())
            .map(|o| format!("{}\n{}", cc.words(), String::from_utf8_lossy(&o.stdout)));
        match compiler {
            Some(compiler) => Cache {
                objects,
                compiler,
                cc,
                prints: Fingerprints::default(),
            },
            None => Cache {
                objects: None,
                compiler: String::new(),
                cc,
                prints: Fingerprints::default(),
            },
        }
    }

    /// The C compiler this build runs.
    pub(super) fn cc(&self) -> &Compiler {
        &self.cc
    }

    /// The cache's root (`<root>` above), when it has one: the bundled
    /// collector's sources live in its `gc/` (milestone 131).
    pub(super) fn root(&self) -> Option<&Path> {
        self.objects.as_deref().and_then(Path::parent)
    }

    /// The object for `source` (of `kind`, `"runtime"` or `"c"`) compiled
    /// with `flags`: the cache's when it has a fresh one; else `compile`
    /// builds `temp_object` (given extra arguments for the dependency file),
    /// the result is cached, and `temp_object` is returned.
    ///
    /// # Errors
    ///
    /// Only `compile`'s: a cache problem falls back to compiling.
    pub(super) fn object(
        &self,
        kind: &str,
        source: &[u8],
        flags: &[OsString],
        temp_object: &Path,
        compile: impl FnOnce(&[OsString]) -> Result<(), LinkError>,
    ) -> Result<PathBuf, LinkError> {
        let entry = Entry {
            compiler: self.compiler.clone(),
            source: source.to_vec(),
            flags: flags.to_vec(),
        };
        let Some(objects) = self
            .objects
            .as_ref()
            .filter(|_| flags_text(flags).is_some())
        else {
            compile(&[])?;
            return Ok(temp_object.to_path_buf());
        };
        let key = key(&self.compiler, kind, source, flags);
        if let Some(hit) = entry.hit(&objects.join(&key), &self.prints) {
            return Ok(hit);
        }
        let deps = temp_object.with_extension("d");
        let extra = [
            OsString::from("-MD"),
            OsString::from("-MF"),
            deps.clone().into(),
        ];
        compile(&extra)?;
        if let Ok(text) = std::fs::read_to_string(&deps) {
            let headers: Vec<PathBuf> = headers_in(&text).into_iter().map(absolute).collect();
            // A cache that can't be written is simply not used (decision 312).
            let _ = entry.install(objects, &key, temp_object, (&headers, &self.prints));
        }
        Ok(temp_object.to_path_buf())
    }
}

/// What made an object: compared byte for byte on a hit.
pub(super) struct Entry {
    pub compiler: String,
    pub source: Vec<u8>,
    pub flags: Vec<OsString>,
}

impl Entry {
    /// The cached object in `dir` when it was made from this entry's inputs
    /// and every header it read is unchanged; `None` for anything else.
    pub(super) fn hit(&self, dir: &Path, prints: &Fingerprints) -> Option<PathBuf> {
        let read = |name: &str| std::fs::read(dir.join(name)).ok();
        let same = read("compiler")? == self.compiler.as_bytes()
            && read("source")? == self.source
            && read("flags")? == flags_text(&self.flags)?.as_bytes();
        if !same {
            return None;
        }
        let headers = String::from_utf8(read("headers")?).ok()?;
        for line in headers.lines() {
            let (fingerprint, path) = line.split_once(' ')?;
            let wanted = u64::from_str_radix(fingerprint, 16).ok()?;
            if prints.of(Path::new(path))? != wanted {
                return None;
            }
        }
        // A damaged object (a crash mid-copy, a stray edit) is a miss, not a broken link.
        let object = dir.join("object.o");
        let sum = u64::from_str_radix(String::from_utf8(read("object")?).ok()?.trim(), 16).ok()?;
        (fnv1a(&std::fs::read(&object).ok()?) == sum).then_some(object)
    }

    /// Stores `object` and this entry as `<objects>/<key>`, through a
    /// temporary folder renamed into place, so a concurrent build sees a
    /// whole entry or none. An older entry for `key` is replaced.
    pub(super) fn install(
        &self,
        objects: &Path,
        key: &str,
        object: &Path,
        (headers, prints): (&[PathBuf], &Fingerprints),
    ) -> io::Result<()> {
        // Generated hex only: the entry can't name anything outside `objects`.
        debug_assert!(!key.is_empty() && key.chars().all(|c| c.is_ascii_alphanumeric()));
        let flags = flags_text(&self.flags).ok_or_else(|| io::Error::other("flags"))?;
        let mut lines = String::new();
        for header in headers {
            let fingerprint = prints
                .of(header)
                .ok_or_else(|| io::Error::other("an unreadable header"))?;
            let path = header.to_str().ok_or_else(|| io::Error::other("path"))?;
            lines.push_str(&format!("{fingerprint:016x} {path}\n"));
        }
        std::fs::create_dir_all(objects)?;
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let tmp = objects.join(format!(".tmp-{}-{n}", std::process::id()));
        std::fs::create_dir(&tmp)?;
        let written = (|| {
            let bytes = std::fs::read(object)?;
            std::fs::write(tmp.join("object.o"), &bytes)?;
            std::fs::write(tmp.join("object"), format!("{:016x}\n", fnv1a(&bytes)))?;
            std::fs::write(tmp.join("compiler"), &self.compiler)?;
            std::fs::write(tmp.join("source"), &self.source)?;
            std::fs::write(tmp.join("flags"), flags)?;
            std::fs::write(tmp.join("headers"), lines)?;
            let target = objects.join(key);
            if target.exists() {
                std::fs::remove_dir_all(&target)?;
            }
            std::fs::rename(&tmp, &target)
        })();
        if written.is_err() {
            // Best effort: a leftover temporary folder only takes space.
            let _ = std::fs::remove_dir_all(&tmp);
        }
        written
    }
}

/// The entry's key: the FNV-1a fingerprint, in hex, of lughac's version,
/// the compiler, the kind, the source and the flags, each length-prefixed
/// so field boundaries count.
pub(super) fn key(compiler: &str, kind: &str, source: &[u8], flags: &[OsString]) -> String {
    let mut bytes = Vec::new();
    let mut field = |b: &[u8]| {
        bytes.extend_from_slice(&(b.len() as u64).to_le_bytes());
        bytes.extend_from_slice(b);
    };
    field(env!("CARGO_PKG_VERSION").as_bytes());
    field(compiler.as_bytes());
    field(kind.as_bytes());
    field(source);
    for flag in flags {
        field(flag.as_encoded_bytes());
    }
    format!("{:016x}", fnv1a(&bytes))
}

/// The flags one per line; `None` when one can't be written so (not
/// UTF-8, or holding a newline), and the file is then not cached.
fn flags_text(flags: &[OsString]) -> Option<String> {
    let mut text = String::new();
    for flag in flags {
        let flag = flag.to_str().filter(|f| !f.contains('\n'))?;
        text.push_str(flag);
        text.push('\n');
    }
    Some(text)
}

/// 64-bit FNV-1a.
pub(super) fn fnv1a(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |h, &b| {
        (h ^ u64::from(b)).wrapping_mul(0x100000001b3)
    })
}

/// The headers a `cc -MD` dependency file lists: its prerequisites after
/// the first (the source itself), with `\ ` read as a space.
pub(super) fn headers_in(deps: &str) -> Vec<PathBuf> {
    let joined = deps.replace("\\\n", " ");
    let Some((_, prerequisites)) = joined.split_once(": ") else {
        return Vec::new();
    };
    let mut words = Vec::new();
    let mut word = String::new();
    let mut chars = prerequisites.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\\' if chars.peek() == Some(&' ') => {
                word.push(' ');
                chars.next();
            }
            '$' if chars.peek() == Some(&'$') => {
                word.push('$');
                chars.next();
            }
            c if c.is_whitespace() => {
                if !word.is_empty() {
                    words.push(std::mem::take(&mut word));
                }
            }
            c => word.push(c),
        }
    }
    if !word.is_empty() {
        words.push(word);
    }
    words.into_iter().skip(1).map(PathBuf::from).collect()
}

/// `path`, made absolute against the current folder, so a later build
/// from elsewhere checks the same file.
fn absolute(path: PathBuf) -> PathBuf {
    if path.is_absolute() {
        return path;
    }
    std::env::current_dir().map_or(path.clone(), |cwd| cwd.join(path))
}

#[cfg(test)]
#[path = "object_cache_tests.rs"]
mod tests;
