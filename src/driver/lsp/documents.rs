//! Open documents, the reader that prefers their unsaved text to the disk,
//! and the `file:` URIs that name them.

use std::collections::HashMap;
use std::io;
use std::path::{Component, Path, PathBuf};

/// The documents the editor has open, by normalized path.
#[derive(Default, Clone)]
pub(super) struct Documents {
    open: HashMap<PathBuf, String>,
}

impl Documents {
    /// Records `path`'s text, opened or changed.
    pub(super) fn set(&mut self, path: &Path, text: String) {
        self.open.insert(normalize(path), text);
    }

    /// Forgets `path`: the disk is its text again.
    pub(super) fn close(&mut self, path: &Path) {
        self.open.remove(&normalize(path));
    }

    /// The open text of `path`, if the editor has it open.
    pub(super) fn text(&self, path: &Path) -> Option<&str> {
        self.open.get(&normalize(path)).map(String::as_str)
    }

    /// Every open document's path.
    pub(super) fn paths(&self) -> Vec<PathBuf> {
        let mut paths: Vec<PathBuf> = self.open.keys().cloned().collect();
        paths.sort();
        paths
    }

    /// Reads `file` as the module loader wants it: the open text, else the disk.
    pub(super) fn read(&self, file: &Path) -> io::Result<Vec<u8>> {
        match self.text(file) {
            Some(text) => Ok(text.as_bytes().to_vec()),
            None => std::fs::read(file),
        }
    }
}

/// Resolves `.` and `..` without touching the disk, so `src/../geo.la` from
/// the loader and `geo.la` from the editor are one document.
pub(super) fn normalize(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for part in path.components() {
        match part {
            Component::CurDir => {}
            Component::ParentDir => {
                out.pop();
            }
            other => out.push(other),
        }
    }
    out
}

/// The path of a `file:` URI (`file:///a%20b/x.la` → `/a b/x.la`); `None`
/// for any other scheme, a remote host or bad percent-encoding.
pub(super) fn uri_to_path(uri: &str) -> Option<PathBuf> {
    let rest = uri.strip_prefix("file://")?;
    // `file:///x` has an empty host; `file://localhost/x` names this machine.
    let rest = rest.strip_prefix("localhost").unwrap_or(rest);
    if !rest.starts_with('/') {
        return None;
    }
    let bytes = rest.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            let hex = std::str::from_utf8(bytes.get(i + 1..i + 3)?).ok()?;
            out.push(u8::from_str_radix(hex, 16).ok()?);
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    let path = PathBuf::from(String::from_utf8(out).ok()?);
    // A `..` in a URI would name a file outside what the editor showed.
    Some(normalize(&path))
}

/// The `file:` URI of an absolute `path`, percent-encoding all but the
/// characters VS Code itself leaves as they are.
pub(super) fn path_to_uri(path: &Path) -> String {
    let mut uri = String::from("file://");
    for &b in path.to_string_lossy().as_bytes() {
        if b.is_ascii_alphanumeric() || b"/-._~".contains(&b) {
            uri.push(b as char);
        } else {
            uri.push_str(&format!("%{b:02X}"));
        }
    }
    uri
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn uris_round_trip_through_paths() {
        let path = Path::new("/home/me/my proj/é.la");
        let uri = path_to_uri(path);
        assert_eq!(uri, "file:///home/me/my%20proj/%C3%A9.la");
        assert_eq!(uri_to_path(&uri).unwrap(), path);
        assert_eq!(
            uri_to_path("file://localhost/x.la").unwrap(),
            Path::new("/x.la")
        );
        assert_eq!(
            uri_to_path("file:///a/../b.la").unwrap(),
            Path::new("/b.la")
        );
    }

    #[test]
    fn other_uris_have_no_path() {
        assert_eq!(uri_to_path("untitled:Untitled-1"), None);
        assert_eq!(uri_to_path("file://server/share/x.la"), None);
        assert_eq!(uri_to_path("file:///bad%zz.la"), None);
        assert_eq!(uri_to_path("file:///cut%4"), None);
    }

    #[test]
    fn open_text_wins_over_the_disk() {
        let dir = std::env::temp_dir().join(format!("lugha-docs-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let file = dir.join("x.la");
        std::fs::write(&file, "disk").unwrap();
        let mut docs = Documents::default();
        assert_eq!(docs.read(&file).unwrap(), b"disk");
        docs.set(&dir.join("sub/../x.la"), "open".to_string());
        assert_eq!(docs.read(&file).unwrap(), b"open");
        assert_eq!(docs.paths(), vec![file.clone()]);
        docs.close(&file);
        assert_eq!(docs.read(&file).unwrap(), b"disk");
        assert!(docs.read(&dir.join("missing.la")).is_err());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
