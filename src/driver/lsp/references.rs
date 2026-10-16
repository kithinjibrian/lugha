//! Find References (v16 draft decision 76): every place a name resolves to
//! the same declaration, across every check the server holds. Each check
//! has its own offset space, so a declaration is known by its file and
//! byte range there, not by span.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use lsp_types::{Location, Range};

use super::definition::innermost;
use super::hover::Snapshot;
use super::publish::{file_path, position, uri};
use crate::check::Target;
use crate::span::Span;

/// What a name resolves to, the same in every check.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) enum Key {
    /// A declaration: its file (a path, or `<prelude>`/`<std…>` for bundled
    /// sources) and its name's bytes in that file.
    Decl {
        file: String,
        start: usize,
        end: usize,
    },
    /// A module, by its path.
    Module(Vec<String>),
}

impl Key {
    /// Declared in a bundled source: `std` or the prelude.
    pub(super) fn bundled(&self) -> bool {
        matches!(self, Key::Decl { file, .. } if file.starts_with('<'))
    }
}

/// A written place: a file and the bytes of the name in its text.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) struct Place {
    pub file: PathBuf,
    pub start: usize,
    pub end: usize,
}

/// The name under byte `offset` of `file` in `snapshot`: where it is
/// written (in `file`'s bytes) and what it resolves to.
pub(super) fn at(snapshot: &Snapshot, file: &Path, offset: usize) -> Option<(Place, Key)> {
    let (base, _) = snapshot.base(file)?;
    let defs = &snapshot.partial.definitions;
    let target = innermost(defs, base + offset)?;
    let (span, _) = defs
        .iter()
        .filter(|(s, t)| *t == target && s.start <= base + offset && base + offset < s.end)
        .min_by_key(|(s, _)| (s.end - s.start, s.start))?;
    let place = Place {
        file: file.to_path_buf(),
        start: span.start - base,
        end: span.end - base,
    };
    Some((place, key(snapshot, target)?))
}

/// The key of `target` in `snapshot`'s offset space.
pub(super) fn key(snapshot: &Snapshot, target: &Target) -> Option<Key> {
    match target {
        Target::Module(path) => Some(Key::Module(path.clone())),
        Target::Decl(span) => {
            let file = snapshot.source.file_at(span.start);
            let name = match file_path(file, &snapshot.root) {
                Some(path) => path.display().to_string(),
                None => file.name.clone(),
            };
            Some(Key::Decl {
                file: name,
                start: span.start - file.base,
                end: span.end - file.base,
            })
        }
    }
}

/// Every place resolving to `key` in `snapshots`, without duplicates, in
/// file and offset order; with the declaration itself if `declaration`.
pub(super) fn find<'s>(
    snapshots: impl IntoIterator<Item = &'s Snapshot>,
    key: &Key,
    declaration: bool,
) -> Vec<Place> {
    let mut found = BTreeSet::new();
    for snapshot in snapshots {
        for (span, target) in &snapshot.partial.definitions {
            if key_matches(snapshot, target, key)
                && let Some(place) = place(snapshot, *span)
            {
                found.insert(place);
            }
        }
    }
    let decl = match key {
        Key::Decl { file, start, end } => Some(Place {
            file: PathBuf::from(file),
            start: *start,
            end: *end,
        }),
        Key::Module(_) => None,
    };
    match (decl, declaration) {
        (Some(d), true) if !key.bundled() => {
            found.insert(d);
        }
        (Some(d), false) => {
            found.remove(&d);
        }
        _ => {}
    }
    found.into_iter().collect()
}

fn key_matches(snapshot: &Snapshot, target: &Target, key: &Key) -> bool {
    match (target, key) {
        (Target::Module(a), Key::Module(b)) => a == b,
        (Target::Decl(_), Key::Decl { .. }) => self::key(snapshot, target).as_ref() == Some(key),
        _ => false,
    }
}

/// The place of `span` in `snapshot`; `None` inside a bundled source.
fn place(snapshot: &Snapshot, span: Span) -> Option<Place> {
    let file = snapshot.source.file_at(span.start);
    Some(Place {
        file: file_path(file, &snapshot.root)?,
        start: span.start - file.base,
        end: span.end - file.base,
    })
}

/// The LSP location of `place`, given its file's text.
pub(super) fn location(place: &Place, text: &str) -> Option<Location> {
    Some(Location {
        uri: uri(&place.file)?,
        range: Range {
            start: position(text, place.start),
            end: position(text, place.end),
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bundled_keys_are_told_apart() {
        let prelude = Key::Decl {
            file: "<prelude>".into(),
            start: 0,
            end: 1,
        };
        let mine = Key::Decl {
            file: "/w/main.la".into(),
            start: 0,
            end: 1,
        };
        assert!(prelude.bundled());
        assert!(!mine.bundled());
        assert!(!Key::Module(vec!["geo".into()]).bundled());
    }

    #[test]
    fn find_adds_or_drops_the_declaration_and_skips_bundled_ones() {
        let key = Key::Decl {
            file: "/w/main.la".into(),
            start: 4,
            end: 5,
        };
        let with = find(std::iter::empty(), &key, true);
        assert_eq!(with.len(), 1);
        assert_eq!(with[0].start, 4);
        assert!(find(std::iter::empty(), &key, false).is_empty());
        let prelude = Key::Decl {
            file: "<prelude>".into(),
            start: 4,
            end: 5,
        };
        assert!(find(std::iter::empty(), &prelude, true).is_empty());
    }
}
