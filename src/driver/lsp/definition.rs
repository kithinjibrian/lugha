//! Go to Definition (v16 draft decisions 23–25): where the name under the
//! cursor is declared, from the checker's findings for the last check.

use std::path::Path;

use lsp_types::{Location, Position, Range};

use super::hover::{Snapshot, byte_offset};
use super::publish::{file_path, position, uri};
use crate::check::Target;
use crate::span::Span;

/// The declaration of what is at `at` in `file`, or `None` when there is
/// no name there or its declaration has no file (`std`, the prelude).
pub(super) fn answer(snapshot: &Snapshot, file: &Path, at: Position) -> Option<Location> {
    let (base, text) = snapshot.base(file)?;
    let offset = base + byte_offset(text, at)?;
    let target = innermost(&snapshot.partial.definitions, offset)?;
    match target {
        Target::Decl(span) => locate(snapshot, *span),
        Target::Module(path) => {
            let module = snapshot.modules.iter().find(|m| m.path == *path)?;
            let file = snapshot.source.file_at(module.base);
            // A module's definition is its file, from the top.
            let path = file_path(file, &snapshot.root)?;
            Some(Location {
                uri: uri(&path)?,
                range: Range::default(),
            })
        }
    }
}

/// The target of the smallest use span containing `offset`.
pub(super) fn innermost(
    definitions: &std::collections::HashMap<Span, Target>,
    offset: usize,
) -> Option<&Target> {
    definitions
        .iter()
        .filter(|(s, _)| s.start <= offset && offset < s.end)
        // The smallest wins; equal spans by start, so the answer is stable.
        .min_by_key(|(s, _)| (s.end - s.start, s.start))
        .map(|(_, t)| t)
}

/// The file and range of a declaration's name; `None` inside `<std>` or
/// `<prelude>`.
fn locate(snapshot: &Snapshot, span: Span) -> Option<Location> {
    let file = snapshot.source.file_at(span.start);
    let path = file_path(file, &snapshot.root)?;
    Some(Location {
        uri: uri(&path)?,
        range: Range {
            start: position(&file.text, span.start - file.base),
            end: position(&file.text, span.end.saturating_sub(file.base)),
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn the_smallest_use_wins() {
        let outer = Span::new(10, 20);
        let inner = Span::new(12, 14);
        let defs = HashMap::from([
            (outer, Target::Decl(Span::new(0, 1))),
            (inner, Target::Module(vec!["geo".into()])),
        ]);
        assert_eq!(
            innermost(&defs, 13),
            Some(&Target::Module(vec!["geo".into()]))
        );
        assert_eq!(innermost(&defs, 18), Some(&Target::Decl(Span::new(0, 1))));
        assert_eq!(innermost(&defs, 20), None);
        assert_eq!(innermost(&defs, 5), None);
    }
}
