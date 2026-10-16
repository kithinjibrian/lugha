//! Rename (v16 draft decisions 77–78): the places Find References finds,
//! each replaced by the new name — after the renamed text has been checked
//! again in memory and found to mean the same. Every refusal is a one-line
//! reason, never a partial edit.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use lsp_types::{DiagnosticSeverity, Range, TextEdit, Uri, WorkspaceEdit};

use super::check;
use super::documents::Documents;
use super::hover::{Snapshot, byte_offset};
use super::publish::{position, uri};
use super::references::{self, Key, Place};
use super::targets::has_main;
use crate::check::Name;
use crate::lexer::token::keyword;
use crate::span::Span;

/// What a rename reads from the server.
pub(super) struct Context<'a> {
    /// Each main file's last check.
    pub snapshots: &'a HashMap<PathBuf, Rc<Snapshot>>,
    pub docs: &'a Documents,
    /// The mains checked as modules alone (E0303 dropped).
    pub alone: &'a HashSet<PathBuf>,
}

/// The range of the name under `at`, if it may be renamed (prepareRename).
pub(super) fn prepare(
    snapshot: &Snapshot,
    file: &Path,
    at: lsp_types::Position,
    cx: &Context,
) -> Result<Range, String> {
    let (place, _, text) = target(snapshot, file, at, cx)?;
    Ok(Range {
        start: position(text, place.start),
        end: position(text, place.end),
    })
}

/// The edits renaming the name under `at` to `new`, or why not.
pub(super) fn rename(
    snapshot: &Snapshot,
    file: &Path,
    at: lsp_types::Position,
    new: &str,
    cx: &Context,
) -> Result<WorkspaceEdit, String> {
    let (place, key, text) = target(snapshot, file, at, cx)?;
    let old = text[place.start..place.end].to_string();
    valid(new)?;
    let places = references::find(cx.snapshots.values().map(|s| &**s), &key, true);
    // The renamed texts, by file, and where each renamed place ends up.
    let mut renamed = cx.docs.clone();
    let mut texts: BTreeMap<PathBuf, (String, Vec<&Place>)> = BTreeMap::new();
    for p in &places {
        let entry = match texts.get_mut(&p.file) {
            Some(entry) => entry,
            None => {
                let text = text_of(&p.file, cx)
                    .ok_or_else(|| format!("can't read `{}`", p.file.display()))?;
                texts.entry(p.file.clone()).or_insert((text, Vec::new()))
            }
        };
        entry.1.push(p);
    }
    let mut moved: Vec<Place> = Vec::new();
    let mut new_key = None;
    for (path, (text, ps)) in &texts {
        let mut out = String::with_capacity(text.len());
        let mut last = 0;
        for p in ps {
            out.push_str(&text[last..p.start]);
            let start = out.len();
            out.push_str(new);
            let moved_place = Place {
                file: path.clone(),
                start,
                end: out.len(),
            };
            if matches!(&key, Key::Decl { file, start: s, .. } if Path::new(file) == path && *s == p.start)
            {
                new_key = Some(Key::Decl {
                    file: path.display().to_string(),
                    start: moved_place.start,
                    end: moved_place.end,
                });
            }
            moved.push(moved_place);
            last = p.end;
        }
        out.push_str(&text[last..]);
        renamed.set(path, out);
    }
    let new_key = new_key.ok_or("couldn't find the declaration to rename")?;
    verify(&old, new, &texts, &renamed, &new_key, &moved, cx)?;
    // A list first: `Uri` as a map key trips clippy's mutable-key lint.
    let mut changes: Vec<(Uri, Vec<TextEdit>)> = Vec::new();
    for (path, (text, ps)) in &texts {
        let edits = ps
            .iter()
            .map(|p| TextEdit {
                range: Range {
                    start: position(text, p.start),
                    end: position(text, p.end),
                },
                new_text: new.to_string(),
            })
            .collect();
        changes.push((uri(path).ok_or("a file without a URI")?, edits));
    }
    Ok(WorkspaceEdit {
        changes: Some(changes.into_iter().collect()),
        ..WorkspaceEdit::default()
    })
}

/// Decision 78: every main whose check loaded a renamed file is checked
/// again on the renamed text; it must gain no error, and the renamed
/// declaration must be named exactly at the renamed places.
fn verify(
    old: &str,
    new: &str,
    texts: &BTreeMap<PathBuf, (String, Vec<&Place>)>,
    renamed: &Documents,
    new_key: &Key,
    moved: &[Place],
    cx: &Context,
) -> Result<(), String> {
    let mut after_snapshots = Vec::new();
    for (main, snapshot) in cx.snapshots {
        if !texts.keys().any(|f| snapshot.has(f)) {
            continue;
        }
        let alone = cx.alone.contains(main);
        let before = check::check(main, cx.docs, alone);
        let after = check::check(main, renamed, alone);
        for (file, diagnostics) in &after.files {
            let errors = |ds: &[lsp_types::Diagnostic]| {
                ds.iter()
                    .filter(|d| d.severity == Some(DiagnosticSeverity::ERROR))
                    .count()
            };
            let had = before.files.get(file).map_or(0, |ds| errors(ds));
            if errors(diagnostics) > had {
                let first = diagnostics
                    .iter()
                    .find(|d| d.severity == Some(DiagnosticSeverity::ERROR))
                    .map_or(String::new(), |d| {
                        d.message.lines().next().unwrap_or("").to_string()
                    });
                let name = file
                    .file_name()
                    .map_or(String::new(), |n| n.to_string_lossy().into_owned());
                return Err(format!(
                    "renaming `{old}` to `{new}` would add an error in {name}: {first}"
                ));
            }
        }
        let snapshot = after
            .snapshot
            .ok_or_else(|| format!("renaming `{old}` to `{new}` would break the program"))?;
        after_snapshots.push(snapshot);
    }
    let found = references::find(&after_snapshots, new_key, true);
    let mut want = moved.to_vec();
    want.sort();
    if found != want {
        return Err(format!(
            "renaming `{old}` to `{new}` would make a use point elsewhere"
        ));
    }
    Ok(())
}

/// The name under `at` and its declaration, unless it may not be renamed.
fn target<'s>(
    snapshot: &'s Snapshot,
    file: &Path,
    at: lsp_types::Position,
    cx: &Context,
) -> Result<(Place, Key, &'s str), String> {
    let nothing = || "nothing to rename here".to_string();
    let (_, text) = snapshot.base(file).ok_or_else(nothing)?;
    let offset = byte_offset(text, at).ok_or_else(nothing)?;
    let (place, key) = references::at(snapshot, file, offset).ok_or_else(nothing)?;
    let name = &text[place.start..place.end];
    match &key {
        Key::Module(_) => Err(format!(
            "`{name}` is a module: renaming it would rename a file"
        )),
        key if key.bundled() => Err(format!(
            "`{name}` belongs to std or the prelude, which can't be renamed"
        )),
        Key::Decl {
            file: decl,
            start,
            end,
        } => {
            if !reachable(snapshot, Path::new(decl), Span::new(*start, *end), cx) {
                return Err(format!(
                    "`{name}` can be used by files the server hasn't checked: open, or list in `lugha.entries`, a main file that uses it"
                ));
            }
            Ok((place, key.clone(), text))
        }
    }
}

/// Whether every use of the declaration at `span` of `decl` is among the
/// checked files: a local, a `private` item, anything in a main file, or a
/// module some other checked main imports.
fn reachable(snapshot: &Snapshot, decl: &Path, span: Span, cx: &Context) -> bool {
    let Some((base, text)) = snapshot.base(decl) else {
        return false;
    };
    let local = matches!(
        snapshot
            .partial
            .names
            .get(&Span::new(base + span.start, base + span.end)),
        Some(Name::Local { .. })
    );
    let line = &text[text[..span.start].rfind('\n').map_or(0, |i| i + 1)..span.start];
    let private = line.split_whitespace().any(|w| w == "private");
    let imported = cx
        .snapshots
        .iter()
        .any(|(main, s)| main.as_path() != decl && s.has(decl));
    local || private || has_main(text) || imported
}

/// `new` is an identifier and not a keyword.
fn valid(new: &str) -> Result<(), String> {
    let mut chars = new.chars();
    let first_ok = chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_');
    if !first_ok || !chars.all(|c| c.is_ascii_alphanumeric() || c == '_') || new == "_" {
        return Err(format!(
            "`{new}` isn't a name: use letters, digits and `_`, not starting with a digit"
        ));
    }
    if keyword(new).is_some() {
        return Err(format!("`{new}` is a keyword"));
    }
    Ok(())
}

/// A file's current text: the editor's, else the disk's.
fn text_of(file: &Path, cx: &Context) -> Option<String> {
    String::from_utf8(cx.docs.read(file).ok()?).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_names_must_be_identifiers_and_not_keywords() {
        assert!(valid("total").is_ok());
        assert!(valid("_x1").is_ok());
        assert!(valid("while").unwrap_err().contains("keyword"));
        assert!(valid("1abc").unwrap_err().contains("isn't a name"));
        assert!(valid("a-b").is_err());
        assert!(valid("é").is_err());
        assert!(valid("").is_err());
        assert!(valid("_").is_err());
    }
}
