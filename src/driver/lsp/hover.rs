//! Hover (v16 draft decisions 18–19): the expression or declared name under
//! the cursor, from the checker's findings for the last check of a main file.

use std::path::{Path, PathBuf};

use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position, Range};

use super::docs;
use super::documents::normalize;
use super::publish::{file_path, position};
use crate::ast::{FunDecl, Item};
use crate::check::{self, Name, Partial, Type};
use crate::driver::source::Source;
use crate::modules::Module;
use crate::span::Span;

/// What a check left for hover: its files, modules and findings.
pub(super) struct Snapshot {
    /// The main file's folder, which relative file names start from.
    pub root: PathBuf,
    pub source: Source,
    pub modules: Vec<Module>,
    pub partial: Partial,
    /// The text had syntax errors: the tree is partial (milestone 75).
    pub broken: bool,
    /// Each file's doc comments, found on first use (milestone 77).
    pub docs: super::docs::DocCache,
}

impl Snapshot {
    /// Whether this check loaded `file`.
    pub(super) fn has(&self, file: &Path) -> bool {
        self.base(file).is_some()
    }

    /// Where `file` starts in the check's shared offset space, and its text.
    pub(super) fn base(&self, file: &Path) -> Option<(usize, &str)> {
        let file = normalize(file);
        self.source
            .files
            .iter()
            .find(|f| file_path(f, &self.root).as_ref() == Some(&file))
            .map(|f| (f.base, f.text.as_str()))
    }
}

/// What a span under the cursor is.
enum Found<'a> {
    Name(&'a Name),
    Expr(usize, &'a Type),
    /// A struct, enum, interface or variant name: its declaration and line (v16).
    Type(Span, String),
}

/// The hover at `at` in `file`, or `None` when nothing there has one.
pub(super) fn answer(snapshot: &Snapshot, file: &Path, at: Position) -> Option<Hover> {
    let (base, text) = snapshot.base(file)?;
    let offset = base + byte_offset(text, at)?;
    let contains = |s: &Span| s.start <= offset && offset < s.end;
    let partial = &snapshot.partial;
    let mut best: Option<(Span, Found)> = None;
    let mut names: Vec<(&Span, &Name)> =
        partial.names.iter().filter(|(s, _)| contains(s)).collect();
    // Deterministic among equal spans: by start.
    names.sort_by_key(|(s, _)| (s.end - s.start, s.start));
    for (span, name) in names {
        // A function with no declaration in the source (an interface's
        // method) has no header: the expression around it answers instead.
        if let Name::Function { decl, .. } = name
            && declaration(snapshot, *decl).is_none()
        {
            continue;
        }
        consider(&mut best, *span, Found::Name(name));
    }
    // Type and variant names (milestone 77): through their definitions.
    let mut types: Vec<(Span, Span)> = partial
        .definitions
        .iter()
        .filter(|(s, _)| contains(s))
        .filter_map(|(s, t)| match t {
            check::Target::Decl(d) => Some((*s, *d)),
            check::Target::Module(_) => None,
        })
        .collect();
    types.sort_by_key(|(s, _)| (s.end - s.start, s.start));
    for (span, decl) in types {
        if let Some(line) = type_line(snapshot, decl) {
            consider(&mut best, span, Found::Type(decl, line));
        }
    }
    for (id, span) in partial.spans.iter().enumerate() {
        let (Some(span), Some(ty)) = (span, partial.types.get(id).and_then(Option::as_ref)) else {
            continue;
        };
        if contains(span) && !matches!(ty, Type::Error | Type::Void) {
            consider(&mut best, *span, Found::Expr(id, ty));
        }
    }
    let (span, found) = best?;
    let (line, notes) = render(snapshot, &found)?;
    let mut value = format!("```lugha\n{line}\n```");
    for note in notes {
        value.push_str("\n\n");
        value.push_str(&note);
    }
    let documented = match &found {
        Found::Name(Name::Function { decl, .. }) | Found::Type(decl, _) => Some(*decl),
        // A field's name at its declaration.
        Found::Name(Name::Field { .. }) => Some(span),
        _ => None,
    };
    if let Some(doc) = documented.and_then(|d| docs::doc_for(snapshot, d)) {
        let doc = doc.markdown();
        if !doc.is_empty() {
            value.push_str("\n\n");
            value.push_str(&doc);
        }
    }
    let range = Range {
        start: position(text, span.start - base),
        end: position(text, span.end.saturating_sub(base)),
    };
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: Some(range),
    })
}

/// Keeps `found` if its span is smaller than the best so far. Names are
/// considered first, so they win ties.
fn consider<'a>(best: &mut Option<(Span, Found<'a>)>, span: Span, found: Found<'a>) {
    if best
        .as_ref()
        .is_none_or(|(b, _)| span.end - span.start < b.end - b.start)
    {
        *best = Some((span, found));
    }
}

/// `struct Point`, `enum Shape`, `interface Area` or a variant as written
/// (`Circle(f64)`), for the type or variant named at `decl`.
fn type_line(snapshot: &Snapshot, decl: Span) -> Option<String> {
    for program in snapshot.modules.iter().filter_map(|m| m.program.as_ref()) {
        for item in &program.items {
            match item {
                Item::Struct(s) if s.name.span == decl => {
                    let c = if s.c_layout.is_some() { "extern " } else { "" };
                    return Some(format!("{c}struct {}", s.name.name));
                }
                Item::Interface(i) if i.name.span == decl => {
                    return Some(format!("interface {}", i.name.name));
                }
                Item::ExternType(t) if t.name.span == decl => {
                    return Some(format!("extern type {}", t.name.name));
                }
                Item::Enum(e) if e.name.span == decl => {
                    return Some(format!("enum {}", e.name.name));
                }
                Item::Enum(e) => {
                    if let Some(v) = e.variants.iter().find(|v| v.name.span == decl) {
                        let file = snapshot.source.file_at(v.span.start);
                        let text = file
                            .text
                            .get(v.span.start - file.base..v.span.end - file.base)?;
                        return Some(text.split_whitespace().collect::<Vec<_>>().join(" "));
                    }
                }
                _ => {}
            }
        }
    }
    None
}

/// The code line and notes for what was found; `None` for a function whose
/// declaration isn't in the program's source (an interface's method).
fn render(snapshot: &Snapshot, found: &Found) -> Option<(String, Vec<String>)> {
    let partial = &snapshot.partial;
    Some(match found {
        Found::Type(_, line) => (line.clone(), Vec::new()),
        Found::Name(Name::Local { name, ty, mutable }) => {
            let mutable = if *mutable { "mut " } else { "" };
            (format!("let {mutable}{name}: {ty}"), Vec::new())
        }
        Found::Name(Name::Field { name, ty }) => (format!("{name}: {ty}"), Vec::new()),
        Found::Name(Name::Function { decl, call }) => {
            let (header, name) = declaration(snapshot, *decl)?;
            let notes = call
                .and_then(|c| partial.instances.get(&c))
                .map(|args| vec![instance(&name, args)])
                .unwrap_or_default();
            (header, notes)
        }
        Found::Expr(id, ty) => {
            let id = crate::ast::ExprId(*id as u32);
            let mut notes = Vec::new();
            if let (Some(args), Some(key)) = (partial.instances.get(&id), partial.callees.get(&id))
            {
                notes.push(instance(key.rsplit('.').next().unwrap_or(key), args));
            }
            if let Some(to) = partial.conversions.get(&id) {
                notes.push(format!("converted to {to}"));
            }
            (ty.to_string(), notes)
        }
    })
}

fn instance(name: &str, args: &[Type]) -> String {
    let args: Vec<String> = args.iter().map(Type::to_string).collect();
    format!("instance: {name}<{}>", args.join(", "))
}

/// The header of the function or method whose name is at `decl`, as
/// written (`fun max<T: Ord>(a: T, b: T): T`), a method's with its type
/// (`fun Circle.area(self): f64`); and its name.
pub(super) fn declaration(snapshot: &Snapshot, decl: Span) -> Option<(String, String)> {
    for program in snapshot.modules.iter().filter_map(|m| m.program.as_ref()) {
        for item in &program.items {
            let found = match item {
                Item::Fun(f) if f.name.span == decl => Some((f, None)),
                Item::Impl(i) => i
                    .methods
                    .iter()
                    .find(|m| m.name.span == decl)
                    .map(|m| (m, Some(i.ty.name.as_str()))),
                _ => None,
            };
            if let Some((f, owner)) = found {
                return Some((header(&snapshot.source, f, owner)?, f.name.name.clone()));
            }
        }
    }
    None
}

fn header(source: &Source, f: &FunDecl, owner: Option<&str>) -> Option<String> {
    let file = source.file_at(f.span.start);
    let text = file
        .text
        .get(f.span.start - file.base..f.body.span.start.checked_sub(file.base)?)?;
    // An expression body starts after `=`, a block body at `{`.
    let text = text.trim_end().trim_end_matches(['=', '{']).trim_end();
    let header = text.split_whitespace().collect::<Vec<_>>().join(" ");
    Some(match (owner, header.strip_prefix("fun ")) {
        (Some(owner), Some(rest)) => format!("fun {owner}.{rest}"),
        _ => header,
    })
}

/// The byte offset of a UTF-16 position in `text`; `None` past its end.
pub(super) fn byte_offset(text: &str, at: Position) -> Option<usize> {
    let mut start = 0;
    for _ in 0..at.line {
        start += text[start..].find('\n')? + 1;
    }
    let line = &text[start..];
    let line = &line[..line.find('\n').unwrap_or(line.len())];
    let mut units = 0;
    for (i, c) in line.char_indices() {
        if units >= at.character as usize {
            return Some(start + i);
        }
        units += c.len_utf16();
    }
    (units == at.character as usize).then_some(start + line.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn utf16_positions_become_byte_offsets() {
        let text = "ab\nd é👋 x\n";
        assert_eq!(byte_offset(text, Position::new(0, 1)), Some(1));
        assert_eq!(byte_offset(text, Position::new(1, 0)), Some(3));
        // `x`: `d`, ` `, `é` (1 unit, 2 bytes), `👋` (2 units, 4 bytes), ` `.
        assert_eq!(byte_offset(text, Position::new(1, 6)), Some(12));
        assert_eq!(byte_offset(text, Position::new(1, 7)), Some(13));
        assert_eq!(byte_offset(text, Position::new(1, 8)), None);
        assert_eq!(byte_offset(text, Position::new(5, 0)), None);
    }
}
