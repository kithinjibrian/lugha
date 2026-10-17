//! Signature help (v16 draft decisions 33–36): the signature of the call
//! the cursor is inside, with its active parameter, from the last check
//! that parsed (as completion reads it).

use std::path::Path;

use lsp_types::{
    Documentation, MarkupContent, MarkupKind, ParameterInformation, ParameterLabel, Position,
    SignatureHelp, SignatureInformation,
};

use super::completion::{Here, Receiver};
use super::docs;
use super::hover::{Snapshot, byte_offset, declaration};
use crate::ast::Item;
use crate::check::{Detail, Member, Name, Type};
use crate::span::Span;

/// The intrinsics' call forms as spec §5's table writes them; a test keeps
/// it equal to `check::intrinsics()` and to the spec.
const INTRINSIC_SIGNATURES: [(&str, &str); 24] = [
    ("print", "print(x)"),
    ("println", "println(x)"),
    ("panic", "panic(msg)"),
    ("to_string", "to_string(x)"),
    ("swap", "swap(a, b)"),
    ("args", "args()"),
    ("parse_i64", "parse_i64(s)"),
    ("parse_u64", "parse_u64(s)"),
    ("parse_f64", "parse_f64(s)"),
    ("read_line", "read_line()"),
    ("read_file", "read_file(path)"),
    ("write_file", "write_file(path, text)"),
    ("append_file", "append_file(path, text)"),
    ("eprint", "eprint(x)"),
    ("eprintln", "eprintln(x)"),
    ("exit", "exit(code)"),
    ("read_bytes", "read_bytes(path)"),
    ("write_bytes", "write_bytes(path, data)"),
    ("append_bytes", "append_bytes(path, data)"),
    ("read_stdin_bytes", "read_stdin_bytes()"),
    ("string_from_utf8", "string_from_utf8(data)"),
    ("c_read", "c_read(p, offset, len)"),
    ("c_write", "c_write(p, offset, bytes)"),
    ("format", "format(template, …)"),
];

/// How far back to look for the call's `(`.
const REACH: usize = 2000;

/// The call around the cursor.
#[derive(Debug, PartialEq)]
struct Call {
    /// Where the callee's name starts and ends (before the `(`).
    name: (usize, usize),
    /// Where the receiver's name ends (its `.`), for `x.name(`.
    dot: Option<usize>,
    /// Commas before the cursor at the call's depth.
    active: u32,
}

/// The signature help at `at` in `file`, whose current text is `text`.
pub(super) fn answer(
    snapshot: &Snapshot,
    file: &Path,
    text: &str,
    at: Position,
) -> Option<SignatureHelp> {
    let cursor = byte_offset(text, at)?;
    let call = call(text, cursor)?;
    let here = Here::new(snapshot, file, text)?;
    let (label, note, decl) = signature(&here, &call)?;
    // Prelude types as programs write them (v17); before the offsets are taken.
    let (label, note) = (
        crate::check::shown(&label),
        note.map(|n| crate::check::shown(&n)),
    );
    let doc = decl.and_then(|d| docs::doc_for(snapshot, d));
    let mut parameters = parameters(&label);
    // Each parameter's `@param` text under its highlight (milestone 77).
    if let Some(doc) = &doc {
        let units: Vec<u16> = label.encode_utf16().collect();
        for p in &mut parameters {
            if let ParameterLabel::LabelOffsets([a, b]) = p.label {
                let text = String::from_utf16_lossy(&units[a as usize..b as usize]);
                let name = text.split(':').next().unwrap_or_default().trim();
                p.documentation = doc
                    .param(name)
                    .map(|t| Documentation::String(t.to_string()));
            }
        }
    }
    let summary = doc.map(|d| d.summary()).filter(|s| !s.is_empty());
    let documentation = match (note, summary) {
        (None, None) => None,
        (Some(note), None) => Some(Documentation::String(note)),
        (note, Some(summary)) => {
            let value = match note {
                Some(note) => format!("```lugha\n{note}\n```\n\n{summary}"),
                None => summary,
            };
            Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value,
            }))
        }
    };
    let active = call.active.min(parameters.len().saturating_sub(1) as u32);
    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label,
            documentation,
            parameters: Some(parameters),
            active_parameter: None,
        }],
        active_signature: Some(0),
        active_parameter: Some(active),
    })
}

/// Finds the innermost unclosed `(` before `cursor` that follows a name,
/// scanning forward from a line start up to `REACH` bytes back, skipping
/// strings, `char`s and comments; `None` outside any call.
fn call(text: &str, cursor: usize) -> Option<Call> {
    let mut from = cursor.saturating_sub(REACH);
    while !text.is_char_boundary(from) {
        from += 1;
    }
    let from = text[..from].rfind('\n').map_or(from, |i| i + 1).min(cursor);
    // Open brackets: (offset, is a paren, commas so far).
    let mut open: Vec<(usize, bool, u32)> = Vec::new();
    let bytes = text.as_bytes();
    let mut i = from;
    while i < cursor {
        match bytes[i] {
            b'"' | b'\'' => {
                let quote = bytes[i];
                i += 1;
                while i < cursor && bytes[i] != quote && bytes[i] != b'\n' {
                    i += if bytes[i] == b'\\' { 2 } else { 1 };
                }
            }
            b'/' if bytes.get(i + 1) == Some(&b'/') => {
                while i < cursor && bytes[i] != b'\n' {
                    i += 1;
                }
                continue;
            }
            b'(' => open.push((i, true, 0)),
            b'[' | b'{' => open.push((i, false, 0)),
            b')' | b']' | b'}' => {
                open.pop();
            }
            b',' => {
                if let Some(top) = open.last_mut() {
                    top.2 += 1;
                }
            }
            _ => {}
        }
        i += 1;
    }
    if i > cursor {
        return None; // the cursor is inside a string
    }
    let &(paren, true, active) = open.last()? else {
        return None;
    };
    let before = text[..paren].trim_end_matches([' ', '\t']);
    let start = before.len()
        - before
            .chars()
            .rev()
            .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
            .count();
    if start == before.len() || before[start..].starts_with(|c: char| c.is_ascii_digit()) {
        return None; // `(a + b)`: a grouping, not a call
    }
    let dot = before[..start].ends_with('.').then(|| start - 1);
    Some(Call {
        name: (start, before.len()),
        dot,
        active,
    })
}

/// The label and documentation for `call`.
/// The label, a note (the written header under a filled-in generic) and
/// the declaration whose doc applies.
fn signature(here: &Here, call: &Call) -> Option<(String, Option<String>, Option<Span>)> {
    let name = &here.text[call.name.0..call.name.1];
    let partial = &here.snapshot.partial;
    // The callee as the last good check resolved it, with its call's instance.
    if call.name.1 <= here.same {
        let at = Span::new(here.base + call.name.0, here.base + call.name.1);
        if let Some(Name::Function { decl, call: id }) = partial.names.get(&at) {
            let (header, _) = declaration(here.snapshot, *decl)?;
            let filled = id
                .and_then(|id| partial.instances.get(&id))
                .and_then(|args| {
                    let owner = header
                        .strip_prefix("fun ")?
                        .split_once('.')
                        .map(|(o, _)| o.to_string());
                    let owner = owner.filter(|o| !o.contains('('));
                    partial
                        .signatures
                        .get(decl)?
                        .filled(name, owner.as_deref(), args)
                });
            return Some(match filled {
                Some(text) => (text, Some(header), Some(*decl)),
                None => (header, None, Some(*decl)),
            });
        }
    }
    if let Some(dot) = call.dot {
        return match here.receiver(dot)? {
            Receiver::Members(members) => {
                let m = members.into_iter().find(|m| m.name == name)?;
                Some((member_text(here, &m), None, m.decl))
            }
            Receiver::Module(path) => {
                let module = here.snapshot.modules.iter().find(|m| m.path == path)?;
                function_named(here, module.program.as_ref()?, name)
            }
        };
    }
    if let Some((_, Type::Fun(params, ret), _)) = here
        .locals(call.name.0)
        .into_iter()
        .find(|(n, ..)| n == name)
    {
        let params: Vec<String> = params.iter().map(Type::to_string).collect();
        return Some((format!("{name}({}): {ret}", params.join(", ")), None, None));
    }
    if let Some(found) = here.program().and_then(|p| function_named(here, p, name)) {
        return Some(found);
    }
    INTRINSIC_SIGNATURES
        .iter()
        .find(|(n, _)| *n == name)
        .map(|(_, form)| (form.to_string(), None, None))
}

fn function_named(
    here: &Here,
    program: &crate::ast::Program,
    name: &str,
) -> Option<(String, Option<String>, Option<Span>)> {
    program.items.iter().find_map(|item| match item {
        Item::Fun(f) if f.name.name == name => {
            Some((here.header(f.name.span, name), None, Some(f.name.span)))
        }
        _ => None,
    })
}

fn member_text(here: &Here, m: &Member) -> String {
    match &m.detail {
        Detail::Decl(decl) => here.header(*decl, &m.name),
        Detail::Text(text) => text.clone(),
        Detail::Type(ty) => format!("{}: {ty}", m.name),
    }
}

/// The parameters in `label`'s first parenthesised list (after any type
/// parameters), as UTF-16 offsets, without `self` / `mut self`.
fn parameters(label: &str) -> Vec<ParameterInformation> {
    let generics_end = label
        .find('>')
        .filter(|g| label.find('(').is_some_and(|p| *g < p))
        .unwrap_or(0);
    let Some(open) = label[generics_end..].find('(').map(|i| i + generics_end) else {
        return Vec::new();
    };
    let mut out = Vec::new();
    let (mut depth, mut start) = (0, open + 1);
    for (i, c) in label.char_indices().skip_while(|(i, _)| *i <= open) {
        match c {
            '(' | '[' | '<' => depth += 1,
            ')' | ']' | '>' if depth > 0 => depth -= 1,
            ',' | ')' if depth == 0 => {
                let part = label[start..i].trim();
                if !part.is_empty() && part != "self" && part != "mut self" {
                    let begin = start + label[start..i].find(part).unwrap_or(0);
                    let units = |b: usize| label[..b].encode_utf16().count() as u32;
                    out.push(ParameterInformation {
                        label: ParameterLabel::LabelOffsets([
                            units(begin),
                            units(begin + part.len()),
                        ]),
                        documentation: None,
                    });
                }
                if c == ')' {
                    break;
                }
                start = i + 1;
            }
            _ => {}
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn at(text: &str) -> Option<(String, Option<String>, u32)> {
        let cursor = text.find('|').expect("a cursor");
        let text = text.replace('|', "");
        let c = call(&text, cursor)?;
        let receiver = c.dot.map(|d| {
            text[..d]
                .rsplit(|ch: char| !ch.is_ascii_alphanumeric())
                .next()
                .unwrap()
                .to_string()
        });
        Some((text[c.name.0..c.name.1].to_string(), receiver, c.active))
    }

    #[test]
    fn the_call_is_the_innermost_unclosed_paren_after_a_name() {
        assert_eq!(at("max(1.5, |"), Some(("max".into(), None, 1)));
        assert_eq!(at("f(a, g(b, |"), Some(("g".into(), None, 1)));
        assert_eq!(at("f(a, g(b), |"), Some(("f".into(), None, 2)));
        assert_eq!(at("f([1, 2], |"), Some(("f".into(), None, 1)));
        assert_eq!(at("f(\"a,b(\", |"), Some(("f".into(), None, 1)));
        assert_eq!(at("p.scale(|"), Some(("scale".into(), Some("p".into()), 0)));
        assert_eq!(at("f(1,\n    2, |"), Some(("f".into(), None, 2)));
        assert_eq!(at("f( // (x,\n |"), Some(("f".into(), None, 0)));
    }

    #[test]
    fn groupings_strings_and_closed_calls_have_none() {
        assert_eq!(at("let y = (1 + |"), None);
        assert_eq!(at("f(1) + |"), None);
        assert_eq!(at("let s = \"f(|"), None);
        assert_eq!(at("|"), None);
    }

    #[test]
    fn parameters_are_offsets_without_self() {
        let labels = |l: &str| -> Vec<String> {
            let units: Vec<u16> = l.encode_utf16().collect();
            parameters(l)
                .iter()
                .map(|p| match p.label {
                    ParameterLabel::LabelOffsets([a, b]) => {
                        String::from_utf16_lossy(&units[a as usize..b as usize])
                    }
                    ParameterLabel::Simple(ref s) => s.clone(),
                })
                .collect()
        };
        assert_eq!(labels("fun max<T: Ord>(a: T, b: T): T"), ["a: T", "b: T"]);
        assert_eq!(labels("fun Point.scale(self, k: f64): Point"), ["k: f64"]);
        assert_eq!(
            labels("fun map(xs: T[], f: fun(T): U): U[]"),
            ["xs: T[]", "f: fun(T): U"]
        );
        assert_eq!(labels("Circle(f64)"), ["f64"]);
        assert_eq!(labels("read_line()"), Vec::<String>::new());
    }

    #[test]
    fn the_intrinsic_table_matches_the_checker_and_the_spec() {
        let mut ours: Vec<&str> = INTRINSIC_SIGNATURES.iter().map(|(n, _)| *n).collect();
        let mut theirs: Vec<&str> = crate::check::intrinsics().collect();
        ours.sort();
        theirs.sort();
        assert_eq!(ours, theirs);
        for (name, form) in INTRINSIC_SIGNATURES {
            if name != "format" {
                assert!(
                    crate::SPEC.contains(&format!("`{form}`")),
                    "{form} isn't in the spec"
                );
            }
        }
    }
}
