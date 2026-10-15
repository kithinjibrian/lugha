//! Snippets from the server (v16 draft decisions 53–54): a completed call
//! with a placeholder per parameter, and the postfix `.match` that writes
//! out every variant. Only for a client that supports snippets.

use lsp_types::{
    CompletionItem, CompletionItemKind as Kind, CompletionTextEdit, InsertTextFormat, Position,
    Range, TextEdit,
};

use super::completion::Here;
use super::publish::position;
use crate::check::{MemberKind, Type};
use crate::span::Span;

/// Gives `item` a call with a placeholder per parameter when it names a
/// function, method or a variant with a payload; `decl` is its declaration.
pub(super) fn call(here: &Here, item: &mut CompletionItem, decl: Option<Span>) {
    if !here.snippets {
        return;
    }
    let sig = decl.and_then(|d| here.snapshot.partial.signatures.get(&d));
    let params: Vec<String> = match (item.kind, sig) {
        (Some(Kind::FUNCTION | Kind::METHOD), Some(sig)) => {
            let names = sig.params.iter().map(|(n, _)| n.clone());
            // A prelude helper called as a method (`xs.map(f)`) takes its
            // receiver as the first parameter: the call doesn't write it.
            let skip = usize::from(item.kind == Some(Kind::METHOD) && sig.receiver.is_none());
            names.skip(skip).collect()
        }
        // A built-in method or a variant: its types are all there is.
        (Some(Kind::METHOD | Kind::ENUM_MEMBER), None) => {
            match item.detail.as_deref().and_then(types_in) {
                Some(types) if !types.is_empty() || item.kind == Some(Kind::METHOD) => types,
                _ => return,
            }
        }
        _ => return,
    };
    let slots: Vec<String> = params
        .iter()
        .enumerate()
        .map(|(i, p)| format!("${{{}:{}}}", i + 1, escape(p)))
        .collect();
    item.insert_text = Some(format!("{}({})", item.label, slots.join(", ")));
    item.insert_text_format = Some(InsertTextFormat::SNIPPET);
}

/// The postfix `match` for the value before the `.` at `dot`, when its type
/// is an enum, `Option` or `Result`; it replaces `value.word` up to `cursor`.
pub(super) fn postfix_match(here: &Here, dot: usize, cursor: Position) -> Option<CompletionItem> {
    if !here.snippets {
        return None;
    }
    let (ty, start) = receiver_type(here, dot)?;
    let arms: Vec<(String, usize)> = match &ty {
        Type::Option(_) => vec![("Some".to_string(), 1), ("None".to_string(), 0)],
        Type::Enum(key, _) => here
            .snapshot
            .partial
            .members
            .get(key)?
            .members
            .iter()
            .filter(|m| m.kind == MemberKind::Variant)
            .map(|m| {
                let payloads = match &m.detail {
                    crate::check::Detail::Text(text) => types_in(text).map_or(0, |t| t.len()),
                    _ => 0,
                };
                (m.name.clone(), payloads)
            })
            .collect(),
        _ => return None,
    };
    let receiver = &here.text[start..dot];
    let new_text = arms_text(receiver, &arms);
    let range = Range {
        start: position(here.text, start),
        end: cursor,
    };
    Some(CompletionItem {
        label: "match".to_string(),
        kind: Some(Kind::SNIPPET),
        detail: Some(format!("match {receiver} {{ … }} on every variant")),
        filter_text: Some(format!("{receiver}.match")),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        text_edit: Some(CompletionTextEdit::Edit(TextEdit { range, new_text })),
        ..CompletionItem::default()
    })
}

/// `match receiver { … }` with an arm per `(variant, payload slots)`:
/// placeholders for each slot, then each arm's body, `$0` after it all.
fn arms_text(receiver: &str, arms: &[(String, usize)]) -> String {
    let mut n = 0;
    let mut next = || {
        n += 1;
        n
    };
    let mut text = format!("match {receiver} {{\n");
    for (name, slots) in arms {
        text.push('\t');
        text.push_str(name);
        if *slots > 0 {
            let slots: Vec<String> = (0..*slots).map(|_| format!("${{{}:_}}", next())).collect();
            text.push_str(&format!("({})", slots.join(", ")));
        }
        text.push_str(&format!(" => ${{{}}},\n", next()));
    }
    text.push_str("}$0");
    text
}

/// The type of the value before the `.` at `dot`, and where it starts: the
/// check's expression ending there, or a local of that name.
fn receiver_type(here: &Here, dot: usize) -> Option<(Type, usize)> {
    let partial = &here.snapshot.partial;
    if here.same >= dot {
        let end = here.base + dot;
        let found = partial
            .spans
            .iter()
            .enumerate()
            .filter_map(|(id, s)| Some(((*s)?, partial.types.get(id)?.as_ref()?)))
            .filter(|(s, ty)| s.end == end && !matches!(ty, Type::Void | Type::Error | Type::Never))
            .min_by_key(|(s, _)| (s.end - s.start, s.start));
        if let Some((span, ty)) = found {
            return Some((ty.clone(), span.start - here.base));
        }
    }
    let len: usize = here.text[..dot]
        .chars()
        .rev()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
        .map(char::len_utf8)
        .sum();
    let start = dot - len;
    let name = &here.text[start..dot];
    if name.is_empty() || here.text[..start].ends_with('.') {
        return None;
    }
    let (_, ty, _) = here.locals(start).into_iter().find(|(n, ..)| n == name)?;
    Some((ty, start))
}

/// The comma-separated parts inside the first `( … )` of `text`, without
/// `self` / `mut self`; `None` when there are no parentheses.
fn types_in(text: &str) -> Option<Vec<String>> {
    let open = text.find('(')?;
    let mut parts = Vec::new();
    let (mut depth, mut start) = (0, open + 1);
    for (i, c) in text.char_indices().skip_while(|(i, _)| *i <= open) {
        match c {
            '(' | '[' | '<' => depth += 1,
            ')' | ']' | '>' if depth > 0 => depth -= 1,
            ',' | ')' if depth == 0 => {
                let part = text[start..i].trim();
                if !part.is_empty() && part != "self" && part != "mut self" {
                    parts.push(part.to_string());
                }
                if c == ')' {
                    return Some(parts);
                }
                start = i + 1;
            }
            _ => {}
        }
    }
    Some(parts)
}

/// `text` safe inside a snippet placeholder.
fn escape(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('$', "\\$")
        .replace('}', "\\}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arms_number_slots_then_bodies() {
        let arms = [
            ("A".to_string(), 0),
            ("B".to_string(), 1),
            ("C".to_string(), 2),
        ];
        assert_eq!(
            arms_text("v", &arms),
            "match v {\n\tA => ${1},\n\tB(${2:_}) => ${3},\n\tC(${4:_}, ${5:_}) => ${6},\n}$0"
        );
    }

    #[test]
    fn types_are_read_from_a_signature_or_payload_text() {
        assert_eq!(types_in("fun push(i64)"), Some(vec!["i64".to_string()]));
        assert_eq!(
            types_in("Rect(f64, f64)"),
            Some(vec!["f64".to_string(), "f64".to_string()])
        );
        assert_eq!(types_in("fun area(self): f64"), Some(vec![]));
        assert_eq!(
            types_in("fun m(f: fun(T, U): V, xs: T[])"),
            Some(vec!["f: fun(T, U): V".to_string(), "xs: T[]".to_string()])
        );
        assert_eq!(types_in("Dot"), None);
    }

    #[test]
    fn placeholder_text_is_escaped() {
        assert_eq!(escape("a}$\\"), "a\\}\\$\\\\");
    }
}
