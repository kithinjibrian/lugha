//! Completion inside derive blocks (v17, milestone 142): `derive.` there
//! offers the field intrinsics, and a block's items count as functions for
//! the locals in scope.

use lsp_types::{CompletionItem, CompletionItemKind as Kind};

use crate::ast::{FunDecl, Item, Program};
use crate::span::Span;

/// The field intrinsics and what each gives.
const INTRINSICS: [(&str, &str); 5] = [
    ("fields", "derive.fields(x, ctx, f): R[]"),
    ("build", "derive.build(ctx, f): Result<Self, E>"),
    ("variant", "derive.variant(x, ctx, f): (string, R[])"),
    ("variants", "derive.variants(): (string, i64)[]"),
    (
        "build_variant",
        "derive.build_variant(name, ctx, f): Option<Result<Self, E>>",
    ),
];

/// The function, method or derive block item whose body holds `offset`.
pub(super) fn enclosing(program: &Program, offset: usize) -> Option<Span> {
    let holds = |f: &FunDecl| f.span.start <= offset && offset <= f.span.end;
    program.items.iter().find_map(|it| match it {
        Item::Fun(f) if holds(f) => Some(f.span),
        Item::Impl(i) => i.methods.iter().find(|m| holds(m)).map(|m| m.span),
        Item::Derive(d) => d.methods.iter().find(|m| holds(m)).map(|m| m.span),
        _ => None,
    })
}

/// The intrinsics, when the `.` at `dot` (in `text`, which starts at
/// `base` in `program`'s offsets) follows `derive` inside a derive block.
pub(super) fn derive_intrinsics(
    program: &Program,
    (text, base): (&str, usize),
    dot: usize,
) -> Option<Vec<CompletionItem>> {
    let before = text[..dot].strip_suffix("derive")?;
    if before.ends_with(|c: char| c.is_alphanumeric() || c == '_' || c == '.') {
        return None;
    }
    let at = base + dot;
    let parsed = program
        .items
        .iter()
        .any(|it| matches!(it, Item::Derive(d) if d.span.start <= at && at <= d.span.end));
    // While `derive.` is half typed the block doesn't parse: read the text.
    (parsed || in_block_text(&text[..dot])).then(|| {
        INTRINSICS
            .iter()
            .map(|(name, detail)| CompletionItem {
                label: name.to_string(),
                kind: Some(Kind::FUNCTION),
                detail: Some(detail.to_string()),
                ..CompletionItem::default()
            })
            .collect()
    })
}

/// True if `before` (the text up to the cursor) is inside a block whose
/// item starts with `derive` on its own line: its braces are still open.
fn in_block_text(before: &str) -> bool {
    let Some(start) = before
        .match_indices("\nderive")
        .map(|(i, _)| i + 1)
        .chain(before.starts_with("derive").then_some(0))
        .max()
    else {
        return false;
    };
    let rest = &before[start..];
    rest.matches('{').count() > rest.matches('}').count()
}

#[cfg(test)]
mod tests {
    use super::derive_intrinsics;
    use crate::ast::Program;

    fn parse_src(src: &str) -> Result<Program, Vec<crate::diagnostic::Diagnostic>> {
        let (tokens, _) = crate::lexer::lex(src)?;
        crate::parser::parse(&tokens).map(|(p, _)| p)
    }

    #[test]
    fn derive_dot_offers_the_intrinsics_only_inside_a_block() {
        let text = "interface S { fun s(self): i64; }\nderive S for struct { fun s(self): i64 = derive.f; }\nfun g(): i64 = 1;";
        let program = parse_src(&text.replace("derive.f", "1")).expect("parses");
        let dot = text.find("derive.f").expect("there") + "derive".len();
        let found = derive_intrinsics(&program, (text, 0), dot).expect("inside");
        assert_eq!(found.len(), 5);
        assert!(found.iter().any(|i| i.label == "fields"));
        // In `g`, outside every block: nothing.
        assert!(derive_intrinsics(&program, (text, 0), text.len()).is_none());
        // A block that doesn't parse yet still counts.
        let empty = parse_src("fun g(): i64 = 1;").expect("parses");
        let typing = "derive S for struct {\n    fun s(self): i64 = derive.";
        assert!(derive_intrinsics(&empty, (typing, 0), typing.len() - 1).is_some());
        let closed = "derive S for struct {}\nfun g(): i64 = derive.";
        assert!(derive_intrinsics(&empty, (closed, 0), closed.len() - 1).is_none());
    }
}
