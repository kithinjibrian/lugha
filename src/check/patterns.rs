//! Pattern typing (v1 §4; v2 draft §3, §5): each pattern is checked against
//! the type it matches, at any depth, giving its bindings and its `Pat`s for
//! the usefulness check: one per way it can match, or-patterns expanded
//! (v9), so the usefulness check never sees an or-pattern.

use super::{Checker, Type, errors, slot_names_variant};
use crate::ast::{Ident, Pattern, PatternKind};
use crate::span::Span;

/// A pattern's bindings: name, type, and whether it is `mut` (v9).
pub(super) type Bindings = Vec<(Ident, Type, bool)>;

/// A pattern as the usefulness check sees it: bindings and `_` are `Wild`.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Pat {
    Wild,
    Ctor(Ctor, Vec<Pat>),
}

/// A constructor: a variant (its index and name), or a literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum Ctor {
    Variant(usize, String),
    Bool(bool),
    /// The literal's value, and whether it is negative (`-0` isn't).
    Int(u64, bool),
    Str(String),
    /// A type pattern's type, as it prints (v5): one of infinitely many.
    Type(String),
}

impl Checker {
    /// Types the top-level `pattern` against `ty`, adding its bindings. Each
    /// way it can match comes with the span of its top-level alternative,
    /// for W0102 (v9).
    pub(super) fn arm_pattern(
        &mut self,
        pattern: &Pattern,
        ty: &Type,
        bindings: &mut Bindings,
    ) -> Vec<(Pat, Span)> {
        if let PatternKind::Or(alternatives) = &pattern.kind {
            let groups = self.or_alternatives(alternatives, ty, bindings, true);
            return groups
                .into_iter()
                .flat_map(|(pats, span)| pats.into_iter().map(move |p| (p, span)))
                .collect();
        }
        let pats = self.pattern_at(pattern, ty, bindings, true);
        pats.into_iter().map(|p| (p, pattern.span)).collect()
    }

    /// One pattern at any depth, as the ways it can match. A bare name in a
    /// payload slot binds the payload unless the slot's type has a variant by
    /// that name (v2 draft §3); at the top level it must be a variant.
    pub(super) fn pattern_at(
        &mut self,
        pattern: &Pattern,
        ty: &Type,
        bindings: &mut Bindings,
        top: bool,
    ) -> Vec<Pat> {
        match &pattern.kind {
            PatternKind::Or(alternatives) => self
                .or_alternatives(alternatives, ty, bindings, top)
                .into_iter()
                .flat_map(|(pats, _)| pats)
                .collect(),
            PatternKind::Mut(inner) => self.mut_pattern(inner, ty, bindings, top),
            PatternKind::Tuple(given) => self.tuple_pattern(pattern, given, ty, bindings),
            PatternKind::Variant(name, slots) if !self.binds(pattern, ty, top) => {
                self.variant_pattern(pattern, name, slots.as_deref(), ty, bindings)
            }
            _ => vec![self.single(pattern, ty, bindings, top)],
        }
    }

    /// True if `pattern` is a bare name that binds rather than names a variant.
    pub(super) fn binds(&self, pattern: &Pattern, ty: &Type, top: bool) -> bool {
        match &pattern.kind {
            PatternKind::Variant(name, None) => {
                !top && !slot_names_variant(self.variants_of(ty).as_deref(), &name.name)
            }
            _ => false,
        }
    }

    /// A pattern with no alternatives inside it.
    fn single(&mut self, pattern: &Pattern, ty: &Type, bindings: &mut Bindings, top: bool) -> Pat {
        let fits = |ok: bool| ok || matches!(ty, Type::Error | Type::Never);
        match &pattern.kind {
            PatternKind::Wildcard => Pat::Wild,
            PatternKind::Int { value, negative } => {
                if !fits(ty.is_integer()) {
                    self.report(errors::pattern_mismatch(ty, pattern.span));
                } else if ty.is_integer() {
                    let (max, neg) = ty.literal_range();
                    if (*negative && *value > neg) || (!*negative && *value > max) {
                        self.report(errors::out_of_range(ty.clone(), pattern.span));
                    }
                }
                Pat::Ctor(Ctor::Int(*value, *negative && *value != 0), Vec::new())
            }
            PatternKind::Str(text) => {
                if !fits(*ty == Type::String) {
                    self.report(errors::pattern_mismatch(ty, pattern.span));
                }
                Pat::Ctor(Ctor::Str(text.clone()), Vec::new())
            }
            PatternKind::Bool(value) => {
                if !fits(*ty == Type::Bool) {
                    self.report(errors::pattern_mismatch(ty, pattern.span));
                }
                Pat::Ctor(Ctor::Bool(*value), Vec::new())
            }
            PatternKind::Variant(name, None) if self.binds(pattern, ty, top) => {
                bindings.push((name.clone(), ty.clone(), false));
                Pat::Wild
            }
            PatternKind::Type(name, written) => {
                self.type_pattern(pattern, name.as_ref(), written, ty, bindings)
            }
            _ => unreachable!("handled by `pattern_at`"),
        }
    }

    fn variant_pattern(
        &mut self,
        pattern: &Pattern,
        name: &Ident,
        slots: Option<&[Pattern]>,
        ty: &Type,
        bindings: &mut Bindings,
    ) -> Vec<Pat> {
        if matches!(ty, Type::Error | Type::Never) {
            return vec![Pat::Wild];
        }
        let Some(variants) = self.variants_of(ty) else {
            self.report(errors::pattern_mismatch(ty, pattern.span));
            return vec![Pat::Wild];
        };
        let Some((index, (_, payloads))) = variants
            .into_iter()
            .enumerate()
            .find(|(_, (v, _))| *v == name.name)
        else {
            self.report(errors::no_variant(&name.name, ty, name.span));
            return vec![Pat::Wild];
        };
        let slots = slots.unwrap_or(&[]);
        if slots.len() != payloads.len() {
            self.report(errors::payload_count(
                &name.name,
                payloads.len(),
                slots.len(),
                pattern.span,
            ));
            return vec![Pat::Wild];
        }
        // Every combination of the slots' alternatives (v9).
        let mut rows: Vec<Vec<Pat>> = vec![Vec::new()];
        for (slot, payload) in slots.iter().zip(&payloads) {
            let options = self.pattern_at(slot, payload, bindings, false);
            rows = rows
                .into_iter()
                .flat_map(|row| {
                    options
                        .iter()
                        .map(move |o| [row.clone(), vec![o.clone()]].concat())
                })
                .collect();
        }
        let ctor = Ctor::Variant(index, name.name.clone());
        rows.into_iter()
            .map(|args| Pat::Ctor(ctor.clone(), args))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    const SHAPE: &str =
        "enum Shape { Circle(f64), Rect(f64, f64), Empty }\nenum Pair { P(i64, Option<string>) }\n";

    fn main(body: &str) -> String {
        format!(
            "{SHAPE}fun main() {{ let s: Option<Shape> = None; let p = Pair.P(1, None); {body} }}"
        )
    }

    #[test]
    fn nested_patterns_bind_payload_types() {
        let src = main("let a = match s { Some(Circle(r)) => r, _ => 0.0 };");
        assert_eq!(let_type(&src, "a"), Type::F64);
        let src = main("let b = match s { Some(x) => x, None => Shape.Empty };");
        assert_eq!(let_type(&src, "b"), Type::Enum("Shape".into(), Vec::new()));
        ok(&main(
            "let c = match p { P(1, Some(\"a\")) => 1, P(n, _) => n }; let d = match s { Some(Empty) => 1, _ => 2 };",
        ));
    }

    #[test]
    fn nested_patterns_must_fit_their_slots() {
        assert_eq!(
            errors(&main("match p { P(1, Some(1)) => {} _ => {} }")),
            [("E0403", "1")]
        );
        assert_eq!(
            errors(&main("match s { Some(Circle(Rect(_, _))) => {} _ => {} }")),
            [("E0403", "Rect(_, _)")]
        );
        assert_eq!(
            errors(&main("match s { Some(Rect(_)) => {} _ => {} }")),
            [("E0416", "Rect(_)")]
        );
        assert_eq!(
            errors(&main("match s { Nope => {} _ => {} }")),
            [("E0415", "Nope")]
        );
    }
}
