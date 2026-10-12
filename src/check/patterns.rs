//! Pattern typing (v1 §4; v2 draft §3, §5): each pattern is checked against
//! the type it matches, at any depth, giving its bindings and a `Pat` for
//! the usefulness check.

use super::{Checker, Type, errors, slot_names_variant};
use crate::ast::{Ident, Pattern, PatternKind};

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
}

impl Checker {
    /// Types the top-level `pattern` against `ty`, adding its bindings.
    pub(super) fn arm_pattern(
        &mut self,
        pattern: &Pattern,
        ty: &Type,
        bindings: &mut Vec<(Ident, Type)>,
    ) -> Pat {
        self.pattern_at(pattern, ty, bindings, true)
    }

    /// One pattern at any depth. A bare name in a payload slot binds the
    /// payload unless the slot's type has a variant by that name (v2 draft
    /// §3); at the top level it must be a variant.
    fn pattern_at(
        &mut self,
        pattern: &Pattern,
        ty: &Type,
        bindings: &mut Vec<(Ident, Type)>,
        top: bool,
    ) -> Pat {
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
            PatternKind::Variant(name, None)
                if !top && !slot_names_variant(self.variants_of(ty).as_deref(), &name.name) =>
            {
                bindings.push((name.clone(), ty.clone()));
                Pat::Wild
            }
            PatternKind::Variant(name, slots) => {
                self.variant_pattern(pattern, name, slots.as_deref(), ty, bindings)
            }
        }
    }

    fn variant_pattern(
        &mut self,
        pattern: &Pattern,
        name: &Ident,
        slots: Option<&[Pattern]>,
        ty: &Type,
        bindings: &mut Vec<(Ident, Type)>,
    ) -> Pat {
        if matches!(ty, Type::Error | Type::Never) {
            return Pat::Wild;
        }
        let Some(variants) = self.variants_of(ty) else {
            self.report(errors::pattern_mismatch(ty, pattern.span));
            return Pat::Wild;
        };
        let Some((index, (_, payloads))) = variants
            .into_iter()
            .enumerate()
            .find(|(_, (v, _))| *v == name.name)
        else {
            self.report(errors::no_variant(&name.name, ty, name.span));
            return Pat::Wild;
        };
        let slots = slots.unwrap_or(&[]);
        if slots.len() != payloads.len() {
            self.report(errors::payload_count(
                &name.name,
                payloads.len(),
                slots.len(),
                pattern.span,
            ));
            return Pat::Wild;
        }
        let args = slots
            .iter()
            .zip(&payloads)
            .map(|(slot, payload)| self.pattern_at(slot, payload, bindings, false))
            .collect();
        Pat::Ctor(Ctor::Variant(index, name.name.clone()), args)
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
        assert_eq!(let_type(&src, "b"), Type::Enum("Shape".into()));
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
