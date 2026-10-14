//! Or-patterns and `mut` bindings (v9 draft §4, milestone 31): every
//! alternative of `p | q` binds the same names at the same types (E0428),
//! and `mut name` makes the binding the arm's own mutable copy.

use super::patterns::{Bindings, Pat};
use super::{Checker, Type, errors, slot_names_variant};
use crate::ast::{Pattern, PatternKind};
use crate::span::Span;

impl Checker {
    /// The ways each alternative can match, with its span. The first
    /// alternative's bindings are the pattern's; each other alternative must
    /// bind the same names at the same types (E0428).
    pub(super) fn or_alternatives(
        &mut self,
        alternatives: &[Pattern],
        ty: &Type,
        bindings: &mut Bindings,
        top: bool,
    ) -> Vec<(Vec<Pat>, Span)> {
        let mut first: Option<Bindings> = None;
        let mut groups = Vec::new();
        for alternative in alternatives {
            let mut own = Bindings::new();
            let pats = self.pattern_at(alternative, ty, &mut own, top);
            match &first {
                None => first = Some(own),
                Some(first) => self.same_bindings(first, &own, alternative.span),
            }
            groups.push((pats, alternative.span));
        }
        bindings.extend(first.unwrap_or_default());
        groups
    }

    /// E0428 unless `other` binds what `first` does, name for name and type
    /// for type.
    fn same_bindings(&mut self, first: &Bindings, other: &Bindings, at: Span) {
        let named = |list: &Bindings, name: &str| {
            list.iter()
                .find(|(n, _, _)| n.name == name)
                .map(|(_, ty, _)| ty.clone())
        };
        for (name, ty, _) in first {
            match named(other, &name.name) {
                None => {
                    let why = format!("`{}` is not bound in this alternative", name.name);
                    return self.report(errors::or_bindings(why, at));
                }
                Some(other_ty) if other_ty != *ty && !matches!(other_ty, Type::Error) => {
                    let why = format!(
                        "`{}` is {ty} in the first alternative but {other_ty} here",
                        name.name
                    );
                    return self.report(errors::or_bindings(why, at));
                }
                Some(_) => {}
            }
        }
        if let Some((extra, _, _)) = other
            .iter()
            .find(|(n, _, _)| named(first, &n.name).is_none())
        {
            let why = format!(
                "`{}` is bound here but not in the first alternative",
                extra.name
            );
            self.report(errors::or_bindings(why, extra.span));
        }
    }

    /// `mut name` or `mut name: T`: the inner binding, marked mutable. A
    /// name that is a variant here can't be `mut` (E0428).
    pub(super) fn mut_pattern(
        &mut self,
        inner: &Pattern,
        ty: &Type,
        bindings: &mut Bindings,
        top: bool,
    ) -> Vec<Pat> {
        if let PatternKind::Variant(name, None) = &inner.kind {
            // `mut` makes a bare name a binding, even at the top level,
            // unless the matched type has a variant by that name.
            let binds = !slot_names_variant(self.variants_of(ty).as_deref(), &name.name);
            if !binds {
                let why = format!("`mut` needs a binding, but `{}` is a variant", name.name);
                self.report(errors::or_bindings(why, inner.span));
                return vec![Pat::Wild];
            }
            bindings.push((name.clone(), ty.clone(), true));
            return vec![Pat::Wild];
        }
        let before = bindings.len();
        let pats = self.pattern_at(inner, ty, bindings, top);
        for binding in &mut bindings[before..] {
            binding.2 = true;
        }
        pats
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok, warnings};

    const SHAPE: &str = "enum Shape { Circle(f64), Square(f64), Count(i64), Empty }\n\
        enum Color { Red, Green, Blue }\n";

    fn codes(body: &str) -> Vec<&'static str> {
        let src = format!("{SHAPE}fun main() {{ let s = Shape.Empty; let c = Color.Red; {body} }}");
        // Only errors: an unused binding's W0103 is beside the point here.
        let found = errors(&src).into_iter().map(|(c, _)| c);
        found.filter(|c| c.starts_with('E')).collect()
    }

    #[test]
    fn alternatives_bind_the_same_names() {
        let src = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; let r = match s {{ Circle(x) | Square(x) => x, _ => 0.0 }}; }}"
        );
        assert_eq!(let_type(&src, "r"), Type::F64);
        ok(&format!(
            "{SHAPE}fun main() {{ let n = match Some(3) {{ Some(1 | 2) => 1, Some(_) | None => 2 }}; }}"
        ));
        assert_eq!(
            codes("match s { Circle(x) | Empty => {} _ => {} }"),
            ["E0428"]
        );
        assert_eq!(
            codes("match s { Circle(x) | Count(x) => {} _ => {} }"),
            ["E0428"]
        );
        assert_eq!(
            codes("match s { Empty | Circle(x) => {} _ => {} }"),
            ["E0428"]
        );
        assert_eq!(codes("match Some(1) { mut None => {} _ => {} }"), ["E0428"]);
    }

    #[test]
    fn or_patterns_count_for_coverage() {
        assert_eq!(codes("match c { Red | Green => {} }"), ["E0417"]);
        ok(&format!(
            "{SHAPE}fun main() {{ let c = Color.Red; match c {{ Red | Green => {{}} Blue => {{}} }} }}"
        ));
        let src = format!(
            "{SHAPE}fun main() {{ let c = Color.Red; match c {{ Red => {{}} Red | Blue => {{}} Green => {{}} }} }}"
        );
        assert_eq!(warnings(&src), [("W0102", "Red")]);
    }

    #[test]
    fn mut_bindings_are_the_arms_own() {
        ok("fun main() { match Some(1) { Some(mut n) => { n += 1; } None => {} } }");
        assert_eq!(
            errors("fun main() { match Some(1) { Some(n) => { n += 1; } None => {} } }")
                .into_iter()
                .map(|(c, _)| c)
                .collect::<Vec<_>>(),
            ["E0501"]
        );
        let src = "fun main() { let k = match 5 { mut n => { n += 1; n } }; }";
        assert_eq!(let_type(src, "k"), Type::I64);
    }
}
