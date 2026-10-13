//! Type patterns (v5 draft §4, milestone 21): `c: Circle` matches when an
//! interface value holds a `Circle`, and binds it; E0426 for misuse.

use super::patterns::{Ctor, Pat};
use super::{Checker, Type, errors};
use crate::ast::{Ident, Pattern, Type as Annotation, TypeKind};

impl Checker {
    /// `name: T` or `_: T` against a value of type `ty`, which must be an
    /// interface that `T`, a concrete type, satisfies (E0426 otherwise).
    pub(super) fn type_pattern(
        &mut self,
        pattern: &Pattern,
        name: Option<&Ident>,
        written: &Annotation,
        ty: &Type,
        bindings: &mut Vec<(Ident, Type)>,
    ) -> Pat {
        let span = pattern.span;
        let Some(interface) = super::applied::Applied::of(ty) else {
            if !matches!(ty, Type::Error | Type::Never) {
                let message = format!("a type pattern needs an interface value, found `{ty}`");
                self.report(errors::bad_type_pattern(message, span, None));
            }
            return Pat::Wild;
        };
        // In a pattern nothing can infer a generic type's arguments.
        if let TypeKind::Named(n) = &written.kind
            && !self.generics_of(&self.key(n)).is_empty()
            && !self.type_params.iter().any(|g| g.name == *n)
        {
            let message = format!("`{n}` is generic: write its type arguments, `{n}<…>`");
            self.report(errors::bad_type_pattern(message, span, None));
            return Pat::Wild;
        }
        let found = self.resolve(written);
        let concrete = "a type pattern names a concrete type";
        let problem = match &found {
            Type::Error => return Pat::Wild,
            Type::Interface(..) => Some((format!("`{found}` is an interface; {concrete}"), None)),
            Type::Param(_) => Some((format!("`{found}` is a type parameter; {concrete}"), None)),
            _ => {
                let unmet = self.unmet(&found, &interface);
                let message =
                    format!("`{found}` doesn't satisfy `{ty}`, so this arm can never match");
                (!unmet.is_empty()).then(|| (message, Some(unmet.join("; "))))
            }
        };
        if let Some((message, help)) = problem {
            self.report(errors::bad_type_pattern(message, span, help));
            return Pat::Wild;
        }
        if let Some(name) = name {
            bindings.push((name.clone(), found.clone()));
        }
        Pat::Ctor(Ctor::Type(found.to_string()), Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{SHAPES, diagnostics, errors, ok, warnings};

    fn e(body: &str) -> Vec<(&'static str, String)> {
        let src = format!("{SHAPES}{body}\nfun main() {{}}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    fn help(body: &str) -> String {
        let src = format!("{SHAPES}{body}\nfun main() {{}}");
        diagnostics(&src)[0].help.clone().unwrap_or_default()
    }

    #[test]
    fn type_patterns_bind_the_value_inside() {
        ok(&format!(
            "{SHAPES}fun f(s: Shape): f64 = match s {{\n\
                 c: Circle if c.r > 1.0 => c.r,\n\
                 c: Circle => c.area(),\n\
                 _: Square => 0.0,\n\
                 _ => 1.0,\n\
             }};\n\
             fun g(o: Option<Shape>): f64 = match o {{ Some(c: Circle) => c.r, Some(_) => 1.0, None => 0.0 }};\n\
             fun main() {{}}"
        ));
        assert_eq!(
            e("fun f(s: Shape): i64 = match s { c: Circle => { let r: i64 = c.r; r } _ => 0 };"),
            [("E0403", "c.r".into())]
        );
    }

    #[test]
    fn misused_type_patterns_are_e0426() {
        assert_eq!(
            e("fun f(n: i64): i64 = match n { c: Circle => 1, _ => 0 };"),
            [("E0426", "c: Circle".into())]
        );
        assert_eq!(
            e("fun f(s: Shape): i64 = match s { t: Shape => 1, _ => 0 };"),
            [("E0426", "t: Shape".into())]
        );
        let dot = "struct Dot {}\nfun f(s: Shape): i64 = match s { d: Dot => 1, _ => 0 };";
        assert_eq!(e(dot), [("E0426", "d: Dot".into())]);
        assert!(help(dot).contains("no method `area`"), "{}", help(dot));
        assert_eq!(
            e(
                "struct Stack<T> { items: T[] }\nfun f(s: Shape): i64 = match s { x: Stack => 1, _ => 0 };"
            ),
            [("E0426", "x: Stack".into())]
        );
        assert_eq!(
            e("fun f<T>(s: Shape, t: T): i64 = match s { x: T => 1, _ => 0 };"),
            [("E0426", "x: T".into())]
        );
    }

    #[test]
    fn a_match_on_an_interface_value_needs_a_wildcard() {
        let body = "fun f(s: Shape): i64 = match s { _: Circle => 1, _: Square => 2 };";
        assert_eq!(e(body)[0].0, "E0417");
        let src = format!("{SHAPES}{body}\nfun main() {{}}");
        assert!(
            diagnostics(&src)[0].message.contains('_'),
            "{:?}",
            diagnostics(&src)
        );
    }

    #[test]
    fn a_repeated_type_is_unreachable_unless_guarded() {
        let twice = format!(
            "{SHAPES}fun f(s: Shape): i64 = match s {{ _: Circle => 1, _: Circle => 2, _ => 0 }};\nfun main() {{}}"
        );
        assert_eq!(warnings(&twice)[0].0, "W0102");
        let guarded = format!(
            "{SHAPES}fun f(s: Shape): i64 = match s {{ c: Circle if c.r > 1.0 => 1, _: Circle => 2, _ => 0 }};\nfun main() {{}}"
        );
        assert!(warnings(&guarded).is_empty());
    }

    #[test]
    fn bindings_are_immutable() {
        assert_eq!(
            e("fun f(s: Shape) { match s { c: Circle => { c.grow(1.0); } _ => {} } }")[0].0,
            "E0501"
        );
    }
}
