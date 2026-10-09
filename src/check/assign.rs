//! Assignment: places (spec §3), mutability rooted at the variable (spec §4),
//! immutable strings, and the iteration guard (spec §5).

use super::expr::Expect;
use super::{Binding, Checker, Checking, Type, errors};
use crate::ast::{AssignOp, Expr, ExprKind};
use crate::span::Span;

impl Checker {
    /// `place op value;` — the place must be a variable, field or element
    /// whose root is a `let mut` binding, not inside a string, and not part of
    /// an array being iterated.
    pub(super) fn assign(&mut self, op: AssignOp, place: &Expr, value: &Expr) -> Checking<()> {
        // Typing the place reports E0301/E0406/E0410/E0411 as for any expression.
        let target = self.expr(place, None)?;
        if !self.check_place(place) {
            self.expr(value, None)?;
            return Ok(());
        }
        if op != AssignOp::Assign && !target.is_numeric() && target != Type::Error {
            let span = Span::new(place.span.start, value.span.end);
            self.report(errors::bad_operands(op.symbol(), &[target], span));
            self.expr(value, None)?;
            return Ok(());
        }
        let why = format!("`{}` is {target}", root_text(place));
        self.expect_type(value, &Expect::because(target, place.span, why))?;
        Ok(())
    }

    /// Reports why `place` can't be assigned, if it can't; true when the
    /// value should still be type-checked against it.
    fn check_place(&mut self, place: &Expr) -> bool {
        match &place.kind {
            ExprKind::Name(_) => {}
            ExprKind::Field(base, _) | ExprKind::Index(base, _, _) => {
                let base_ty = self.types[base.id.0 as usize].clone();
                if base_ty == Some(Type::String) {
                    self.report(errors::string_immutable(place.span));
                    return false;
                }
                // `.len` reads a header; it is not a field you can assign.
                if matches!(place.kind, ExprKind::Field(..))
                    && matches!(base_ty, Some(Type::Array(_)))
                {
                    self.report(errors::not_place(place.span));
                    return false;
                }
            }
            _ => {
                self.report(errors::not_place(place.span));
                return false;
            }
        }
        let Some(path) = self.place_path(place) else {
            // Rooted in an undefined name (already E0301) or a non-place base.
            if !matches!(place.kind, ExprKind::Name(_)) && root_name(place).is_none() {
                self.report(errors::not_place(place.span));
            }
            return true;
        };
        let name = root_name(place).expect("a place path has a root name");
        if let Some(local) = self.local(name)
            && local.binding != (Binding::Let { mutable: true })
        {
            self.report(errors::not_mutable(
                name,
                place.span,
                local.binding,
                local.span,
            ));
        }
        if let Some((_, for_span)) = self
            .iterating
            .iter()
            .find(|(iterated, _)| iterated.overlaps(&path))
        {
            let for_span = *for_span;
            self.report(errors::assign_while_iterating(name, place.span, for_span));
        }
        true
    }
}

/// The variable at the root of a place chain, if the chain is rooted in one.
fn root_name(place: &Expr) -> Option<&str> {
    match &place.kind {
        ExprKind::Name(name) => Some(name),
        ExprKind::Field(base, _) | ExprKind::Index(base, _, _) => root_name(base),
        _ => None,
    }
}

/// How a place is named in a "`x` is T" label.
fn root_text(place: &Expr) -> String {
    match &place.kind {
        ExprKind::Name(name) => name.clone(),
        _ => "this element".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    #[test]
    fn immutable_bindings_are_e0501() {
        assert_eq!(errors("fun main() { let x = 1; x = 2; }"), [("E0501", "x")]);
        assert_eq!(
            errors("fun main() { let x = 1; x += 2; }"),
            [("E0501", "x")]
        );
        assert_eq!(
            errors("fun f(n: i64) { n = 2; }\nfun main() {}"),
            [("E0501", "n")]
        );
        assert_eq!(
            errors("fun main() { for i in 0..3 { i = 5; } }"),
            [("E0501", "i")]
        );
        ok("fun main() { let mut x = 1; x = 2; x += 3; }");
        ok("fun f(n: i64) { let mut n = n; n -= 1; }\nfun main() {}");
    }

    #[test]
    fn e0501_help_depends_on_the_binding() {
        let help = |src: &str| {
            let d = crate::check::test_util::diagnostics(src);
            (d[0].help.clone().unwrap_or_default(), d[0].labels.len())
        };
        assert!(
            help("fun main() { let x = 1; x = 2; }")
                .0
                .contains("let mut x")
        );
        assert!(
            help("fun f(n: i64) { n = 2; }\nfun main() {}")
                .0
                .contains("let mut n = n;")
        );
        assert!(
            help("fun main() { for i in 0..3 { i = 5; } }")
                .0
                .contains("loop variables")
        );
        // A label points at the declaration.
        assert_eq!(help("fun main() { let x = 1; x = 2; }").1, 1);
    }

    #[test]
    fn non_places_are_e0502() {
        assert_eq!(errors("fun main() { 1 = 2; }"), [("E0502", "1")]);
        assert_eq!(
            errors("fun f(): i64 = 1;\nfun main() { f() = 3; }"),
            [("E0502", "f()")]
        );
        assert_eq!(
            errors("fun main() { let a = 1; let b = 2; (a + b) += 1; }"),
            [("E0502", "a + b")]
        );
        assert_eq!(
            errors("fun g(): i64[] = [1];\nfun main() { g()[0] = 2; }"),
            [("E0502", "g()[0]")]
        );
    }
}
