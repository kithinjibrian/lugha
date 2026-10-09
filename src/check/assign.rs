//! Assignment: places (spec §3) and mutability (spec §4).

use super::expr::Expect;
use super::{Binding, Checker, Checking, Type, errors};
use crate::ast::{AssignOp, Expr, ExprKind};
use crate::span::Span;

impl Checker {
    /// `place op value;` — the place must be a variable, field or element,
    /// rooted in a `let mut` binding.
    pub(super) fn assign(&mut self, op: AssignOp, place: &Expr, value: &Expr) -> Checking<()> {
        let ExprKind::Name(name) = &place.kind else {
            // Typing the place reports E0410/E0411 for a bad field or index.
            self.expr(place, None)?;
            match &place.kind {
                ExprKind::Field(base, _) | ExprKind::Index(base, _, _) => {
                    if self.types[base.id.0 as usize] == Some(Type::String) {
                        self.report(errors::string_immutable(place.span));
                    }
                }
                _ => self.report(errors::not_place(place.span)),
            }
            self.expr(value, None)?;
            return Ok(());
        };
        let Some(local) = self.local(name) else {
            // Reports E0301, or E0406 for a function name.
            self.expr(place, None)?;
            self.expr(value, None)?;
            return Ok(());
        };
        self.record(place, local.ty);
        if local.binding != (Binding::Let { mutable: true }) {
            self.report(errors::not_mutable(
                name,
                place.span,
                local.binding,
                local.span,
            ));
        }
        let target = local.ty;
        if op != AssignOp::Assign && !target.is_numeric() && target != Type::Error {
            let span = Span::new(place.span.start, value.span.end);
            self.report(errors::bad_operands(op.symbol(), &[target], span));
            self.expr(value, None)?;
            return Ok(());
        }
        let why = format!("`{name}` is {target}");
        self.expect_type(value, &Expect::because(target, place.span, why))?;
        Ok(())
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
        use crate::check::{CheckError, check};
        let help = |src: &str| {
            let (tokens, _) = crate::lexer::lex(src).unwrap();
            let (program, _) = crate::parser::parse(&tokens).unwrap();
            let Err(CheckError::Program(d)) = check(&program) else {
                panic!("{src}")
            };
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
    }
}
