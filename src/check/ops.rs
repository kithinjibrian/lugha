//! Binary operators and literal inference between operands (spec §4).

use super::expr::{Expect, is_literal};
use super::{Checker, Checking, Type, errors};
use crate::ast::{BinOp, Expr};
use crate::span::Span;

impl Checker {
    /// Types a binary operator per the spec §4 table.
    pub(super) fn binary(
        &mut self,
        expr: &Expr,
        op: BinOp,
        lhs: &Expr,
        rhs: &Expr,
        expect: Option<&Expect>,
    ) -> Checking<Type> {
        if matches!(op, BinOp::And | BinOp::Or) {
            let boolean = Expect::of(Type::Bool);
            let (a, b) = (
                self.expect_type(lhs, &boolean)?,
                self.expect_type(rhs, &boolean)?,
            );
            return Ok(if a == Type::Error || b == Type::Error {
                Type::Error
            } else {
                Type::Bool
            });
        }
        let arithmetic = matches!(
            op,
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem
        );
        // Only arithmetic passes the context's type to its operands; a comparison's `bool` doesn't.
        let outer = if arithmetic { expect } else { None };
        let (l, r) = self.operands(lhs, rhs, outer)?;
        let (l, r) = match (l, r) {
            (Type::Error, _) | (_, Type::Error) => return Ok(Type::Error),
            (Type::Never, ty) | (ty, Type::Never) => (ty, ty),
            pair => pair,
        };
        let allowed = l == r
            && match op {
                // Strings join with `+` and compare by content (spec §4).
                BinOp::Add if l == Type::String => true,
                BinOp::Eq | BinOp::Ne => l.is_numeric() || matches!(l, Type::Bool | Type::String),
                _ => l.is_numeric(),
            };
        if !allowed && l != Type::Never {
            let types = if l == r { vec![l] } else { vec![l, r] };
            self.report(errors::bad_operands(op.symbol(), &types, expr.span));
            return Ok(Type::Error);
        }
        Ok(if arithmetic { l } else { Type::Bool })
    }

    /// Checks two operands (or range bounds) so a literal takes its type from
    /// the other side: the non-literal is checked first (spec §4 rule 3).
    pub(super) fn operands(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        outer: Option<&Expect>,
    ) -> Checking<(Type, Type)> {
        if is_literal(lhs) && !is_literal(rhs) {
            let r = self.value(rhs, outer)?;
            let l = self.value(lhs, from_operand(r, rhs.span).as_ref())?;
            return Ok((l, r));
        }
        let l = self.value(lhs, outer)?;
        let expect = match outer {
            Some(outer) if is_literal(lhs) => Some(outer.clone()),
            _ => from_operand(l, lhs.span),
        };
        let r = self.value(rhs, expect.as_ref())?;
        Ok((l, r))
    }
}

impl Checker {
    /// `expr as T` between numeric types; `bool` can't be cast (spec §4).
    pub(super) fn cast(
        &mut self,
        expr: &Expr,
        inner: &Expr,
        target: &crate::ast::Type,
    ) -> Checking<Type> {
        let to = self.resolve(target)?;
        let from = self.value(inner, None)?;
        if matches!(from, Type::Error | Type::Never) || to == Type::Error {
            return Ok(to);
        }
        if from.is_numeric() && to.is_numeric() {
            return Ok(to);
        }
        self.report(errors::bad_cast(from, to, expr.span));
        Ok(Type::Error)
    }
}

/// The type an operand gives the other side. Only numeric types are passed
/// on: `true + 1` is an operator error, not a literal error.
fn from_operand(ty: Type, span: Span) -> Option<Expect> {
    ty.is_numeric()
        .then(|| Expect::because(ty, span, format!("this operand is {ty}")))
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type};
    use crate::{check, lexer, parser};

    #[test]
    fn literal_operands_take_the_other_operands_type() {
        let src = "fun main() { let b: i32 = 5; let c = b + 1; let d = 1 + b; let e = 1 + 2; }";
        assert_eq!(let_type(src, "c"), Type::I32);
        assert_eq!(let_type(src, "d"), Type::I32);
        assert_eq!(let_type(src, "e"), Type::I64);
        assert_eq!(
            let_type("fun main() { let f: f64 = 1.5; let g = f < 2.0; }", "g"),
            Type::Bool
        );
    }

    #[test]
    fn e0401_matches_the_spec_example() {
        let src = "fun main() {\n    let x: i32 = 5;\n    let y = x + 2.5;\n}\n";
        let (tokens, _) = lexer::lex(src).unwrap();
        let (program, _) = parser::parse(&tokens).unwrap();
        let Err(check::CheckError::Program(diags)) = check::check(&program) else {
            panic!("expected E0401")
        };
        assert_eq!(diags.len(), 1, "{diags:?}");
        let d = &diags[0];
        assert_eq!(
            (d.code, d.message.as_str()),
            ("E0401", "float literal where i32 expected")
        );
        assert_eq!(
            (d.span.start, d.span.end, d.label.as_deref()),
            (49, 52, Some("expected i32"))
        );
        assert_eq!(d.labels.len(), 1);
        assert_eq!((d.labels[0].span.start, d.labels[0].span.end), (45, 46));
        assert_eq!(d.labels[0].message, "this operand is i32");
    }

    #[test]
    fn operand_types_must_fit_the_operator() {
        let cases = [
            ("fun main() { let a = true + 1; }", ("E0404", "true + 1")),
            ("fun main() { let b = true; let c = -b; }", ("E0404", "-b")),
            ("fun main() { let x = 1; let n = !x; }", ("E0404", "!x")),
            ("fun main() { let u: u8 = 1; let v = -u; }", ("E0404", "-u")),
            (
                "fun main() { let x = 1; let t = x == true; }",
                ("E0404", "x == true"),
            ),
            (
                "fun main() { let x: i32 = 1; let y: i64 = 2; let z = x + y; }",
                ("E0404", "x + y"),
            ),
            ("fun main() { let b = true && 1; }", ("E0401", "1")),
        ];
        for (src, want) in cases {
            assert_eq!(errors(src), [want], "{src}");
        }
    }

    #[test]
    fn one_mistake_gives_one_diagnostic() {
        // `y` is an error after the E0401, so the following uses stay silent.
        let src = "fun main() { let x: i32 = 5; let y = x + 2.5; let z = y * 2; let w = z + y; }";
        assert_eq!(errors(src), [("E0401", "2.5")]);
        assert_eq!(
            errors("fun main() { let a = nope + 1; let b = a * 2; }"),
            [("E0301", "nope")]
        );
    }

    #[test]
    fn numeric_casts_are_allowed_and_bool_casts_are_e0408() {
        assert_eq!(
            let_type("fun main() { let a = 2.5 as i32; }", "a"),
            Type::I32
        );
        assert_eq!(let_type("fun main() { let b = 300 as u8; }", "b"), Type::U8);
        assert_eq!(
            let_type("fun main() { let x: i64 = 1; let c = x as i64; }", "c"),
            Type::I64
        );
        assert_eq!(
            let_type("fun main() { let x: u8 = 1; let d = x as f64; }", "d"),
            Type::F64
        );
        assert_eq!(
            errors("fun main() { let a = true as i32; }"),
            [("E0408", "true as i32")]
        );
        assert_eq!(
            errors("fun main() { let x = 1; let b = x as bool; }"),
            [("E0408", "x as bool")]
        );
    }
}
