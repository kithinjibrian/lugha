//! Binary operators and type inference between operands (spec §4; v2 draft
//! §4: `==` on every type, string order, either operand typing the other).

use super::expr::{Expect, is_literal};
use super::{Checker, Type, errors};
use crate::ast::{BinOp, Expr, ExprKind};
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
    ) -> Type {
        if matches!(op, BinOp::And | BinOp::Or) {
            let boolean = Expect::of(Type::Bool);
            let (a, b) = (
                self.expect_type(lhs, &boolean),
                self.expect_type(rhs, &boolean),
            );
            return if a == Type::Error || b == Type::Error {
                Type::Error
            } else {
                Type::Bool
            };
        }
        let arithmetic = op.is_bitwise()
            || matches!(
                op,
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem
            );
        // Only arithmetic passes the context's type to its operands; a comparison's `bool` doesn't.
        let outer = if arithmetic { expect } else { None };
        // Comparing passes any type between operands (`o == None`); arithmetic only numbers.
        let (l, r) = self.operands(lhs, rhs, outer, !arithmetic);
        let (l, r) = match (l, r) {
            (Type::Error, _) | (_, Type::Error) => return Type::Error,
            (Type::Never, ty) | (ty, Type::Never) => (ty.clone(), ty),
            pair => pair,
        };
        let allowed = l == r
            && match op {
                // Strings join with `+` (spec §4).
                BinOp::Add if l == Type::String => true,
                // Every type has deep equality, numbers and strings are
                // ordered, and a parameter has what its bounds give (v2 draft §4).
                BinOp::Eq | BinOp::Ne => self.eq_ok(&l),
                BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => self.ord_ok(&l),
                // Bits are for integers; a shift's amount has the value's type (v7).
                _ if op.is_bitwise() => l.is_integer(),
                _ => l.is_numeric(),
            };
        if !allowed && l != Type::Never {
            let ord = !matches!(op, BinOp::Eq | BinOp::Ne);
            let wanted = self
                .missing_bound(&l, ord)
                .filter(|_| l == r && !arithmetic);
            let types = if l == r { vec![l] } else { vec![l, r] };
            let d = errors::bad_operands_on(op.symbol(), &types, expr.span);
            self.report(errors::with_bound_help(d, wanted, ord));
            return Type::Error;
        }
        if arithmetic { l } else { Type::Bool }
    }

    /// Checks two operands (or range bounds) so one that can't be typed
    /// alone (a literal, `None`, `[]`) takes its type from the other side,
    /// which is checked first (spec §4 rule 3; v2 draft §4). `any` passes
    /// every type across, not just numbers.
    pub(super) fn operands(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        outer: Option<&Expect>,
        any: bool,
    ) -> (Type, Type) {
        let swap = defers(lhs) && !defers(rhs);
        let (first, second) = if swap { (rhs, lhs) } else { (lhs, rhs) };
        let a = self.value(first, outer);
        let expect = match outer {
            Some(outer) if defers(first) => Some(outer.clone()),
            // Both sides need a type from context (`None == None`): the
            // first already reported E0412, so the second stays quiet.
            _ if a == Type::Error && defers(second) => Some(Expect::of(Type::Error)),
            _ => from_operand(&a, first.span, second, any),
        };
        let b = self.value(second, expect.as_ref());
        if swap { (b, a) } else { (a, b) }
    }
}

impl Checker {
    /// `expr as T` between numeric types; `bool` can't be cast (spec §4).
    pub(super) fn cast(&mut self, expr: &Expr, inner: &Expr, target: &crate::ast::Type) -> Type {
        let to = self.resolve(target);
        let from = self.value(inner, None);
        if matches!(from, Type::Error | Type::Never) || to == Type::Error {
            return to;
        }
        // `char` casts to and from any integer (v14).
        let char_cast =
            (from == Type::Char && to.is_integer()) || (to == Type::Char && from.is_integer());
        if (from.is_numeric() && to.is_numeric()) || char_cast {
            return to;
        }
        self.report(errors::bad_cast(from, to, expr.span));
        Type::Error
    }
}

/// The type an operand gives the other side, `to`. Numeric types always
/// pass; with `any`, every type does, except to a numeric literal: `true + 1`
/// and `"a" == 1` are operator errors (E0404), not literal errors.
fn from_operand(ty: &Type, span: Span, to: &Expr, any: bool) -> Option<Expect> {
    let passes =
        ty.is_numeric() || (any && !is_literal(to) && !matches!(ty, Type::Error | Type::Never));
    passes.then(|| Expect::because(ty.clone(), span, format!("this operand is {ty}")))
}

/// True if `e` can't be typed without an expected type: a numeric literal,
/// `None`, `[]`, or a list, repeat or `Some` whose first value is one.
pub(super) fn defers(e: &Expr) -> bool {
    match &e.kind {
        _ if is_literal(e) => true,
        ExprKind::Name(name) => name == "None",
        ExprKind::Array(elements) => elements.first().is_none_or(defers),
        ExprKind::Repeat(value, _) => defers(value),
        // A closure waits for its parameter types (v3).
        ExprKind::Closure(c) => c.params.iter().any(|(_, t)| t.is_none()),
        ExprKind::Call(callee, args) => {
            matches!(&callee.kind, ExprKind::Name(n) if n == "Some")
                && args.first().is_some_and(defers)
        }
        _ => false,
    }
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
        let Err(diags) = check::check(&program) else {
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
    fn equality_is_deep_and_strings_are_ordered() {
        let types = "struct P { xs: i64[][], s: string }\nenum E { A(P), B }\n";
        let src = format!(
            "{types}fun main() {{ let p = P {{ xs: [[1]], s: \"a\" }}; let e = E.A(p); \
             let a = p == p; let b = e != E.B; let c = [[1.5]] == [[2.5]]; let d = \"a\" < \"b\"; }}"
        );
        for name in ["a", "b", "c", "d"] {
            assert_eq!(let_type(&src, name), Type::Bool, "{name}");
        }
        let order = [
            ("fun main() { let a = \"a\" < 1; }", "\"a\" < 1"),
            ("fun main() { let a = true < false; }", "true < false"),
            ("fun main() { let a = \"a\" == 1; }", "\"a\" == 1"),
            ("fun main() { let a = [1] < [2]; }", "[1] < [2]"),
        ];
        for (src, at) in order {
            assert_eq!(errors(src), [("E0404", at)], "{src}");
        }
        assert_eq!(
            errors("struct A {}\nstruct B {}\nfun main() { let e = A {} == B {}; }"),
            [("E0404", "A {} == B {}")]
        );
    }

    #[test]
    fn either_operand_can_type_the_other() {
        let o = "fun main() { let o: Option<u8> = Some(3); let xs: u8[] = [1];";
        for (i, test) in [
            "o == None",
            "None == o",
            "o == Some(3)",
            "xs == []",
            "[] == xs",
            "[1] == xs",
        ]
        .iter()
        .enumerate()
        {
            let src = format!("{o} let t{i} = {test}; }}");
            assert_eq!(let_type(&src, &format!("t{i}")), Type::Bool, "{test}");
        }
        assert_eq!(
            errors("fun main() { let e = None == None; }"),
            [("E0412", "None")]
        );
        assert_eq!(
            errors("fun main() { let e = [] == []; }"),
            [("E0412", "[]")]
        );
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
