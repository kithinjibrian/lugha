//! Numeric literals: the expected type, or `i64`/`f64` by default, with range
//! checks after folding a directly applied `-` (spec §4 rules 3–6).

use std::collections::HashSet;

use super::expr::Expect;
use super::generics::subst;
use super::instances::{Instance, show};
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, UnOp};
use crate::span::Span;

impl Checker {
    /// A numeric literal takes the expected type, or `i64`/`f64` (spec §4).
    pub(super) fn literal(&mut self, expr: &Expr, expect: Option<&Expect>) -> Type {
        let (negated, kind) = match &expr.kind {
            ExprKind::Unary(UnOp::Neg, inner) => (true, &inner.kind),
            kind => (false, kind),
        };
        let expected = expect
            .map(|e| e.ty.clone())
            // Facing an interface, a literal takes its default type and converts (v5).
            .filter(|t| {
                !matches!(
                    t,
                    Type::Unit | Type::Never | Type::Error | Type::Interface(..)
                )
            });
        let reason = expect.and_then(|e| e.reason.as_ref());
        match (kind, expected) {
            (ExprKind::Float(_), None | Some(Type::F64)) => Type::F64,
            // Any float literal is a `T: Float` (v15, milestone 58).
            (ExprKind::Float(_), Some(Type::Param(p))) if self.has_float(&p) => Type::Param(p),
            (ExprKind::Float(_), Some(Type::F32)) => Type::F32,
            (ExprKind::Float(_), Some(ty)) => {
                self.report(errors::wrong_literal(true, ty, expr.span, reason));
                Type::Error
            }
            // Any integer literal is a `T: Num` (v15, milestones 52 and 58):
            // each instance makes it its type, and checks its range
            // (`instance_literals`).
            (ExprKind::Int(value), Some(Type::Param(p))) if self.has_num(&p) => {
                self.param_literals.push(ParamLiteral {
                    owner: self.owner.clone(),
                    param: p.clone(),
                    value: *value,
                    negated,
                    span: expr.span,
                });
                Type::Param(p)
            }
            // Only integer types hold integer literals: not f64, bool or string.
            (ExprKind::Int(_), Some(ty)) if !ty.is_integer() => {
                self.report(errors::wrong_literal(false, ty, expr.span, reason));
                Type::Error
            }
            (ExprKind::Int(value), expected) => {
                let ty = expected.unwrap_or(Type::I64);
                if fits(&ty, *value, negated) {
                    ty
                } else {
                    self.report(errors::out_of_range(ty, expr.span));
                    Type::Error
                }
            }
            _ => unreachable!("only numeric literals reach here"),
        }
    }
}

/// An integer literal whose type is a type parameter, for the range check
/// at each instance (v15, milestone 58).
#[derive(Debug, Clone)]
pub(super) struct ParamLiteral {
    /// The key of the generic body it is in.
    owner: String,
    param: String,
    value: u64,
    negated: bool,
    span: Span,
}

impl Checker {
    /// E0402 for each integer literal typed as a type parameter that doesn't
    /// fit that parameter's type at some instance, once per literal and type.
    pub(super) fn instance_literals(&mut self, monos: &[Instance]) {
        let mut seen: HashSet<(Span, Type)> = HashSet::new();
        let mut found = Vec::new();
        for instance in monos {
            let generics = self.params_of(&instance.key);
            for literal in self
                .param_literals
                .iter()
                .filter(|l| l.owner == instance.key)
            {
                let ty = subst(
                    &Type::Param(literal.param.clone()),
                    &generics,
                    &instance.args,
                );
                if !ty.is_integer()
                    || fits(&ty, literal.value, literal.negated)
                    || !seen.insert((literal.span, ty.clone()))
                {
                    continue;
                }
                found.push(errors::out_of_range_in(ty, literal.span, &show(instance)));
            }
        }
        found.into_iter().for_each(|d| self.report(d));
    }
}

/// Whether integer literal `value` (`-value` when `negated`) fits `ty`. A
/// negated unsigned literal never fits: no negative values (spec §4 rule 6).
fn fits(ty: &Type, value: u64, negated: bool) -> bool {
    let (max, negated_max) = ty.literal_range();
    if negated {
        value <= negated_max && !ty.is_unsigned()
    } else {
        value <= max
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    #[test]
    fn literals_default_to_i64_and_f64() {
        let src =
            "fun main() { let a = 5; let b: i32 = 5; let d = 2.5; let n = -9223372036854775808; }";
        assert_eq!(let_type(src, "a"), Type::I64);
        assert_eq!(let_type(src, "b"), Type::I32);
        assert_eq!(let_type(src, "d"), Type::F64);
        assert_eq!(let_type(src, "n"), Type::I64);
    }

    #[test]
    fn wrong_literal_kind_is_e0401() {
        assert_eq!(errors("fun main() { let e: f64 = 5; }"), [("E0401", "5")]);
        assert_eq!(
            errors("fun main() { let i: i32 = 2.5; }"),
            [("E0401", "2.5")]
        );
        assert_eq!(errors("fun main() { if 5 { } }"), [("E0401", "5")]);
        assert_eq!(errors("fun main() { let b = !1; }"), [("E0401", "1")]);
    }

    #[test]
    fn literals_out_of_range_are_e0402() {
        assert_eq!(
            errors("fun main() { let b: u8 = 256; }"),
            [("E0402", "256")]
        );
        assert_eq!(errors("fun main() { let c: u8 = -1; }"), [("E0402", "-1")]);
        assert_eq!(
            errors("fun main() { let d: i32 = 2147483648; }"),
            [("E0402", "2147483648")]
        );
        assert_eq!(
            errors("fun main() { let n = 9223372036854775808; }"),
            [("E0402", "9223372036854775808")]
        );
        ok("fun main() { let m: i32 = -2147483648; let u: u8 = 255; }");
    }

    #[test]
    fn numeric_type_parameters_cast_both_ways_v15() {
        ok("fun mean<T: Num>(xs: T[]): T { let mut s: T = 0; for x of xs { s += x; } s / xs.len as T }
            fun back<T: Num>(x: T): f64 = x as f64;
            fun across<T: Num, U: Float>(x: T): U = x as U;
            fun same<T: Float>(x: T): T = x as T;
            fun main() { let m = mean([1, 2]); let b = back(2.5); let a: f32 = across(3); let s = same(1.5); }");
        for (src, cast) in [
            ("fun f<T>(n: i64): T = n as T;", "n as T"),
            ("fun f<T: Eq>(n: i64): T = n as T;", "n as T"),
            ("fun f<T: Num>(b: bool): T = b as T;", "b as T"),
            ("fun f<T: Num>(x: T): char = x as char;", "x as char"),
        ] {
            assert_eq!(
                errors(&format!("{src}\nfun main() {{}}")),
                [("E0408", cast)],
                "{src}"
            );
        }
    }

    #[test]
    fn any_literal_is_a_numeric_type_parameter_v15() {
        ok("fun double<T: Num>(x: T): T = x * 2;
            fun less<T: Num>(x: T): T = x - 10;
            fun half<T: Float>(x: T): T = x * 0.5;
            fun main() { let a = double(3); let b = double(1.5); let c = half(3.0); let d = less(2); }");
        assert_eq!(
            errors("fun half<T: Num>(x: T): T = x * 0.5;\nfun main() {}"),
            [("E0401", "0.5")]
        );
    }

    #[test]
    fn literals_out_of_range_for_an_instance_are_e0402_v15() {
        let big = "fun big<T: Num>(x: T): T = x + 300;\n";
        assert_eq!(
            errors(&format!("{big}fun main() {{ let a = big(1 as u8); }}")),
            [("E0402", "300")]
        );
        // One report per literal and type, however many calls.
        assert_eq!(
            errors(&format!(
                "{big}fun main() {{ let a = big(1 as u8); let b = big(2 as u8); let c = big(1); }}"
            )),
            [("E0402", "300")]
        );
        assert_eq!(
            errors("fun neg<T: Num>(x: T): T = x * -1;\nfun main() { let a = neg(1 as u32); }"),
            [("E0402", "-1")]
        );
        // Generic methods, and closures inside generic functions.
        assert_eq!(
            errors(
                "struct B<T> { v: T }\nimpl B<T: Num> { fun up(self): T = self.v + 1000; }\nfun main() { let b = B { v: 1 as i8 }; let c = b.up(); }"
            ),
            [("E0402", "1000")]
        );
        assert_eq!(
            errors(
                "fun g<T: Num>(x: T): T { let f = fun(y: T): T = y * 500; f(x) }\nfun main() { let a = g(1 as u8); }"
            ),
            [("E0402", "500")]
        );
        let d = crate::check::test_util::diagnostics(&format!(
            "{big}fun main() {{ let a = big(1 as u8); }}"
        ));
        assert!(
            d[0].help.as_deref().is_some_and(|h| h.contains("big<u8>")),
            "{d:?}"
        );
    }
}
