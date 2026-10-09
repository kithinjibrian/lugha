//! Numeric literals: the expected type, or `i64`/`f64` by default, with range
//! checks after folding a directly applied `-` (spec §4 rules 3–6).

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, UnOp};

impl Checker {
    /// A numeric literal takes the expected type, or `i64`/`f64` (spec §4).
    pub(super) fn literal(&mut self, expr: &Expr, expect: Option<&Expect>) -> Type {
        let (negated, kind) = match &expr.kind {
            ExprKind::Unary(UnOp::Neg, inner) => (true, &inner.kind),
            kind => (false, kind),
        };
        let expected = expect
            .map(|e| e.ty.clone())
            .filter(|t| !matches!(t, Type::Void | Type::Never | Type::Error));
        let reason = expect.and_then(|e| e.reason.as_ref());
        match (kind, expected) {
            (ExprKind::Float(_), None | Some(Type::F64)) => Type::F64,
            (ExprKind::Float(_), Some(ty)) => {
                self.report(errors::wrong_literal(true, ty, expr.span, reason));
                Type::Error
            }
            // Only integer types hold integer literals: not f64, bool or string.
            (ExprKind::Int(_), Some(ty)) if !ty.is_integer() => {
                self.report(errors::wrong_literal(false, ty, expr.span, reason));
                Type::Error
            }
            (ExprKind::Int(value), expected) => {
                let ty = expected.unwrap_or(Type::I64);
                let (max, negated_max) = ty.literal_range();
                // A negated u8 literal never fits: u8 has no negative values (spec §4 rule 6).
                let fits = if negated {
                    *value <= negated_max && ty != Type::U8
                } else {
                    *value <= max
                };
                if fits {
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
}
