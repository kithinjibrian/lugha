//! A float's raw bits (v17, milestone 108): `x.to_bits()` (in `builtins.rs`)
//! and `f64.from_bits(b)` / `f32.from_bits(b)`, called on the type keyword.

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind};

/// The float type of `f64.from_bits` / `f32.from_bits` when `callee` is
/// one (any member of a float keyword, so a wrong name is reported here).
pub fn from_bits(callee: &Expr) -> Option<(Type, &str)> {
    let ExprKind::Field(base, member) = &callee.kind else {
        return None;
    };
    let ExprKind::Name(keyword) = &base.kind else {
        return None;
    };
    let float = match keyword.as_str() {
        "f64" => Type::F64,
        "f32" => Type::F32,
        _ => return None,
    };
    Some((float, member.name.as_str()))
}

/// The unsigned integer type holding a float's bits: `u64` or `u32`.
pub fn bits_of(float: &Type) -> Type {
    if *float == Type::F32 {
        Type::U32
    } else {
        Type::U64
    }
}

impl Checker {
    /// `f64.from_bits(b)`: `b` exactly `u64` (`u32` for `f32`), E0415 for
    /// another member, E0405 for another argument count.
    pub(super) fn float_from_bits(
        &mut self,
        call: &Expr,
        callee: &Expr,
        (float, member): (Type, &str),
        args: &[Expr],
    ) -> Type {
        self.record(callee, Type::Unit);
        if let ExprKind::Field(base, field) = &callee.kind {
            self.record(base, Type::Unit);
            if member != "from_bits" {
                self.report(errors::no_method(member, &float, field.span, None));
                return self.unchecked_args(args);
            }
        }
        if args.len() != 1 {
            self.report(errors::arity("from_bits", 1, args.len(), call.span));
            return self.unchecked_args(args);
        }
        self.expect_type(&args[0], &Expect::of(bits_of(&float)));
        float
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type};

    #[test]
    fn floats_turn_into_their_bits_and_back() {
        let main = |body: &str| format!("fun main() {{ {body} }}");
        assert_eq!(let_type(&main("let b = 1.5.to_bits();"), "b"), Type::U64);
        assert_eq!(
            let_type(&main("let x: f32 = 1.5; let b = x.to_bits();"), "b"),
            Type::U32
        );
        assert_eq!(let_type(&main("let y = f64.from_bits(4);"), "y"), Type::F64);
        assert_eq!(
            let_type(&main("let b: u32 = 4; let y = f32.from_bits(b);"), "y"),
            Type::F32
        );
    }

    #[test]
    fn float_bits_are_checked() {
        let one = |body: &str| errors(&format!("fun main() {{ {body} }}"))[0].0;
        assert_eq!(one("let b: u32 = 4; let y = f64.from_bits(b);"), "E0403");
        assert_eq!(one("let b: i64 = 4; let y = f32.from_bits(b);"), "E0403");
        assert_eq!(one("let y = f64.from_bits(1, 2);"), "E0405");
        assert_eq!(one("let y = f64.nope(1);"), "E0415");
        assert_eq!(one("let b = (1).to_bits();"), "E0415");
    }
}
