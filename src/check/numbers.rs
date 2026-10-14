//! The numeric types beyond v0's (v7 draft §4, milestone 27): `u32`, `u64`
//! and `f32`, then `i8`, `i16` and `u16` with `I.MIN`/`I.MAX` (v10), and
//! the questions every numeric type answers.

use super::Type;
use crate::ast::{Expr, ExprKind, TypeKind};

impl Type {
    /// `f32` or `f64`.
    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    /// `u8`, `u16`, `u32` or `u64`: no negative values, so no negation (E0404).
    pub fn is_unsigned(&self) -> bool {
        matches!(self, Type::U8 | Type::U16 | Type::U32 | Type::U64)
    }

    /// The integer type an integer keyword names (`i16`), for `i16.MAX`
    /// (v10); `None` for any other name.
    pub fn int_keyword(name: &str) -> Option<Type> {
        Some(match name {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            _ => return None,
        })
    }

    /// The integer type and constant of `I.MIN` / `I.MAX` (v10), `None`
    /// for any other expression; the checker reports other names (E0415).
    pub fn int_limit(base: &Expr, name: &str) -> Option<(Type, bool)> {
        let ExprKind::Name(keyword) = &base.kind else {
            return None;
        };
        let ty = Type::int_keyword(keyword)?;
        match name {
            "MIN" => Some((ty, false)),
            "MAX" => Some((ty, true)),
            _ => None,
        }
    }

    /// The type a built-in type keyword names, `None` for any other written type.
    pub(super) fn primitive(kind: &TypeKind) -> Option<Type> {
        Some(match kind {
            TypeKind::I32 => Type::I32,
            TypeKind::I64 => Type::I64,
            TypeKind::U8 => Type::U8,
            TypeKind::F64 => Type::F64,
            TypeKind::U32 => Type::U32,
            TypeKind::U64 => Type::U64,
            TypeKind::F32 => Type::F32,
            TypeKind::I8 => Type::I8,
            TypeKind::I16 => Type::I16,
            TypeKind::U16 => Type::U16,
            TypeKind::Bool => Type::Bool,
            TypeKind::String => Type::String,
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    fn ty(body: &str, name: &str) -> Type {
        let_type(&format!("fun main() {{ {body} }}"), name)
    }

    fn codes(body: &str) -> Vec<&'static str> {
        let src = format!("fun main() {{ {body} }}");
        errors(&src).into_iter().map(|(c, _)| c).collect()
    }

    #[test]
    fn literals_take_the_new_types() {
        assert_eq!(ty("let b: u64 = 18446744073709551615;", "b"), Type::U64);
        assert_eq!(ty("let n: u32 = 4294967295;", "n"), Type::U32);
        assert_eq!(ty("let x: f32 = 0.5;", "x"), Type::F32);
        assert_eq!(
            ty("let h: u64 = 1; let c = h > 9223372036854775807;", "c"),
            Type::Bool
        );
        assert_eq!(codes("let c: u32 = 4294967296;"), ["E0402"]);
        assert_eq!(codes("let y: f32 = 1;"), ["E0401"]);
        assert_eq!(codes("let z: u64 = -1;"), ["E0402"]);
    }

    #[test]
    fn unsigned_values_dont_negate() {
        assert_eq!(codes("let n: u32 = 1; let m = -n;"), ["E0404"]);
        assert_eq!(codes("let n: u64 = 1; let m = -n;"), ["E0404"]);
        assert_eq!(ty("let x: f32 = 1.5; let y = -x;", "y"), Type::F32);
    }

    #[test]
    fn every_numeric_pair_casts() {
        let types = ["i32", "i64", "u8", "u32", "u64", "f32", "f64"];
        for from in types {
            for to in types {
                ok(&format!(
                    "fun main() {{ let a: {from} = 1 as {from}; let b = a as {to}; }}"
                ));
            }
        }
        assert_eq!(codes("let b = true as u32;"), ["E0408"]);
    }

    #[test]
    fn types_dont_mix() {
        // As for `i32 + i64`: an operator on two different types is E0404.
        assert_eq!(
            codes("let a: u32 = 1; let b: u64 = 2; let c = a + b;"),
            ["E0404"]
        );
        assert_eq!(
            codes("let a: f32 = 1.0; let b: f64 = 2.0; let c = a + b;"),
            ["E0404"]
        );
    }

    #[test]
    fn externs_take_them() {
        ok("extern fun f(a: u32, b: u64, c: f32): f32;\nfun main() {}");
    }

    #[test]
    fn bit_operators_take_one_integer_type() {
        assert_eq!(ty("let a: u32 = 6; let b = a & 3;", "b"), Type::U32);
        assert_eq!(ty("let a: u8 = 6; let b = 1 << a;", "b"), Type::U8);
        assert_eq!(ty("let a: i64 = 6; let b = ~a;", "b"), Type::I64);
        assert_eq!(
            ty("let a: u64 = 1; let mut b = a; b <<= 3; let c = b;", "c"),
            Type::U64
        );
        assert_eq!(codes("let a = true & false;"), ["E0404"]);
        assert_eq!(codes("let x: f64 = 1.0; let y = x | 1.0;"), ["E0404"]);
        assert_eq!(
            codes("let a: u32 = 1; let b: u64 = 2; let c = a & b;"),
            ["E0404"]
        );
        assert_eq!(codes("let a = ~true;"), ["E0404"]);
        assert_eq!(codes("let mut x: f64 = 1.0; x &= 2.0;"), ["E0404"]);
    }

    #[test]
    fn the_v10_integer_types_have_their_ranges_and_limits() {
        assert_eq!(ty("let a: i8 = -128;", "a"), Type::I8);
        assert_eq!(ty("let b: u16 = 65535;", "b"), Type::U16);
        assert_eq!(ty("let c: i16 = -32768;", "c"), Type::I16);
        assert_eq!(codes("let c: i8 = 128;"), ["E0402"]);
        assert_eq!(codes("let d: u16 = -1;"), ["E0402"]);
        assert_eq!(ty("let m = i8.MIN;", "m"), Type::I8);
        assert_eq!(ty("let m = u64.MAX;", "m"), Type::U64);
        assert_eq!(codes("let t = i64.TOP;"), ["E0415"]);
        assert_eq!(
            ty("let p = parse_u64(\"1\");", "p"),
            Type::Option(Box::new(Type::U64))
        );
        assert_eq!(
            errors("fun parse_u64(s: string): i64 = 1;\nfun main() {}"),
            [("E0302", "parse_u64")]
        );
    }
}
