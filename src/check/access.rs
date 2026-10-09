//! Field access and indexing (spec §4): `.len` and elements of strings and
//! arrays. Structs join in PRP-015.

use super::expr::Expect;
use super::{Checker, Checking, Type, errors};
use crate::ast::{Expr, Ident};

impl Checker {
    /// `base.field`: `.len` of strings and arrays.
    pub(super) fn field(&mut self, base: &Expr, field: &Ident) -> Checking<Type> {
        let ty = self.value(base, None)?;
        Ok(match (ty, field.name.as_str()) {
            (Type::Error | Type::Never, _) => Type::Error,
            (Type::String | Type::Array(_), "len") => Type::I64,
            (ty, name) => {
                self.report(errors::no_field(name, ty, field.span));
                Type::Error
            }
        })
    }

    /// `base[index]`: a string's byte (`u8`) or an array's element; the index is an `i64`.
    pub(super) fn index(&mut self, base: &Expr, index: &Expr) -> Checking<Type> {
        let ty = self.value(base, None)?;
        self.expect_type(index, &Expect::of(Type::I64))?;
        Ok(match ty {
            Type::Error | Type::Never => Type::Error,
            Type::String => Type::U8,
            Type::Array(element) => *element,
            ty => {
                self.report(errors::not_indexable(ty, base.span));
                Type::Error
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type};

    #[test]
    fn strings_have_len_and_bytes() {
        assert_eq!(
            let_type("fun main() { let n = \"abc\".len; }", "n"),
            Type::I64
        );
        assert_eq!(
            let_type("fun main() { let s = \"abc\"; let b = s[0]; }", "b"),
            Type::U8
        );
        assert_eq!(
            errors("fun main() { let n = \"s\".size; }"),
            [("E0410", "size")]
        );
        assert_eq!(
            errors("fun main() { let n = (5).len; }"),
            [("E0410", "len")]
        );
        assert_eq!(
            errors("fun main() { let n = 5; let d = n[0]; }"),
            [("E0411", "n")]
        );
        assert_eq!(
            errors("fun main() { let s = \"ab\"; let b = s[1.5]; }"),
            [("E0401", "1.5")]
        );
        assert_eq!(
            errors("fun main() { let s = \"ab\"; let k: i32 = 0; let b = s[k]; }"),
            [("E0403", "k")]
        );
    }

    #[test]
    fn strings_are_immutable() {
        assert_eq!(
            errors("fun main() { let mut s = \"hi\"; s[0] = 104; }"),
            [("E0506", "s[0]")]
        );
        assert_eq!(
            errors("fun main() { let mut s = \"hi\"; s.len = 2; }"),
            [("E0506", "s.len")]
        );
    }

    #[test]
    fn string_operators() {
        assert_eq!(
            let_type("fun main() { let s = \"a\" + \"b\"; }", "s"),
            Type::String
        );
        assert_eq!(
            let_type("fun main() { let e = \"a\" == \"b\"; }", "e"),
            Type::Bool
        );
        assert_eq!(
            errors("fun main() { let o = \"a\" < \"b\"; }"),
            [("E0404", "\"a\" < \"b\"")]
        );
        assert_eq!(
            errors("fun main() { let o = \"a\" + 1; }"),
            [("E0404", "\"a\" + 1")]
        );
    }
}
