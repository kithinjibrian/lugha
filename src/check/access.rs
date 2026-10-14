//! Field access and indexing (spec §4): `.len` and elements of strings and
//! arrays, and struct fields.

use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, Ident};

impl Checker {
    /// `base.field`: `.len` of strings and arrays, or a struct's field.
    pub(super) fn field(&mut self, base: &Expr, field: &Ident) -> Type {
        // `i16.MAX` (v10): an integer keyword is no value, so nothing else applies.
        if let ExprKind::Name(keyword) = &base.kind
            && let Some(int) = Type::int_keyword(keyword)
        {
            self.record(base, Type::Void);
            if Type::int_limit(base, &field.name).is_none() {
                self.report(errors::no_method(&field.name, &int, field.span, None));
                return Type::Error;
            }
            return int;
        }
        let ty = self.value(base, None);
        match (ty, field.name.as_str()) {
            (Type::Error | Type::Never, _) => Type::Error,
            (Type::String | Type::Array(_), "len") => Type::I64,
            (Type::Struct(s, args), name) if self.struct_field(&s, name).is_some() => {
                self.field_visible(&s, name, field.span);
                self.field_type(&s, &args, name).expect("just found")
            }
            (ty, name) => {
                self.report(errors::no_field_on(name, ty, field.span));
                Type::Error
            }
        }
    }

    /// `base[index]`: a string's byte (`u8`) or an array's element; the index is an `i64`.
    pub(super) fn index(&mut self, base: &Expr, index: &Expr) -> Type {
        let ty = self.value(base, None);
        // Any integer type indexes (v14); a bare literal is an `i64`.
        let at = self.value(index, None);
        if !at.is_integer() && !matches!(at, Type::Error | Type::Never) {
            self.report(errors::mismatch(Type::I64, at, index.span, None));
        }
        match ty {
            Type::Error | Type::Never => Type::Error,
            Type::String => Type::U8,
            Type::Array(element) => *element,
            ty => {
                self.report(errors::not_indexable(ty, base.span));
                Type::Error
            }
        }
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
            [("E0403", "1.5")]
        );
        assert_eq!(
            // v14: any integer indexes; a `bool` doesn't.
            errors("fun main() { let s = \"ab\"; let k = true; let b = s[k]; }"),
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
        // Strings are ordered bytewise in v2 (draft §4).
        assert_eq!(
            let_type("fun main() { let o = \"a\" < \"b\"; }", "o"),
            Type::Bool
        );
        assert_eq!(
            errors("fun main() { let o = \"a\" + 1; }"),
            [("E0404", "\"a\" + 1")]
        );
    }

    #[test]
    fn any_integer_indexes_and_bytes_v14() {
        use crate::check::test_util::ok;
        ok(
            "fun main() { let xs = [1, 2]; let a: u8 = 0; let b: u32 = 1; let c: i32 = 0; let d: u64 = 1;\n\
            let n = xs[a] + xs[b] + xs[c] + xs[d]; let s = \"ab\"; let e: u16 = 1; let y = s[e]; }",
        );
        assert_eq!(
            errors("fun main() { let xs = [1]; let f = 0.5; let n = xs[f]; }"),
            [("E0403", "f")]
        );
        assert_eq!(
            let_type("fun main() { let b = \"hé\".to_bytes(); }", "b"),
            Type::Array(Box::new(Type::U8))
        );
        assert_eq!(
            let_type("fun main() { let r = read_stdin_bytes(); }", "r"),
            Type::Array(Box::new(Type::U8))
        );
        assert_eq!(
            errors("fun read_bytes(p: string): i64 = 1;\nfun main() {}"),
            [("E0302", "read_bytes")]
        );
    }
}
