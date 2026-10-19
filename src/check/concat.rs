//! Joining arrays (v17, milestone 122; PRP-154): `xs + ys` on two arrays
//! of one type is a call of the prelude's `$concat`, recorded as a use so
//! its instance is compiled; `T[]` satisfies `Add` as a built-in, and a
//! `T: Add` body at an array type reaches the same instance.

use super::instances::Instance;
use super::{Checker, Type};
use crate::ast::Expr;

/// The key of the prelude function `xs + ys` calls on arrays.
pub(crate) const CONCAT: &str = "$prelude.$concat";

/// The `$concat` instance joining arrays of type `ty`, if it is one.
pub(super) fn concat_target(ty: &Type) -> Option<Instance> {
    match ty {
        Type::Array(element) => Some(Instance {
            key: CONCAT.to_string(),
            args: vec![(**element).clone()],
        }),
        _ => None,
    }
}

impl Checker {
    /// Records that `at` joins two arrays of type `ty` with `+` or `+=`.
    pub(super) fn concat_used(&mut self, at: &Expr, ty: &Type) {
        if let Some(target) = concat_target(ty) {
            self.used(at, &target.key, &target.args);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    fn arr(t: Type) -> Type {
        Type::Array(Box::new(t))
    }

    fn main(body: &str) -> String {
        format!("fun main() {{ let xs = [1, 2]; {body} }}")
    }

    #[test]
    fn arrays_join_with_plus() {
        assert_eq!(let_type(&main("let a = xs + [3];"), "a"), arr(Type::I64));
        assert_eq!(let_type(&main("let a = [1] + [2];"), "a"), arr(Type::I64));
        assert_eq!(let_type(&main("let a = xs + [];"), "a"), arr(Type::I64));
        assert_eq!(let_type(&main("let a = [] + xs;"), "a"), arr(Type::I64));
        assert_eq!(
            let_type(&main("let a = [\"a\"] + [\"b\"];"), "a"),
            arr(Type::String)
        );
        assert_eq!(
            let_type(&main("let a = [xs] + [[3]];"), "a"),
            arr(arr(Type::I64))
        );
        // The expected type reaches the operands, as for numbers.
        assert_eq!(
            let_type(&main("let a: u8[] = [1] + [2];"), "a"),
            arr(Type::U8)
        );
    }

    #[test]
    fn plus_assign_joins_arrays_and_strings() {
        ok(&main(
            "let mut ys = xs; ys += [3]; ys += []; let mut s = \"a\"; s += \"b\"; println(\"{ys} {s}\");",
        ));
        ok("struct B { items: i64[] }\n\
            fun main() { let mut b = B { items: [] }; b.items += [1]; println(b.items); }");
    }

    #[test]
    fn arrays_satisfy_add() {
        ok("fun both<T: Add>(a: T, b: T): T = a + b;\n\
            fun main() { println(both([1], [2])); println(both(\"a\", \"b\")); println(both(1, 2)); }");
    }

    #[test]
    fn mismatched_joins_are_errors() {
        assert_eq!(
            errors(&main("let a = xs + [\"a\"];")),
            [("E0404", "xs + [\"a\"]")]
        );
        assert_eq!(errors(&main("let a = xs + 1;")), [("E0404", "xs + 1")]);
        assert_eq!(errors(&main("let a = xs - xs;")), [("E0404", "xs - xs")]);
        assert_eq!(errors(&main("xs += [3];"))[0].0, "E0501");
        assert_eq!(
            errors(&main("let mut ys = xs; ys -= [3];")),
            [("E0404", "ys -= [3]")]
        );
        assert_eq!(
            errors("fun main() { let mut s = \"a\"; s -= \"b\"; }"),
            [("E0404", "s -= \"b\"")]
        );
        assert_eq!(
            errors(&main("let mut ys = xs; ys += [\"a\"];"))[0].0,
            "E0403"
        );
        // Arrays still have no `add` method.
        assert_eq!(errors(&main("let a = xs.add(xs);"))[0].0, "E0415");
    }
}
