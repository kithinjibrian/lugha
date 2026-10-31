//! Tests and assertions (v16 draft decisions 81 and 83, milestone 84):
//! a `@test` function's shape (E0434), the rule that nothing calls a test,
//! and `assert`, `assert_eq` and `assert_ne`.

use super::errors;
use super::expr::Expect;
use super::{Checker, Type};
use crate::ast::{Expr, Item, Program};
use crate::span::Span;

/// The assertion intrinsics, usable in any code; and `skip(reason)` (v17,
/// milestone 97), which ends a test as skipped.
pub(super) const ASSERTIONS: [&str; 4] = ["assert", "assert_eq", "assert_ne", "skip"];

/// The names of `program`'s tests, in declaration order.
pub(super) fn tests_of(program: &Program) -> Vec<String> {
    program
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Fun(f) if f.test.is_some() => Some(f.name.name.clone()),
            _ => None,
        })
        .collect()
}

impl Checker {
    /// E0434 for every test of `program` that takes parameters, type
    /// parameters or returns something.
    pub(super) fn test_shapes(&mut self, program: &Program) {
        for item in &program.items {
            if let Item::Fun(f) = item
                && f.test.is_some()
                && (!f.params.is_empty() || !f.type_params.is_empty() || f.ret.is_some())
            {
                self.report(errors::bad_test(f.name.span));
            }
        }
    }

    /// E0434 when the function `key`, named at `at`, is a test: tests are
    /// compiled only by `lughac test`, which alone runs them.
    pub(super) fn not_a_test(&mut self, key: &str, at: Span) {
        if self.functions.get(key).is_some_and(|sig| sig.test) {
            let name = key.rsplit('.').next().unwrap_or(key);
            self.report(errors::test_used(name, at));
        }
    }

    /// `assert(cond)`, `assert_eq(a, b)` or `assert_ne(a, b)`: `a` and `b`
    /// of one type with `==` that prints, so a failure can show both.
    pub(super) fn assertion(&mut self, call: &Expr, name: &str, args: &[Expr]) -> Type {
        let wanted = if matches!(name, "assert" | "skip") {
            1
        } else {
            2
        };
        if args.len() != wanted {
            self.report(errors::arity(name, wanted, args.len(), call.span));
            self.unchecked_args(args);
            return Type::Unit;
        }
        if name == "assert" {
            self.expect_type(&args[0], &Expect::of(Type::Bool));
            return Type::Unit;
        }
        // `skip(reason)` ends the test: nothing after it runs (v17).
        if name == "skip" {
            self.expect_type(&args[0], &Expect::of(Type::String));
            return Type::Never;
        }
        // The side that can't be typed alone (a literal, a call only its
        // result types) takes the other side's type (v17).
        let (first, second) = match self.waits(&args[0]) && !self.waits(&args[1]) {
            true => (&args[1], &args[0]),
            false => (&args[0], &args[1]),
        };
        let ty = self.value(first, None);
        self.expect_type(second, &Expect::of(ty.clone()));
        if matches!(ty, Type::Error | Type::Never) {
            return Type::Unit;
        }
        if super::derive::compared_through_eq(&ty) {
            // E0451 with the path, as `==` (v17, milestone 144).
            if !self.meets_derived(super::derive::EQ, &ty, call.span) {
                return Type::Unit;
            }
        }
        if !self.eq_ok(&ty) {
            self.report(errors::bad_operands_on("==", &[ty], call.span));
        } else if !super::printing::printable(&ty) {
            self.report(errors::intrinsic_argument("print", ty, args[0].span));
        } else {
            self.printed(&args[0], &ty);
            self.printed(&args[1], &ty);
            self.equality_used(call, &ty);
        }
        Type::Unit
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok, test_mode};

    #[test]
    fn a_test_takes_nothing_and_returns_nothing() {
        let main = "fun main() {}\n";
        assert_eq!(
            errors(&format!("{main}@test fun t(n: i64) {{}}")),
            [("E0434", "t")]
        );
        assert_eq!(
            errors(&format!("{main}@test fun t(): i64 = 1;")),
            [("E0434", "t")]
        );
        assert_eq!(
            errors(&format!("{main}@test fun t<T>() {{}}")),
            [("E0434", "t")]
        );
        ok(&format!("{main}@test fun t() {{ assert(true); }}"));
    }

    #[test]
    fn nothing_calls_a_test() {
        let src = "@test fun t() {}\nfun main() { t(); let f = t; }";
        assert_eq!(errors(src), [("E0434", "t"), ("E0434", "t")]);
    }

    #[test]
    fn assertions_take_a_bool_or_two_equal_printable_values() {
        let wrap = |body: &str| format!("fun main() {{ {body} }}");
        ok(&wrap(
            "assert(1 < 2); assert_eq(\"a\", \"a\"); assert_ne([1], [2]);",
        ));
        ok(&wrap("let x: u8 = 3; assert_eq(x, 3);"));
        assert_eq!(errors(&wrap("assert(1);")), [("E0401", "1")]);
        // The literal takes the other side's type, as in `1 == "a"` (v17).
        assert_eq!(errors(&wrap("assert_eq(1, \"a\");")), [("E0401", "1")]);
        assert_eq!(errors(&wrap("assert_eq(1);"))[0].0, "E0405");
        let boxes = wrap("let r = Ref.new(1); assert_eq(r, r);");
        assert_eq!(errors(&boxes)[0].0, "E0404");
    }

    #[test]
    fn test_mode_needs_no_main_and_lists_the_tests_in_order() {
        let src = "@test fun b() {}\nfun helper() {}\n@test fun a() {}";
        let checked = test_mode(src).expect("no errors");
        assert_eq!(checked.tests, Some(vec!["b".to_string(), "a".to_string()]));
        assert_eq!(errors(src), [("E0303", "")]);
        assert_eq!(ok("fun main() {}\n@test fun a() {}").tests, None);
    }
}
