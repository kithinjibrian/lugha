//! Inference from sibling arguments (v17, milestone 96, PRP-127): a call to
//! a generic function whose type parameters appear only in its result
//! (`text<M>(s: string): View<M>`) can't be typed alone, so it waits for its
//! siblings — the other arguments of the call it is passed to, the other
//! operand of `==`, or the other side of `assert_eq` — as literals and
//! `None` already do, and takes the type they fix as its expected type.

use super::ops::defers;
use super::{Checker, Type};
use crate::ast::{Expr, ExprKind};

impl Checker {
    /// True if `e` can't be typed without an expected type, so it waits for
    /// its siblings: what `defers` says (literals, `None`, `[]`, untyped
    /// closures), and a call of a generic function some of whose type
    /// parameters appear only in its result.
    pub(super) fn waits(&self, e: &Expr) -> bool {
        defers(e) || self.result_only_call(e)
    }

    /// A call `f(…)` or `m.f(…)` of a generic function whose result has a
    /// type parameter none of its parameters mention.
    fn result_only_call(&self, e: &Expr) -> bool {
        let ExprKind::Call(callee, _) = &e.kind else {
            return false;
        };
        let key = match &callee.kind {
            ExprKind::Name(name) if self.local(name).is_none() => self.key(name),
            ExprKind::Field(base, f) => {
                let Some((path, _)) = Self::module_name(base).and_then(|m| self.module_path(&m))
                else {
                    return false;
                };
                self.follow(super::qualify(&path, &f.name))
            }
            _ => return false,
        };
        let Some(sig) = self.functions.get(&key) else {
            return false;
        };
        sig.generics.iter().any(|g| {
            !sig.params.iter().any(|p| mentions(p, &g.name)) && mentions(&sig.ret, &g.name)
        })
    }
}

/// True if `ty` mentions the type parameter `name`.
fn mentions(ty: &Type, name: &str) -> bool {
    match ty {
        Type::Param(n) => n == name,
        Type::Array(inner) | Type::Option(inner) | Type::Ref(inner) => mentions(inner, name),
        Type::Struct(_, args) | Type::Enum(_, args) | Type::Interface(_, args) => {
            args.iter().any(|a| mentions(a, name))
        }
        Type::Fun(params, ret) => params.iter().any(|p| mentions(p, name)) || mentions(ret, name),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok, test_mode};

    const VIEW: &str = "enum View<M> { Text(string), Button(string, M) }\n\
        enum Msg { Go }\n\
        fun text<M>(s: string): View<M> = View.Text(s);\n";

    #[test]
    fn assert_eq_takes_the_type_from_either_side() {
        let src = format!(
            "{VIEW}@test fun t() {{\n\
                 let expected: View<Msg> = View.Text(\"hi\");\n\
                 assert_eq(text(\"hi\"), expected);\n\
                 assert_eq(expected, text(\"hi\"));\n\
                 assert_ne(text(\"ho\"), expected);\n\
             }}"
        );
        let found = test_mode(&src).err();
        assert_eq!(found, None);
    }

    #[test]
    fn equality_takes_the_type_from_either_side() {
        ok(&format!(
            "{VIEW}fun main() {{\n\
                 let expected: View<Msg> = View.Text(\"hi\");\n\
                 println(text(\"hi\") == expected);\n\
                 println(expected != text(\"hi\"));\n\
             }}"
        ));
    }

    #[test]
    fn a_generic_calls_arguments_type_each_other() {
        ok(&format!(
            "{VIEW}fun pair<A>(a: A, b: A): A[] = [a, b];\n\
             fun main() {{\n\
                 let expected: View<Msg> = View.Text(\"hi\");\n\
                 let both = pair(text(\"a\"), expected);\n\
                 let other = pair(expected, text(\"b\"));\n\
             }}"
        ));
    }

    #[test]
    fn with_nothing_to_fix_it_it_is_still_e0412() {
        let src = format!("{VIEW}fun main() {{ println(text(\"hi\") == text(\"ho\")); }}");
        let found = errors(&src);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0412"), "{found:?}");
    }

    #[test]
    fn a_literal_first_takes_the_other_sides_type_too() {
        assert_eq!(
            test_mode("@test fun t() { let x: u8 = 3; assert_eq(3, x); }").err(),
            None
        );
    }
}
