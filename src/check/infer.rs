//! Inferring type arguments (v2 draft §4, PRP-035): each use of a generic
//! binds its parameters from the expected type, then the arguments that can
//! be typed alone, then the ones that defer (literals, `None`, `[]`).

use std::collections::HashMap;

use super::expr::Expect;
use super::generics::{Generic, replace};
use super::ops::defers;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind};

/// Type arguments bound so far, by parameter name.
type Bound = HashMap<String, Type>;

impl Checker {
    /// Checks `inputs` — each an expression and the type its declaration
    /// gives it — for a use `at` of something with type parameters
    /// `generics`, some perhaps already `bound`. `result` is the declared
    /// type of the use with the context's expectation. Returns the type
    /// arguments in declaration order (recorded in `instances`), or `None`
    /// after an error: E0412 for an argument nothing decides, E0419 for one
    /// that misses its bound.
    pub(super) fn infer(
        &mut self,
        at: &Expr,
        generics: &[Generic],
        mut bound: Bound,
        result: Option<(&Type, Option<&Expect>)>,
        inputs: &[(&Expr, Expect)],
    ) -> Option<Vec<Type>> {
        if generics.is_empty() {
            for (e, want) in inputs {
                self.expect_type(e, want);
            }
            return Some(Vec::new());
        }
        let names: Vec<&str> = generics.iter().map(|g| g.name.as_str()).collect();
        let mut failed = false;
        if let Some((declared, Some(expect))) = result {
            // A context that already failed was reported; it can't decide anything.
            failed = expect.ty == Type::Error;
            unify(declared, &expect.ty, &names, &mut bound);
        }
        // What can be typed alone goes first, so `max(x, 3)` makes `3` a `u8`.
        let (now, later): (Vec<_>, Vec<_>) = inputs.iter().partition(|(e, _)| !defers(e));
        // Closures last: their parameter types need what literals decide (v3).
        let is_closure = |(e, _): &&(&Expr, Expect)| matches!(e.kind, ExprKind::Closure(_));
        let (closures, later): (Vec<_>, Vec<_>) = later.into_iter().partition(is_closure);
        for (e, want) in now.into_iter().chain(later).chain(closures) {
            failed |= self.input(e, want, &names, &mut bound);
        }
        self.through_bounds(generics, &mut bound);
        let mut args = Vec::new();
        for g in generics {
            match bound.get(&g.name) {
                Some(ty) => args.push(ty.clone()),
                // An argument already reported explains why: no second error.
                None if failed => return None,
                None => {
                    self.report(errors::cannot_infer(&g.name, at.span));
                    return None;
                }
            }
        }
        if !self.meets_bounds(generics, &args, at.span) {
            return None;
        }
        self.instances.insert(at.id, args.clone());
        Some(args)
    }

    /// One input: checked against its type when every parameter in it is
    /// bound, otherwise typed alone and used to bind them. True if it failed.
    fn input(&mut self, e: &Expr, want: &Expect, names: &[&str], bound: &mut Bound) -> bool {
        if !unbound(&want.ty, names, bound) {
            let ty = substitute(&want.ty, bound);
            let found = self.expect_type(
                e,
                &Expect {
                    ty,
                    reason: want.reason.clone(),
                },
            );
            return matches!(found, Type::Error | Type::Never);
        }
        // A closure gets what is known of its type (v3): `fun(T): U` with
        // `T` bound gives its parameter, and its body decides `U`.
        // A variant used as a function value takes the same hint (v17): its
        // known parameter types fix the enum's type arguments (`map(Some)`).
        let hint = (matches!(e.kind, ExprKind::Closure(_)) || self.may_be_variant_value(e))
            .then(|| Expect::of(known(&want.ty, names, bound)));
        let found = self.value(e, hint.as_ref());
        unify(&want.ty, &found, names, bound);
        if let Some(failed) = self.interface_input(e, want, &found, names, bound) {
            return failed;
        }
        let ty = substitute(&want.ty, bound);
        if !found.fits(&ty) {
            self.report(errors::mismatch(ty, found, e.span, want.reason.as_ref()));
            return true;
        }
        matches!(found, Type::Error | Type::Never)
    }
}

/// The parameters among `generics` that `declared` puts where `actual` has
/// a type, as `infer` takes them (`bound`).
pub(super) fn bind(declared: &Type, actual: &Type, generics: &[Generic]) -> Bound {
    let names: Vec<&str> = generics.iter().map(|g| g.name.as_str()).collect();
    let mut bound = Bound::new();
    unify(declared, actual, &names, &mut bound);
    bound
}

/// `ty` with the bound parameters replaced and the unbound ones `Error`
/// (which a closure reads as "not known"), in
/// one substitution: replacing in two steps would take a bound parameter's
/// type for one of `names` when they share a name (`map`'s `T` bound to a
/// caller's own `T`).
fn known(ty: &Type, names: &[&str], bound: &Bound) -> Type {
    let map: HashMap<String, Type> = names
        .iter()
        .map(|n| (n.to_string(), bound.get(*n).cloned().unwrap_or(Type::Error)))
        .collect();
    ty.substitute(&map)
}

/// `ty` with the bound parameters replaced.
fn substitute(ty: &Type, bound: &Bound) -> Type {
    let map = bound.iter().map(|(k, v)| (k.as_str(), v)).collect();
    replace(ty, &map)
}

/// True if `ty` mentions one of `names` not yet bound. Asked of the
/// declared type, so a caller's own `T` is never taken for the callee's.
pub(super) fn unbound(ty: &Type, names: &[&str], bound: &Bound) -> bool {
    match ty {
        Type::Param(n) => names.contains(&n.as_str()) && !bound.contains_key(n),
        Type::Array(inner)
        | Type::Option(inner)
        | Type::Ref(inner)
        | Type::Task(inner)
        | Type::Chan(inner) => unbound(inner, names, bound),
        Type::Struct(_, args) | Type::Enum(_, args) | Type::Interface(_, args) => {
            args.iter().any(|a| unbound(a, names, bound))
        }
        Type::Fun(params, ret) => {
            params.iter().any(|p| unbound(p, names, bound)) || unbound(ret, names, bound)
        }
        _ => false,
    }
}

/// Binds the parameters among `names` that `declared` puts where `actual`
/// has a type; the first binding wins, and a later mismatch is E0403.
pub(super) fn unify(declared: &Type, actual: &Type, names: &[&str], bound: &mut Bound) {
    match (declared, actual) {
        (_, Type::Error | Type::Never) => {}
        (Type::Param(n), _) if names.contains(&n.as_str()) => {
            bound.entry(n.clone()).or_insert_with(|| actual.clone());
        }
        (Type::Array(d), Type::Array(a))
        | (Type::Option(d), Type::Option(a))
        | (Type::Ref(d), Type::Ref(a))
        | (Type::Task(d), Type::Task(a))
        | (Type::Chan(d), Type::Chan(a)) => {
            unify(d, a, names, bound);
        }
        (Type::Fun(dp, dr), Type::Fun(ap, ar)) => {
            for (d, a) in dp.iter().zip(ap) {
                unify(d, a, names, bound);
            }
            unify(dr, ar, names, bound);
        }
        (Type::Struct(k, ds), Type::Struct(l, as_))
        | (Type::Enum(k, ds), Type::Enum(l, as_))
        | (Type::Interface(k, ds), Type::Interface(l, as_))
            if k == l =>
        {
            for (d, a) in ds.iter().zip(as_) {
                unify(d, a, names, bound);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    const LIB: &str = "fun max<T: Ord>(a: T, b: T): T = if a > b { a } else { b };\n\
        fun first<T>(xs: T[]): Option<T> = if xs.len > 0 { Some(xs[0]) } else { None };\n\
        struct Stack<T> { items: T[] }\n\
        impl Stack<T> {\n\
            fun new(): Stack<T> = Stack { items: [] };\n\
            fun push(mut self, x: T) { self.items.push(x); }\n\
            fun pop(mut self): Option<T> = self.items.pop();\n\
        }\n\
        struct Pair<A, B> { a: A, b: B }\n\
        struct Wrap<T> { v: T }\n\
        enum Tree<T> { Leaf, Node(T, Tree<T>) }\n\
        fun largest<T: Ord>(xs: T[]): Option<T> = first(xs);\n";

    fn ty(body: &str, name: &str) -> Type {
        let_type(&format!("{LIB}fun main() {{ {body} }}"), name)
    }

    fn e(body: &str) -> Vec<(&'static str, String)> {
        let src = format!("{LIB}fun main() {{ {body} }}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    fn s(name: &str, args: Vec<Type>) -> Type {
        Type::Struct(name.into(), args)
    }

    #[test]
    fn arguments_decide_the_type_arguments() {
        assert_eq!(ty("let m = max(3, 7);", "m"), Type::I64);
        assert_eq!(ty("let x: u8 = 1; let m = max(x, 3);", "m"), Type::U8);
        assert_eq!(
            ty("let f = first([\"a\"]);", "f"),
            Type::Option(Box::new(Type::String))
        );
    }

    #[test]
    fn the_expected_type_decides_too() {
        assert_eq!(
            ty("let s: Stack<string> = Stack.new();", "s"),
            s("Stack", vec![Type::String])
        );
        assert_eq!(
            ty("let p: Pair<i64, f64> = Pair { a: 1, b: 2.0 };", "p"),
            s("Pair", vec![Type::I64, Type::F64])
        );
        assert_eq!(
            ty("let w = Wrap { v: 2.5 };", "w"),
            s("Wrap", vec![Type::F64])
        );
        assert_eq!(
            ty("let t: Tree<i64> = Tree.Node(1, Tree.Leaf);", "t"),
            Type::Enum("Tree".into(), vec![Type::I64])
        );
    }

    #[test]
    fn methods_see_their_receivers_arguments() {
        assert_eq!(
            ty(
                "let mut s: Stack<string> = Stack.new(); let p = s.pop();",
                "p"
            ),
            Type::Option(Box::new(Type::String))
        );
        assert_eq!(
            e("let mut s: Stack<string> = Stack.new(); s.push(1);"),
            [("E0401", "1".to_string())]
        );
    }

    #[test]
    fn undecided_and_unsatisfied_arguments_are_errors() {
        assert_eq!(
            e("let s = Stack.new();"),
            [("E0412", "Stack.new()".to_string())]
        );
        // A context that already failed decides nothing, and says nothing more.
        assert_eq!(
            e("let p: Pair<i64> = Stack.new();"),
            [("E0418", "Pair<i64>".to_string())]
        );
        assert_eq!(
            e("let l = largest([[1]]);"),
            [("E0419", "largest([[1]])".to_string())]
        );
        // The string decides `T` before the literal, which defers (PRP-035 amendment).
        assert_eq!(e("let m = max(1, \"a\");"), [("E0401", "1".to_string())]);
        ok(&format!(
            "{LIB}fun main() {{ let l = largest([\"b\", \"a\"]); }}"
        ));
    }

    #[test]
    fn the_drafts_generics_program_checks() {
        let draft = include_str!("../../docs/specs/Language v2 Draft.md");
        let start = draft
            .find("**Generics** (milestone 11)")
            .expect("the draft has the generics program");
        let code = draft[start..]
            .split("```\n")
            .nth(1)
            .expect("a fenced block follows");
        ok(code);
    }

    #[test]
    fn every_generic_use_records_its_type_arguments() {
        let checked = ok(&format!(
            "{LIB}fun main() {{ let m = max(3, 7); let s: Stack<string> = Stack.new(); }}"
        ));
        let mut recorded: Vec<Vec<Type>> = checked.instances.values().cloned().collect();
        recorded.sort_by_key(|args| format!("{args:?}"));
        assert!(recorded.contains(&vec![Type::I64]), "{recorded:?}");
        assert!(recorded.contains(&vec![Type::String]), "{recorded:?}");
    }
}
