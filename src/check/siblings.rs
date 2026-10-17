//! Inference from sibling arguments (v17, milestone 96, PRP-127): a call to
//! a generic function whose type parameters appear only in its result
//! (`text<M>(s: string): View<M>`) can't be typed alone, so it waits for its
//! siblings — the other arguments of the call it is passed to, the other
//! operand of `==`, or the other side of `assert_eq` — as literals and
//! `None` already do, and takes the type they fix as its expected type.

use super::expr::Expect;
use super::ops::defers;
use super::{Checker, Type};
use crate::ast::{Expr, ExprKind};

impl Checker {
    /// True if `e` can't be typed without an expected type, so it waits for
    /// its siblings: what `defers` says (literals, `None`, `[]`, untyped
    /// closures); a call of a generic function with a type parameter in its
    /// result that only waiting arguments could fix (`text("a")`, and since
    /// kivuli's second report `column([text("a")])`); a list or `Some` of
    /// such calls; and a tuple holding one.
    pub(super) fn waits(&self, e: &Expr) -> bool {
        match &e.kind {
            _ if defers(e) => true,
            ExprKind::Array(elements) => elements.iter().all(|x| self.waits(x)),
            // A tuple's type needs every element's: one waiting call makes
            // it wait (`("a", text("a"))`); literals alone keep their types.
            ExprKind::Tuple(elements) => elements.iter().any(|x| !defers(x) && self.waits(x)),
            ExprKind::Call(callee, args)
                if matches!(&callee.kind, ExprKind::Name(n) if n == "Some") && args.len() == 1 =>
            {
                self.waits(&args[0])
            }
            _ => self.waiting_call(e) || self.waiting_receiver(e).is_some(),
        }
    }

    /// The type a waiting expression will have, by name: a waiting call's
    /// declared result (`Node` for `leaf<M>(s): Node<M>`), or, through a
    /// method that returns its receiver's own type, the receiver's.
    fn waiting_head(&self, e: &Expr) -> Option<String> {
        if let Some(head) = self.waiting_receiver(e) {
            return Some(head);
        }
        let ExprKind::Call(callee, _) = &e.kind else {
            return None;
        };
        if !self.waiting_call(e) {
            return None;
        }
        let (_, _, ret) = self.callee_signature(callee)?;
        match &ret {
            Type::Struct(key, _) | Type::Enum(key, _) => Some(key.clone()),
            _ => None,
        }
    }

    /// For `r.m(…)` whose receiver `r` waits and whose method `m` returns
    /// the receiver's own type (`fun pad(self, n: i64): Node<M>` in
    /// `impl Node<M>`): the type's name. Such a call waits too, and its
    /// expected type is the receiver's (kivuli: `text("a").padding(8)`).
    fn waiting_receiver(&self, e: &Expr) -> Option<String> {
        let ExprKind::Call(callee, _) = &e.kind else {
            return None;
        };
        let ExprKind::Field(base, name) = &callee.kind else {
            return None;
        };
        let head = self.waiting_head(base)?;
        self.returns_self(&head, &name.name).then_some(head)
    }

    /// True if `head`'s method `name` returns `head` at the `impl`'s own
    /// type parameters, in order.
    fn returns_self(&self, head: &str, name: &str) -> bool {
        let Some(m) = self.methods.get(head).and_then(|ms| ms.get(name)) else {
            return false;
        };
        let own: Vec<Type> = m.generics[..m.outer]
            .iter()
            .map(|g| Type::Param(g.name.clone()))
            .collect();
        matches!(&m.ret, Type::Struct(k, args) | Type::Enum(k, args) if k == head && *args == own)
    }

    /// The expected type for the receiver of `base.name(…)`: the call's own,
    /// when the receiver waits and the method returns the receiver's type.
    pub(super) fn receiver_expect(
        &self,
        base: &Expr,
        name: &str,
        expect: Option<&Expect>,
    ) -> Option<Expect> {
        let expected = expect?;
        let head = self.waiting_head(base)?;
        let fits = matches!(&expected.ty, Type::Struct(k, _) | Type::Enum(k, _) if *k == head);
        (fits && self.returns_self(&head, name)).then(|| Expect::of(expected.ty.clone()))
    }

    /// A call `f(…)` or `m.f(…)` of a generic function whose result has a
    /// type parameter that every argument able to fix it leaves open: none
    /// of the parameters mentions it, or each argument for one that does
    /// waits itself.
    fn waiting_call(&self, e: &Expr) -> bool {
        let ExprKind::Call(callee, args) = &e.kind else {
            return false;
        };
        let Some((generics, params, ret)) = self.callee_signature(callee) else {
            return false;
        };
        if args.len() != params.len() {
            return false;
        }
        generics.iter().any(|g| {
            mentions(&ret, g)
                && params
                    .iter()
                    .zip(args)
                    .all(|(p, a)| !mentions(p, g) || self.waits(a))
        })
    }

    /// The type parameters, parameter types and result of what `callee`
    /// names: a function (`f`, `m.f`) or, since kivuli's `Cmd.none()`, an
    /// associated function of a generic type (`T.f`), whose type parameters
    /// are its `impl`'s and its own.
    fn callee_signature(&self, callee: &Expr) -> Option<(Vec<String>, Vec<Type>, Type)> {
        if let Some(sig) = self.callee_key(callee).and_then(|k| self.functions.get(&k)) {
            let generics = sig.generics.iter().map(|g| g.name.clone()).collect();
            return Some((generics, sig.params.clone(), sig.ret.clone()));
        }
        let ExprKind::Field(base, f) = &callee.kind else {
            return None;
        };
        let ExprKind::Name(t) = &base.kind else {
            return None;
        };
        if self.local(t).is_some() {
            return None;
        }
        let m = self.methods.get(&self.key(t))?.get(&f.name)?;
        if m.receiver.is_some() {
            return None;
        }
        let generics = m.generics.iter().map(|g| g.name.clone()).collect();
        Some((generics, m.params.clone(), m.ret.clone()))
    }
}

impl Checker {
    /// The function a callee names, `f` or `m.f`, without reporting.
    fn callee_key(&self, callee: &Expr) -> Option<String> {
        match &callee.kind {
            ExprKind::Name(name) if self.local(name).is_none() => Some(self.key(name)),
            ExprKind::Field(base, f) => {
                let (path, _) = Self::module_name(base).and_then(|m| self.module_path(&m))?;
                Some(self.follow(super::qualify(&path, &f.name)))
            }
            _ => None,
        }
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
    fn a_call_whose_arguments_all_wait_waits_too_v17() {
        let src = format!(
            "{VIEW}fun column<M>(kids: View<M>[]): View<M> = kids[0];\n\
             fun wrap<M>(v: View<M>): View<M> = v;\n\
             @test fun t() {{\n\
                 let expected: View<Msg> = View.Text(\"a\");\n\
                 assert_eq(column([text(\"a\")]), expected);\n\
                 assert_eq(expected, column([text(\"a\"), text(\"b\")]));\n\
                 assert_eq(wrap(column([wrap(text(\"a\"))])), expected);\n\
                 assert_eq(Some(text(\"a\")), Some(expected));\n\
                 println(column([text(\"a\")]) == expected);\n\
             }}"
        );
        assert_eq!(test_mode(&src).err(), None);
        // A sibling that fixes the type stops the waiting.
        ok(&format!(
            "{VIEW}fun column<M>(kids: View<M>[]): View<M> = kids[0];\n\
             fun main() {{\n\
                 let e: View<Msg> = View.Text(\"a\");\n\
                 let v = column([text(\"a\"), e]);\n\
                 let w = column([text(\"a\"), View.Button(\"+\", Msg.Go)]);\n\
             }}"
        ));
        let src = format!(
            "{VIEW}fun column<M>(kids: View<M>[]): View<M> = kids[0];\n\
             fun main() {{ println(column([text(\"a\")]) == column([])); }}"
        );
        let found = errors(&src);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0412"), "{found:?}");
    }

    #[test]
    fn a_tuple_holding_a_waiting_call_waits_v17() {
        let src = format!(
            "{VIEW}fun keyed_column<M>(kids: (string, View<M>)[]): View<M> = kids[0].1;\n\
             @test fun t() {{\n\
                 let expected: View<Msg> = View.Text(\"a\");\n\
                 assert_eq(keyed_column([(\"a\", text(\"a\"))]), expected);\n\
                 assert_eq(expected, keyed_column([(\"a\", text(\"a\")), (\"b\", text(\"b\"))]));\n\
                 println(keyed_column([(\"a\", text(\"a\"))]) == expected);\n\
                 let pair: (string, View<Msg>) = (\"a\", text(\"a\"));\n\
                 assert_eq((\"a\", text(\"a\")), pair);\n\
             }}"
        );
        assert_eq!(test_mode(&src).err(), None);
        // Tuples of literals keep their own types.
        ok(
            "fun main() { let t = (1, \"a\"); let u: (i64, string) = t; println(t == (1, \"a\")); }",
        );
    }

    #[test]
    fn a_waiting_receiver_takes_the_calls_expected_type_v17() {
        let node = "enum Node<M> { Leaf(string), Wrapped(Node<M>, i64) }\n\
            impl Node<M> {\n\
                fun pad(self, n: i64): Node<M> = Node.Wrapped(self, n);\n\
                fun size(self): i64 = 1;\n\
            }\n\
            fun leaf<M>(s: string): Node<M> = Node.Leaf(s);\n\
            enum Msg { Go }\n";
        ok(&format!(
            "{node}fun a(): Node<Msg> = leaf(\"x\").pad(1).pad(2);\n\
             fun main() {{ let n: Node<Msg> = leaf(\"x\").pad(1); }}"
        ));
        // A method not returning the receiver's type passes nothing back.
        let src = format!("{node}fun main() {{ let s: i64 = leaf(\"x\").size(); }}");
        let found = errors(&src);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0412"), "{found:?}");
        let src = format!("{node}fun main() {{ println(leaf(\"x\").pad(1) == leaf(\"y\")); }}");
        let found = errors(&src);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0412"), "{found:?}");
    }

    #[test]
    fn an_associated_function_only_its_result_types_waits_v17() {
        let cmd = "enum Cmd<M> { Nothing, Send(M) }\n\
            impl Cmd<M> { fun none(): Cmd<M> = Cmd.Nothing; }\n\
            enum Ev { Go }\n";
        ok(&format!(
            "{cmd}fun pair<A>(a: A, b: A): A[] = [a, b];\n\
             fun main() {{ let c: Cmd<Ev> = Cmd.Nothing; let xs = pair(Cmd.none(), c); }}"
        ));
        let src = format!("{cmd}fun main() {{ let c = Cmd.none(); }}");
        let found = errors(&src);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0412"), "{found:?}");
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
