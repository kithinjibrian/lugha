//! Inference from sibling arguments (v17, milestone 96, PRP-127): a call to
//! a generic function whose type parameters appear only in its result
//! (`text<M>(s: string): View<M>`) can't be typed alone, so it waits for its
//! siblings — the other arguments of the call it is passed to, the other
//! operand of `==`, or the other side of `assert_eq` — as literals and
//! `None` already do, and takes the type they fix as its expected type.

use super::expr::Expect;
use super::infer::{Bound, known, unify};
use super::ops::defers;
use super::results::RESULT;
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

    /// The declared type a waiting expression will have, when its head is
    /// known: a waiting call's declared result (`Node<M>` for
    /// `leaf<M>(s): Node<M>`, `Result<T, DecodeError>` for `json.decode`),
    /// or a method's result through a waiting receiver. Only its head
    /// (struct, enum or `Option`) is read.
    fn waiting_head(&self, e: &Expr) -> Option<Type> {
        let ty = match self.waiting_receiver(e) {
            Some(ret) => ret,
            None => {
                let ExprKind::Call(callee, _) = &e.kind else {
                    return None;
                };
                if !self.waiting_call(e) {
                    return None;
                }
                self.callee_signature(callee)?.2
            }
        };
        matches!(ty, Type::Struct(..) | Type::Enum(..) | Type::Option(_)).then_some(ty)
    }

    /// For `r.m(…)` whose receiver `r` waits and whose method `m`'s result
    /// mentions the receiver type's own parameters (`fun pad(self, n: i64):
    /// Node<M>` in `impl Node<M>`, `unwrap` on a `Result<T, E>`): that
    /// result, declared. Such a call waits too, and its expected type
    /// reaches the receiver (kivuli: `text("a").padding(8)`; v17, milestone
    /// 148: `json.decode(v).unwrap()`).
    fn waiting_receiver(&self, e: &Expr) -> Option<Type> {
        let ExprKind::Call(callee, _) = &e.kind else {
            return None;
        };
        let ExprKind::Field(base, name) = &callee.kind else {
            return None;
        };
        let head = self.waiting_head(base)?;
        let (ret, params, _) = self.result_of(&head, &name.name)?;
        params.iter().any(|p| mentions(&ret, p)).then_some(ret)
    }

    /// The method `name` on a receiver headed like `head`: its declared
    /// result, the receiver type's own parameters it is written in, and
    /// the receiver's type at those parameters. `unwrap` and `expect` on
    /// `Option` and `Result` are built in, so they are answered here.
    fn result_of(&self, head: &Type, name: &str) -> Option<(Type, Vec<String>, Type)> {
        // `$`-names can't clash with a program's type parameters.
        let p = |n: &str| Type::Param(n.to_string());
        let payload = matches!(name, "unwrap" | "expect");
        match head {
            Type::Option(_) if payload => {
                Some((p("$T"), vec!["$T".into()], Type::Option(Box::new(p("$T")))))
            }
            Type::Enum(k, _) if payload && k == RESULT => Some((
                p("$T"),
                vec!["$T".into(), "$E".into()],
                Type::Enum(k.clone(), vec![p("$T"), p("$E")]),
            )),
            Type::Struct(k, _) | Type::Enum(k, _) => {
                let m = self.methods.get(k)?.get(name)?;
                m.receiver?;
                let params: Vec<String> = m.generics[..m.outer]
                    .iter()
                    .map(|g| g.name.clone())
                    .collect();
                let own = params.iter().map(|g| p(g)).collect();
                let receiver = match head {
                    Type::Struct(..) => Type::Struct(k.clone(), own),
                    _ => Type::Enum(k.clone(), own),
                };
                Some((m.ret.clone(), params, receiver))
            }
            _ => None,
        }
    }

    /// The expected type for the receiver of `base.name(…)`, when the
    /// receiver waits: the call's expected type matched against the
    /// method's result fixes some of the receiver type's parameters, and
    /// the others are left open, as `?` leaves the error type open.
    pub(super) fn receiver_expect(
        &self,
        base: &Expr,
        name: &str,
        expect: Option<&Expect>,
    ) -> Option<Expect> {
        let expected = expect?;
        let head = self.waiting_head(base)?;
        let (ret, params, receiver) = self.result_of(&head, name)?;
        let names: Vec<&str> = params.iter().map(String::as_str).collect();
        let mut bound = Bound::new();
        unify(&ret, &expected.ty, &names, &mut bound);
        (!bound.is_empty()).then(|| Expect::of(known(&receiver, &names, &bound)))
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
    fn unwrap_and_expect_pass_the_wanted_type_back_v17() {
        let lib = "struct Person { name: string }\n\
            fun decode<T>(s: string): Result<T, string> = Err(s);\n\
            fun maybe<T>(): Option<T> = None;\n\
            fun take(p: Person) {}\n";
        ok(&format!(
            "{lib}fun get(): Person = decode(\"x\").unwrap();\n\
             fun main() {{\n\
                 let p: Person = decode(\"x\").unwrap();\n\
                 let q: Person = decode(\"x\").expect(\"a person\");\n\
                 let r: Person = maybe().unwrap();\n\
                 let s: string = maybe().expect(\"text\");\n\
                 take(decode(\"x\").unwrap());\n\
                 let all: Person[] = [decode(\"x\").unwrap(), maybe().unwrap()];\n\
                 println(decode(\"x\").unwrap() == p);\n\
                 println(q == maybe().unwrap());\n\
             }}"
        ));
        // Nothing wanting a type: still E0412.
        let src = format!("{lib}fun main() {{ let p = decode(\"x\").unwrap(); }}");
        let found = errors(&src);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0412"), "{found:?}");
        // A concrete receiver that doesn't fit: one error, as before.
        let src = format!(
            "{lib}fun one(): Result<i64, string> = Ok(1);\n\
             fun main() {{ let s: string = one().unwrap(); }}"
        );
        let found = errors(&src);
        assert_eq!(found.len(), 1, "{found:?}");
    }

    #[test]
    fn a_programs_own_methods_pass_it_back_too_v17() {
        let lib = "struct Pair<T> { a: T, b: T }\n\
            impl Pair<T> {\n\
                fun first(self): T = self.a;\n\
                fun wrap(self): Option<T> = Some(self.a);\n\
                fun size(self): i64 = 2;\n\
            }\n\
            fun zeros<T: Num>(): Pair<T> = Pair { a: 0, b: 0 };\n";
        ok(&format!(
            "{lib}fun main() {{\n\
                 let n: u8 = zeros().first();\n\
                 let m: Option<i32> = zeros().wrap();\n\
                 let k: u16 = zeros().wrap().unwrap();\n\
             }}"
        ));
        let src = format!("{lib}fun main() {{ let n: i64 = zeros().size(); }}");
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
