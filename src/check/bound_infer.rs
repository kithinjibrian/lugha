//! Inference through bounds (v6 draft §4, milestone 23): in
//! `fun collect<T, S: Source<T>>(src: S)`, `T` appears in no argument, so
//! `collect(Count { … })` reads it off `Count.next` once `S = Count`.

use std::collections::HashMap;

use super::applied::Applied;
use super::expr::Expect;
use super::generics::{Generic, replace, subst};
use super::infer::{unbound, unify};
use super::interfaces::Requirement;
use super::{Checker, Type};
use crate::ast::Expr;

impl Checker {
    /// An argument `e`, found to be of concrete type `found`, for a
    /// parameter declared as interface `want` with unbound arguments (v6):
    /// they are read off `found`'s methods, then `e` converts. `Some`
    /// (whether it failed) when it applied.
    pub(super) fn interface_input(
        &mut self,
        e: &Expr,
        want: &Expect,
        found: &Type,
        names: &[&str],
        bound: &mut HashMap<String, Type>,
    ) -> Option<bool> {
        let applied = Applied::of(&want.ty)?;
        if matches!(found, Type::Interface(..) | Type::Error | Type::Never) {
            return None;
        }
        self.read_off(&applied, found, names, bound);
        let map = bound.iter().map(|(k, v)| (k.as_str(), v)).collect();
        let ty = replace(&want.ty, &map);
        if unbound(&ty, names, bound) {
            return None; // E0403 or E0412 follows
        }
        let expect = Expect {
            ty,
            reason: want.reason.clone(),
        };
        Some(self.convert(e, found.clone(), Some(&expect)) == Type::Error)
    }

    /// Binds the parameters among `generics` that only a bound's arguments
    /// mention, from what the bound parameters' types offer for the
    /// interface's items, until nothing new is bound.
    pub(super) fn through_bounds(&self, generics: &[Generic], bound: &mut HashMap<String, Type>) {
        let names: Vec<&str> = generics.iter().map(|g| g.name.as_str()).collect();
        loop {
            let before = bound.len();
            for g in generics {
                let Some(concrete) = bound.get(&g.name).cloned() else {
                    continue;
                };
                for applied in &g.bounds.interfaces {
                    if applied.args.iter().any(|a| unbound(a, &names, bound)) {
                        self.read_off(applied, &concrete, &names, bound);
                    }
                }
            }
            if bound.len() == before {
                return;
            }
        }
    }

    /// Unifies each item of `applied` with what `concrete` offers for it.
    fn read_off(
        &self,
        applied: &Applied,
        concrete: &Type,
        names: &[&str],
        bound: &mut HashMap<String, Type>,
    ) {
        for req in self.items(applied) {
            let Some(offered) = self.offered(concrete, &req.name) else {
                continue;
            };
            if offered.receiver != req.receiver || offered.params.len() != req.params.len() {
                continue; // satisfaction reports it (E0422)
            }
            for (declared, actual) in req.params.iter().zip(&offered.params) {
                unify(declared, actual, names, bound);
            }
            unify(&req.ret, &offered.ret, names, bound);
        }
    }

    /// The signature `ty` offers for item `name`: a struct's or enum's
    /// visible, non-generic method at its type arguments, a parameter's
    /// bound item, or an interface value's item.
    fn offered(&self, ty: &Type, name: &str) -> Option<Requirement> {
        match ty {
            Type::Param(p) => self.bound_item(p, name),
            Type::Interface(..) => self
                .items(&Applied::of(ty).expect("an interface type"))
                .into_iter()
                .find(|r| r.name == name),
            Type::Struct(owner, args) | Type::Enum(owner, args) => {
                let m = self.methods.get(owner)?.get(name)?;
                if !self.method_usable(owner, name) || m.generics.len() > m.outer {
                    return None;
                }
                let outer = &m.generics[..m.outer];
                Some(Requirement {
                    name: name.to_string(),
                    receiver: m.receiver,
                    params: m.params.iter().map(|t| subst(t, outer, args)).collect(),
                    ret: subst(&m.ret, outer, args),
                    span: m.span,
                    default: None,
                })
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    const SOURCE: &str = "interface Source<T> { fun next(mut self): Option<T>; }\n\
        interface Box<T> { fun get(self): T; }\n\
        struct Count { n: i64 }\n\
        impl Count { fun next(mut self): Option<i64> = Some(self.n); }\n\
        struct Words { xs: string[] }\n\
        impl Words { fun next(mut self): Option<string> = None; }\n\
        struct Repeat<T> { x: T }\n\
        impl Repeat<T> { fun next(mut self): Option<T> = Some(self.x); }\n\
        struct Holder { c: Count }\n\
        impl Holder { fun get(self): Count = self.c; }\n\
        fun collect<T, S: Source<T>>(src: S): T[] = [];\n\
        fun first<T, S: Source<T>>(src: S, d: T): T = d;\n\
        fun deep<T, S: Source<T>, B: Box<S>>(b: B): T[] = [];\n\
        fun lost<T, U, S: Source<T>>(src: S): U[] = [];\n";

    fn ty(body: &str, name: &str) -> Type {
        let_type(&format!("{SOURCE}fun main() {{ {body} }}"), name)
    }

    fn codes(body: &str) -> Vec<&'static str> {
        let src = format!("{SOURCE}fun main() {{ {body} }}");
        errors(&src).into_iter().map(|(c, _)| c).collect()
    }

    fn array(t: Type) -> Type {
        Type::Array(Box::new(t))
    }

    #[test]
    fn a_parameter_only_a_bound_mentions_is_read_off_the_methods() {
        let count = "Count { n: 1 }";
        assert_eq!(
            ty(&format!("let x = collect({count});"), "x"),
            array(Type::I64)
        );
        assert_eq!(
            ty("let x = collect(Words { xs: [] });", "x"),
            array(Type::String)
        );
        assert_eq!(
            ty("let x = collect(Repeat { x: 1.5 });", "x"),
            array(Type::F64)
        );
        assert_eq!(ty(&format!("let x = first({count}, 2);"), "x"), Type::I64);
    }

    #[test]
    fn inference_chains_through_bounds() {
        assert_eq!(
            ty("let x = deep(Holder { c: Count { n: 1 } });", "x"),
            array(Type::I64)
        );
    }

    #[test]
    fn what_nothing_fixes_is_e0412() {
        assert_eq!(codes("let x = lost(Count { n: 1 });"), ["E0412"]);
    }

    #[test]
    fn a_disagreeing_argument_is_e0422() {
        assert_eq!(codes("let x = first(Count { n: 1 }, \"x\");"), ["E0422"]);
    }

    #[test]
    fn a_callers_bound_offers_its_items() {
        ok(&format!(
            "{SOURCE}fun pass<U: Source<string>>(u: U): string[] = collect(u);\nfun main() {{}}"
        ));
    }

    #[test]
    fn an_interface_typed_parameter_reads_its_arguments_off_the_value() {
        let lib =
            "fun count<T>(s: Source<T>): T[] = [];\nfun pick<T>(s: Source<T>, d: T): T = d;\n";
        let at = |body: &str, name: &str| {
            let_type(&format!("{SOURCE}{lib}fun main() {{ {body} }}"), name)
        };
        assert_eq!(at("let x = count(Count { n: 1 });", "x"), array(Type::I64));
        assert_eq!(at("let x = pick(Count { n: 1 }, 0);", "x"), Type::I64);
        assert_eq!(
            at(
                "let w: Source<string> = Words { xs: [] }; let x = count(w);",
                "x"
            ),
            array(Type::String)
        );
        let src = format!("{SOURCE}{lib}fun main() {{ let x = pick(Count {{ n: 1 }}, \"s\"); }}");
        // The value goes first and fixes `T`, so the literal is the mismatch.
        assert_eq!(errors(&src), [("E0403", "\"s\"")]);
    }
}
