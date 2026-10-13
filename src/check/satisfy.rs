//! Interfaces at work (v2 draft §4, milestone 12): calls through a type
//! parameter's bounds (`s.area()`, `T.make(…)`, E0421) and whether a type
//! argument satisfies an interface bound (E0422), structurally.

use super::generics::subst;
use super::interfaces::Requirement;
use super::methods::Method;
use super::{Checker, Type, errors};
use crate::ast::{Expr, Ident};

/// What a type offers for one requirement.
enum Offer {
    Missing,
    Mismatched,
    Matches,
}

impl Checker {
    /// The item `name` that one of `param`'s interface bounds declares.
    fn bound_item(&self, param: &str, name: &str) -> Option<Requirement> {
        let bounds = self.bounds(param);
        bounds.interfaces.iter().find_map(|key| {
            let items = &self.interfaces[key].items;
            items.iter().find(|r| r.name == name).cloned()
        })
    }

    /// `x.m(…)` (`on_value`) or `T.f(…)` on type parameter `param`: the
    /// signature its bounds declare, as a method to check the call against.
    /// E0421 if no bound declares it, E0415 for the wrong kind of call.
    pub(super) fn through_bound(
        &mut self,
        call: &Expr,
        param: &str,
        name: &Ident,
        on_value: bool,
    ) -> Option<Method> {
        let Some(req) = self.bound_item(param, &name.name) else {
            self.report(errors::no_bound_declares(param, &name.name, name.span));
            return None;
        };
        if req.receiver.is_some() != on_value {
            let ty = Type::Param(param.to_string());
            let help = if on_value {
                format!("call it on the type: `{param}.{}(…)`", name.name)
            } else {
                format!("call it on a value: `x.{}(…)`", name.name)
            };
            self.report(errors::no_method(&name.name, &ty, name.span, Some(help)));
            return None;
        }
        self.bound_used(call, param, &name.name);
        self.default_callee(call, &req);
        Some(Method {
            receiver: req.receiver,
            params: req.params,
            ret: req.ret,
            span: req.span,
            generics: Vec::new(),
            outer: 0,
            key: String::new(),
        })
    }

    /// E0422 unless `arg` satisfies interface `key`: every item it declares
    /// has a visible method or associated function with the same name,
    /// receiver kind and types.
    pub(super) fn satisfies(&mut self, arg: &Type, key: &str, span: crate::span::Span) -> bool {
        let problems = self.unmet(arg, key);
        if problems.is_empty() || matches!(arg, Type::Error) {
            return true;
        }
        self.report(errors::unsatisfied(arg, key, &problems, span));
        false
    }

    /// What `arg` lacks to satisfy interface `key`, one entry per item:
    /// empty when it satisfies it.
    pub(super) fn unmet(&self, arg: &Type, key: &str) -> Vec<String> {
        let mut problems = Vec::new();
        for req in &self.interfaces[key].items {
            match self.offer(arg, req) {
                Offer::Matches => {}
                Offer::Missing if req.default.is_some() => {}
                Offer::Missing => problems.push(format!("no method `{}`", req.name)),
                Offer::Mismatched => {
                    problems.push(format!("`{}` should be `{}`", req.name, shown(req)));
                }
            }
        }
        problems
    }

    /// What `arg` offers for `req`: a parameter offers its bounds' items, a
    /// struct or enum its visible methods (an `impl`'s parameters
    /// substituted), and every other type nothing.
    fn offer(&self, arg: &Type, req: &Requirement) -> Offer {
        let found = match arg {
            Type::Param(p) => match self.bound_item(p, &req.name) {
                Some(item) if item.same(req) => return Offer::Matches,
                Some(_) => return Offer::Mismatched,
                None => return Offer::Missing,
            },
            // An interface value offers what its interface declares (v5).
            Type::Interface(key) => {
                let items = self.interfaces.get(key).map(|i| i.items.as_slice());
                return match items
                    .unwrap_or_default()
                    .iter()
                    .find(|r| r.name == req.name)
                {
                    Some(item) if item.same(req) => Offer::Matches,
                    Some(_) => Offer::Mismatched,
                    None => Offer::Missing,
                };
            }
            Type::Struct(owner, args) | Type::Enum(owner, args) => {
                let method = self.methods.get(owner).and_then(|m| m.get(&req.name));
                match method {
                    Some(m) if self.method_usable(owner, &req.name) => (m, args),
                    _ => return Offer::Missing,
                }
            }
            _ => return Offer::Missing,
        };
        let (m, args) = found;
        // A method with type parameters of its own never matches: interface
        // signatures are concrete.
        if m.generics.len() > m.outer {
            return Offer::Mismatched;
        }
        let outer = &m.generics[..m.outer];
        let offered = Requirement {
            name: req.name.clone(),
            receiver: m.receiver,
            params: m.params.iter().map(|t| subst(t, outer, args)).collect(),
            ret: subst(&m.ret, outer, args),
            span: m.span,
            default: None,
        };
        if offered.same(req) {
            Offer::Matches
        } else {
            Offer::Mismatched
        }
    }
}

/// `fun area(self): f64`, as an interface declares it.
fn shown(req: &Requirement) -> String {
    let receiver = req.receiver.map(|m| if m { "mut self" } else { "self" });
    let params = req.params.iter().map(Type::to_string);
    let list: Vec<String> = receiver
        .map(String::from)
        .into_iter()
        .chain(params)
        .collect();
    let ret = if req.ret == Type::Void {
        String::new()
    } else {
        format!(": {}", req.ret)
    };
    format!("fun {}({}){ret}", req.name, list.join(", "))
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

    const SHAPES: &str = "interface Shape { fun area(self): f64; fun name(self): string; }\n\
        struct Circle { r: f64 }\n\
        impl Circle {\n\
            fun area(self): f64 = 3.0 * self.r * self.r;\n\
            fun name(self): string = \"circle\";\n\
            fun make(r: f64): Circle = Circle { r: r };\n\
            fun grow(mut self, by: f64) { self.r = self.r + by; }\n\
        }\n\
        interface Make { fun make(r: f64): Circle; }\n\
        interface Grow { fun grow(mut self, by: f64); }\n\
        fun total<T: Shape>(xs: T[]): f64 { let mut sum = 0.0; for x of xs { sum += x.area(); } sum }\n";

    fn e(body: &str) -> Vec<(&'static str, String)> {
        let src = format!("{SHAPES}{body}\nfun main() {{}}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    #[test]
    fn calls_go_through_the_bounds() {
        ok(&format!(
            "{SHAPES}fun f<T: Shape>(s: T): f64 = s.area();\n\
             fun g<T: Make>(): Circle = T.make(1.0);\n\
             fun h<T: Grow>(s: T) {{ let mut t = s; t.grow(1.0); }}\n\
             fun main() {{ let n = total([Circle {{ r: 1.0 }}]); }}"
        ));
        assert_eq!(
            e("fun f<T: Shape>(s: T): i64 = s.area();"),
            [("E0403", "s.area()".into())]
        );
        assert_eq!(
            e("fun f<T: Grow>(s: T) { let mut t = s; t.grow(\"x\"); }"),
            [("E0403", "\"x\"".into())]
        );
        assert_eq!(e("fun f<T: Grow>(s: T) { s.grow(1.0); }")[0].0, "E0501");
    }

    #[test]
    fn a_method_no_bound_declares_is_e0421() {
        let src = format!("{SHAPES}fun f<T: Shape>(s: T): f64 = s.perimeter();\nfun main() {{}}");
        assert_eq!(errors(&src), [("E0421", "perimeter")]);
        let help = diagnostics(&src)[0].help.clone().expect("a help");
        assert!(
            help.contains("add an interface that declares `perimeter`"),
            "{help}"
        );
        assert_eq!(
            e("fun f<T: Make>(s: T): Circle = s.make(1.0);")[0].0,
            "E0415"
        );
    }

    #[test]
    fn unsatisfied_bounds_are_e0422() {
        let at = |body: &str| e(body).into_iter().map(|(c, _)| c).collect::<Vec<_>>();
        assert_eq!(at("fun f() { let n = total([1, 2]); }"), ["E0422"]);
        let d = diagnostics(&format!(
            "{SHAPES}fun f() {{ let n = total([1, 2]); }}\nfun main() {{}}"
        ));
        assert_eq!(d[0].message, "`i64` doesn't satisfy `Shape`");
        assert!(
            d[0].help.as_deref().unwrap().contains("no method `area`"),
            "{:?}",
            d[0].help
        );
        let wrong = "struct Sq { s: f64 }\nimpl Sq { fun area(self): i64 = 1; fun name(self): string = \"sq\"; }\n";
        assert_eq!(
            at(&format!(
                "{wrong}fun f() {{ let n = total([Sq {{ s: 1.0 }}]); }}"
            )),
            ["E0422"]
        );
        let d = diagnostics(&format!(
            "{SHAPES}{wrong}fun f() {{ let n = total([Sq {{ s: 1.0 }}]); }}\nfun main() {{}}"
        ));
        assert!(
            d[0].help
                .as_deref()
                .unwrap()
                .contains("`area` should be `fun area(self): f64`"),
            "{:?}",
            d[0].help
        );
        let kind = "struct K {}\nimpl K { fun grow(self, by: f64) {} }\nfun g<T: Grow>(x: T) {}\n";
        assert_eq!(at(&format!("{kind}fun f() {{ g(K {{}}); }}")), ["E0422"]);
    }

    #[test]
    fn another_modules_private_method_doesnt_count() {
        use crate::check::modules::test_util::errors;
        let found = errors(&[
            (
                "main.la",
                "import geo;\ninterface Shape { fun area(self): f64; }\n\
                 fun a<T: Shape>(x: T): f64 = x.area();\n\
                 fun main() { let n = a(geo.Sq { s: 1.0 }); }\n",
            ),
            (
                "geo.la",
                "struct Sq { s: f64 }\nimpl Sq { private fun area(self): f64 = self.s; }\n",
            ),
        ]);
        assert_eq!(found.iter().map(|(c, _)| *c).collect::<Vec<_>>(), ["E0422"]);
    }

    #[test]
    fn generic_types_and_parameters_satisfy_too() {
        ok("interface Sized { fun size(self): i64; }\n\
            struct Stack<T> { items: T[] }\n\
            impl Stack<T> { fun size(self): i64 = self.items.len; }\n\
            fun count<T: Sized>(x: T): i64 = x.size();\n\
            fun pass<U: Sized>(u: U): i64 = count(u);\n\
            fun main() { let s = Stack { items: [1] }; let n = count(s); let m = pass(s); }");
    }
}
