//! Default methods (v5 draft §4, milestone 22): an interface method with a
//! body is a generic method of the interface, `<Interface>.<method>`, with
//! one hidden type parameter `Self` bounded by the interface, then the
//! interface's own parameters (v6). Its body is checked once; each type
//! (and argument list) that uses it gets an instance.

use super::applied::Applied;
use super::generics::{Bounds, Generic, params, subst};
use super::instances::Instance;
use super::interfaces::Requirement;
use super::methods::Method;
use super::{Checker, Type};
use crate::ast::{Expr, Item, Program};

/// The hidden type parameter of a default method: the receiver's type.
pub const SELF: &str = "Self";

impl Checker {
    /// Registers the default methods among `items` of interface `key` as its
    /// methods, generic over `Self: key<P…>` and then the interface's
    /// parameters `P…`, so bodies, calls and instances work as for any
    /// generic method.
    pub(super) fn register_defaults(&mut self, key: &str, items: &[Requirement]) {
        let own = self.interface_generics(key);
        for r in items.iter().filter(|r| r.default.is_some()) {
            let bounds = Bounds {
                interfaces: vec![Applied {
                    key: key.to_string(),
                    args: params(&own),
                }],
                ..Bounds::default()
            };
            let method = Method {
                receiver: r.receiver,
                params: r.params.clone(),
                ret: r.ret.clone(),
                span: r.span,
                generics: [
                    vec![Generic {
                        name: SELF.to_string(),
                        bounds,
                    }],
                    own.clone(),
                ]
                .concat(),
                outer: 1 + own.len(),
                key: format!("{key}.{}", r.name),
            };
            let methods = self.methods.entry(key.to_string()).or_default();
            methods.insert(r.name.clone(), method);
        }
    }

    /// Checks every default body of `program` once, with `self: Self`.
    pub(super) fn default_bodies(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Interface(i) = item else { continue };
            let key = self.key(&i.name.name);
            for it in &i.items {
                let Some(f) = &it.default else { continue };
                let found = self.methods.get(&key).and_then(|m| m.get(&it.name.name));
                // A duplicate item, or an interface reported already: skip it.
                let Some(m) = found.filter(|m| m.span == it.name.span).cloned() else {
                    continue;
                };
                self.owner = m.key.clone();
                self.type_params = m.generics.clone();
                self.body(f, m.params, m.ret, Some(Type::Param(SELF.to_string())));
            }
        }
    }

    /// A call through a bound of a method that has a default: codegen falls
    /// back to the default's instance for a type without its own method.
    /// `args` are the bound's interface arguments (v6), recorded for a
    /// generic interface.
    pub(super) fn default_callee(&mut self, call: &Expr, req: &Requirement, args: Vec<Type>) {
        if let Some(interface) = &req.default {
            let key = format!("{interface}.{}", req.name);
            self.callees.insert(call.id, key);
            if !args.is_empty() {
                self.default_args.insert(call.id, args);
            }
        }
    }

    /// E0415's help for a default method called on a concrete type.
    pub(super) fn default_help(&self, name: &str) -> Option<String> {
        let mut owners: Vec<&String> = self
            .interfaces
            .iter()
            .filter(|(_, info)| {
                info.items
                    .iter()
                    .any(|r| r.name == name && r.default.is_some())
            })
            .map(|(key, _)| key)
            .collect();
        owners.sort();
        let owner = owners.first()?;
        Some(format!(
            "`{name}` is a default method of `{owner}`: call it through a `{owner}` value or a bound `T: {owner}`"
        ))
    }

    /// The default's instance a call of `name` through `param` reaches when
    /// `generics` are `args` and `param` is `concrete`, a type without its
    /// own `name`: its arguments are `concrete` and the bound's (v6).
    pub(super) fn default_target(
        &self,
        generics: &[Generic],
        args: &[Type],
        param: &str,
        name: &str,
        concrete: &Type,
    ) -> Option<Instance> {
        let lacks = match concrete {
            Type::Struct(owner, _) | Type::Enum(owner, _) => {
                self.methods.get(owner).and_then(|m| m.get(name)).is_none()
            }
            // Interface values dispatch at run time; the others don't occur.
            Type::Interface(..) | Type::Param(_) | Type::Error => false,
            _ => true,
        };
        if !lacks {
            return None;
        }
        let bounds = &generics.iter().find(|g| g.name == param)?.bounds;
        let interface = bounds.interfaces.iter().find(|a| {
            let items = &self.interfaces[&a.key].items;
            items.iter().any(|r| r.name == name && r.default.is_some())
        })?;
        let own = interface.args.iter().map(|a| subst(a, generics, args));
        Some(Instance {
            key: format!("{}.{name}", interface.key),
            args: std::iter::once(concrete.clone()).chain(own).collect(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

    const ERR: &str = "interface Error {\n\
            fun message(self): string;\n\
            fun report(self): string = \"error: {self.message()}\";\n\
        }\n\
        struct Plain { text: string }\n\
        impl Plain { fun message(self): string = self.text; }\n\
        struct Own { n: i64 }\n\
        impl Own { fun message(self): string = \"own\"; fun report(self): string = \"mine\"; }\n";

    fn e(body: &str) -> Vec<(&'static str, String)> {
        let src = format!("{ERR}{body}\nfun main() {{}}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    fn monos(src: &str) -> Vec<String> {
        ok(src)
            .monos
            .iter()
            .map(|i| format!("{}{:?}", i.key, i.args))
            .collect()
    }

    #[test]
    fn a_missing_defaulted_method_still_satisfies() {
        ok(&format!(
            "{ERR}fun tell<T: Error>(x: T): string = x.report();\n\
             fun main() {{ let a: Error = Plain {{ text: \"p\" }}; let b: Error = Own {{ n: 1 }}; let s = a.report() + tell(Plain {{ text: \"q\" }}); }}"
        ));
        let clash = "struct Bad {}\nimpl Bad { fun message(self): string = \"b\"; fun report(self): i64 = 1; }\n\
                     fun f() { let x: Error = Bad {}; }";
        assert_eq!(e(clash), [("E0422", "Bad {}".into())]);
    }

    #[test]
    fn a_default_body_sees_only_the_interface() {
        assert_eq!(
            e("interface I { fun a(self): i64; fun b(self): i64 = self.c(); }"),
            [("E0421", "c".into())]
        );
        assert_eq!(
            e("interface I { fun a(self): i64; fun b(self): i64 = self.n; }"),
            [("E0410", "n".into())]
        );
        // v6: `Self` may be written in a default body; it is the receiver's type.
        assert_eq!(
            e(
                "interface I { fun a(self): i64; fun b(self): i64 { let s: Self = self; s.a() + s.n } }"
            ),
            [("E0410", "n".into())]
        );
        assert_eq!(
            e("interface I { fun a(self): i64; fun b(self): string = self.a(); }"),
            [("E0403", "self.a()".into())]
        );
    }

    #[test]
    fn a_default_is_a_method_only_of_types_that_satisfy_it() {
        // v10: `Plain` satisfies `Error`, so the default applies to it directly.
        ok(&format!(
            "{ERR}fun main() {{ let p = Plain {{ text: \"p\" }}; let s: string = p.report(); }}"
        ));
        let body = "struct Mute {}\nfun f() { let p = Mute {}; let s = p.report(); }";
        assert_eq!(e(body), [("E0415", "report".into())]);
        let src = format!("{ERR}{body}\nfun main() {{}}");
        let help = diagnostics(&src)[0].help.clone().unwrap_or_default();
        assert!(help.contains("`Error`"), "{help}");
    }

    #[test]
    fn only_used_defaults_are_instances() {
        let converted = monos(&format!(
            "{ERR}fun main() {{ let a: Error = Plain {{ text: \"p\" }}; let b: Error = Own {{ n: 1 }}; }}"
        ));
        assert!(
            converted.contains(&"Error.report[Struct(\"Plain\", [])]".to_string()),
            "{converted:?}"
        );
        assert!(
            !converted.iter().any(|m| m.contains("Own")),
            "{converted:?}"
        );
        let bound = monos(&format!(
            "{ERR}fun tell<T: Error>(x: T): string = x.report();\nfun main() {{ let s = tell(Plain {{ text: \"q\" }}); }}"
        ));
        assert!(
            bound.contains(&"Error.report[Struct(\"Plain\", [])]".to_string()),
            "{bound:?}"
        );
    }

    #[test]
    fn a_generic_interfaces_default_is_generic_over_its_parameters_too() {
        let source = "interface Source<T> {\n\
                fun next(mut self): Option<T>;\n\
                fun first(mut self, d: T): T = match self.next() { Some(x) => x, None => d };\n\
            }\n\
            struct Count { n: i64 }\n\
            impl Count { fun next(mut self): Option<i64> = Some(self.n); }\n\
            fun pick<T, S: Source<T>>(s: S, d: T): T { let mut t = s; t.first(d) }\n";
        let found = monos(&format!(
            "{source}fun main() {{ let n = pick(Count {{ n: 1 }}, 0); }}"
        ));
        assert!(
            found.contains(&"Source.first[Struct(\"Count\", []), I64]".to_string()),
            "{found:?}"
        );
        let wrong = "interface Source<T> { fun next(mut self): Option<T>; fun bad(mut self): i64 = self.next(); }\nfun main() {}";
        assert_eq!(errors(wrong), [("E0403", "self.next()")]);
    }
}
