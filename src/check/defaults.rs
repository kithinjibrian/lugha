//! Default methods (v5 draft §4, milestone 22): an interface method with a
//! body is a generic method of the interface, `<Interface>.<method>`, with
//! one hidden type parameter `Self` bounded by the interface. Its body is
//! checked once; each type that uses it gets an instance.

use super::generics::{Bounds, Generic};
use super::instances::Instance;
use super::interfaces::Requirement;
use super::methods::Method;
use super::{Checker, Type};
use crate::ast::{Expr, Item, Program};

/// The hidden type parameter of a default method: the receiver's type.
pub const SELF: &str = "Self";

impl Checker {
    /// Registers the default methods among `items` of interface `key` as its
    /// methods, generic over `Self: key`, so bodies, calls and instances
    /// work as for any generic method.
    pub(super) fn register_defaults(&mut self, key: &str, items: &[Requirement]) {
        for r in items.iter().filter(|r| r.default.is_some()) {
            let bounds = Bounds {
                interfaces: vec![key.to_string()],
                ..Bounds::default()
            };
            let method = Method {
                receiver: r.receiver,
                params: r.params.clone(),
                ret: r.ret.clone(),
                span: r.span,
                generics: vec![Generic {
                    name: SELF.to_string(),
                    bounds,
                }],
                outer: 1,
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
    pub(super) fn default_callee(&mut self, call: &Expr, req: &Requirement) {
        if let Some(interface) = &req.default {
            let key = format!("{interface}.{}", req.name);
            self.callees.insert(call.id, key);
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
    /// `param` is `concrete`, a type without its own `name`.
    pub(super) fn default_target(
        &self,
        generics: &[Generic],
        param: &str,
        name: &str,
        concrete: &Type,
    ) -> Option<Instance> {
        let lacks = match concrete {
            Type::Struct(owner, _) | Type::Enum(owner, _) => {
                self.methods.get(owner).and_then(|m| m.get(name)).is_none()
            }
            // Interface values dispatch at run time; the others don't occur.
            Type::Interface(_) | Type::Param(_) | Type::Error => false,
            _ => true,
        };
        if !lacks {
            return None;
        }
        let bounds = &generics.iter().find(|g| g.name == param)?.bounds;
        let interface = bounds.interfaces.iter().find(|key| {
            let items = &self.interfaces[*key].items;
            items.iter().any(|r| r.name == name && r.default.is_some())
        })?;
        Some(Instance {
            key: format!("{interface}.{name}"),
            args: vec![concrete.clone()],
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
        assert_eq!(
            e("interface I { fun a(self): i64; fun b(self): i64 { let s: Self = self; 1 } }"),
            [("E0305", "Self".into())]
        );
        assert_eq!(
            e("interface I { fun a(self): i64; fun b(self): string = self.a(); }"),
            [("E0403", "self.a()".into())]
        );
    }

    #[test]
    fn a_default_isnt_a_method_of_the_concrete_type() {
        let body = "fun f() { let p = Plain { text: \"p\" }; let s = p.report(); }";
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
}
