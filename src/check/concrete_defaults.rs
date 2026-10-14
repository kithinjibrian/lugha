//! Default methods on concrete types (v10 draft §4, milestone 34):
//! `c.describe()` on a struct or enum without its own `describe` calls the
//! default of the one visible interface that supplies it and that the type
//! satisfies (E0430 for more than one).

use std::collections::HashMap;

use super::applied::Applied;
use super::generics::params;
use super::methods::Method;
use super::{Checker, Type, errors};
use crate::ast::{Expr, Ident};

impl Checker {
    /// `x.name(…)` on `receiver`, a struct or enum lacking `name`: the
    /// default's signature with `Self` as `receiver`, after recording the
    /// call's callee and instance. `None` when no visible interface supplies
    /// it (E0415 follows); `Some(None)` after E0430.
    pub(super) fn concrete_default(
        &mut self,
        call: &Expr,
        receiver: &Type,
        name: &Ident,
    ) -> Option<Option<Method>> {
        if !matches!(receiver, Type::Struct(..) | Type::Enum(..)) {
            return None;
        }
        let mut found = self.default_suppliers(receiver, &name.name, name);
        let applied = match found.len() {
            0 => return None,
            1 => found.remove(0),
            _ => {
                let names = [found[0].to_string(), found[1].to_string()];
                self.report(errors::two_defaults(
                    &name.name, receiver, &names, name.span,
                ));
                return Some(None);
            }
        };
        let req = self
            .items_for(&applied, receiver)
            .into_iter()
            .find(|r| r.name == name.name)
            .expect("a supplier declares the item");
        let key = format!("{}.{}", applied.key, name.name);
        self.callees.insert(call.id, key.clone());
        if !applied.args.is_empty() {
            self.default_args.insert(call.id, applied.args.clone());
        }
        let instance: Vec<Type> = std::iter::once(receiver.clone())
            .chain(applied.args.iter().cloned())
            .collect();
        self.used(call, &key, &instance);
        Some(Some(Method {
            receiver: req.receiver,
            params: req.params,
            ret: req.ret,
            span: req.span,
            generics: Vec::new(),
            outer: 0,
            key: String::new(),
        }))
    }

    /// The visible interfaces, sorted by key, that give `name` a default
    /// and that `ty` satisfies, at the arguments `ty`'s methods give them.
    fn default_suppliers(&self, ty: &Type, name: &str, at: &Ident) -> Vec<Applied> {
        let mut keys: Vec<&String> = self
            .interfaces
            .iter()
            .filter(|(_, info)| {
                info.items
                    .iter()
                    .any(|r| r.name == name && r.default.is_some() && r.receiver.is_some())
            })
            .map(|(key, _)| key)
            .filter(|key| self.interface_in_sight(key, at))
            .collect();
        keys.sort();
        keys.into_iter()
            .filter_map(|key| {
                let generics = self.interfaces[key].generics.clone();
                let names: Vec<&str> = generics.iter().map(|g| g.name.as_str()).collect();
                let mut bound = HashMap::new();
                let open = Applied {
                    key: key.clone(),
                    args: params(&generics),
                };
                self.read_off(&open, ty, &names, &mut bound);
                let args = names.iter().map(|n| bound.get(*n).cloned());
                let applied = Applied {
                    key: key.clone(),
                    args: args.collect::<Option<Vec<Type>>>()?,
                };
                self.unmet(ty, &applied).is_empty().then_some(applied)
            })
            .collect()
    }

    /// Whether interface `key` is declared here, in an imported module or
    /// in the prelude, and visible here.
    fn interface_in_sight(&self, key: &str, at: &Ident) -> bool {
        let module = key.rsplit_once('.').map_or("", |(m, _)| m);
        let here = |path: &[String]| path.join(".") == module;
        let near =
            module == "$prelude" || here(&self.module) || self.imports.values().any(|p| here(p));
        let Some(owner) = self.privacy.items.get(key) else {
            return near;
        };
        let name = key.rsplit('.').next().unwrap_or(key);
        near && self
            .denied(name, owner, owner.level, at.span, None)
            .is_none()
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const DESCRIBE: &str = "interface Describe {\n\
        fun name(self): string;\n\
        fun describe(self): string = \"this is {self.name()}\";\n\
        }\n";

    #[test]
    fn a_default_is_called_on_structs_and_enums() {
        ok(&format!(
            "{DESCRIBE}struct P {{ x: i64 }}\nimpl P {{ fun name(self): string = \"p\"; }}\n\
             enum L {{ Red, Green }}\nimpl L {{ fun name(self): string = \"l\"; }}\n\
             fun main() {{ let s: string = P {{ x: 1 }}.describe(); println(L.Red.describe()); }}"
        ));
    }

    #[test]
    fn the_types_own_method_wins() {
        ok(&format!(
            "{DESCRIBE}struct P {{ x: i64 }}\nimpl P {{ fun name(self): string = \"p\"; \
             fun describe(self): i64 = 1; }}\n\
             fun main() {{ let n: i64 = P {{ x: 1 }}.describe(); }}"
        ));
    }

    #[test]
    fn a_generic_interfaces_arguments_come_from_the_type() {
        ok("interface Source<T> {\n\
            fun next(mut self): Option<T>;\n\
            fun first(mut self): T = match self.next() { Some(x) => x, None => panic(\"empty\") };\n\
            }\n\
            struct C { n: i64 }\n\
            impl C { fun next(mut self): Option<i64> = Some(self.n); }\n\
            fun main() { let mut c = C { n: 4 }; let n: i64 = c.first(); }");
    }

    #[test]
    fn two_suppliers_are_e0430() {
        let src = format!(
            "{DESCRIBE}interface Other {{ fun describe(self): string = \"other\"; }}\n\
             struct P {{ x: i64 }}\nimpl P {{ fun name(self): string = \"p\"; }}\n\
             fun main() {{ println(P {{ x: 1 }}.describe()); }}"
        );
        assert_eq!(errors(&src), [("E0430", "describe")]);
    }

    #[test]
    fn no_supplier_is_still_e0415() {
        // `Q` lacks `name`, so it doesn't satisfy `Describe`.
        let src = format!(
            "{DESCRIBE}struct Q {{ x: i64 }}\nfun main() {{ println(Q {{ x: 1 }}.describe()); }}"
        );
        assert_eq!(errors(&src), [("E0415", "describe")]);
        let src = format!("{DESCRIBE}fun main() {{ let n = 1; println(n.describe()); }}");
        assert_eq!(errors(&src), [("E0415", "describe")]);
    }

    #[test]
    fn self_in_an_impl_checks_as_the_type() {
        ok("struct P { x: i64 }\n\
            impl P { fun o(): Self = Self { x: 0 }; fun up(self): Self = Self { x: self.x + 1 }; }\n\
            enum L { Red, Green }\n\
            impl L { fun flip(self): Self = match self { Red => Self.Green, Green => Self.Red }; }\n\
            fun main() { let p: P = P.o().up(); let l: L = L.Red.flip(); }");
        assert_eq!(
            errors("struct N { kids: Self[] }\nfun main() {}"),
            [("E0305", "Self")]
        );
    }
}
