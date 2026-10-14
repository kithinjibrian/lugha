//! Printing values (v12 draft §4–5, milestone 39): which types `print`,
//! `to_string` and `"{x}"` accept, and the `show` instances a printed value
//! needs. A struct's or enum's own `fun show(self): string` gives its text;
//! codegen builds every other form.

use std::collections::HashSet;

use super::generics::subst;
use super::instances::Instance;
use super::methods::Method;
use super::{Checker, Type};
use crate::ast::Expr;

/// The key of a print site's use: its one argument is the printed type.
pub(super) const PRINT: &str = "$print";

/// True if a value of `ty` can be printed: everything but `void` (v12).
pub(super) fn printable(ty: &Type) -> bool {
    *ty != Type::Void
}

/// True for the types codegen prints without any helper.
fn plain(ty: &Type) -> bool {
    ty.is_numeric() || matches!(ty, Type::Bool | Type::String | Type::Error | Type::Never)
}

impl Checker {
    /// Records that the body being checked prints a value of `ty`, so each
    /// instance lists the `show` methods it reaches (`show_instances`).
    pub(super) fn printed(&mut self, at: &Expr, ty: &Type) {
        if !plain(ty) {
            self.used(at, PRINT, std::slice::from_ref(ty));
        }
    }

    /// `r.unwrap()` on a `Result<T, E>` prints the error when it fails (v12).
    pub(super) fn unwrap_prints(&mut self, call: &Expr, receiver: &Type, name: &str) {
        if let Type::Enum(key, args) = receiver
            && key == super::results::RESULT
            && name == "unwrap"
        {
            self.printed(call, &args[1]);
        }
    }

    /// The type's own `show`, when it is `fun show(self): string`; a `show`
    /// of any other shape is ignored.
    pub(super) fn own_show(&self, owner: &str) -> Option<&Method> {
        let m = self.methods.get(owner)?.get("show")?;
        let fits = m.receiver == Some(false)
            && m.params.is_empty()
            && m.ret == Type::String
            && m.generics.len() == m.outer;
        fits.then_some(m)
    }

    /// The generic `show` instances printing a value of concrete type `ty`
    /// reaches: its own, or else those inside its fields, payloads,
    /// elements, and the values converted to an interface it is.
    pub(super) fn show_instances(&self, ty: &Type) -> Vec<Instance> {
        let mut found = Vec::new();
        self.reach(ty, &mut HashSet::new(), &mut found);
        found
    }

    fn reach(&self, ty: &Type, seen: &mut HashSet<Type>, found: &mut Vec<Instance>) {
        if plain(ty) || !seen.insert(ty.clone()) {
            return;
        }
        match ty {
            Type::Array(inner) | Type::Option(inner) => self.reach(inner, seen, found),
            Type::Struct(key, args) | Type::Enum(key, args) => {
                if let Some(m) = self.own_show(key) {
                    if !args.is_empty() {
                        found.push(Instance {
                            key: m.key.clone(),
                            args: args.clone(),
                        });
                    }
                    return;
                }
                let mut parts: Vec<Type> = Vec::new();
                if let Some(info) = self.structs.get(key) {
                    parts.extend(
                        info.fields
                            .iter()
                            .map(|(_, t)| subst(t, &info.generics, args)),
                    );
                }
                for (_, payloads) in self.enum_variants(key, args).unwrap_or_default() {
                    parts.extend(payloads);
                }
                for part in &parts {
                    self.reach(part, seen, found);
                }
            }
            Type::Interface(key, _) => {
                for (id, target) in &self.conversions {
                    let Type::Interface(k, _) = target else {
                        continue;
                    };
                    let source = self.types.get(id.0 as usize).cloned().flatten();
                    if let Some(source) = source.filter(|_| k == key) {
                        self.reach(&source, seen, found);
                    }
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, ok};

    const TYPES: &str = "struct P { x: i64, s: string }\nenum E { A(f64), B }\n\
        struct Stack<T> { items: T[] }\n\
        impl Stack<T> { fun show(self): string = \"stack\"; }\n\
        interface Shape { fun area(self): f64; }\nfun nothing() {}\n";

    fn monos(body: &str) -> Vec<String> {
        ok(&format!("{TYPES}{body}"))
            .monos
            .iter()
            .map(|m| {
                let args: Vec<String> = m.args.iter().map(Type::to_string).collect();
                format!("{}<{}>", m.key, args.join(", "))
            })
            .collect()
    }

    #[test]
    fn every_value_but_void_prints_v12() {
        ok(&format!(
            "{TYPES}fun dump<T>(x: T): string = to_string(x);\n\
             fun main() {{\n\
             let p = P {{ x: 1, s: \"a\" }};\n\
             println(p); print(E.B); println([1]); println(Some(2));\n\
             let r: Result<i64, string> = Ok(1); eprintln(r);\n\
             let t = \"{{p}} {{[E.A(1.0)]}}\"; let u = to_string(\"s\"); let v = dump(p);\n\
             let f = fun(x: i64): i64 = x; println(f); let g = \"{{f}}\";\n}}"
        ));
        let e = |body: &str| {
            errors(&format!("{TYPES}fun main() {{ {body} }}"))
                .into_iter()
                .map(|(c, _)| c)
                .collect::<Vec<_>>()
        };
        assert_eq!(e("println(nothing());"), ["E0407"]);
        assert_eq!(e("let t = \"{nothing()}\";"), ["E0407"]);
    }

    #[test]
    fn a_printed_generic_type_lists_its_show() {
        let direct = monos("fun main() { println(Stack { items: [1] }); }");
        assert!(
            direct.contains(&"Stack.show<i64>".to_string()),
            "{direct:?}"
        );
        let inside =
            monos("fun main() { let xs = [Stack { items: [\"a\"] }]; println(\"{xs}\"); }");
        assert!(
            inside.contains(&"Stack.show<string>".to_string()),
            "{inside:?}"
        );
        let generic = monos(
            "fun dump<T>(x: T) { println(x); }\nfun main() { dump(Some(Stack { items: [true] })); }",
        );
        assert!(
            generic.contains(&"Stack.show<bool>".to_string()),
            "{generic:?}"
        );
    }

    #[test]
    fn interface_values_print_and_unwrap_shows_the_error_v12() {
        ok(&format!(
            "{TYPES}struct C {{ r: f64 }}\nimpl C {{ fun area(self): f64 = self.r; }}\n\
             fun main() {{ let s: Shape = C {{ r: 1.0 }}; println(s); let t = \"{{s}}\"; let u = to_string([s]); }}"
        ));
        let found =
            monos("fun main() { let r: Result<i64, Stack<i64>> = Ok(1); let n = r.unwrap(); }");
        assert!(found.contains(&"Stack.show<i64>".to_string()), "{found:?}");
    }
}
