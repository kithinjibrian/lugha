//! Visibility across modules (v2 draft decision 9, §6; PRP-034): `private`
//! items, methods and fields are usable only in their own module (E0313),
//! `public` waits for packages (E0317), and a signature other modules can
//! use may not mention a `private` type (E0318).

use std::collections::HashMap;

use super::{Checker, Type, errors};
use crate::ast::{FunDecl, Item, Program, Type as Annotation, Visibility, VisibilityKind};
use crate::span::Span;

/// Who may use what: each item's owning module and whether it is `private`.
#[derive(Default)]
pub(super) struct Privacy {
    /// Functions, externs, structs and enums by key.
    items: HashMap<String, (Vec<String>, bool)>,
    /// Methods and associated functions by (type key, name).
    methods: HashMap<(String, String), bool>,
    /// Fields by (struct key, name).
    fields: HashMap<(String, String), bool>,
}

/// True for `private`.
pub(super) fn is_private(v: Option<Visibility>) -> bool {
    v.is_some_and(|v| v.kind == VisibilityKind::Private)
}

impl Checker {
    /// Records that the current module declares item `key`.
    pub(super) fn register_item(&mut self, key: &str, v: Option<Visibility>) {
        let entry = (self.module.clone(), is_private(v));
        self.privacy.items.insert(key.to_string(), entry);
    }

    pub(super) fn register_method(&mut self, owner: &str, name: &str, v: Option<Visibility>) {
        let key = (owner.to_string(), name.to_string());
        self.privacy.methods.insert(key, is_private(v));
    }

    pub(super) fn register_field(&mut self, owner: &str, name: &str, v: Option<Visibility>) {
        let key = (owner.to_string(), name.to_string());
        self.privacy.fields.insert(key, is_private(v));
    }

    /// The module that declares item `key`, if another module does.
    fn foreign(&self, key: &str) -> Option<Vec<String>> {
        let (owner, _) = self.privacy.items.get(key)?;
        (*owner != self.module).then(|| owner.clone())
    }

    /// E0313 if item `key` is another module's `private` item.
    pub(super) fn item_visible(&mut self, key: &str, span: Span) {
        let private = self.privacy.items.get(key).is_some_and(|(_, p)| *p);
        if let (true, Some(owner)) = (private, self.foreign(key)) {
            let name = key.rsplit('.').next().unwrap_or(key);
            self.report(errors::private_item(name, &owner, span, None));
        }
    }

    /// E0313 if method `name` of type `owner` is `private` to another module.
    pub(super) fn method_visible(&mut self, owner: &str, name: &str, span: Span) {
        let key = (owner.to_string(), name.to_string());
        if self.privacy.methods.get(&key) == Some(&true)
            && let Some(module) = self.foreign(owner)
        {
            self.report(errors::private_item(name, &module, span, None));
        }
    }

    /// True if method `name` of type `owner` may be used here (milestone 12:
    /// only such methods satisfy an interface).
    pub(super) fn method_usable(&self, owner: &str, name: &str) -> bool {
        let key = (owner.to_string(), name.to_string());
        self.privacy.methods.get(&key) != Some(&true) || self.foreign(owner).is_none()
    }

    /// E0313 if field `name` of struct `owner` is `private` to another module.
    pub(super) fn field_visible(&mut self, owner: &str, name: &str, span: Span) {
        let key = (owner.to_string(), name.to_string());
        if self.privacy.fields.get(&key) == Some(&true)
            && let Some(module) = self.foreign(owner)
        {
            self.report(errors::private_item(name, &module, span, None));
        }
    }

    /// E0313 for a literal of another module's struct with a `private` field:
    /// it must be built through one of the struct's functions.
    pub(super) fn literal_visible(&mut self, owner: &str, span: Span) {
        let Some(module) = self.foreign(owner) else {
            return;
        };
        let hidden = self
            .privacy
            .fields
            .iter()
            .find(|((s, _), private)| s == owner && **private)
            .map(|((_, f), _)| f.clone());
        if let Some(field) = hidden {
            let name = owner.rsplit('.').next().unwrap_or(owner);
            let help = format!("`{field}` is private: build it with one of `{name}`'s functions");
            self.report(errors::private_item(name, &module, span, Some(help)));
        }
    }

    /// E0317 for every `public` (v3 packages), then E0318 for every
    /// signature other modules can use that mentions a `private` type.
    pub(super) fn visibility_rules(&mut self, program: &Program) {
        for item in &program.items {
            match item {
                Item::Fun(f) => self.signature(f),
                Item::Extern(e) => {
                    self.public_keyword(e.visibility);
                    if !is_private(e.visibility) {
                        e.params.iter().for_each(|p| self.leak(&p.ty));
                        e.ret.iter().for_each(|t| self.leak(t));
                    }
                }
                Item::Struct(s) => {
                    self.public_keyword(s.visibility);
                    for field in &s.fields {
                        self.public_keyword(field.visibility);
                        // A private struct's fields are private already.
                        if !is_private(s.visibility) && !is_private(field.visibility) {
                            self.leak(&field.ty);
                        }
                    }
                }
                Item::Enum(e) => {
                    self.public_keyword(e.visibility);
                    if !is_private(e.visibility) {
                        e.variants
                            .iter()
                            .flat_map(|v| &v.payloads)
                            .for_each(|t| self.leak(t));
                    }
                }
                Item::Impl(i) => i.methods.iter().for_each(|m| self.signature(m)),
                Item::Interface(i) => {
                    self.public_keyword(i.visibility);
                    if !is_private(i.visibility) {
                        let types = i
                            .items
                            .iter()
                            .flat_map(|it| it.params.iter().map(|p| &p.ty).chain(&it.ret));
                        types.for_each(|t| self.leak(t));
                    }
                }
            }
        }
    }

    fn signature(&mut self, f: &FunDecl) {
        self.public_keyword(f.visibility);
        if !is_private(f.visibility) {
            f.params.iter().for_each(|p| self.leak(&p.ty));
            f.ret.iter().for_each(|t| self.leak(t));
        }
    }

    fn public_keyword(&mut self, v: Option<Visibility>) {
        if let Some(v) = v
            && v.kind == VisibilityKind::Public
        {
            self.report(errors::public_reserved(v.span));
        }
    }

    /// E0318 if the written type `ty` mentions a `private` type.
    fn leak(&mut self, ty: &Annotation) {
        let Some(resolved) = self.annotations.get(&ty.span).cloned() else {
            return;
        };
        if let Some(key) = self.private_inside(&resolved) {
            let declared = self.type_span(&key);
            self.report(errors::leaked_private(&key, ty.span, declared));
        }
    }

    /// The first `private` struct or enum `ty` mentions, looking inside
    /// arrays and `Option`.
    fn private_inside(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Struct(k, _) | Type::Enum(k, _) => self
                .privacy
                .items
                .get(k)
                .is_some_and(|(_, private)| *private)
                .then(|| k.clone()),
            Type::Array(inner) | Type::Option(inner) => self.private_inside(inner),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::modules::test_util::{errors, ok};

    const RANGE: &str = "struct Range { private lo: i64, private hi: i64 }\n\
        impl Range {\n\
            fun new(lo: i64, hi: i64): Range = if lo <= hi { Range { lo: lo, hi: hi } } else { Range { lo: hi, hi: lo } };\n\
            fun len(self): i64 = self.hi - self.lo;\n\
            private fun mid(self): i64 = self.lo + self.len() / 2;\n\
            private fun zero(): Range = Range { lo: 0, hi: 0 };\n\
        }\n\
        private fun helper(): i64 = Range.new(1, 2).mid();\n\
        private struct Secret { n: i64 }\n";

    fn run(main_body: &str) -> Vec<(&'static str, String)> {
        let main = format!("import range;\nfun main() {{ {main_body} }}\n");
        errors(&[("main.la", main.as_str()), ("range.la", RANGE)])
    }

    #[test]
    fn private_things_are_usable_in_their_own_module() {
        let main =
            "import range;\nfun main() { let r = range.Range.new(5, 1); let n = r.len(); }\n";
        ok(&[("main.la", main), ("range.la", RANGE)]);
    }

    #[test]
    fn private_things_are_e0313_elsewhere() {
        assert_eq!(
            run("let n = range.helper();"),
            [("E0313", "helper".to_string())]
        );
        assert_eq!(
            run("let r = range.Range.zero();"),
            [("E0313", "zero".to_string())]
        );
        assert_eq!(
            run("let r = range.Range.new(1, 2); let m = r.mid();"),
            [("E0313", "mid".to_string())]
        );
        assert_eq!(
            run("let r = range.Range.new(1, 2); let a = r.lo;"),
            [("E0313", "lo".to_string())]
        );
        assert_eq!(
            run("let mut r = range.Range.new(1, 2); r.hi = 0;"),
            [("E0313", "hi".to_string())]
        );
        assert_eq!(
            run("let mut r = range.Range.new(1, 2); let mut x = 3; swap(r.lo, x);"),
            [("E0313", "lo".to_string())]
        );
        assert_eq!(
            run("let r = range.Range { lo: 1, hi: 2 };"),
            [("E0313", "range.Range { lo: 1, hi: 2 }".to_string())]
        );
        assert_eq!(
            run("let s = range.Secret { n: 1 };"),
            [("E0313", "range.Secret { n: 1 }".to_string())]
        );
    }

    #[test]
    fn public_waits_for_packages() {
        assert_eq!(
            errors(&[(
                "main.la",
                "public fun f() {}\nstruct S { public x: i64 }\nimpl S { public fun g(self) {} }\nfun main() {}\n"
            )]),
            [
                ("E0317", "public".to_string()),
                ("E0317", "public".to_string()),
                ("E0317", "public".to_string())
            ]
        );
    }

    #[test]
    fn private_types_do_not_leak() {
        let src = "private struct S { n: i64 }\n\
            fun make(): S = S { n: 1 };\n\
            struct Box2 { inner: S[] }\n\
            enum E { A(Option<S>) }\n\
            private fun fine(s: S): S = s;\n\
            fun main() {}\n";
        assert_eq!(
            errors(&[("main.la", src)]),
            [
                ("E0318", "S".to_string()),
                ("E0318", "S[]".to_string()),
                ("E0318", "Option<S>".to_string())
            ]
        );
    }
}
