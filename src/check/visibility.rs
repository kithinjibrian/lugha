//! Visibility across modules and packages (v2 draft decision 9, §6;
//! PRP-034; v8 PRP-059): `private` items, methods and fields are usable only
//! in their own module, those without a keyword only in their own package,
//! and `public` ones everywhere (E0313). A declaration may not expose a type
//! less visible than itself (E0318).

use std::collections::HashMap;

use super::levels::{Level, Owner, level};
use super::{Checker, errors};
use crate::ast::{FunDecl, Item, Program, Type as Annotation, TypeParam, Visibility};
use crate::span::Span;

/// Who may use what: each item's owner and level, and the levels of methods
/// and fields (their owner is their type's).
#[derive(Default)]
pub(super) struct Privacy {
    /// Functions, externs, structs, enums and interfaces by key.
    pub(super) items: HashMap<String, Owner>,
    /// Methods and associated functions by (type key, name).
    methods: HashMap<(String, String), Level>,
    /// Fields by (struct key, name).
    fields: HashMap<(String, String), Level>,
}

impl Checker {
    /// Records that the current module declares item `key`.
    pub(super) fn register_item(&mut self, key: &str, v: Option<Visibility>) {
        let owner = self.owner_here(v);
        self.privacy.items.insert(key.to_string(), owner);
    }

    pub(super) fn register_method(&mut self, owner: &str, name: &str, v: Option<Visibility>) {
        let key = (owner.to_string(), name.to_string());
        self.privacy.methods.insert(key, self.owner_here(v).level);
    }

    pub(super) fn register_field(&mut self, owner: &str, name: &str, v: Option<Visibility>) {
        let key = (owner.to_string(), name.to_string());
        self.privacy.fields.insert(key, self.owner_here(v).level);
    }

    /// E0313 if item `key` may not be used here.
    pub(super) fn item_visible(&mut self, key: &str, span: Span) {
        let Some(owner) = self.privacy.items.get(key).cloned() else {
            return;
        };
        let name = key.rsplit('.').next().unwrap_or(key);
        if let Some(d) = self.denied(name, &owner, owner.level, span, None) {
            self.report(d);
        }
    }

    /// E0313 for a method or field `name` of type `owner` at `level`, if it
    /// may not be used here.
    fn member_visible(&mut self, owner: &str, name: &str, level: Option<Level>, span: Span) {
        let (Some(level), Some(type_owner)) = (level, self.privacy.items.get(owner).cloned())
        else {
            return;
        };
        if let Some(d) = self.denied(name, &type_owner, level, span, None) {
            self.report(d);
        }
    }

    /// E0313 if method `name` of type `owner` may not be used here.
    pub(super) fn method_visible(&mut self, owner: &str, name: &str, span: Span) {
        let level = self
            .privacy
            .methods
            .get(&(owner.to_string(), name.to_string()));
        self.member_visible(owner, name, level.copied(), span);
    }

    /// True if method `name` of type `owner` may be used here (milestone 12:
    /// only such methods satisfy an interface).
    pub(super) fn method_usable(&self, owner: &str, name: &str) -> bool {
        let level = self
            .privacy
            .methods
            .get(&(owner.to_string(), name.to_string()));
        match (level, self.privacy.items.get(owner)) {
            (Some(level), Some(type_owner)) => self
                .denied(name, type_owner, *level, Span::new(0, 0), None)
                .is_none(),
            _ => true,
        }
    }

    /// E0313 if field `name` of struct `owner` may not be used here.
    pub(super) fn field_visible(&mut self, owner: &str, name: &str, span: Span) {
        let level = self
            .privacy
            .fields
            .get(&(owner.to_string(), name.to_string()));
        self.member_visible(owner, name, level.copied(), span);
    }

    /// E0313 for a literal of a struct with a field this module can't see:
    /// it must be built through one of the struct's functions.
    pub(super) fn literal_visible(&mut self, owner: &str, span: Span) {
        let Some(type_owner) = self.privacy.items.get(owner).cloned() else {
            return;
        };
        let name = owner.rsplit('.').next().unwrap_or(owner).to_string();
        // A type this module can't see is reported already (`item_visible`).
        let here = Span::new(0, 0);
        if self
            .denied(&name, &type_owner, type_owner.level, here, None)
            .is_some()
        {
            return;
        }
        let mut fields: Vec<(&String, &Level)> = self
            .privacy
            .fields
            .iter()
            .filter(|((s, _), _)| s == owner)
            .map(|((_, f), level)| (f, level))
            .collect();
        fields.sort();
        let hidden = fields.into_iter().find_map(|(field, level)| {
            let help = format!(
                "a `{name}` literal sets `{field}`: build it with one of `{name}`'s functions"
            );
            self.denied(field, &type_owner, *level, span, Some(help))
        });
        if let Some(d) = hidden {
            self.report(d);
        }
    }

    /// E0318 for every declaration that exposes a less visible type.
    pub(super) fn visibility_rules(&mut self, program: &Program) {
        for item in &program.items {
            match item {
                Item::Fun(f) => self.signature(f),
                // A handle holds nothing that could be less visible (v16).
                Item::ExternType(_) => {}
                // A constant's type is no more visible than the constant (v17).
                Item::Const(c) => self.leak(&c.ty, level(c.visibility)),
                Item::Extern(e) => {
                    let at = level(e.visibility);
                    e.params.iter().for_each(|p| self.leak(&p.ty, at));
                    e.ret.iter().for_each(|t| self.leak(t, at));
                }
                Item::Struct(s) => {
                    for field in &s.fields {
                        // A field is no more visible than its struct.
                        let at = level(s.visibility).min(level(field.visibility));
                        self.leak(&field.ty, at);
                    }
                }
                Item::Enum(e) => {
                    let at = level(e.visibility);
                    let payloads = e.variants.iter().flat_map(|v| &v.payloads);
                    payloads.for_each(|t| self.leak(t, at));
                }
                Item::Impl(i) => i.methods.iter().for_each(|m| self.signature(m)),
                // A derive block declares no name (v17, milestone 141).
                Item::Derive(_) => {}
                Item::Interface(i) => {
                    let at = level(i.visibility);
                    let types = i
                        .items
                        .iter()
                        .flat_map(|it| it.params.iter().map(|p| &p.ty).chain(&it.ret));
                    types.for_each(|t| self.leak(t, at));
                }
            }
        }
    }

    fn signature(&mut self, f: &FunDecl) {
        let at = level(f.visibility);
        f.params.iter().for_each(|p| self.leak(&p.ty, at));
        f.ret.iter().for_each(|t| self.leak(t, at));
        self.bound_leaks(&f.type_params, at);
    }

    /// E0318 if the written type `ty`, in a declaration visible at `at`,
    /// mentions a less visible type.
    fn leak(&mut self, ty: &Annotation, at: Level) {
        if at == Level::Private {
            return;
        }
        let Some(resolved) = self.annotations.get(&ty.span).cloned() else {
            return;
        };
        if let Some(key) = self.hidden_inside(&resolved, at) {
            let declared = self.type_span(&key);
            self.report(errors::leaked_type(
                &key,
                at == Level::Public,
                ty.span,
                declared,
            ));
        }
    }

    /// E0318 for a `public` function's bound naming an interface that isn't
    /// `public` (v8).
    fn bound_leaks(&mut self, params: &[TypeParam], at: Level) {
        if at != Level::Public {
            return;
        }
        for b in params.iter().flat_map(|p| &p.bounds) {
            let Some(key) = self.interface_key(b) else {
                continue;
            };
            if self.privacy.items.get(&key).is_some_and(|o| o.level < at) {
                let declared = self.type_span(&key);
                self.report(errors::leaked_type(&key, true, b.span, declared));
            }
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
    fn public_is_allowed_in_v8() {
        ok(&[(
            "main.la",
            "public fun f() {}\npublic struct S { public x: i64 }\nimpl S { public fun g(self) {} }\nfun main() {}\n",
        )]);
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
