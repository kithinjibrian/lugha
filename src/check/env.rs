//! Globals, scopes and annotations: spec §6 pass 1, `main`, and name lookup.

use std::collections::HashMap;

use super::{Binding, Checker, Local, Signature, Type, errors};
use crate::ast::{Ident, Item, Program, Type as Annotation, TypeKind};
use crate::span::Span;

/// Built into the compiler from milestone 4; their names are reserved (spec §6).
pub(super) const INTRINSICS: [&str; 5] = ["print", "println", "panic", "to_string", "swap"];

/// True for every intrinsic name, the v3 input intrinsics (`input.rs`) included.
pub(super) fn is_intrinsic(name: &str) -> bool {
    INTRINSICS.contains(&name)
        || super::input::INPUT.contains(&name)
        || super::test_fns::ASSERTIONS.contains(&name)
}

/// True if `symbol` is a C identifier lughac may call: not one of its own (`lugha_…`).
fn c_name(symbol: &str) -> bool {
    let mut chars = symbol.chars();
    chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        && !symbol.starts_with("lugha_")
}

impl Checker {
    /// Every function and method body of the current module, once the
    /// passes of spec §6 have run over every module (`modules.rs`).
    pub(super) fn bodies(&mut self, program: &Program) {
        for item in &program.items {
            // A duplicate or reserved definition was reported; skip its body to avoid a cascade.
            if let Item::Fun(f) = item
                && self
                    .functions
                    .get(&self.key(&f.name.name))
                    .is_some_and(|sig| sig.span == f.name.span)
            {
                self.function(f);
            }
        }
        self.method_bodies(program);
    }

    /// Pass 1: every function signature, in source order.
    pub(super) fn collect(&mut self, program: &Program) {
        for item in &program.items {
            let (name, params, ret, is_extern, visibility) = match item {
                Item::Fun(f) => (&f.name, &f.params, f.ret.as_ref(), false, f.visibility),
                Item::Extern(e) => (&e.name, &e.params, e.ret.as_ref(), true, e.visibility),
                Item::Struct(_)
                | Item::Enum(_)
                | Item::Impl(_)
                | Item::Interface(_)
                | Item::ExternType(_) => continue,
            };
            let generics = self.enter_params(item);
            // An extern's callers write neither `data` nor `release`, and see
            // each callback as a plain function type (v16, milestone 88).
            let (declared, params) = match item {
                Item::Extern(e) => self.extern_params(e, &self.key(&name.name)),
                _ => {
                    let types = params.iter().map(|p| self.resolve(&p.ty)).collect();
                    (params.clone(), types)
                }
            };
            let declared = &declared;
            // A `string` result is C's text, copied (v16, milestone 89).
            let ret_type = match ret {
                Some(ty) => self.resolve(ty),
                None => Type::Unit,
            };
            if is_extern {
                // Only numbers and strings cross the C boundary (spec §8);
                // handles and callbacks too (v16).
                let annotations = declared.iter().map(|p| &p.ty).chain(ret);
                let count = params.len();
                for (i, (annotation, ty)) in annotations
                    .zip(params.iter().chain([&ret_type]))
                    .enumerate()
                {
                    let at = annotation.span;
                    // Pointer modes, arrays and unions (v17, milestones 102–104).
                    let mode = declared.get(i).and_then(|p| p.mode).map(|(m, _)| m);
                    if let Some(verdict) = self.pointer_param(ty, mode, i < count, at) {
                        if let Some(d) = verdict {
                            self.report(d);
                        }
                        continue;
                    }
                    let d = match ty {
                        // A written callback, checked by `extern_params`.
                        Type::Fun(..) if i < count => continue,
                        // A handle, or `Option` of one (`NULL` is `None`), is a C pointer (v16).
                        Type::Handle(_) => continue,
                        // `Option<string>` too: `NULL` is `None` (v16, milestone 89).
                        Type::Option(inner)
                            if matches!(**inner, Type::Handle(_) | Type::String) =>
                        {
                            continue;
                        }
                        // An `extern struct` goes by value, as C passes it (v16, milestone 90).
                        Type::Struct(key, _) if self.c_structs.contains(key) => continue,
                        Type::Array(_) => errors::extern_array(at),
                        Type::Struct(_, _) => errors::extern_struct(at),
                        Type::Fun(..) | Type::Interface(..) => errors::extern_value(ty, at),
                        // `()` has no C form; an extern without a result is C's `void` (v17).
                        Type::Unit if i < count => errors::extern_value(ty, at),
                        Type::Enum(_, _) | Type::Option(_) => errors::extern_enum(at),
                        _ => continue,
                    };
                    self.report(d);
                }
            }
            self.type_params.clear();
            let text = &name.name;
            let key = self.key(text);
            let renamed = match item {
                Item::Extern(e) => e.symbol.as_ref(),
                _ => None,
            };
            if let Some(symbol) = renamed.filter(|s| !c_name(&s.name)) {
                self.report(errors::bad_symbol(&symbol.name, symbol.span));
            }
            if is_extern && renamed.is_none() && text.starts_with("lugha_") {
                self.report(errors::reserved_extern(text, name.span));
            } else if is_intrinsic(text) {
                self.report(errors::reserved(text, name.span));
            } else if super::enums::BUILTIN_NAMES.contains(&text.as_str()) {
                self.report(errors::builtin_name(text, name.span));
            } else if let Some(first) = self
                .functions
                .get(&key)
                .map(|f| f.span)
                .or_else(|| self.type_span(&key))
            {
                self.report(errors::duplicate(text, name.span, first));
            } else {
                // A renamed extern may give its C function another signature (v16).
                if is_extern && renamed.is_none() {
                    self.same_extern(text, &params, &ret_type, name.span);
                }
                self.register_item(&key, visibility);
                let sig = super::Sig::of(&generics, 0, None, (declared, &params), &ret_type);
                self.hover.signatures.insert(name.span, sig);
                let signature = Signature {
                    generics,
                    params,
                    ret: ret_type,
                    span: name.span,
                    is_extern,
                    test: matches!(item, Item::Fun(f) if f.test.is_some()),
                };
                self.functions.insert(key, signature);
                self.hover.function(name.span, name.span, None);
            }
        }
    }

    /// `main` must exist as `fun main()` or `fun main(): i32` (spec §6).
    pub(super) fn check_main(&mut self, program: &Program) {
        let main = program.items.iter().find_map(|item| match item {
            Item::Fun(f) if f.name.name == "main" => Some(f),
            _ => None,
        });
        match main {
            None => self.report(errors::missing_main()),
            Some(f) => {
                let bad_return = f.ret.as_ref().is_some_and(|ty| ty.kind != TypeKind::I32);
                // A generic `main` has no one instance to call (v2 draft §9).
                if !f.params.is_empty() || !f.type_params.is_empty() || bad_return {
                    self.report(errors::bad_main(f.name.span));
                }
            }
        }
    }

    /// The type an annotation names. Unknown names are E0305 and become
    /// `Error`. Every result is recorded for codegen (`Checked::annotations`).
    pub(super) fn resolve(&mut self, ty: &Annotation) -> Type {
        let resolved = self.resolve_here(ty);
        self.annotations.insert(ty.span, resolved.clone());
        self.body_type(&resolved);
        resolved
    }

    fn resolve_here(&mut self, ty: &Annotation) -> Type {
        match &ty.kind {
            TypeKind::Array(element) => Type::Array(Box::new(self.resolve(element))),
            TypeKind::Option(inner) => Type::Option(Box::new(self.resolve(inner))),
            TypeKind::Named(name) => self.resolve_named(name, ty.span),
            // `m.T`, `T<args>`, `m.T<args>` (PRP-034, PRP-035).
            TypeKind::Applied(applied) => {
                let resolved = self.resolve_applied(applied, ty.span);
                // `Map<f64, V>` or `Set<f64>` written anywhere is E0441 (v17).
                self.float_keys(&resolved, ty.span);
                resolved
            }
            TypeKind::Fun(params, ret) => self.resolve_fun(params, ret.as_deref()),
            TypeKind::Tuple(written) => {
                let types = written.iter().map(|t| self.resolve(t)).collect();
                self.tuple_type(types)
            }
            primitive => Type::primitive(primitive).expect("every other kind is a keyword type"),
        }
    }

    pub(super) fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(super) fn pop(&mut self) {
        self.scopes.pop();
    }

    /// Declares a local in the innermost scope, shadowing earlier bindings.
    pub(super) fn declare(&mut self, name: &Ident, ty: Type, binding: Binding) {
        self.check_local_name(name);
        let mutable = matches!(binding, Binding::Let { mutable: true } | Binding::SelfMut);
        self.hover.local(name, &ty, mutable);
        let scope = self
            .scopes
            .last_mut()
            .expect("locals are declared inside a scope");
        scope.insert(
            name.name.clone(),
            Local {
                ty,
                binding,
                span: name.span,
            },
        );
    }

    /// Where the struct or enum with key `name` is declared.
    pub(super) fn type_span(&self, name: &str) -> Option<Span> {
        let as_struct = self.structs.get(name).map(|s| s.span);
        let as_enum = || self.enums.get(name).map(|e| e.span);
        as_struct
            .or_else(as_enum)
            .or_else(|| self.interfaces.get(name).map(|i| i.span))
            .or_else(|| self.handles.get(name).copied())
    }

    /// True if `name` names a type here, or an imported module, so no local
    /// may use it (E0311).
    pub(super) fn is_type_name(&self, name: &str) -> bool {
        let key = self.key(name);
        matches!(name, "Option" | "Result")
            || self.structs.contains_key(&key)
            || self.enums.contains_key(&key)
            || self.interfaces.contains_key(&key)
            || self.handles.contains_key(&key)
            || self.imports.contains_key(name)
    }

    /// Reports E0311 for a local named like a type or a module.
    pub(super) fn check_local_name(&mut self, name: &Ident) {
        if self.is_type_name(&name.name) {
            let module = self.imports.contains_key(&name.name);
            self.report(errors::local_named_like_type(&name.name, name.span, module));
        }
    }

    /// The innermost local named `name`.
    pub(super) fn local(&self, name: &str) -> Option<Local> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    #[test]
    fn duplicates_and_reserved_names_are_e0302() {
        assert_eq!(
            errors("fun f() {}\nfun f() {}\nfun main() {}"),
            [("E0302", "f")]
        );
        assert_eq!(
            errors("fun println() {}\nfun main() {}"),
            [("E0302", "println")]
        );
    }

    #[test]
    fn main_must_exist_with_the_right_signature() {
        assert_eq!(errors("fun helper() {}"), [("E0303", "")]);
        assert_eq!(errors("fun main(x: i64) {}"), [("E0304", "main")]);
        assert_eq!(errors("fun main(): i64 { 1 }"), [("E0304", "main")]);
        ok("fun main(): i32 { 0 }");
    }

    #[test]
    fn unknown_types_are_e0305() {
        assert_eq!(
            errors("fun main() { let p: Pointt = 1; }"),
            [("E0305", "Pointt")]
        );
        assert_eq!(errors("fun f(p: Q) {}\nfun main() {}"), [("E0305", "Q")]);
    }

    #[test]
    fn extern_declarations_follow_spec_section_8() {
        ok(
            "extern fun puts(s: string): i32;\nextern fun abs(x: i32): i32;\nfun main() { puts(\"hi\"); let a = abs(-3); }",
        );
        assert_eq!(
            errors("extern fun lugha_rt_alloc(n: i64): i64;\nfun main() {}"),
            [("E0306", "lugha_rt_alloc")]
        );
        // v16: C's text comes back as a copy.
        ok(
            "extern fun getenv(k: string): Option<string>;\nfun main() { println(getenv(\"HOME\")); }",
        );
        assert_eq!(
            errors("extern fun f(): i64[];\nfun main() {}"),
            [("E0409", "i64[]")]
        );
        assert_eq!(
            errors("extern fun print(x: i32);\nfun main() {}"),
            [("E0302", "print")]
        );
        assert_eq!(
            errors("extern fun f(x: i32);\nfun f() {}\nfun main() {}"),
            [("E0302", "f")]
        );
        assert_eq!(
            errors("extern fun abs(x: i32): i32;\nfun main() { abs(); }"),
            [("E0405", "abs()")]
        );
        assert_eq!(
            errors("extern fun abs(x: i32): i32;\nfun main() { abs(true); }"),
            [("E0403", "true")]
        );
        assert_eq!(
            errors("extern fun f(p: Point);\nfun main() {}"),
            [("E0305", "Point")]
        );
    }

    #[test]
    fn locals_may_not_be_named_like_types() {
        assert_eq!(
            errors("enum Shape { Circle(f64), Empty }\nfun main() { let Shape = 3; }"),
            [("E0311", "Shape")]
        );
        assert_eq!(
            errors("enum Shape { Circle(f64), Empty }\nfun f(Shape: i64) {}\nfun main() {}"),
            [("E0311", "Shape")]
        );
        assert_eq!(
            errors("struct P { x: i64 }\nfun main() { for P in 0..2 { } }"),
            [("E0311", "P")]
        );
        assert_eq!(
            errors("fun main() { let Option = 1; }"),
            [("E0311", "Option")]
        );
    }
}
