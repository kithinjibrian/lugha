//! Globals, scopes and annotations: spec §6 pass 1, `main`, and name lookup.

use std::collections::HashMap;

use super::{Binding, Checker, Local, Signature, Type, errors};
use crate::ast::{Ident, Item, Program, Type as Annotation, TypeKind};

/// Built into the compiler from milestone 4; their names are reserved (spec §6).
pub(super) const INTRINSICS: [&str; 4] = ["print", "println", "panic", "to_string"];

impl Checker {
    /// Collects signatures, checks `main`, then every function body (spec §6).
    pub(super) fn program(&mut self, program: &Program) {
        self.collect_structs(program);
        self.resolve_structs(program);
        self.collect(program);
        self.check_main(program);
        for item in &program.items {
            // A duplicate or reserved definition was reported; skip its body to avoid a cascade.
            if let Item::Fun(f) = item
                && self
                    .functions
                    .get(&f.name.name)
                    .is_some_and(|sig| sig.span == f.name.span)
            {
                self.function(f);
            }
        }
    }

    /// Pass 1: every function signature, in source order.
    fn collect(&mut self, program: &Program) {
        for item in &program.items {
            let (name, params, ret, is_extern) = match item {
                Item::Fun(f) => (&f.name, &f.params, f.ret.as_ref(), false),
                Item::Extern(e) => (&e.name, &e.params, e.ret.as_ref(), true),
                Item::Struct(_) => continue,
            };
            let declared = params;
            let params = params
                .iter()
                .map(|p| self.resolve(&p.ty))
                .collect::<Vec<_>>();
            let mut ret_type = match ret {
                Some(ty) => self.resolve(ty),
                None => Type::Void,
            };
            if is_extern && ret_type == Type::String {
                let span = ret.map_or(name.span, |ty| ty.span);
                self.report(errors::extern_string_return(span));
                // Keep the function callable so its calls don't cascade.
                ret_type = Type::Error;
            }
            if is_extern {
                // Arrays can't cross the C boundary (spec §8).
                let annotations = declared.iter().map(|p| &p.ty).chain(ret);
                for (annotation, ty) in annotations.zip(params.iter().chain([&ret_type])) {
                    match ty {
                        Type::Array(_) => self.report(errors::extern_array(annotation.span)),
                        Type::Struct(_) => self.report(errors::extern_struct(annotation.span)),
                        _ => {}
                    }
                }
            }
            let text = &name.name;
            if is_extern && text.starts_with("lugha_") {
                self.report(errors::reserved_extern(text, name.span));
            } else if INTRINSICS.contains(&text.as_str()) {
                self.report(errors::reserved(text, name.span));
            } else if let Some(first) = self
                .functions
                .get(text)
                .map(|f| f.span)
                .or_else(|| self.structs.get(text).map(|s| s.span))
            {
                self.report(errors::duplicate(text, name.span, first));
            } else {
                let signature = Signature {
                    params,
                    ret: ret_type,
                    span: name.span,
                };
                self.functions.insert(text.clone(), signature);
            }
        }
    }

    /// `main` must exist as `fun main()` or `fun main(): i32` (spec §6).
    fn check_main(&mut self, program: &Program) {
        let main = program.items.iter().find_map(|item| match item {
            Item::Fun(f) if f.name.name == "main" => Some(f),
            _ => None,
        });
        match main {
            None => self.report(errors::missing_main()),
            Some(f) => {
                let bad_return = f.ret.as_ref().is_some_and(|ty| ty.kind != TypeKind::I32);
                if !f.params.is_empty() || bad_return {
                    self.report(errors::bad_main(f.name.span));
                }
            }
        }
    }

    /// The type an annotation names. Unknown names are E0305 and become `Error`.
    pub(super) fn resolve(&mut self, ty: &Annotation) -> Type {
        match &ty.kind {
            TypeKind::I32 => Type::I32,
            TypeKind::I64 => Type::I64,
            TypeKind::U8 => Type::U8,
            TypeKind::F64 => Type::F64,
            TypeKind::Bool => Type::Bool,
            TypeKind::String => Type::String,
            TypeKind::Array(element) => Type::Array(Box::new(self.resolve(element))),
            TypeKind::Named(name) if self.structs.contains_key(name) => Type::Struct(name.clone()),
            TypeKind::Named(name) => {
                self.report(errors::unknown_type(name, ty.span));
                Type::Error
            }
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
        assert_eq!(
            errors("extern fun getenv(k: string): string;\nfun main() {}"),
            [("E0409", "string")]
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
}
