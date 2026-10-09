//! Globals, scopes and annotations: spec §6 pass 1, `main`, and name lookup.

use std::collections::HashMap;

use super::{Binding, Checker, Checking, Local, Signature, Type, errors, stop};
use crate::ast::{Ident, Item, Program, Type as Annotation, TypeKind};

/// Built into the compiler from milestone 4; their names are reserved (spec §6).
pub(super) const INTRINSICS: [&str; 4] = ["print", "println", "panic", "to_string"];

impl Checker {
    /// Collects signatures, checks `main`, then every function body (spec §6).
    pub(super) fn program(&mut self, program: &Program) -> Checking<()> {
        self.collect(program)?;
        self.check_main(program);
        for item in &program.items {
            // A duplicate or reserved definition was reported; skip its body to avoid a cascade.
            if let Item::Fun(f) = item
                && self
                    .functions
                    .get(&f.name.name)
                    .is_some_and(|sig| sig.span == f.name.span)
            {
                self.function(f)?;
            }
        }
        Ok(())
    }

    /// Pass 1: every function signature, in source order.
    fn collect(&mut self, program: &Program) -> Checking<()> {
        for item in &program.items {
            let f = match item {
                Item::Fun(f) => f,
                Item::Extern(e) => return Err(stop("extern functions", 4, e.span)),
                Item::Struct(s) => return Err(stop("structs", 5, s.span)),
            };
            let params = f
                .params
                .iter()
                .map(|p| self.resolve(&p.ty))
                .collect::<Checking<Vec<_>>>()?;
            let ret = match &f.ret {
                Some(ty) => self.resolve(ty)?,
                None => Type::Void,
            };
            let name = &f.name.name;
            if INTRINSICS.contains(&name.as_str()) {
                self.report(errors::reserved(name, f.name.span));
            } else if let Some(first) = self.functions.get(name) {
                let first = first.span;
                self.report(errors::duplicate(name, f.name.span, first));
            } else {
                self.functions.insert(
                    name.clone(),
                    Signature {
                        params,
                        ret,
                        span: f.name.span,
                    },
                );
            }
        }
        Ok(())
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
    pub(super) fn resolve(&mut self, ty: &Annotation) -> Checking<Type> {
        Ok(match &ty.kind {
            TypeKind::I32 => Type::I32,
            TypeKind::I64 => Type::I64,
            TypeKind::U8 => Type::U8,
            TypeKind::F64 => Type::F64,
            TypeKind::Bool => Type::Bool,
            TypeKind::String => Type::String,
            TypeKind::Array(_) => return Err(stop("arrays", 5, ty.span)),
            TypeKind::Named(name) => {
                self.report(errors::unknown_type(name, ty.span));
                Type::Error
            }
        })
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
            .find_map(|scope| scope.get(name).copied())
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok, stopped};

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
    fn later_milestone_items_stop_the_checker() {
        assert_eq!(
            stopped("extern fun abs(x: i32): i32;\nfun main() {}"),
            ("extern functions", 4, "extern fun abs(x: i32): i32;")
        );
        assert_eq!(
            stopped("struct P { x: i64 }\nfun main() {}"),
            ("structs", 5, "struct P { x: i64 }")
        );
        assert_eq!(
            errors("fun main() { let s: string = 1; }"),
            [("E0401", "1")]
        );
        assert_eq!(
            stopped("fun main() { let a: i64[] = 1; }"),
            ("arrays", 5, "i64[]")
        );
    }
}
