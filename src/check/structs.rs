//! Structs (spec §4, §6): declarations collected in pass 1, field types
//! resolved in pass 2, self-containment (E0307), and struct literals.

use std::collections::HashSet;

use super::env::is_intrinsic;
use super::generics::Generic;
use super::{Checker, Type, errors};
use crate::ast::{Item, Program};
use crate::span::Span;

/// A declared struct.
pub(super) struct StructInfo {
    /// Fields in declaration order; empty until pass 2 resolves them.
    pub fields: Vec<(String, Type)>,
    /// The struct's name in its declaration.
    pub span: Span,
    /// Type parameters, `struct Stack<T>` (v2 draft §4).
    pub generics: Vec<Generic>,
}

impl Checker {
    /// Pass 1: struct names, so any annotation can name any struct (spec §6).
    pub(super) fn collect_structs(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Struct(s) = item else { continue };
            let name = &s.name.name;
            let key = self.key(name);
            if is_intrinsic(name) {
                self.report(errors::reserved(name, s.name.span));
            } else if super::enums::BUILTIN_NAMES.contains(&name.as_str()) {
                self.report(errors::builtin_name(name, s.name.span));
            } else if let Some(first) = self.type_span(&key) {
                self.report(errors::duplicate(name, s.name.span, first));
            } else {
                self.register_item(&key, s.visibility);
                for field in &s.fields {
                    self.register_field(&key, &field.name.name, field.visibility);
                }
                let info = StructInfo {
                    fields: Vec::new(),
                    span: s.name.span,
                    generics: self.generics(&s.type_params),
                };
                self.structs.insert(key, info);
            }
        }
    }

    /// Pass 2: every field's type (E0305, E0308), then structs that contain
    /// themselves (E0307).
    pub(super) fn resolve_structs(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Struct(s) = item else { continue };
            let key = self.key(&s.name.name);
            if self
                .structs
                .get(&key)
                .is_none_or(|info| info.span != s.name.span)
            {
                continue; // a duplicate, already reported
            }
            // Again now every type is known: bounds' arguments may name them (v6).
            let generics = self.generics(&s.type_params);
            self.structs.get_mut(&key).expect("collected").generics = generics.clone();
            self.type_params = generics;
            self.check_params(&s.type_params, &[]);
            let mut fields: Vec<(String, Type)> = Vec::new();
            for (i, field) in s.fields.iter().enumerate() {
                let ty = self.resolve(&field.ty);
                if let Some(first) = s.fields[..i]
                    .iter()
                    .find(|f| f.name.name == field.name.name)
                {
                    self.report(errors::duplicate_field(
                        &field.name.name,
                        &s.name.name,
                        field.name.span,
                        first.name.span,
                    ));
                    continue;
                }
                self.hover.field(&field.name, &ty);
                fields.push((field.name.name.clone(), ty));
            }
            self.structs
                .get_mut(&key)
                .expect("collected in pass 1")
                .fields = fields;
        }
        self.type_params.clear();
        let mut reported = HashSet::new();
        for item in &program.items {
            let Item::Struct(s) = item else { continue };
            let start = &self.key(&s.name.name);
            if reported.contains(start)
                || self
                    .structs
                    .get(start)
                    .is_none_or(|i| i.span != s.name.span)
            {
                continue;
            }
            let mut path = vec![start.clone()];
            if self.cycle(start, &mut path, &mut HashSet::new()) {
                path.push(start.clone());
                self.report(errors::recursive_struct(
                    start,
                    &path.join(" -> "),
                    s.name.span,
                ));
                reported.extend(path);
            }
        }
    }

    /// Extends `path` until it returns to `path[0]` through fields held inline
    /// (an array is a pointer, so it ends the search).
    fn cycle(&self, start: &str, path: &mut Vec<String>, seen: &mut HashSet<String>) -> bool {
        let current = path.last().expect("path starts at the struct").clone();
        let Some(info) = self.structs.get(&current) else {
            return false;
        };
        for (_, ty) in &info.fields {
            let Type::Struct(next, _) = ty else { continue };
            if next == start {
                return true;
            }
            if seen.insert(next.clone()) {
                path.push(next.clone());
                if self.cycle(start, path, seen) {
                    return true;
                }
                path.pop();
            }
        }
        false
    }

    /// The type of field `name` of struct `s`.
    pub(super) fn struct_field(&self, s: &str, name: &str) -> Option<Type> {
        let info = self.structs.get(s)?;
        info.fields
            .iter()
            .find(|(f, _)| f == name)
            .map(|(_, ty)| ty.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

    const POINT: &str = "struct P { x: f64, y: f64 }\n";

    #[test]
    fn structs_resolve_in_any_order() {
        let src = "fun main() { let r = R { p: P { y: 2.0, x: 1.0 } }; let x = r.p.x; }\n\
                   struct R { p: P }\nstruct P { x: f64, y: f64 }";
        assert_eq!(let_type(src, "r"), Type::Struct("R".into(), Vec::new()));
        assert_eq!(let_type(src, "x"), Type::F64);
        ok(
            "struct Node { value: i64, kids: Node[] }\nfun main() { let n = Node { value: 1, kids: [] }; }",
        );
    }

    #[test]
    fn declarations_are_checked() {
        assert_eq!(
            errors("struct f { x: i64 }\nfun f() {}\nfun main() {}"),
            [("E0302", "f")]
        );
        assert_eq!(
            errors("struct print { x: i64 }\nfun main() {}"),
            [("E0302", "print")]
        );
        assert_eq!(errors("struct P { q: Q }\nfun main() {}"), [("E0305", "Q")]);
        assert_eq!(
            errors("struct P { x: i64, x: f64 }\nfun main() {}"),
            [("E0308", "x")]
        );
        assert_eq!(errors("struct A { a: A }\nfun main() {}"), [("E0307", "A")]);
        let cycle =
            diagnostics("struct A { b: B }\nstruct B { a: A }\nstruct C { a: A }\nfun main() {}");
        assert_eq!(cycle.len(), 1, "one report per cycle: {cycle:?}");
        assert_eq!(
            (cycle[0].code, cycle[0].label.as_deref()),
            ("E0307", Some("A -> B -> A"))
        );
        assert_eq!(
            errors("extern fun f(p: P);\nstruct P { x: i64 }\nfun main() {}"),
            [("E0409", "P")]
        );
    }

    #[test]
    fn fields_are_read_and_assigned_like_places() {
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 1.0, y: 2.0 }}; let z = p.z; }}"
            )),
            [("E0410", "z")]
        );
        // Structs compare field by field in v2 (draft §4).
        crate::check::test_util::ok(&format!(
            "{POINT}fun main() {{ let p = P {{ x: 1.0, y: 2.0 }}; let q = p; let e = p == q; }}"
        ));
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 1.0, y: 2.0 }}; p.x = 2.0; }}"
            )),
            [("E0501", "p.x")]
        );
        ok(&format!(
            "{POINT}fun main() {{ let mut ps = [P {{ x: 1.0, y: 2.0 }}]; ps[0].x += 1.0; }}"
        ));
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let mut ps = [P {{ x: 1.0, y: 2.0 }}]; for p of ps {{ ps[0].x = 1.0; }} }}"
            )),
            [("E0507", "ps[0].x")]
        );
    }
}
