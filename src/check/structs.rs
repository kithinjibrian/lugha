//! Structs (spec §4, §6): declarations collected in pass 1, field types
//! resolved in pass 2, self-containment (E0307), and struct literals.

use std::collections::HashSet;

use super::env::INTRINSICS;
use super::expr::Expect;
use super::{Checker, Checking, Type, errors};
use crate::ast::{Expr, Ident, Item, Program};
use crate::span::Span;

/// A declared struct.
pub(super) struct StructInfo {
    /// Fields in declaration order; empty until pass 2 resolves them.
    pub fields: Vec<(String, Type)>,
    /// The struct's name in its declaration.
    pub span: Span,
}

impl Checker {
    /// Pass 1: struct names, so any annotation can name any struct (spec §6).
    pub(super) fn collect_structs(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Struct(s) = item else { continue };
            let name = &s.name.name;
            if INTRINSICS.contains(&name.as_str()) {
                self.report(errors::reserved(name, s.name.span));
            } else if let Some(first) = self.structs.get(name) {
                let first = first.span;
                self.report(errors::duplicate(name, s.name.span, first));
            } else {
                let info = StructInfo {
                    fields: Vec::new(),
                    span: s.name.span,
                };
                self.structs.insert(name.clone(), info);
            }
        }
    }

    /// Pass 2: every field's type (E0305, E0308), then structs that contain
    /// themselves (E0307).
    pub(super) fn resolve_structs(&mut self, program: &Program) -> Checking<()> {
        for item in &program.items {
            let Item::Struct(s) = item else { continue };
            if self
                .structs
                .get(&s.name.name)
                .is_none_or(|info| info.span != s.name.span)
            {
                continue; // a duplicate, already reported
            }
            let mut fields: Vec<(String, Type)> = Vec::new();
            for (i, field) in s.fields.iter().enumerate() {
                let ty = self.resolve(&field.ty)?;
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
                fields.push((field.name.name.clone(), ty));
            }
            self.structs
                .get_mut(&s.name.name)
                .expect("collected in pass 1")
                .fields = fields;
        }
        let mut reported = HashSet::new();
        for item in &program.items {
            let Item::Struct(s) = item else { continue };
            let start = &s.name.name;
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
        Ok(())
    }

    /// Extends `path` until it returns to `path[0]` through fields held inline
    /// (an array is a pointer, so it ends the search).
    fn cycle(&self, start: &str, path: &mut Vec<String>, seen: &mut HashSet<String>) -> bool {
        let current = path.last().expect("path starts at the struct").clone();
        let Some(info) = self.structs.get(&current) else {
            return false;
        };
        for (_, ty) in &info.fields {
            let Type::Struct(next) = ty else { continue };
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

    /// `S { f: e, … }`: every field exactly once, in any order, each
    /// expecting its declared type (spec §4); values checked in source order.
    pub(super) fn struct_literal(
        &mut self,
        expr: &Expr,
        name: &Ident,
        given: &[(Ident, Expr)],
    ) -> Checking<Type> {
        let Some(info) = self.structs.get(&name.name) else {
            self.report(errors::unknown_type(&name.name, name.span));
            for (_, value) in given {
                self.value(value, None)?;
            }
            return Ok(Type::Error);
        };
        let declared = info.fields.clone();
        let ty = Type::Struct(name.name.clone());
        for (i, (field, value)) in given.iter().enumerate() {
            if let Some((first, _)) = given[..i].iter().find(|(f, _)| f.name == field.name) {
                self.report(errors::field_given_twice(
                    &field.name,
                    field.span,
                    first.span,
                ));
            }
            match declared.iter().find(|(f, _)| *f == field.name) {
                Some((_, field_ty)) => {
                    let why = format!("`{}` is declared as {field_ty}", field.name);
                    self.expect_type(value, &Expect::because(field_ty.clone(), field.span, why))?;
                }
                None => {
                    self.report(errors::no_field(&field.name, ty.clone(), field.span));
                    self.value(value, None)?;
                }
            }
        }
        let missing: Vec<&str> = declared
            .iter()
            .map(|(f, _)| f.as_str())
            .filter(|f| !given.iter().any(|(g, _)| g.name == *f))
            .collect();
        if !missing.is_empty() {
            self.report(errors::missing_fields(&missing, &name.name, expr.span));
        }
        Ok(ty)
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
        assert_eq!(let_type(src, "r"), Type::Struct("R".into()));
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
    fn literals_give_every_field_once() {
        assert_eq!(
            errors(&format!("{POINT}fun main() {{ let p = P {{ x: 1.0 }}; }}")),
            [("E0413", "P { x: 1.0 }")]
        );
        let missing = diagnostics(&format!("{POINT}fun main() {{ let p = P {{ }}; }}"));
        assert_eq!(missing[0].message, "missing fields `x`, `y` in `P`");
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 1.0, y: 2.0, x: 3.0 }}; }}"
            )),
            [("E0414", "x")]
        );
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 1.0, y: 2.0, z: 3.0 }}; }}"
            )),
            [("E0410", "z")]
        );
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 1, y: 2.0 }}; }}"
            )),
            [("E0401", "1")]
        );
        assert_eq!(
            errors("fun main() { let p = Q { x: 1 }; }"),
            [("E0305", "Q")]
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
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 1.0, y: 2.0 }}; let q = p; let e = p == q; }}"
            )),
            [("E0404", "p == q")]
        );
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
