//! Struct literals (spec §4; v2 draft §5): `S { f: e, … }` and, from
//! another module, `m.S { … }`, which needs every field visible (E0313).

use super::expr::Expect;
use super::generics::{params, subst};
use super::{Checker, Type, errors};
use crate::ast::{Expr, StructLiteral};

impl Checker {
    /// `S { f: e, … }`: every field exactly once, in any order, each
    /// expecting its declared type (spec §4); values checked in source
    /// order, except that a generic struct's deferring values go last (PRP-035).
    pub(super) fn struct_literal(
        &mut self,
        expr: &Expr,
        lit: &StructLiteral,
        expect: Option<&Expect>,
    ) -> Type {
        let (module, name, given) = (lit.module.as_ref(), &lit.name, &lit.fields);
        // `m.S { … }` names its module (PRP-034); the span covers the whole
        // literal, where a private-item error belongs.
        let key = match module {
            Some(_) => self.type_key(module, name, expr.span),
            None if self.structs.contains_key(&self.key(&name.name)) => {
                let key = self.key(&name.name);
                self.item_visible(&key, expr.span);
                Some(key)
            }
            // A prelude struct no module declares, `Pair { … }` (v13).
            None => match self.prelude_iter_type(&name.name) {
                Some(key) => Some(key),
                None => {
                    self.report(errors::unknown_type(&name.name, name.span));
                    None
                }
            },
        };
        let Some(info) = key.as_ref().and_then(|k| self.structs.get(k)) else {
            for (_, value) in given {
                self.value(value, None);
            }
            return Type::Error;
        };
        let (declared, generics) = (info.fields.clone(), info.generics.clone());
        let key = key.expect("found just above");
        self.literal_visible(&key, expr.span);
        let ty = Type::Struct(key, params(&generics));
        let mut inputs = Vec::new();
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
                    let want = Expect::because(field_ty.clone(), field.span, why);
                    if generics.is_empty() {
                        self.expect_type(value, &want);
                    } else {
                        inputs.push((value, want));
                    }
                }
                None => {
                    self.report(errors::no_field(&field.name, ty.clone(), field.span));
                    self.value(value, None);
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
        match self.infer(
            expr,
            &generics,
            Default::default(),
            Some((&ty, expect)),
            &inputs,
        ) {
            Some(targs) => subst(&ty, &generics, &targs),
            None => Type::Error,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors};

    const POINT: &str = "struct P { x: f64, y: f64 }\n";

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
}
