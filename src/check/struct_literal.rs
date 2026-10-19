//! Struct literals (spec §4; v2 draft §5): `S { f: e, … }` and, from
//! another module, `m.S { … }`, which needs every field visible (E0313).

use super::applied::Applied;
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
            if let Some(base) = &lit.base {
                self.value(base, None);
            }
            return Type::Error;
        };
        let (declared, generics) = (info.fields.clone(), info.generics.clone());
        let key = key.expect("found just above");
        self.literal_visible(&key, expr.span);
        if let Some(decl) = self.type_decl(&key) {
            self.hover.define(name.span, decl);
        }
        for (field, _) in given {
            self.hover.field_use(&key, field);
        }
        let ty = Type::Struct(key.clone(), params(&generics));
        // `..base` (v17): checked first — its type arguments then fix a
        // generic struct's, so the fields need no inference. Evaluation
        // order is unaffected: codegen runs `base` after the fields.
        let base_args = lit.base.as_ref().and_then(|base| {
            // No expected type: a mismatch is E0440, not the base's own error.
            match self.value(base, None) {
                Type::Struct(k, args) if k == key => Some(args),
                Type::Error | Type::Never => None,
                found => {
                    self.report(errors::base_type(&ty, &found, base.span));
                    None
                }
            }
        });
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
                    let field_ty = match &base_args {
                        Some(args) => subst(field_ty, &generics, args),
                        None => field_ty.clone(),
                    };
                    let why = format!("`{}` is declared as {field_ty}", field.name);
                    let want = Expect::because(field_ty, field.span, why);
                    if generics.is_empty() || base_args.is_some() {
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
        // A union's literal writes exactly one field (v17, milestone 104).
        if self.c_unions.contains(&key) {
            if given.len() != 1 || lit.base.is_some() {
                self.report(super::out_params::union_literal(&name.name, expr.span));
            }
        } else {
            match &lit.base {
                None if !missing.is_empty() => {
                    self.report(errors::missing_fields(&missing, &name.name, expr.span));
                }
                Some(base) if missing.is_empty() => self.report(errors::base_unused(base.span)),
                _ => {}
            }
        }
        // With a base, its type is the literal's: checked against what's expected here.
        // An expected interface is `convert`'s to judge (in `expr`), as for any value.
        if let Some(args) = base_args {
            let found = subst(&ty, &generics, &args);
            return match expect {
                Some(want) if !found.fits(&want.ty) && Applied::of(&want.ty).is_none() => {
                    self.report(errors::mismatch(
                        want.ty.clone(),
                        found,
                        expr.span,
                        want.reason.as_ref(),
                    ));
                    Type::Error
                }
                _ => found,
            };
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

    #[test]
    fn base_supplies_the_fields_not_written_v17() {
        use crate::check::test_util::ok;
        ok(&format!(
            "{POINT}fun origin(): P = P {{ x: 0.0, y: 0.0 }};\n\
             fun main() {{ let p = P {{ x: 1.0, ..origin() }}; let q = P {{ ..p }}; println(q.y); }}"
        ));
        // Generic: the base fixes the type arguments.
        ok("struct Pair<A, B> { a: A, b: B }\n\
            fun main() { let p = Pair { a: 1, b: \"s\" }; let q = Pair { a: 2, ..p }; println(q.b); }");
    }

    #[test]
    fn a_base_of_another_type_is_e0440_v17() {
        let src = format!(
            "{POINT}struct Q {{ x: f64, y: f64 }}\n\
             fun main() {{ let q = Q {{ x: 1.0, y: 2.0 }}; let p = P {{ x: 1.0, ..q }}; }}"
        );
        assert_eq!(errors(&src), [("E0440", "q")]);
        let d = diagnostics(&src);
        assert!(
            d[0].message.contains("`P`") && d[0].message.contains("`Q`"),
            "{d:?}"
        );
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 1.0, ..5 }}; }}"
            )),
            [("E0440", "5")]
        );
        assert_eq!(
            errors(
                "struct Pair<A, B> { a: A, b: B }\n\
                    fun main() { let p = Pair { a: 1, b: 2 }; let q: Pair<i64, string> = Pair { a: 2, ..p }; }"
            ),
            [("E0403", "Pair { a: 2, ..p }")]
        );
    }

    #[test]
    fn a_base_that_copies_nothing_is_w0104_v17() {
        use crate::check::test_util::warnings;
        let src = format!(
            "{POINT}fun main() {{ let p = P {{ x: 0.0, y: 0.0 }}; let q = P {{ x: 1.0, y: 2.0, ..p }}; println(q.x); }}"
        );
        let w = warnings(&src);
        assert_eq!(w, [("W0104", "p")]);
    }

    #[test]
    fn written_fields_still_must_exist_and_differ_v17() {
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 0.0, y: 0.0 }}; let q = P {{ z: 1.0, ..p }}; }}"
            )),
            [("E0410", "z")]
        );
        assert_eq!(
            errors(&format!(
                "{POINT}fun main() {{ let p = P {{ x: 0.0, y: 0.0 }}; let q = P {{ x: 1.0, x: 2.0, ..p }}; }}"
            )),
            [("E0414", "x")]
        );
    }

    const SQ: &str = "interface Shape { fun area(self): i64; }\n\
        struct Sq { side: i64, tag: i64 }\n\
        impl Sq { fun base(): Sq = Sq { side: 1, tag: 0 }; fun area(self): i64 = self.side * self.side; }\n\
        struct Box { w: i64 }\n";

    #[test]
    fn a_literal_with_a_base_converts_to_an_interface_v17() {
        use crate::check::Type;
        use crate::check::test_util::{let_type, ok};
        let shape = Type::Interface("Shape".into(), Vec::new());
        let src = format!(
            "{SQ}fun main() {{ let s: Shape = Sq {{ side: 3, ..Sq.base() }}; let t = s; }}"
        );
        assert_eq!(let_type(&src, "t"), shape);
        // Arguments, array elements, arrays passed to methods, returns.
        ok(&format!(
            "{SQ}fun f(s: Shape): i64 = s.area();\n\
             struct Node {{ n: i64 }}\n\
             impl Node {{ fun all(self, xs: Shape[]): i64 = xs[0].area(); }}\n\
             fun g(b: Sq): Shape = Sq {{ side: 2, ..b }};\n\
             fun main() {{\n\
               println(f(Sq {{ side: 3, ..Sq.base() }}));\n\
               let xs: Shape[] = [Sq {{ side: 4, ..Sq.base() }}];\n\
               let node = Node {{ n: 0 }};\n\
               println(node.all([Sq {{ side: 5, ..Sq.base() }}]));\n\
               println(g(Sq.base()).area() + xs[0].area());\n\
             }}"
        ));
        // Generic: the base fixes the type arguments, then the value converts.
        ok("interface Named { fun name(self): string; }\n\
            struct W<T> { v: T, n: string }\n\
            impl W<T> { fun name(self): string = self.n; }\n\
            fun main() { let w = W { v: 1, n: \"a\" }; let s: Named = W { n: \"b\", ..w }; println(s.name()); }");
    }

    #[test]
    fn a_base_literal_still_must_fit_what_is_expected_v17() {
        // A struct without the interface's items: E0422, as for a plain literal.
        assert_eq!(
            errors(&format!(
                "{SQ}fun main() {{ let s: Shape = Box {{ w: 1 }}; }}"
            )),
            [("E0422", "Box { w: 1 }")]
        );
        assert_eq!(
            errors(&format!(
                "{SQ}fun main() {{ let b = Box {{ w: 1 }}; let s: Shape = Box {{ ..b }}; }}"
            )),
            [("E0422", "Box { ..b }")]
        );
        // Another struct expected: still E0403.
        assert_eq!(
            errors(&format!(
                "{SQ}fun main() {{ let p: Box = Sq {{ side: 3, ..Sq.base() }}; }}"
            )),
            [("E0403", "Sq { side: 3, ..Sq.base() }")]
        );
    }

    #[test]
    fn unwritten_fields_must_be_visible_too_v17() {
        use crate::check::modules::test_util::errors;
        let lib = (
            "m.la",
            "public struct R { public lo: i64, private hi: i64 }\npublic fun make(): R = R { lo: 0, hi: 1 };\n",
        );
        let found = errors(&[
            (
                "main.la",
                "import m;\nfun main() { let r = m.R { lo: 5, ..m.make() }; }\n",
            ),
            lib,
        ]);
        assert_eq!(found.iter().map(|(c, _)| *c).collect::<Vec<_>>(), ["E0313"]);
    }
}
