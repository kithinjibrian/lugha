//! Enums (v1 §4, §6): declarations, construction (`E.V`, `E.V(..)`,
//! `Some(x)`, `None`) and the E0311 rule for locals named like types.

use super::env::INTRINSICS;
use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, Ident, Item, Program};
use crate::span::Span;

/// Built-in names a program can't define (v1 §2).
pub(super) const BUILTIN_NAMES: [&str; 3] = ["Option", "Some", "None"];

/// A declared enum.
pub(super) struct EnumInfo {
    /// Variants and their payload types, in declaration order.
    pub variants: Vec<(String, Vec<Type>)>,
    /// The enum's name in its declaration.
    pub span: Span,
}

impl Checker {
    /// Pass 1: enum names, after struct names (v1 §6).
    pub(super) fn collect_enums(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Enum(e) = item else { continue };
            let name = &e.name.name;
            let key = self.key(name);
            if INTRINSICS.contains(&name.as_str()) {
                self.report(errors::reserved(name, e.name.span));
            } else if BUILTIN_NAMES.contains(&name.as_str()) {
                self.report(errors::builtin_name(name, e.name.span));
            } else if let Some(first) = self.type_span(&key) {
                self.report(errors::duplicate(name, e.name.span, first));
            } else {
                self.register_item(&key, e.visibility);
                let info = EnumInfo {
                    variants: Vec::new(),
                    span: e.name.span,
                };
                self.enums.insert(key, info);
            }
        }
    }

    /// Pass 2: payload types (E0305) and duplicate variants (E0309).
    pub(super) fn resolve_enums(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Enum(e) = item else { continue };
            let key = self.key(&e.name.name);
            if self
                .enums
                .get(&key)
                .is_none_or(|info| info.span != e.name.span)
            {
                continue; // a duplicate, already reported
            }
            let mut variants: Vec<(String, Vec<Type>)> = Vec::new();
            for (i, v) in e.variants.iter().enumerate() {
                let payloads = v.payloads.iter().map(|t| self.resolve(t)).collect();
                if let Some(first) = e.variants[..i].iter().find(|w| w.name.name == v.name.name) {
                    let d = errors::duplicate_variant(
                        &v.name.name,
                        &e.name.name,
                        v.name.span,
                        first.name.span,
                    );
                    self.report(d);
                    continue;
                }
                variants.push((v.name.name.clone(), payloads));
            }
            self.enums
                .get_mut(&key)
                .expect("collected in pass 1")
                .variants = variants;
        }
    }

    /// The variants of an enum or `Option` type.
    pub(super) fn variants_of(&self, ty: &Type) -> Option<Vec<(String, Vec<Type>)>> {
        match ty {
            Type::Enum(name) => self.enums.get(name).map(|e| e.variants.clone()),
            Type::Option(inner) => Some(vec![
                ("None".into(), Vec::new()),
                ("Some".into(), vec![(**inner).clone()]),
            ]),
            _ => None,
        }
    }

    /// The key of the enum `expr` names, `E` or `m.E` (PRP-034), when it
    /// names one rather than a local.
    fn enum_named(&mut self, expr: &Expr) -> Option<String> {
        let key = match &expr.kind {
            ExprKind::Name(name) if self.local(name).is_none() => self.key(name),
            ExprKind::Field(base, e) => match &base.kind {
                ExprKind::Name(m) => super::qualify(&self.imported(m)?, &e.name),
                _ => return None,
            },
            _ => return None,
        };
        if !self.enums.contains_key(&key) {
            return None;
        }
        self.item_visible(&key, expr.span);
        Some(key)
    }

    /// `E.V` without a call: a payload-less variant. `None` if `base` isn't an enum name.
    pub(super) fn variant_value(&mut self, base: &Expr, variant: &Ident) -> Option<Type> {
        let name = self.enum_named(base)?;
        self.record(base, Type::Void);
        Some(self.construct(&name, variant, &[], variant.span))
    }

    /// `E.V(args)`, `Some(x)`: a construction call. `None` if `callee` isn't one.
    pub(super) fn construction(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Option<Type> {
        if let ExprKind::Name(name) = &callee.kind
            && name == "Some"
            && self.local(name).is_none()
        {
            self.record(callee, Type::Void);
            return Some(self.some(call, args, expect));
        }
        let ExprKind::Field(base, variant) = &callee.kind else {
            return None;
        };
        let name = self.enum_named(base)?;
        self.record(base, Type::Void);
        self.record(callee, Type::Void);
        Some(self.construct(&name, variant, args, call.span))
    }

    fn construct(&mut self, name: &str, variant: &Ident, args: &[Expr], span: Span) -> Type {
        let ty = Type::Enum(name.to_string());
        let found = self.enums[name]
            .variants
            .iter()
            .find(|(v, _)| *v == variant.name)
            .cloned();
        let Some((_, payloads)) = found else {
            self.report(errors::no_variant(&variant.name, &ty, variant.span));
            args.iter().for_each(|a| {
                self.expr(a, None);
            });
            return Type::Error;
        };
        if payloads.len() != args.len() {
            let what = format!("{name}.{}", variant.name);
            self.report(errors::payload_count(
                &what,
                payloads.len(),
                args.len(),
                span,
            ));
            args.iter().for_each(|a| {
                self.expr(a, None);
            });
            return ty;
        }
        for (arg, payload) in args.iter().zip(payloads) {
            self.expect_type(arg, &Expect::of(payload));
        }
        ty
    }

    /// `Some(x)`: `Option<T>` with `x : T`, using an expected `Option<T>` if any.
    fn some(&mut self, call: &Expr, args: &[Expr], expect: Option<&Expect>) -> Type {
        let [arg] = args else {
            self.report(errors::payload_count("Some", 1, args.len(), call.span));
            args.iter().for_each(|a| {
                self.expr(a, None);
            });
            return Type::Error;
        };
        let inner = match expect.map(|e| &e.ty) {
            Some(Type::Option(inner)) => self.expect_type(arg, &Expect::of((**inner).clone())),
            _ => self.value(arg, None),
        };
        match inner {
            Type::Error | Type::Never => Type::Error,
            inner => Type::Option(Box::new(inner)),
        }
    }

    /// `None`: needs an expected `Option<T>` (E0412 otherwise).
    pub(super) fn none(&mut self, span: Span, expect: Option<&Expect>) -> Type {
        match expect.map(|e| &e.ty) {
            Some(ty @ Type::Option(_)) => ty.clone(),
            // The context already failed and was reported: one mistake, one diagnostic.
            Some(Type::Error) => Type::Error,
            _ => {
                self.report(errors::cannot_infer_none(span));
                Type::Error
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

    const SHAPE: &str = "enum Shape { Circle(f64), Rect(f64, f64), Empty }\n";

    #[test]
    fn enums_declare_in_any_order_and_construct() {
        let src =
            format!("fun main() {{ let s = Shape.Circle(2.0); let e = Shape.Empty; }}\n{SHAPE}");
        assert_eq!(let_type(&src, "s"), Type::Enum("Shape".into()));
        assert_eq!(let_type(&src, "e"), Type::Enum("Shape".into()));
        ok(&format!(
            "{SHAPE}struct Car {{ s: Shape }}\nfun main() {{ let c = Car {{ s: Shape.Rect(1.0, 2.0) }}; }}"
        ));
    }

    #[test]
    fn declarations_are_checked() {
        assert_eq!(
            errors("enum E { A, B, A }\nfun main() {}"),
            [("E0309", "A")]
        );
        assert_eq!(
            errors("enum P { A }\nstruct P { x: i64 }\nfun main() {}"),
            [("E0302", "P")]
        );
        assert_eq!(
            errors("enum Option { A }\nfun main() {}"),
            [("E0302", "Option")]
        );
        assert_eq!(errors("enum E { A(Q) }\nfun main() {}"), [("E0305", "Q")]);
        let boxed = diagnostics("enum L { N, C(Box<L>) }\nfun main() {}");
        assert_eq!(boxed[0].code, "E0305");
        assert!(boxed[0].help.as_deref().unwrap().contains("need no `Box`"));
    }

    #[test]
    fn construction_checks_variants_and_payloads() {
        assert_eq!(
            errors(&format!(
                "{SHAPE}fun main() {{ let s = Shape.Circel(1.0); }}"
            )),
            [("E0415", "Circel")]
        );
        assert_eq!(
            errors(&format!("{SHAPE}fun main() {{ let s = Shape.Rect(1.0); }}")),
            [("E0416", "Shape.Rect(1.0)")]
        );
        assert_eq!(
            errors(&format!("{SHAPE}fun main() {{ let s = Shape.Circle; }}")),
            [("E0416", "Circle")]
        );
        assert_eq!(
            errors(&format!("{SHAPE}fun main() {{ let s = Shape.Circle(1); }}")),
            [("E0401", "1")]
        );
        // Enums compare by variant and payloads in v2 (draft §4).
        crate::check::test_util::ok(&format!(
            "{SHAPE}fun main() {{ let a = Shape.Empty; let b = a == a; }}"
        ));
    }

    #[test]
    fn options_infer_from_payloads_or_context() {
        assert_eq!(
            let_type("fun main() { let o = Some(3); }", "o"),
            Type::Option(Box::new(Type::I64))
        );
        assert_eq!(
            let_type("fun main() { let o: Option<u8> = Some(3); }", "o"),
            Type::Option(Box::new(Type::U8))
        );
        assert_eq!(
            let_type("fun main() { let o: Option<string> = None; }", "o"),
            Type::Option(Box::new(Type::String))
        );
        assert_eq!(errors("fun main() { let o = None; }"), [("E0412", "None")]);
        assert_eq!(
            errors("fun main() { let o: Option<u8> = Some(300); }"),
            [("E0402", "300")]
        );
    }
}
