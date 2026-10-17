//! Enum construction (v1 §4; PRP-035): `E.V`, `E.V(..)`, `m.E.V`,
//! `Some(x)` and `None`, inferring a generic enum's type arguments.

use super::expr::Expect;
use super::generics::subst;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, Ident};
use crate::span::Span;

impl Checker {
    /// The key of the enum `expr` names, `E` or `m.E` (PRP-034), when it
    /// names one rather than a local.
    fn enum_named(&mut self, expr: &Expr) -> Option<String> {
        let key = match &expr.kind {
            // The prelude's `Ordering` (v15) unless the module declares its own.
            ExprKind::Name(name) if self.local(name).is_none() => {
                let key = self.key(name);
                match self.prelude_type(name) {
                    Some(prelude) if !self.enums.contains_key(&key) => prelude,
                    _ => key,
                }
            }
            ExprKind::Field(base, e) => self.member_through(&Self::module_name(base)?, e)?,
            _ => return None,
        };
        if !self.enums.contains_key(&key) {
            return None;
        }
        self.item_visible(&key, expr.span);
        Some(key)
    }

    /// `E.V` without a call: a payload-less variant. `None` if `base` isn't an enum name.
    pub(super) fn variant_value(
        &mut self,
        expr: &Expr,
        (base, variant): (&Expr, &Ident),
        expect: Option<&Expect>,
    ) -> Option<Type> {
        let name = self.enum_named(base)?;
        self.record(base, Type::Unit);
        // A variant with payloads, not called, is a function value (v17).
        if let Some(ty) = self.enum_variant_function(expr, &name, &variant.name, expect) {
            self.hover.variant_use(&name, variant);
            return Some(ty);
        }
        Some(self.construct((expr, variant.span), &name, variant, &[], expect))
    }

    /// `E.V(args)`, `Some(x)`: a construction call. `None` if `callee` isn't one.
    pub(super) fn construction(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Option<Type> {
        if let Some(ty) = self.result_construction(call, callee, args, expect) {
            return Some(ty);
        }
        if let ExprKind::Name(name) = &callee.kind
            && name == "Some"
            && self.local(name).is_none()
        {
            self.record(callee, Type::Unit);
            return Some(self.some(call, args, expect));
        }
        let ExprKind::Field(base, variant) = &callee.kind else {
            return None;
        };
        let name = self.enum_named(base)?;
        self.record(base, Type::Unit);
        self.record(callee, Type::Unit);
        Some(self.construct((call, call.span), &name, variant, args, expect))
    }

    /// `E.V` or `E.V(args)` at `at`, inferring a generic enum's arguments (PRP-035).
    pub(super) fn construct(
        &mut self,
        (at, span): (&Expr, Span),
        name: &str,
        variant: &Ident,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Type {
        let generics = self.enums[name].generics.clone();
        let ty = Type::Enum(name.to_string(), Vec::new());
        let found = self.enums[name]
            .variants
            .iter()
            .find(|(v, _)| *v == variant.name)
            .cloned();
        self.hover.variant_use(name, variant);
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
            return if generics.is_empty() { ty } else { Type::Error };
        }
        let inputs: Vec<_> = args
            .iter()
            .zip(payloads)
            .map(|(a, p)| (a, Expect::of(p)))
            .collect();
        let declared = Type::Enum(name.to_string(), super::generics::params(&generics));
        match self.infer(
            at,
            &generics,
            Default::default(),
            Some((&declared, expect)),
            &inputs,
        ) {
            Some(targs) => subst(&declared, &generics, &targs),
            None => Type::Error,
        }
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
    use crate::check::test_util::{errors, let_type};

    const SHAPE: &str = "enum Shape { Circle(f64), Rect(f64, f64), Empty }\n";

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
        // v17: a variant with payloads, not called, is a function value.
        assert_eq!(
            errors(&format!(
                "{SHAPE}fun main() {{ let s: Shape = Shape.Circle; }}"
            )),
            [("E0403", "Shape.Circle")]
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
