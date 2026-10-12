//! `match` (v1 §4, §5): pattern typing, payload bindings, exhaustiveness
//! (E0417) and unreachable arms (W0102).

use std::collections::HashMap;

use super::expr::Expect;
use super::{Binding, Checker, Type, errors};
use crate::ast::{Arm, Expr, Pattern, PatternKind};
use crate::span::Span;

/// What an arm's pattern covers, for exhaustiveness and W0102.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Covers {
    Variant(String),
    Int(u64, bool),
    Str(String),
    Bool(bool),
}

impl Checker {
    /// `match scrutinee { arms }`: arms agree like `if`/`else` branches.
    pub(super) fn match_expr(
        &mut self,
        expr: &Expr,
        scrutinee: &Expr,
        arms: &[Arm],
        expect: Option<&Expect>,
    ) -> Type {
        let sty = self.value(scrutinee, None);
        let mut seen: HashMap<Covers, Span> = HashMap::new();
        let mut wildcard: Option<Span> = None;
        let mut result: Option<(Type, Span)> = None;
        for arm in arms {
            let (covers, bindings) = self.pattern(&arm.pattern, &sty);
            let earlier = wildcard.or_else(|| covers.as_ref().and_then(|c| seen.get(c).copied()));
            if let Some(cause) = earlier {
                self.report(errors::unreachable_arm(arm.pattern.span, cause));
            }
            match covers {
                Some(c) => {
                    seen.entry(c).or_insert(arm.pattern.span);
                }
                None => {
                    wildcard.get_or_insert(arm.pattern.span);
                }
            }
            self.push();
            for (name, ty) in bindings {
                // `declare` reports E0311 for a binding named like a type.
                self.declare(&name, ty, Binding::Pattern);
            }
            let ty = match &result {
                // Arms without a value: the others must have none either.
                Some((Type::Void, _)) => {
                    let ty = self.expr(&arm.body, None);
                    if !matches!(ty, Type::Void | Type::Never | Type::Error) {
                        self.report(errors::mismatch(
                            Type::Void,
                            ty.clone(),
                            arm.body.span,
                            None,
                        ));
                    }
                    ty
                }
                Some((first, span)) => {
                    let why = format!("the first arm is {first}");
                    self.expect_type(&arm.body, &Expect::because(first.clone(), *span, why))
                }
                None => self.expr(&arm.body, expect),
            };
            self.pop();
            if result.is_none() && ty != Type::Never {
                result = Some((ty, arm.body.span));
            }
        }
        if wildcard.is_none() {
            self.exhaustive(expr, scrutinee, &sty, &seen);
        }
        match result {
            Some((ty, _)) => ty,
            None if arms.is_empty() && !matches!(sty, Type::Enum(_)) => Type::Error,
            None => Type::Never,
        }
    }

    /// Reports E0417 unless `seen` covers every value of `sty`.
    fn exhaustive(
        &mut self,
        expr: &Expr,
        scrutinee: &Expr,
        sty: &Type,
        seen: &HashMap<Covers, Span>,
    ) {
        let missing: Vec<String> = match sty {
            Type::Error | Type::Never => return,
            Type::Bool => [true, false]
                .into_iter()
                .filter(|b| !seen.contains_key(&Covers::Bool(*b)))
                .map(|b| b.to_string())
                .collect(),
            ty => match self.variants_of(ty) {
                Some(variants) => variants
                    .into_iter()
                    .map(|(v, _)| v)
                    .filter(|v| !seen.contains_key(&Covers::Variant(v.clone())))
                    .collect(),
                // Integers and strings have too many values to list.
                None => {
                    let span = Span::new(expr.span.start, scrutinee.span.end);
                    self.report(errors::non_exhaustive_values(ty, span));
                    return;
                }
            },
        };
        if !missing.is_empty() {
            let span = Span::new(expr.span.start, scrutinee.span.end);
            self.report(errors::non_exhaustive(&missing, span));
        }
    }

    /// Checks one pattern against the scrutinee type: what it covers (`None`
    /// for `_`) and the bindings it introduces.
    fn pattern(
        &mut self,
        pattern: &Pattern,
        sty: &Type,
    ) -> (Option<Covers>, Vec<(crate::ast::Ident, Type)>) {
        let fits = |ok: bool| ok || matches!(sty, Type::Error | Type::Never);
        match &pattern.kind {
            PatternKind::Wildcard => (None, Vec::new()),
            PatternKind::Int { value, negative } => {
                if !fits(sty.is_integer()) {
                    self.report(errors::pattern_mismatch(sty, pattern.span));
                } else if sty.is_integer() {
                    let (max, neg) = sty.literal_range();
                    if (*negative && *value > neg) || (!*negative && *value > max) {
                        self.report(errors::out_of_range(sty.clone(), pattern.span));
                    }
                }
                (
                    Some(Covers::Int(*value, *negative && *value != 0)),
                    Vec::new(),
                )
            }
            PatternKind::Str(text) => {
                if !fits(*sty == Type::String) {
                    self.report(errors::pattern_mismatch(sty, pattern.span));
                }
                (Some(Covers::Str(text.clone())), Vec::new())
            }
            PatternKind::Bool(value) => {
                if !fits(*sty == Type::Bool) {
                    self.report(errors::pattern_mismatch(sty, pattern.span));
                }
                (Some(Covers::Bool(*value)), Vec::new())
            }
            PatternKind::Variant(name, slots) => {
                self.variant_pattern(pattern, name, slots.as_deref(), sty)
            }
        }
    }

    fn variant_pattern(
        &mut self,
        pattern: &Pattern,
        name: &crate::ast::Ident,
        slots: Option<&[Pattern]>,
        sty: &Type,
    ) -> (Option<Covers>, Vec<(crate::ast::Ident, Type)>) {
        let covers = Some(Covers::Variant(name.name.clone()));
        if matches!(sty, Type::Error | Type::Never) {
            return (covers, Vec::new());
        }
        let Some(variants) = self.variants_of(sty) else {
            self.report(errors::pattern_mismatch(sty, pattern.span));
            return (covers, Vec::new());
        };
        let Some((_, payloads)) = variants.into_iter().find(|(v, _)| *v == name.name) else {
            self.report(errors::no_variant(&name.name, sty, name.span));
            return (covers, Vec::new());
        };
        let slots = slots.unwrap_or(&[]);
        if slots.len() != payloads.len() {
            self.report(errors::payload_count(
                &name.name,
                payloads.len(),
                slots.len(),
                pattern.span,
            ));
            return (covers, Vec::new());
        }
        let bindings = slots
            .iter()
            .zip(payloads)
            .filter_map(|(slot, ty)| slot.slot_binding().map(|ident| (ident.clone(), ty)))
            .collect();
        (covers, bindings)
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok, warnings};

    const SHAPE: &str = "enum Shape { Circle(f64), Rect(f64, f64), Empty }\n";

    #[test]
    fn match_arms_agree_and_bind_payloads() {
        let src = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; let a = match s {{ Circle(r) => r, Rect(w, _) => w, Empty => 0.0 }}; }}"
        );
        assert_eq!(let_type(&src, "a"), Type::F64);
        let bad = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; let a = match s {{ Circle(r) => r, _ => true }}; }}"
        );
        assert_eq!(errors(&bad), [("E0403", "true")]);
        let mutate = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; match s {{ Circle(r) => {{ r = 1.0; }} _ => {{}} }} }}"
        );
        assert_eq!(errors(&mutate), [("E0501", "r")]);
        let scoped = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; match s {{ Circle(r) => {{}} _ => {{ let x = r; }} }} }}"
        );
        assert_eq!(errors(&scoped), [("E0301", "r")]);
    }

    #[test]
    fn missing_cases_are_e0417_listing_each() {
        let src =
            format!("{SHAPE}fun f(s: Shape): f64 = match s {{ Circle(r) => r }};\nfun main() {{}}");
        let d = crate::check::test_util::diagnostics(&src);
        assert_eq!(
            (d[0].code, d[0].message.as_str()),
            ("E0417", "non-exhaustive match: `Rect`, `Empty` not covered")
        );
        assert_eq!(
            errors("fun main() { let n = 1; let s = match n { 1 => 2 }; }"),
            [("E0417", "match n")]
        );
        ok("fun main() { let b = true; let s = match b { true => 1, false => 0 }; }");
        ok("fun main() { let o = Some(2); let v = match o { Some(x) => x, None => 0 }; }");
        assert_eq!(
            errors("fun main() { let o = Some(2); let v = match o { Some(x) => x }; }"),
            [("E0417", "match o")]
        );
    }

    #[test]
    fn unreachable_arms_warn() {
        assert_eq!(
            warnings("fun main() { let n = 1; let s = match n { _ => 1, 2 => 2 }; }"),
            [("W0102", "2")]
        );
        let twice = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; match s {{ Empty => {{}} Empty => {{}} _ => {{}} }} }}"
        );
        assert_eq!(warnings(&twice), [("W0102", "Empty")]);
    }

    #[test]
    fn patterns_must_fit_the_scrutinee() {
        assert_eq!(
            errors("fun main() { let n = 1; match n { \"a\" => {} _ => {} } }"),
            [("E0403", "\"a\"")]
        );
        assert_eq!(
            errors("fun main() { let n: u8 = 1; match n { 300 => {} _ => {} } }"),
            [("E0402", "300")]
        );
        let wrong = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; match s {{ Square => {{}} _ => {{}} }} }}"
        );
        assert_eq!(errors(&wrong), [("E0415", "Square")]);
        let count = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; match s {{ Rect(w) => {{}} _ => {{}} }} }}"
        );
        assert_eq!(errors(&count), [("E0416", "Rect(w)")]);
        let shadow = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; match s {{ Circle(Shape) => {{}} _ => {{}} }} }}"
        );
        assert_eq!(errors(&shadow), [("E0311", "Shape")]);
    }

    #[test]
    fn discarded_and_diverging_matches() {
        let discarded = "fun main() { let n = 1; match n { _ => 1 } let x = 2; }";
        assert_eq!(errors(discarded), [("E0505", "match n { _ => 1 }")]);
        ok("fun f(n: i64): i64 { match n { _ => { return 1; } } }\nfun main() {}");
    }
}
