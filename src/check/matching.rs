//! `match` (v1 §4, §5; v2 draft §5): arms, guards and bindings, then
//! exhaustiveness (E0417), unreachable arms (W0102) and unused bindings
//! (W0103). Patterns are typed in `patterns.rs`; coverage is `usefulness.rs`.

use super::expr::Expect;
use super::patterns::Pat;
use super::usefulness::show;
use super::{Binding, Checker, Type, errors};
use crate::ast::{Arm, Expr, Ident};
use crate::span::Span;

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
        // Unguarded arms that typed cleanly: what coverage is computed from.
        let mut rows: Vec<(Pat, Span)> = Vec::new();
        // A pattern with an error can't be reasoned about: skip E0417 then.
        let mut analysable = !matches!(sty, Type::Error | Type::Never);
        let mut result: Option<(Type, Span)> = None;
        for arm in arms {
            let mut bindings = Vec::new();
            let before = self.diagnostics.len();
            let pats = self.arm_pattern(&arm.pattern, &sty, &mut bindings);
            if self.diagnostics.len() > before {
                analysable = false;
            } else if analysable {
                self.arm_reachable(&rows, &pats, &sty, arm.pattern.span);
                if arm.guard.is_none() {
                    rows.extend(pats);
                }
            }
            self.push();
            for (name, ty, mutable) in &bindings {
                // `declare` reports E0311 for a binding named like a type.
                // A `mut` binding is the arm's own copy (v9).
                let binding = match mutable {
                    true => Binding::Let { mutable: true },
                    false => Binding::Pattern,
                };
                self.declare(name, ty.clone(), binding);
            }
            if let Some(guard) = &arm.guard {
                self.expect_type(guard, &Expect::of(Type::Bool));
            }
            let ty = self.arm_body(arm, &result, expect);
            self.unused(&bindings);
            self.pop();
            if result.is_none() && ty != Type::Never {
                result = Some((ty, arm.body.span));
            }
        }
        if analysable {
            self.exhaustive(expr, scrutinee, &sty, &rows);
        }
        match result {
            Some((ty, _)) => ty,
            None if arms.is_empty() && !matches!(sty, Type::Enum(_, _)) => Type::Error,
            None => Type::Never,
        }
    }

    /// An arm's body, agreeing with the first arm that has a value.
    fn arm_body(
        &mut self,
        arm: &Arm,
        result: &Option<(Type, Span)>,
        expect: Option<&Expect>,
    ) -> Type {
        match result {
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
        }
    }

    /// W0102 for an arm that no value reaches past the earlier unguarded
    /// arms, or for each of its top-level alternatives that none does (v9).
    fn arm_reachable(
        &mut self,
        rows: &[(Pat, Span)],
        pats: &[(Pat, Span)],
        sty: &Type,
        span: Span,
    ) {
        let tys = [sty.clone()];
        let space = self.space();
        let mut earlier: Vec<Vec<Pat>> = rows.iter().map(|(p, _)| vec![p.clone()]).collect();
        // An alternative is dead when none of its expansions is useful.
        let mut dead: Vec<(Span, bool)> = Vec::new();
        for (pat, at) in pats {
            let useful = space.useful(&earlier, std::slice::from_ref(pat), &tys);
            match dead.iter_mut().find(|(s, _)| s == at) {
                Some((_, all_dead)) => *all_dead &= !useful,
                None => dead.push((*at, !useful)),
            }
            earlier.push(vec![pat.clone()]);
        }
        if dead.iter().any(|(_, d)| !d) {
            for (at, _) in dead.iter().filter(|(_, d)| *d) {
                self.report(errors::unreachable_arm_combined(*at));
            }
            return;
        }
        let Some((pat, _)) = pats.first() else { return };
        // Name one arm that covers it alone, when there is one.
        let cause = rows
            .iter()
            .find(|(p, _)| !space.useful(&[vec![p.clone()]], std::slice::from_ref(pat), &tys))
            .map(|(_, s)| *s);
        self.report(match cause {
            Some(cause) => errors::unreachable_arm(span, cause),
            None => errors::unreachable_arm_combined(span),
        });
    }

    /// W0103 for each binding that nothing in its arm mentions.
    pub(super) fn unused(&mut self, bindings: &[(Ident, Type, bool)]) {
        for (name, _, _) in bindings {
            // A binding named like a type is already E0311.
            let named_like_type = self.type_span(&name.name).is_some();
            if !named_like_type && !self.mentions.values().any(|m| m.decl == name.span) {
                self.report(errors::unused_binding(&name.name, name.span));
            }
        }
    }

    /// E0417 unless the unguarded `rows` cover every value of `sty`.
    fn exhaustive(&mut self, expr: &Expr, scrutinee: &Expr, sty: &Type, rows: &[(Pat, Span)]) {
        let matrix: Vec<Vec<Pat>> = rows.iter().map(|(p, _)| vec![p.clone()]).collect();
        let missing = self.space().missing(&matrix, std::slice::from_ref(sty));
        if missing.is_empty() {
            return;
        }
        let span = Span::new(expr.span.start, scrutinee.span.end);
        // Integers and strings have too many values to list.
        if missing.iter().all(|w| w[0] == Pat::Wild) && (sty.is_integer() || *sty == Type::String) {
            self.report(errors::non_exhaustive_values(sty, span));
            return;
        }
        let shown: Vec<String> = missing.iter().map(|w| show(&w[0], true)).collect();
        self.report(errors::non_exhaustive(&shown, span));
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
        assert_eq!(errors(&scoped), [("W0103", "r"), ("E0301", "r")]);
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
    fn guards_are_bool_and_see_the_bindings() {
        let src = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; let a = match s {{ Circle(r) if r > 1.0 => 1, _ => 2 }}; }}"
        );
        assert_eq!(let_type(&src, "a"), Type::I64);
        let bad = format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; let a = match s {{ Circle(r) if r => 1, _ => 2 }}; }}"
        );
        assert_eq!(errors(&bad), [("E0403", "r")]);
    }

    #[test]
    fn unused_bindings_are_w0103() {
        let typo = format!(
            "{SHAPE}fun main() {{ let o: Option<Shape> = None; let a = match o {{ Some(Emtpy) => 1, None => 2 }}; }}"
        );
        let d = crate::check::test_util::diagnostics_or_warnings(&typo);
        assert_eq!(
            (d[0].code, d[0].message.as_str()),
            ("W0103", "unused binding `Emtpy`")
        );
        assert!(d[0].help.as_deref().unwrap().contains("check its spelling"));
        // Used only in the guard is used.
        ok(&format!(
            "{SHAPE}fun main() {{ let s = Shape.Empty; let a = match s {{ Circle(r) if r > 1.0 => 1, _ => 2 }}; }}"
        ));
    }

    #[test]
    fn discarded_and_diverging_matches() {
        let discarded = "fun main() { let n = 1; match n { _ => 1 } let x = 2; }";
        assert_eq!(errors(discarded), [("E0505", "match n { _ => 1 }")]);
        ok("fun f(n: i64): i64 { match n { _ => { return 1; } } }\nfun main() {}");
    }
}
