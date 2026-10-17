//! `out` parameters in the checker (v17, milestone 102, PRP-134): an
//! extern's `out` parameter takes `out place`, checked as an assignment to
//! the place of the parameter's type (E0443 for a missing or misplaced `out`).

use super::assign::PlaceUse;
use super::callbacks::CParam;
use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind, PassMode};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0443: a mode where the extern's parameter has another, or none.
pub(super) fn misplaced(at: Span) -> Diagnostic {
    Diagnostic::error("E0443", "this parameter isn't `out`, `ptr` or `mut`", at)
        .with_help("`out`, `ptr` and `mut` are only for an extern's parameters of that mode: pass the value itself")
}

/// E0409: a string passed by address.
pub(super) fn not_out(ty: &Type, at: Span) -> Diagnostic {
    Diagnostic::error("E0409", format!("`{ty}` can't be passed by address"), at)
        .with_help("`out`, `ptr` and `mut` take numbers, `bool`, handles and extern structs")
}

/// E0444: a union literal writing other than one field.
pub(super) fn union_literal(name: &str, at: Span) -> Diagnostic {
    Diagnostic::error(
        "E0444",
        format!("a `{name}` literal writes exactly one field"),
        at,
    )
    .with_help("a union holds one field at a time: write the one you set, `U { i64: 0 }`")
}

/// The mode `plan` gives each written parameter (`None` for a value).
fn modes(plan: Option<&Vec<CParam>>) -> Vec<Option<PassMode>> {
    plan.map_or_else(Vec::new, |plan| {
        plan.iter()
            .filter(|c| !matches!(c, CParam::Data | CParam::Release))
            .map(|c| match c {
                CParam::Pointer(m) => Some(*m),
                CParam::Elements { mutable: true } => Some(PassMode::Mut),
                _ => None,
            })
            .collect()
    })
}

impl Checker {
    /// The arguments of a call of `key` that infer and check as values; an
    /// extern's `out`/`ptr`/`mut` parameters take an argument written with
    /// that mode (E0443 otherwise), checked here.
    pub(super) fn extern_inputs<'a>(
        &mut self,
        key: &str,
        args: &'a [Expr],
        params: Vec<Type>,
    ) -> Vec<(&'a Expr, Expect)> {
        let modes = modes(self.c_params.get(key));
        let mut inputs = Vec::new();
        for (i, (arg, ty)) in args.iter().zip(params).enumerate() {
            let wanted = modes.get(i).copied().flatten();
            match (&arg.kind, wanted) {
                (ExprKind::Pass(given, inner), Some(wanted)) if *given == wanted => match wanted {
                    PassMode::Ptr => {
                        self.expect_type(inner, &Expect::of(ty.clone()));
                        self.record(arg, ty);
                    }
                    PassMode::Out | PassMode::Mut => self.out_argument(arg, inner, &ty),
                },
                (ExprKind::Pass(_, inner), None) => {
                    self.report(misplaced(arg.span));
                    self.expr(inner, None);
                }
                (_, Some(wanted)) => {
                    let message = format!("this parameter is `{}`", wanted.word());
                    let help = format!("write `{} x` for it", wanted.word());
                    self.report(Diagnostic::error("E0443", message, arg.span).with_help(help));
                    let inner = match &arg.kind {
                        ExprKind::Pass(_, inner) => inner,
                        _ => arg,
                    };
                    self.expr(inner, None);
                }
                (_, None) => inputs.push((arg, Expect::of(ty))),
            }
        }
        inputs
    }

    /// `out place` or `mut place` for a parameter of type `ty`: the place
    /// is assigned after the call, so the assignment rules apply.
    fn out_argument(&mut self, arg: &Expr, place: &Expr, ty: &Type) {
        let mark = self.changes_mark();
        let found = self.expr(place, None);
        if self.check_place(place) && found != *ty && !matches!(found, Type::Error | Type::Never) {
            self.report(errors::mismatch(ty.clone(), found, place.span, None));
        }
        self.no_change_inside(&[place], PlaceUse::Assign, mark);
        self.record(arg, ty.clone());
    }

    /// The C-boundary rule for an extern's parameter or result of type `ty`
    /// with `mode` (v17): `Some(None)` when allowed here, `Some(Some(d))`
    /// when not, `None` when the v16 rules decide.
    pub(super) fn pointer_param(
        &self,
        ty: &Type,
        mode: Option<PassMode>,
        param: bool,
        at: Span,
    ) -> Option<Option<Diagnostic>> {
        let elements_ok = |t: &Type| {
            t.is_numeric()
                || matches!(t, Type::Bool | Type::Handle(_))
                || matches!(t, Type::Struct(k, _) if self.c_structs.contains(k))
        };
        match ty {
            // `xs: T[]` and `mut xs: T[]` (milestone 104).
            Type::Array(element) if param => Some(
                (!(elements_ok(element) && matches!(mode, None | Some(PassMode::Mut)))).then(|| {
                    Diagnostic::error("E0409", "this array can't cross the C boundary", at).with_help(
                        "an extern takes `xs: T[]` or `mut xs: T[]` of numbers, `bool`, handles or extern structs",
                    )
                }),
            ),
            Type::String | Type::Option(_) if mode.is_some() && !matches!(ty, Type::Option(inner) if matches!(**inner, Type::Handle(_))) => {
                Some(Some(not_out(ty, at)))
            }
            // A union only crosses by address or inside an array (milestone 104).
            Type::Struct(key, _) if mode.is_none() && self.holds_union(key) => Some(Some(
                Diagnostic::error("E0409", "a union can't cross the C boundary by value", at)
                    .with_help("pass it, or a struct holding one, through `ptr`, `mut`, `out` or an array"),
            )),
            _ => None,
        }
    }

    /// True if extern struct or union `key` is a union or holds one.
    pub(super) fn holds_union(&self, key: &str) -> bool {
        if self.c_unions.contains(key) {
            return true;
        }
        self.structs.get(key).is_some_and(|s| {
            s.fields
                .iter()
                .any(|(_, t)| matches!(t, Type::Struct(k, _) if k != key && self.holds_union(k)))
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const C: &str = "extern type H;\nextern struct Pair { a: i32, b: i32 }\n\
        extern fun fuel(out n: u64): bool;\n\
        extern fun open(out h: H, out maybe: Option<H>, out p: Pair);\n";

    /// The errors of `C` followed by `body`.
    fn found(body: &str) -> Vec<(&'static str, String)> {
        let src = format!("{C}{body}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    #[test]
    fn out_arguments_are_places_of_the_parameters_type() {
        ok(&format!(
            "{C}struct S {{ n: u64, xs: u64[] }}\n\
             fun main() {{\n\
                 let mut n: u64 = 0;\n\
                 let ok = fuel(out n);\n\
                 let mut s = S {{ n: 0, xs: [0, 0] }};\n\
                 fuel(out s.n);\n\
                 fuel(out s.xs[1]);\n\
                 let mut h: H = make();\n\
                 let mut m: Option<H> = None;\n\
                 let mut p = Pair {{ a: 0, b: 0 }};\n\
                 open(out h, out m, out p);\n\
             }}\n\
             extern fun make(): H;"
        ));
        assert_eq!(
            found("fun main() { let n: u64 = 0; fuel(out n); }"),
            [("E0501", "n".to_string())]
        );
        assert_eq!(
            found("fun main() { let mut n: i64 = 0; fuel(out n); }")[0].0,
            "E0403"
        );
        assert_eq!(
            found("fun main() { let n: u64 = 1; fuel(out n + 1); }")[0].0,
            "E0502"
        );
    }

    #[test]
    fn out_is_written_where_and_only_where_it_belongs() {
        assert_eq!(
            found("fun main() { let mut n: u64 = 0; fuel(n); }"),
            [("E0443", "n".to_string())]
        );
        assert_eq!(
            found("extern fun put(n: u64);\nfun main() { let mut n: u64 = 0; put(out n); }"),
            [("E0443", "out n".to_string())]
        );
        let twice =
            "fun twice(n: u64): u64 = n * 2;\nfun main() { let mut n: u64 = 0; twice(out n); }";
        assert_eq!(found(twice)[0].0, "E0443");
    }

    #[test]
    fn strings_cant_be_out_v17() {
        assert_eq!(
            errors("extern fun name(out s: string);\nfun main() {}"),
            [("E0409", "string")]
        );
    }
}
