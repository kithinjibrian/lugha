//! `out` parameters in the checker (v17, milestone 102, PRP-134): an
//! extern's `out` parameter takes `out place`, checked as an assignment to
//! the place of the parameter's type (E0443 for a missing or misplaced `out`).

use super::assign::PlaceUse;
use super::callbacks::CParam;
use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// E0443: `out` where no extern's `out` parameter takes it.
pub(super) fn misplaced(at: Span) -> Diagnostic {
    Diagnostic::error("E0443", "this parameter isn't `out`", at)
        .with_help("`out` is only for an extern's `out` parameters: pass the value itself")
}

/// E0409: a string as an `out` parameter.
pub(super) fn not_out(ty: &Type, at: Span) -> Diagnostic {
    Diagnostic::error("E0409", format!("`{ty}` can't be an `out` parameter"), at)
        .with_help("C can hand back numbers, `bool`, handles and extern structs through `out`")
}

impl Checker {
    /// The arguments of a call of `key` that infer and check as values: an
    /// extern's `out` parameters take `out place` instead (E0443 otherwise),
    /// checked here.
    pub(super) fn extern_inputs<'a>(
        &mut self,
        key: &str,
        args: &'a [Expr],
        params: Vec<Type>,
    ) -> Vec<(&'a Expr, Expect)> {
        // The caller writes neither `data` nor `release` (v16).
        let outs: Vec<bool> = self.c_params.get(key).map_or_else(Vec::new, |plan| {
            plan.iter()
                .filter(|c| !matches!(c, CParam::Data | CParam::Release))
                .map(|c| *c == CParam::Out)
                .collect()
        });
        let mut inputs = Vec::new();
        for (i, (arg, ty)) in args.iter().zip(params).enumerate() {
            let out = outs.get(i).copied().unwrap_or(false);
            match (&arg.kind, out) {
                (ExprKind::Out(place), true) => self.out_argument(arg, place, &ty),
                (ExprKind::Out(place), false) => {
                    self.report(misplaced(arg.span));
                    self.expr(place, None);
                }
                (_, true) => {
                    let d = Diagnostic::error("E0443", "this parameter is `out`", arg.span)
                        .with_help("C writes it: pass a place as `out x`");
                    self.report(d);
                    self.expr(arg, None);
                }
                (_, false) => inputs.push((arg, Expect::of(ty))),
            }
        }
        inputs
    }

    /// `out place` for an `out` parameter of type `ty`: the place is
    /// assigned after the call, so the assignment rules apply.
    fn out_argument(&mut self, arg: &Expr, place: &Expr, ty: &Type) {
        let mark = self.changes_mark();
        let found = self.expr(place, None);
        if self.check_place(place) && found != *ty && !matches!(found, Type::Error | Type::Never) {
            self.report(errors::mismatch(ty.clone(), found, place.span, None));
        }
        self.no_change_inside(&[place], PlaceUse::Assign, mark);
        self.record(arg, ty.clone());
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
