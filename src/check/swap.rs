//! `swap(a, b)` (v1.1 §5): exchanges the contents of two places of the same
//! type. Both follow the assignment place rules (spec §4); E0509 rejects
//! swapping a value with a part of itself.

use super::assign::PlaceUse;
use super::{Checker, Type, errors};
use crate::ast::Expr;

impl Checker {
    /// `swap(a, b)`: two places, the same type, neither inside the other.
    /// Always `void`, so an error here never cascades.
    pub(super) fn swap(&mut self, call: &Expr, args: &[Expr]) -> Type {
        let [a, b] = args else {
            self.report(errors::arity("swap", 2, args.len(), call.span));
            for arg in args {
                self.expr(arg, None);
            }
            return Type::Unit;
        };
        let mark = self.changes_mark();
        // Places, not values: neither is a store site, so nothing moves or copies.
        let (first, second) = (self.expr(a, None), self.expr(b, None));
        let a_ok = self.check_place_for(a, PlaceUse::Swap);
        let b_ok = self.check_place_for(b, PlaceUse::Swap);
        if !(a_ok && b_ok) {
            return Type::Unit;
        }
        self.no_change_inside(&[a, b], PlaceUse::Swap, mark);
        let failed = |t: &Type| matches!(t, Type::Error | Type::Never);
        if first != second {
            if !failed(&first) && !failed(&second) {
                self.report(errors::swap_types(first, second, b.span, a.span));
            }
            return Type::Unit;
        }
        // Equal depths are fine even when they might be the same place
        // (`swap(xs[i], xs[j])`, `swap(a, a)`): exchanging a place with
        // itself changes nothing. Only a strict prefix nests one in the other.
        if let (Some(pa), Some(pb)) = (self.place_path(a), self.place_path(b))
            && pa.overlaps(&pb)
            && pa.depth() != pb.depth()
        {
            let (whole, part) = if pa.depth() < pb.depth() {
                (a, b)
            } else {
                (b, a)
            };
            let text = if whole.id == a.id {
                pa.describe()
            } else {
                pb.describe()
            };
            self.report(errors::swap_with_part(&text, part.span, whole.span));
        }
        Type::Unit
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

    const P: &str = "struct P { a: i64, b: i64, kids: P[] }\n";

    #[test]
    fn places_of_the_same_type_swap() {
        ok("fun main() { let mut a = [1]; let mut b = [2, 3]; swap(a, b); swap(a, a); }");
        ok(
            "fun main() { let mut xs = [1, 2]; let i = 0; swap(xs[i], xs[1]); swap(xs[0], xs[0]); }",
        );
        ok(&format!(
            "{P}fun main() {{ let mut p = P {{ a: 1, b: 2, kids: [] }}; swap(p.a, p.b); let mut q = p; swap(p, q); }}"
        ));
        ok("fun main() { let mut t = true; let mut f = false; swap(t, f); }");
    }

    #[test]
    fn swap_needs_two_places_of_one_type() {
        assert_eq!(
            errors("fun main() { let mut a = 1; swap(a); }"),
            [("E0405", "swap(a)")]
        );
        assert_eq!(
            errors("fun main() { let mut a = 1; swap(a, a, a); }"),
            [("E0405", "swap(a, a, a)")]
        );
        assert_eq!(
            errors("fun main() { let mut a = 1; swap(a, 1); }"),
            [("E0502", "1")]
        );
        assert_eq!(
            errors("fun main() { let mut a = 1; let mut b = 1.5; swap(a, b); }"),
            [("E0403", "b")]
        );
        let d = diagnostics("fun main() { let mut a = 1; let mut b = 1.5; swap(a, b); }");
        assert_eq!(
            d[0].message,
            "`swap` expects two places of the same type, found i64 and f64"
        );
        assert_eq!(errors("fun main() { let x = swap; }"), [("E0406", "swap")]);
    }

    #[test]
    fn both_places_follow_the_assignment_rules() {
        let d = diagnostics("fun main() { let a = [1]; let mut b = [2]; swap(a, b); }");
        assert_eq!(
            (d[0].code, d[0].message.as_str()),
            ("E0501", "cannot swap `a`: it is not mutable")
        );
        assert_eq!(
            errors("fun main() { let mut a = 1; let b = 2; swap(a, b); }"),
            [("E0501", "b")]
        );
        let d =
            diagnostics("fun main() { let mut s = \"ab\"; let mut t = \"cd\"; swap(s[0], t[0]); }");
        assert_eq!(
            d.iter().map(|d| d.code).collect::<Vec<_>>(),
            ["E0506", "E0506"]
        );
        assert_eq!(
            d[0].message,
            "cannot swap inside a string: strings are immutable"
        );
        let d = diagnostics(
            "fun main() { let mut xs = [1, 2]; let mut y = 0; for x of xs { swap(xs[0], y); } }",
        );
        assert_eq!(
            (d[0].code, d[0].message.as_str()),
            ("E0507", "cannot swap `xs` while iterating over it")
        );
    }

    #[test]
    fn a_value_cannot_swap_with_a_part_of_itself() {
        let src = format!(
            "{P}fun main() {{ let mut n = P {{ a: 1, b: 2, kids: [] }}; swap(n, n.kids[0]); swap(n.kids[0].kids, n.kids); }}"
        );
        assert_eq!(
            errors(&src),
            [("E0509", "n.kids[0]"), ("E0509", "n.kids[0].kids")]
        );
        let d = diagnostics(&src);
        assert_eq!(d[0].message, "cannot swap `n` with a part of itself");
        let grid =
            "fun main() { let mut g = [[1], [2]]; swap(g[0], g[1]); swap(g[0][0], g[1][0]); }";
        ok(grid);
        assert_eq!(
            errors("fun main() { let mut g = [[1], [2]]; swap(g[0], g[1][0]); }"),
            [("E0403", "g[1][0]")]
        );
    }

    #[test]
    fn swap_is_reserved() {
        assert_eq!(errors("fun swap() {}\nfun main() {}"), [("E0302", "swap")]);
    }
}
