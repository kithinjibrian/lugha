//! E0510 (v1.2 §5): an assignment, `swap` or `mut self` call fixes its
//! place's address before the rest of the statement runs, so the rest may
//! not change anything that contains that place: the write would land in an
//! array that is no longer there.

use super::array::Path;
use super::assign::PlaceUse;
use super::{Checker, errors};
use crate::ast::Expr;
use crate::span::Span;

/// A place changed by an assignment, `swap` or `mut self` call.
#[derive(Debug, Clone)]
pub(super) struct Change {
    path: Path,
    span: Span,
    /// Reported once, even when it conflicts with several enclosing statements.
    reported: bool,
}

impl Checker {
    /// Notes that `span`'s place, at `path`, is changed.
    pub(super) fn changed(&mut self, path: Path, span: Span) {
        self.changes.push(Change {
            path,
            span,
            reported: false,
        });
    }

    /// Where the changes made by a statement's own parts will start.
    pub(super) fn changes_mark(&self) -> usize {
        self.changes.len()
    }

    /// E0510 for each change since `mark` to a place strictly containing one
    /// of `targets`; true if any was reported.
    pub(super) fn no_change_inside(
        &mut self,
        targets: &[&Expr],
        place_use: PlaceUse,
        mark: usize,
    ) -> bool {
        // A statement's own places (both of a `swap`) are not changes inside it.
        let own: Vec<Span> = targets.iter().map(|t| t.span).collect();
        let mut any = false;
        for target in targets {
            let Some(at) = self.place_path(target) else {
                continue;
            };
            for i in mark..self.changes.len() {
                let change = &self.changes[i];
                // Only a strict container moves the place: a same-depth or
                // sibling change leaves its address valid.
                let contains = change.path.overlaps(&at) && change.path.depth() < at.depth();
                if change.reported || own.contains(&change.span) || !contains {
                    continue;
                }
                let d = errors::changed_while_writing(
                    &change.path.describe(),
                    change.span,
                    &at.describe(),
                    target.span,
                    place_use,
                );
                self.changes[i].reported = true;
                self.report(d);
                any = true;
            }
        }
        any
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

    fn main(body: &str) -> String {
        format!("fun main() {{ {body} }}")
    }

    const XS: &str = "let mut xs = [1, 2]; let mut ys = [3]; let mut g = [[1], [2]];";
    const P: &str = "struct P { n: i64, items: i64[] }\nstruct V { x: i64 }\nimpl V { fun bump(mut self) { self.x += 1; } fun add(mut self, k: i64) { self.x += k; } }\n";

    #[test]
    fn assignments_reject_changes_to_what_contains_the_place() {
        let cases = [
            ("xs[0] = { xs.push(2); 9 };", "xs"),
            ("xs[{ xs.clear(); 0 }] = 1;", "xs"),
            ("g[0][0] = { g[1] = [5]; 1 };", "g[1]"),
            ("g[0][0] = { g = [[5]]; 1 };", "g"),
            ("xs[0] += { let p = xs.pop(); 1 };", "xs"),
        ];
        for (stmt, changed) in cases {
            assert_eq!(
                errors(&main(&format!("{XS} {stmt}"))),
                [("E0510", changed)],
                "{stmt}"
            );
        }
    }

    #[test]
    fn same_depth_sibling_and_shadowed_changes_are_fine() {
        ok(&main(&format!(
            "{XS} xs[0] = {{ xs[1] = 5; 9 }}; let i = 0; xs[i] = xs[i] + 1;"
        )));
        ok(&main(&format!(
            "{XS} xs = {{ xs.push(1); xs }}; xs.push(xs[0]);"
        )));
        ok(&main(&format!(
            "{XS} xs[0] = {{ let mut xs = [4]; xs.push(5); xs[1] }};"
        )));
        ok(&format!(
            "{P}fun main() {{ let mut p = P {{ n: 0, items: [1] }}; p.items[0] = {{ p.n += 1; 7 }}; }}"
        ));
    }

    #[test]
    fn swap_and_receivers_follow_the_same_rule() {
        assert_eq!(
            errors(&main(&format!(
                "{XS} swap(xs[0], ys[{{ xs.push(1); 0 }}]);"
            ))),
            [("E0510", "xs")]
        );
        ok(&main(&format!("{XS} swap(xs[0], ys[{{ xs[1] = 2; 0 }}]);")));
        assert_eq!(
            errors(&main(&format!("{XS} g[0].push({{ g.clear(); 1 }});"))),
            [("E0510", "g")]
        );
        ok(&main(&format!("{XS} g[0].push({{ g[0] = [7]; 1 }});")));
        let vs = "let mut vs = [V { x: 1 }]; let v = V { x: 2 };";
        assert_eq!(
            errors(&format!(
                "{P}fun main() {{ {vs} vs[{{ vs.push(v); 0 }}].bump(); }}"
            )),
            [("E0510", "vs")]
        );
        // E0510 alone, not E0508 as well.
        assert_eq!(
            errors(&format!(
                "{P}fun main() {{ {vs} vs[0].add({{ vs.clear(); 1 }}); }}"
            )),
            [("E0510", "vs")]
        );
    }

    #[test]
    fn messages_name_both_places_once() {
        let message = |stmt: &str| {
            let d = diagnostics(&main(&format!("{XS} {stmt}")));
            assert_eq!(d.len(), 1, "{stmt}: {d:?}");
            (d[0].message.clone(), d[0].labels.len(), d[0].help.is_some())
        };
        assert_eq!(
            message("xs[0] = { xs.push(2); 9 };"),
            (
                "cannot change `xs` while assigning to `xs[…]`".to_string(),
                1,
                true
            )
        );
        assert_eq!(
            message("swap(g[0][0], ys[{ g[1] = [2]; 0 }]);").0,
            "cannot change `g[…]` while swapping `g[…][…]`"
        );
        assert_eq!(
            message("g[0].push({ g.clear(); 1 });").0,
            "cannot change `g` while calling `push` on `g[…]`"
        );
        // Nested writes to parts of `g` both conflict with `g.clear()`: one report.
        message("g[0][0] = { g[1][0] = { g.clear(); 1 }; 2 };");
    }
}
