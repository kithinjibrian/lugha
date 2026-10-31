//! Loops, labels and jumps (spec §5, §6; `loop`, labels and `break` with a
//! value: v17, milestone 152).
//!
//! Every loop pushes a [`LoopScope`]. `break` and `continue` find theirs:
//! the innermost, or the one their label names (E0512 for none). A `loop`
//! has the type of its breaks' values, or `never` when no `break` targets
//! it, which makes it count as returning (§6).

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{Block, Expr, Ident};
use crate::span::Span;

/// One enclosing loop.
pub(super) struct LoopScope {
    pub label: Option<Ident>,
    /// For a `loop`, what its breaks give; `None` for `while` and `for`,
    /// which give nothing (E0514).
    value: Option<LoopValue>,
    /// True once a `break` targets this loop.
    broken: bool,
}

/// The value of a `loop`: the type it's expected to have, and the type its
/// breaks agreed on so far.
struct LoopValue {
    expect: Option<Expect>,
    ty: Option<Type>,
}

impl Checker {
    /// Checks the body of a `while`, `while let` or `for` loop, with the
    /// label its statement recorded in `loop_labels`.
    pub(super) fn loop_body(&mut self, body: &Block) {
        let label = self.loop_labels.remove(&body.span.start);
        self.enter_loop(label, None);
        self.block(body, None);
        self.loops.pop();
    }

    /// `[label:] loop { body }`: its breaks' type, `()` for plain breaks,
    /// or `never` when no `break` targets it.
    pub(super) fn loop_expr(
        &mut self,
        label: Option<&Ident>,
        body: &Block,
        expect: Option<&Expect>,
    ) -> Type {
        let value = LoopValue {
            expect: expect.cloned(),
            ty: None,
        };
        self.enter_loop(label.cloned(), Some(value));
        let body_ty = self.block(body, None);
        if !matches!(body_ty, Type::Unit | Type::Never | Type::Error) {
            let at = body.tail.as_ref().map_or(body.span, |t| t.span);
            self.report(errors::mismatch(Type::Unit, body_ty, at, None));
        }
        let scope = self.loops.pop().expect("pushed above");
        match (scope.broken, scope.value.and_then(|v| v.ty)) {
            (false, _) => Type::Never,
            (true, ty) => ty.unwrap_or(Type::Unit),
        }
    }

    /// Pushes a loop, reporting E0513 if an enclosing one has its label.
    fn enter_loop(&mut self, label: Option<Ident>, value: Option<LoopValue>) {
        if let Some(label) = &label
            && let Some(outer) = self
                .loops
                .iter()
                .find_map(|l| l.label.as_ref().filter(|o| o.name == label.name))
        {
            self.report(errors::label_in_use(label, outer.span));
        }
        self.loops.push(LoopScope {
            label,
            value,
            broken: false,
        });
    }

    /// `break [label] [value];` or `continue [label];` (`is_break`): finds
    /// the target and types the value. Returns the target's index in
    /// `loops`, or `None` when there is none (already reported).
    pub(super) fn jump(
        &mut self,
        is_break: bool,
        label: Option<&Ident>,
        value: Option<&Expr>,
        span: Span,
    ) -> Option<usize> {
        let keyword = if is_break { "break" } else { "continue" };
        let target = match label {
            None if self.loops.is_empty() => {
                self.report(errors::outside_loop(keyword, span));
                None
            }
            None => Some(self.loops.len() - 1),
            Some(label) => {
                let found = self
                    .loops
                    .iter()
                    .rposition(|l| l.label.as_ref().is_some_and(|o| o.name == label.name));
                if found.is_none() {
                    let in_closure = self.outer_labels.contains(&label.name);
                    self.report(errors::unknown_label(label, in_closure));
                }
                found
            }
        };
        let Some(index) = target else {
            value.iter().for_each(|v| _ = self.expr(v, None));
            return None;
        };
        if is_break {
            self.loops[index].broken = true;
            self.break_value(index, value, span);
        }
        Some(index)
    }

    /// Types a `break`'s value against the `loop` it leaves: the type its
    /// earlier breaks agreed on, else the loop's expected type, else the
    /// value's own. A plain `break;` gives `()`.
    fn break_value(&mut self, index: usize, value: Option<&Expr>, span: Span) {
        let Some(state) = self.loops[index].value.as_ref() else {
            if let Some(value) = value {
                self.report(errors::break_value_outside_loop(value.span));
                self.expr(value, None);
            }
            return;
        };
        let wanted = state
            .ty
            .clone()
            .map(Expect::of)
            .or_else(|| state.expect.clone());
        let ty = match (value, wanted) {
            (Some(value), Some(wanted)) => {
                self.expect_type(value, &wanted);
                wanted.ty
            }
            (Some(value), None) => self.expr(value, None),
            (None, Some(wanted)) => {
                if !matches!(wanted.ty, Type::Unit | Type::Error) {
                    self.report(errors::mismatch(wanted.ty.clone(), Type::Unit, span, None));
                }
                wanted.ty
            }
            (None, None) => Type::Unit,
        };
        let state = self.loops[index].value.as_mut().expect("checked above");
        // A value that never arrives (`break return 1`) decides nothing.
        if state.ty.is_none() && !matches!(ty, Type::Never | Type::Error) {
            state.ty = Some(ty);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    #[test]
    fn a_loop_without_break_never_ends_so_it_returns() {
        ok(
            "fun f(): i64 { let mut i = 0; loop { i += 1; if i > 3 { return i; } } }\nfun main() {}",
        );
        ok("fun f(): string { loop { } }\nfun main() {}");
    }

    #[test]
    fn a_loop_has_its_breaks_type() {
        assert_eq!(
            let_type("fun main() { let x = loop { break 5; }; }", "x"),
            Type::I64
        );
        assert_eq!(
            let_type("fun main() { let x = loop { break; }; }", "x"),
            Type::Unit
        );
        assert_eq!(
            let_type(
                "fun main() { let x: Option<u8> = loop { break None; }; let y = x; }",
                "y"
            ),
            Type::Option(Box::new(Type::U8))
        );
        assert_eq!(
            let_type(
                "fun main() { let x = loop { break 1; }; let y: u8 = 1; }",
                "x"
            ),
            Type::I64
        );
    }

    #[test]
    fn breaks_must_agree() {
        assert_eq!(
            errors("fun main() { let c = true; let x = loop { if c { break 1; } break \"a\"; }; }"),
            [("E0403", "\"a\"")]
        );
        assert_eq!(
            errors("fun main() { let c = true; let x = loop { if c { break 1; } break; }; }"),
            [("E0403", "break;")]
        );
    }

    #[test]
    fn labels_name_enclosing_loops() {
        ok(
            "fun main() { a: for i in 0..3 { b: while true { if i > 1 { break a; } continue a; } } }",
        );
        ok("fun main() { let v = a: loop { loop { break a 3; } }; }");
        // After `break`, a name no enclosing loop has is a value.
        assert_eq!(
            errors("fun main() { loop { break nowhere; } }"),
            [("E0301", "nowhere")]
        );
        assert_eq!(
            errors("fun main() { loop { continue nowhere; } }"),
            [("E0512", "nowhere")]
        );
    }

    #[test]
    fn labels_dont_reach_into_closures() {
        let src = "fun main() { a: loop { let f = fun() { loop { break a; } }; break; } }";
        assert_eq!(errors(src), [("E0512", "a")]);
        let d = crate::check::test_util::diagnostics(src);
        assert!(
            d[0].help.as_deref().is_some_and(|h| h.contains("closure")),
            "{d:?}"
        );
    }

    #[test]
    fn a_label_is_used_once_per_nest() {
        assert_eq!(
            errors("fun main() { a: loop { a: loop { break a; } break; } }"),
            [("E0513", "a")]
        );
        ok("fun main() { a: loop { break; } a: loop { break; } }");
    }

    #[test]
    fn only_loop_gives_a_value() {
        assert_eq!(
            errors("fun main() { while true { break 1; } }"),
            [("E0514", "1")]
        );
        assert_eq!(
            errors("fun main() { a: for i in 0..2 { loop { break a 2; } } }"),
            [("E0514", "2")]
        );
    }

    #[test]
    fn a_loop_body_has_no_value() {
        assert_eq!(errors("fun main() { loop { 1 } }"), [("E0403", "1")]);
    }
}
