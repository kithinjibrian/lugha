//! Control-flow rules: missing returns, `break`/`continue` outside loops,
//! discarded block-like values, stray semicolons and unreachable code
//! (spec §5, §6).

use super::{Checker, Type, errors};
use crate::ast::{Block, ExprKind, FunDecl, Stmt, StmtKind};
use crate::span::Span;

/// Divergence within one block, for W0101.
#[derive(Default)]
pub(super) struct Flow {
    /// The first statement that diverged.
    cause: Option<Span>,
    warned: bool,
}

impl Checker {
    /// Called before each statement (and the tail) of a block: warns once
    /// about the first unreachable one, then marks the rest as dead code.
    pub(super) fn reachable(&mut self, flow: &mut Flow, span: Span) {
        if let Some(cause) = flow.cause
            && !flow.warned
        {
            if !self.dead {
                self.report(errors::unreachable(span, cause));
            }
            flow.warned = true;
            self.dead = true;
        }
    }

    /// Called after each statement: records divergence and checks that a
    /// block-like statement mid-block has no value to discard (spec §5).
    pub(super) fn after_statement(&mut self, flow: &mut Flow, stmt: &Stmt, diverges: bool) {
        if diverges && flow.cause.is_none() {
            flow.cause = Some(stmt.span);
        }
        if let StmtKind::Expr {
            expr,
            semicolon: false,
        } = &stmt.kind
        {
            let ty = self.types[expr.id.0 as usize].unwrap_or(Type::Error);
            if !matches!(ty, Type::Void | Type::Never | Type::Error) {
                let what = if matches!(expr.kind, ExprKind::If { .. }) {
                    "`if`"
                } else {
                    "block"
                };
                self.report(errors::discarded(what, ty, expr.span));
            }
        }
    }

    /// `break`/`continue` must be inside a loop.
    pub(super) fn jump(&mut self, keyword: &str, span: Span) {
        if self.loops == 0 {
            self.report(errors::outside_loop(keyword, span));
        }
    }

    /// A non-void function whose body has type `void` can fall off its end (§6).
    pub(super) fn missing_return(&mut self, f: &FunDecl, ret: Type) {
        let has_loop = f
            .body
            .stmts
            .iter()
            .any(|s| matches!(s.kind, StmtKind::While { .. } | StmtKind::For { .. }));
        let mut d = errors::missing_return(&f.name.name, ret, f.name.span, has_loop);
        if let Some(semicolon) = self.stray_semicolon(&f.body) {
            d = errors::with_stray_semicolon(d, semicolon);
        }
        self.report(d);
    }

    /// The `;` that turned a block's would-be result into a statement: the
    /// block has no tail and ends with `expr;` where `expr` has a value (§5).
    pub(super) fn stray_semicolon(&self, block: &Block) -> Option<Span> {
        if block.tail.is_some() {
            return None;
        }
        let last = block.stmts.last()?;
        let StmtKind::Expr {
            expr,
            semicolon: true,
        } = &last.kind
        else {
            return None;
        };
        let ty = self.types[expr.id.0 as usize]?;
        // The parser ends a `semicolon: true` statement's span with its `;`.
        let has_value = !matches!(ty, Type::Void | Type::Never | Type::Error);
        has_value.then(|| Span::new(last.span.end - 1, last.span.end))
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok, warnings};

    #[test]
    fn missing_returns_are_e0503() {
        assert_eq!(
            errors("fun f(c: bool): i64 { if c { return 1; } }\nfun main() {}"),
            [("E0503", "f")]
        );
        assert_eq!(
            errors("fun g(): i64 { while true { return 1; } }\nfun main() {}"),
            [("E0503", "g")]
        );
        ok(
            "fun a(): i64 { 1 }\nfun b(c: bool): i64 { if c { return 1; } else { return 2; } }\n\
            fun d(c: bool): i64 { if c { return 1; } 2 }\nfun main() {}",
        );
    }

    #[test]
    fn stray_semicolons_are_pointed_out() {
        let src = "fun sq(x: i32): i32 { x * x; }\nfun main() {}";
        assert_eq!(errors(src), [("E0503", "sq")]);
        let d = crate::check::test_util::diagnostics(src);
        assert!(
            d[0].labels
                .iter()
                .any(|l| &src[l.span.start..l.span.end] == ";"
                    && l.message == "remove this semicolon")
        );
        let block = "fun main() { let v: i32 = { 1 + 1; }; }";
        assert_eq!(errors(block), [("E0407", "{ 1 + 1; }")]);
        let d = crate::check::test_util::diagnostics(block);
        assert!(
            d[0].labels
                .iter()
                .any(|l| l.message == "remove this semicolon"),
            "{d:?}"
        );
    }

    #[test]
    fn loops_never_definitely_return_so_the_help_says_so() {
        let d = crate::check::test_util::diagnostics(
            "fun g(): i64 { while true { return 1; } }\nfun main() {}",
        );
        assert!(
            d[0].help
                .as_deref()
                .is_some_and(|h| h.contains("panic(\"unreachable\")")),
            "{d:?}"
        );
    }

    #[test]
    fn jumps_outside_loops_are_e0504() {
        assert_eq!(errors("fun main() { break; }"), [("E0504", "break;")]);
        assert_eq!(errors("fun main() { continue; }"), [("E0504", "continue;")]);
        ok("fun main() { while true { if true { break; } for i in 0..2 { continue; } } }");
    }

    #[test]
    fn discarded_block_like_values_are_e0505() {
        let src = "fun main() { let c = true; if c { 1 } else { 2 } let x = 3; }";
        assert_eq!(errors(src), [("E0505", "if c { 1 } else { 2 }")]);
        ok("fun main() { let c = true; if c { 1 } else { 2 }; if c { } { } let x = 3; }");
        ok("fun main(): i32 { let c = true; if c { 1 } else { 2 } }");
    }

    #[test]
    fn unreachable_code_warns_once_per_block() {
        let src = "fun main(): i32 { return 3; let x = 1; let y = 2; }";
        assert_eq!(warnings(src), [("W0101", "let x = 1;")]);
        assert_eq!(
            warnings("fun main() { while true { break; let z = 1; } }"),
            [("W0101", "let z = 1;")]
        );
        let both = "fun f(c: bool): i64 { if c { return 1; } else { return 2; } 3 }\nfun main() {}";
        assert_eq!(warnings(both), [("W0101", "3")]);
        // Nested dead code gives no second warning.
        assert_eq!(
            warnings("fun main() { return; { let a = 1; return; let b = 2; } }"),
            [("W0101", "{ let a = 1; return; let b = 2; }")]
        );
        assert_eq!(warnings("fun main() { let x = 1; }"), []);
    }
}
