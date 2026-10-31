//! Conditional patterns (v9 draft §5, milestone 33): `if let`, `while let`
//! and `let … else` test one pattern, typed as a `match` arm's, with no
//! exhaustiveness check; a `let … else` block must leave (E0429).

use super::expr::Expect;
use super::patterns::Bindings;
use super::{Binding, Checker, Type, errors};
use crate::ast::{Block, Expr, IfLet, Pattern};

impl Checker {
    /// `pattern` against `scrutinee`'s type, giving its bindings.
    fn tested(&mut self, pattern: &Pattern, scrutinee: &Expr) -> Bindings {
        let ty = self.value(scrutinee, None);
        let mut bindings = Bindings::new();
        // The ways it can match feed no exhaustiveness check here.
        self.arm_pattern(pattern, &ty, &mut bindings);
        bindings
    }

    /// Declares `bindings` in the current scope; `mut` ones are mutable.
    fn declare_bindings(&mut self, bindings: &Bindings) {
        for (name, ty, mutable) in bindings {
            let binding = match mutable {
                true => Binding::Let { mutable: true },
                false => Binding::Pattern,
            };
            self.declare(name, ty.clone(), binding);
        }
    }

    /// `if let pattern = scrutinee { then } [else …]`: like `if`, with the
    /// bindings in scope in `then` only.
    pub(super) fn if_let(&mut self, expr: &Expr, parts: &IfLet, expect: Option<&Expect>) -> Type {
        let (then, else_) = (&parts.then, parts.else_.as_deref());
        let bindings = self.tested(&parts.pattern, &parts.scrutinee);
        self.push();
        self.declare_bindings(&bindings);
        let then_ty = match else_ {
            Some(_) => self.block(then, expect),
            None => self.block(then, None),
        };
        self.unused(&bindings);
        self.pop();
        let Some(else_expr) = else_ else {
            return Type::Unit;
        };
        let else_ty = self.expr(else_expr, expect);
        self.join_branches(expr, then, then_ty, else_expr, else_ty)
    }

    /// `while let pattern = scrutinee { body }`: a loop body with the
    /// bindings in scope.
    pub(super) fn while_let(&mut self, pattern: &Pattern, scrutinee: &Expr, body: &Block) {
        let bindings = self.tested(pattern, scrutinee);
        self.push();
        self.declare_bindings(&bindings);
        self.loop_body(body);
        self.unused(&bindings);
        self.pop();
    }

    /// `let pattern = init else { … };`: the `else` block must leave
    /// (E0429); the bindings stay in scope for the rest of the block.
    pub(super) fn let_else(&mut self, pattern: &Pattern, init: &Expr, else_: Option<&Block>) {
        let ty = self.value(init, None);
        let mut bindings = Bindings::new();
        let pats = self.arm_pattern(pattern, &ty, &mut bindings);
        match else_ {
            Some(else_) => {
                let finished = self.block(else_, None);
                if !matches!(finished, Type::Never | Type::Error) {
                    self.report(errors::let_else_finishes(else_.span));
                }
            }
            // `let (a, b) = t;` (v14): the pattern must always match (E0433).
            None => {
                let pats: Vec<_> = pats.into_iter().map(|(p, _)| p).collect();
                self.always_matches(&pats, &ty, pattern.span);
            }
        }
        self.declare_bindings(&bindings);
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    fn codes(body: &str) -> Vec<&'static str> {
        let src = format!("fun f(): i64 {{ {body} 0 }}\nfun main() {{}}");
        let found = errors(&src).into_iter().map(|(c, _)| c);
        found.filter(|c| c.starts_with('E')).collect()
    }

    #[test]
    fn bindings_live_where_they_should() {
        ok("fun main() { let o = Some(1); if let Some(n) = o { println(n); } }");
        ok("fun main() { let mut xs = [1]; while let Some(x) = xs.pop() { println(x); } }");
        ok("fun f(o: Option<i64>): i64 { let Some(n) = o else { return 0; }; n }\nfun main() {}");
        assert_eq!(
            codes("let o = Some(1); if let Some(n) = o { } else { println(n); }"),
            ["E0301"]
        );
    }

    #[test]
    fn if_let_with_else_is_a_value() {
        let src =
            "fun main() { let o = Some(2); let v = if let Some(n) = o { n * 2 } else { 0 }; }";
        assert_eq!(let_type(src, "v"), Type::I64);
        ok("fun main() { let o = Some(2); if let Some(mut n) | Some(mut n) = o { n += 1; } }");
        assert_eq!(
            codes("let o = Some(1); if let Some(\"x\") = o { }"),
            ["E0403"]
        );
    }

    #[test]
    fn a_let_else_block_must_leave() {
        assert_eq!(
            codes("let o = Some(1); let Some(n) = o else { println(1); };"),
            ["E0429"]
        );
        ok(
            "fun f(): i64 { let o = Some(1); let Some(n) = o else { panic(\"no\"); }; n }\nfun main() {}",
        );
        ok(
            "fun f(): i64 { let o = Some(1); let Some(n) = o else { return 1; }; n }\nfun main() {}",
        );
        ok("fun main() { for x of [1] { let 0 = x else { continue; }; } }");
    }
}
