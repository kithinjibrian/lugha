//! Blocks, `if`, `while`, `for` over ranges, `break` and `continue` (spec §5).
//!
//! After any terminator (`br` for `break`/`continue`) lowering continues in a
//! fresh block with no predecessors, so whatever follows still produces valid
//! IR.

use inkwell::IntPredicate;
use inkwell::basic_block::BasicBlock;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED, unsupported};
use super::scope::Local;
use super::value::{Kind, Value, type_error};
use crate::ast::{Block, Expr, Ident};
use crate::span::Span;

/// Where `continue` and `break` jump in the innermost loop.
pub(super) struct Loop<'ctx> {
    next: BasicBlock<'ctx>,
    exit: BasicBlock<'ctx>,
}

impl<'ctx> Lowerer<'ctx> {
    /// Lowers a block in its own scope; its value is the tail's, or `Void`.
    pub(super) fn block(&mut self, block: &Block) -> Result<Value<'ctx>, CodegenError> {
        self.scopes.push();
        for stmt in &block.stmts {
            self.stmt(stmt)?;
        }
        let value = match &block.tail {
            Some(tail) => self.expr(tail)?,
            None => Value::Void,
        };
        self.scopes.pop();
        Ok(value)
    }

    /// `if cond { then } [else …]`; both branches must have the same kind,
    /// merged with a `phi` when non-void (spec §9).
    pub(super) fn if_expr(
        &mut self,
        span: Span,
        cond: &Expr,
        then: &Block,
        else_: Option<&Expr>,
    ) -> Result<Value<'ctx>, CodegenError> {
        let condition = self.expr(cond)?.bool(cond.span)?;
        let then_block = self.append("if.then");
        let merge = self.append("if.end");
        let Some(else_expr) = else_ else {
            self.builder
                .build_conditional_branch(condition, then_block, merge)
                .expect(POSITIONED);
            self.builder.position_at_end(then_block);
            self.block(then)?;
            self.branch(merge);
            self.builder.position_at_end(merge);
            return Ok(Value::Void);
        };
        let else_block = self.append("if.else");
        self.builder
            .build_conditional_branch(condition, then_block, else_block)
            .expect(POSITIONED);

        self.builder.position_at_end(then_block);
        let then_value = self.block(then)?;
        let then_end = self.current_block();
        self.branch(merge);

        self.builder.position_at_end(else_block);
        let else_value = self.expr(else_expr)?;
        let else_end = self.current_block();
        self.branch(merge);

        self.builder.position_at_end(merge);
        let (kind, a, b) = match (then_value, else_value) {
            (Value::Void, Value::Void) => return Ok(Value::Void),
            (Value::Int(a), Value::Int(b)) => (Kind::Int, a, b),
            (Value::Bool(a), Value::Bool(b)) => (Kind::Bool, a, b),
            _ => return Err(type_error(span)),
        };
        let phi = self
            .builder
            .build_phi(kind.llvm(self.context), "if")
            .expect(POSITIONED);
        phi.add_incoming(&[(&a, then_end), (&b, else_end)]);
        Ok(Value::of(kind, phi.as_basic_value().into_int_value()))
    }

    /// `while cond { body }`: the condition is checked before every iteration.
    pub(super) fn while_loop(&mut self, cond: &Expr, body: &Block) -> Result<(), CodegenError> {
        let cond_block = self.append("while.cond");
        let body_block = self.append("while.body");
        let exit = self.append("while.end");
        self.branch(cond_block);

        self.builder.position_at_end(cond_block);
        let condition = self.expr(cond)?.bool(cond.span)?;
        self.builder
            .build_conditional_branch(condition, body_block, exit)
            .expect(POSITIONED);

        self.builder.position_at_end(body_block);
        self.loops.push(Loop {
            next: cond_block,
            exit,
        });
        self.block(body)?;
        self.loops.pop();
        self.branch(cond_block);

        self.builder.position_at_end(exit);
        Ok(())
    }

    /// `for var in start..end { body }`, per the spec §5 desugaring: bounds are
    /// evaluated once, `var` is a fresh binding each iteration, and the step
    /// also runs on `continue`.
    pub(super) fn for_range(
        &mut self,
        var: &Ident,
        start: &Expr,
        end: &Expr,
        body: &Block,
    ) -> Result<(), CodegenError> {
        let first = self.expr(start)?.int(start.span)?;
        let limit = self.expr(end)?.int(end.span)?;
        let counter = self.entry_alloca(Kind::Int, "for.i");
        let end_slot = self.entry_alloca(Kind::Int, "for.end");
        self.builder.build_store(counter, first).expect(POSITIONED);
        self.builder.build_store(end_slot, limit).expect(POSITIONED);

        let cond_block = self.append("for.cond");
        let body_block = self.append("for.body");
        let step = self.append("for.step");
        let exit = self.append("for.end");
        self.branch(cond_block);

        self.builder.position_at_end(cond_block);
        let int = Kind::Int;
        let i = self.load(
            Local {
                ptr: counter,
                kind: int,
            },
            "i",
        );
        let bound = self.load(
            Local {
                ptr: end_slot,
                kind: int,
            },
            "end",
        );
        let more = self
            .builder
            .build_int_compare(IntPredicate::SLT, i, bound, "more")
            .expect(POSITIONED);
        self.builder
            .build_conditional_branch(more, body_block, exit)
            .expect(POSITIONED);

        self.builder.position_at_end(body_block);
        self.scopes.push();
        let var_slot = self.entry_alloca(Kind::Int, &var.name);
        self.builder.build_store(var_slot, i).expect(POSITIONED);
        self.scopes.declare(
            &var.name,
            Local {
                ptr: var_slot,
                kind: int,
            },
        );
        self.loops.push(Loop { next: step, exit });
        self.block(body)?;
        self.loops.pop();
        self.scopes.pop();
        self.branch(step);

        self.builder.position_at_end(step);
        let i = self.load(
            Local {
                ptr: counter,
                kind: int,
            },
            "i",
        );
        let one = self.context.i64_type().const_int(1, false);
        // Can't overflow: `i < end` held, so `i + 1 <= end`.
        let next = self
            .builder
            .build_int_add(i, one, "next")
            .expect(POSITIONED);
        self.builder.build_store(counter, next).expect(POSITIONED);
        self.branch(cond_block);

        self.builder.position_at_end(exit);
        Ok(())
    }

    /// `break` (`is_break`) or `continue` in the innermost loop.
    pub(super) fn jump(&mut self, is_break: bool, span: Span) -> Result<(), CodegenError> {
        let Some(target) = self.loops.last() else {
            let what = if is_break {
                "checking `break` outside loops"
            } else {
                "checking `continue` outside loops"
            };
            return Err(unsupported(what, 3, span));
        };
        let to = if is_break { target.exit } else { target.next };
        self.branch(to);
        // Code after the jump is unreachable but must still be valid IR.
        let dead = self.append("after.jump");
        self.builder.position_at_end(dead);
        Ok(())
    }

    pub(super) fn append(&self, name: &str) -> BasicBlock<'ctx> {
        let function = self
            .function
            .expect("blocks are appended inside a function");
        self.context.append_basic_block(function, name)
    }

    pub(super) fn current_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block().expect(POSITIONED)
    }

    fn branch(&self, to: BasicBlock<'ctx>) {
        self.builder
            .build_unconditional_branch(to)
            .expect(POSITIONED);
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::{ir, unsupported};

    #[test]
    fn conditions_must_be_booleans() {
        assert_eq!(
            unsupported("fun main() { if 5 { } }"),
            ("type checking", 3, "5")
        );
        assert_eq!(
            unsupported("fun main() { while 1 { } }"),
            ("type checking", 3, "1")
        );
    }

    #[test]
    fn if_branches_must_agree() {
        let src = "fun main() { let v = if true { 1 } else { false }; }";
        assert_eq!(
            unsupported(src),
            ("type checking", 3, "if true { 1 } else { false }")
        );
    }

    #[test]
    fn jumps_outside_loops_are_unsupported() {
        assert_eq!(
            unsupported("fun main() { break; }"),
            ("checking `break` outside loops", 3, "break;")
        );
        assert_eq!(
            unsupported("fun main() { continue; }"),
            ("checking `continue` outside loops", 3, "continue;")
        );
    }

    #[test]
    fn code_after_break_still_verifies() {
        // `ir` panics if the module fails verification.
        ir("fun main() { while true { break; let x = 1; x += 1; } }");
        ir(
            "fun main(): i32 { let mut n = 0; for i in 0..3 { if i == 1 { continue; n += 9; } n += i; } n }",
        );
    }
}
