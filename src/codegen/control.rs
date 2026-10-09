//! Blocks, `if`, `while`, `for` over ranges, `break` and `continue` (spec §5).
//!
//! After any terminator (`br` for `break`/`continue`, `ret`) lowering
//! continues in a fresh block with no predecessors, so whatever follows still
//! produces valid IR. A branch that never finishes ends in `unreachable` and
//! adds no phi edge (spec §9).

use inkwell::IntPredicate;
use inkwell::basic_block::BasicBlock;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::{Value, int_type, is_signed, llvm_type};
use crate::ast::{Block, Expr, Ident};
use crate::check::Type;

/// Where `continue` and `break` jump in the innermost loop.
pub(super) struct Loop<'ctx> {
    next: BasicBlock<'ctx>,
    exit: BasicBlock<'ctx>,
}

impl<'ctx> Lowerer<'ctx> {
    /// A block in its own scope: the tail's value, `Void`, or `Never` once a
    /// statement diverges. Statements after that still lower, into dead blocks.
    pub(super) fn block(&mut self, block: &Block) -> Result<Value<'ctx>, CodegenError> {
        self.scopes.push();
        let mut diverged = false;
        for stmt in &block.stmts {
            diverged |= self.stmt(stmt)?;
        }
        let value = match &block.tail {
            Some(tail) => self.expr(tail)?,
            None => Value::Void,
        };
        self.scopes.pop();
        Ok(if diverged { Value::Never } else { value })
    }

    /// `if cond { then } [else …]`, merged with a `phi` of the `if`'s type.
    pub(super) fn if_expr(
        &mut self,
        expr: &Expr,
        cond: &Expr,
        then: &Block,
        else_: Option<&Expr>,
    ) -> Result<Value<'ctx>, CodegenError> {
        let condition = self.get(cond, &Type::Bool)?.into_int_value();
        let then_block = self.append("if.then");
        let merge = self.append("if.end");
        let Some(else_expr) = else_ else {
            self.builder
                .build_conditional_branch(condition, then_block, merge)
                .expect(POSITIONED);
            self.builder.position_at_end(then_block);
            let value = self.block(then)?;
            self.finish_branch(value, merge);
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
        self.finish_branch(then_value, merge);

        self.builder.position_at_end(else_block);
        let else_value = self.expr(else_expr)?;
        let else_end = self.current_block();
        self.finish_branch(else_value, merge);

        self.builder.position_at_end(merge);
        Ok(match (then_value, else_value) {
            (Value::Never, Value::Never) => Value::Never,
            // The other branch's block is the merge block's only predecessor.
            (Value::Never, value) | (value, Value::Never) => value,
            (Value::Val(a), Value::Val(b)) => {
                let phi = self
                    .builder
                    .build_phi(llvm_type(self.context, &self.ty(expr)), "if")
                    .expect(POSITIONED);
                phi.add_incoming(&[(&a, then_end), (&b, else_end)]);
                Value::Val(phi.as_basic_value())
            }
            _ => Value::Void,
        })
    }

    /// Ends an `if` branch: jump to `merge`, or `unreachable` if it never finishes.
    fn finish_branch(&self, value: Value<'ctx>, merge: BasicBlock<'ctx>) {
        if matches!(value, Value::Never) {
            self.builder.build_unreachable().expect(POSITIONED);
        } else {
            self.branch(merge);
        }
    }

    /// `while cond { body }`: the condition is checked before every iteration.
    pub(super) fn while_loop(&mut self, cond: &Expr, body: &Block) -> Result<(), CodegenError> {
        let cond_block = self.append("while.cond");
        let body_block = self.append("while.body");
        let exit = self.append("while.end");
        self.branch(cond_block);

        self.builder.position_at_end(cond_block);
        let condition = self.get(cond, &Type::Bool)?.into_int_value();
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

    /// `for var in start..end { body }`, per the spec §5 desugaring, at the
    /// range's integer type: bounds evaluated once, a fresh `var` each
    /// iteration, and the step also runs on `continue`.
    pub(super) fn for_range(
        &mut self,
        var: &Ident,
        start: &Expr,
        end: &Expr,
        body: &Block,
    ) -> Result<(), CodegenError> {
        let ty = [self.ty(start), self.ty(end)]
            .into_iter()
            .find(|t| t.is_integer())
            .unwrap_or(Type::I64);
        let first = self.get(start, &ty)?;
        let limit = self.get(end, &ty)?;
        let counter = Local {
            ptr: self.entry_alloca(&ty, "for.i"),
            ty: ty.clone(),
        };
        let end_slot = Local {
            ptr: self.entry_alloca(&ty, "for.end"),
            ty: ty.clone(),
        };
        self.builder
            .build_store(counter.ptr, first)
            .expect(POSITIONED);
        self.builder
            .build_store(end_slot.ptr, limit)
            .expect(POSITIONED);

        let cond_block = self.append("for.cond");
        let body_block = self.append("for.body");
        let step = self.append("for.step");
        let exit = self.append("for.end");
        self.branch(cond_block);

        self.builder.position_at_end(cond_block);
        let i = self.load(&counter, "i").into_int_value();
        let bound = self.load(&end_slot, "end").into_int_value();
        let less = if is_signed(&ty) {
            IntPredicate::SLT
        } else {
            IntPredicate::ULT
        };
        let more = self
            .builder
            .build_int_compare(less, i, bound, "more")
            .expect(POSITIONED);
        self.builder
            .build_conditional_branch(more, body_block, exit)
            .expect(POSITIONED);

        self.builder.position_at_end(body_block);
        self.scopes.push();
        let var_slot = Local {
            ptr: self.entry_alloca(&ty, &var.name),
            ty: ty.clone(),
        };
        self.builder.build_store(var_slot.ptr, i).expect(POSITIONED);
        self.scopes.declare(&var.name, var_slot);
        self.loops.push(Loop { next: step, exit });
        self.block(body)?;
        self.loops.pop();
        self.scopes.pop();
        self.branch(step);

        self.builder.position_at_end(step);
        let i = self.load(&counter, "i").into_int_value();
        // Can't overflow: `i < end` held, so `i + 1 <= end`.
        let next =
            self.builder
                .build_int_add(i, int_type(self.context, &ty).const_int(1, false), "next");
        self.builder
            .build_store(counter.ptr, next.expect(POSITIONED))
            .expect(POSITIONED);
        self.branch(cond_block);

        self.builder.position_at_end(exit);
        Ok(())
    }

    /// `break` (`is_break`) or `continue` in the innermost loop.
    pub(super) fn jump(&mut self, is_break: bool) {
        let target = self
            .loops
            .last()
            .expect("checked: E0504 rejects jumps outside loops");
        let to = if is_break { target.exit } else { target.next };
        self.branch(to);
        self.start_dead_block("after.jump");
    }

    /// Continues lowering in a fresh block nothing jumps to, after a terminator.
    pub(super) fn start_dead_block(&mut self, name: &str) {
        let dead = self.append(name);
        self.builder.position_at_end(dead);
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
    use crate::codegen::test_util::ir;

    #[test]
    fn code_after_jumps_still_verifies() {
        // `ir` panics if the module fails verification.
        ir("fun main() { while true { break; let x = 1; } }");
        ir(
            "fun main(): i32 { let n: i32 = 3; let mut t: i32 = 0; for i in 0..n { if i == 1 { continue; } t += i; } t }",
        );
    }

    #[test]
    fn a_returning_branch_adds_no_phi_edge() {
        let ir = ir("fun abs(x: i64): i64 { if x < 0 { return -x; } else { x } }\nfun main() {}");
        let body = &ir[ir.find("define i64 @lugha_fn_abs").unwrap()..];
        let body = &body[..body.find("\n}").unwrap()];
        assert!(!body.contains("phi"), "{body}");
        assert!(body.contains("unreachable"), "{body}");
    }

    #[test]
    fn u8_ranges_compare_unsigned() {
        let ir = ir("fun main() { let n: u8 = 200; for i in 0..n { } }");
        assert!(ir.contains("icmp ult i8"), "{ir}");
    }
}
