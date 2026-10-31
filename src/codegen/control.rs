//! Blocks, `if`, `while`, `for` over ranges, `break` and `continue` (spec §5).
//!
//! After any terminator (`br` for `break`/`continue`, `ret`) lowering
//! continues in a fresh block with no predecessors, so whatever follows still
//! produces valid IR. A branch that never finishes ends in `unreachable` and
//! adds no phi edge (spec §9).

use inkwell::IntPredicate;
use inkwell::basic_block::BasicBlock;
use inkwell::values::PointerValue;

use super::CodegenError;
use super::hoist::Head;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::{Value, int_type, is_signed, llvm_type};
use crate::ast::{Block, Expr, Ident};
use crate::check::Type;

/// Where `continue` and `break` jump in an enclosing loop.
pub(super) struct Loop<'ctx> {
    pub next: BasicBlock<'ctx>,
    pub exit: BasicBlock<'ctx>,
    /// How many blocks enclosed the loop: a jump runs the defers of those
    /// inside it (milestone 151).
    pub defers: usize,
    /// The loop's label, which `break label` and `continue label` name
    /// (milestone 152).
    pub label: Option<String>,
    /// A `loop`'s result slot and type, which `break value` stores into.
    pub result: Option<(PointerValue<'ctx>, Type)>,
}

impl<'ctx> Lowerer<'ctx> {
    /// A block in its own scope: the tail's value, `Void`, or `Never` once a
    /// statement diverges. Statements after that still lower, into dead blocks.
    pub(super) fn block(&mut self, block: &Block) -> Result<Value<'ctx>, CodegenError> {
        self.block_inner(block, false)
    }

    /// A block; for a function body (`returns`), the tail is lowered as a
    /// returned value while its locals are still in scope (spec §4).
    pub(super) fn block_inner(
        &mut self,
        block: &Block,
        returns: bool,
    ) -> Result<Value<'ctx>, CodegenError> {
        self.scopes.push();
        self.defers.push(Vec::new());
        let mut diverged = false;
        for stmt in &block.stmts {
            diverged |= self.stmt(stmt)?;
        }
        let value = match &block.tail {
            Some(tail)
                if returns && !matches!(self.ty(tail), Type::Never) && self.ret.is_some() =>
            {
                let ty = self.ty(tail);
                Value::Val(self.value_for_return(tail, &ty)?)
            }
            Some(tail) => self.expr(tail)?,
            None => Value::Void,
        };
        // The tail is computed, then the block's defers run (milestone 151).
        let depth = self.defers.len() - 1;
        let value = match (value, &block.tail) {
            (Value::Val(v), Some(tail)) => Value::Val(self.hold(v, &self.ty(tail), depth)),
            (value, _) => value,
        };
        self.run_defers(depth)?;
        self.defers.pop();
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
    pub(super) fn finish_branch(&self, value: Value<'ctx>, merge: BasicBlock<'ctx>) {
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
        let hoists = self.hoist_loop(&[Head::Read(cond)], body)?;
        self.branch(cond_block);

        self.builder.position_at_end(cond_block);
        // A green thread's loop gives the others a turn now and then (v17).
        self.preempt_point();
        let condition = self.get(cond, &Type::Bool)?.into_int_value();
        self.builder
            .build_conditional_branch(condition, body_block, exit)
            .expect(POSITIONED);

        self.builder.position_at_end(body_block);
        self.push_loop(body, cond_block, exit);
        self.block(body)?;
        self.loops.pop();
        self.branch(cond_block);

        self.builder.position_at_end(exit);
        self.end_hoists(hoists);
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
            borrowed: false,
        };
        let end_slot = Local {
            ptr: self.entry_alloca(&ty, "for.end"),
            ty: ty.clone(),
            borrowed: false,
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
        let hoists = self.hoist_loop(&[Head::Binds(&var.name)], body)?;
        self.branch(cond_block);

        self.builder.position_at_end(cond_block);
        // A green thread's loop gives the others a turn now and then (v17).
        self.preempt_point();
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
            borrowed: false,
        };
        self.builder.build_store(var_slot.ptr, i).expect(POSITIONED);
        self.scopes.declare(&var.name, var_slot);
        self.push_loop(body, step, exit);
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
        self.end_hoists(hoists);
        Ok(())
    }

    /// Enters the loop whose body is `body`: `continue` goes to `next`,
    /// `break` to `exit`; its label is the one its statement recorded.
    pub(super) fn push_loop(
        &mut self,
        body: &Block,
        next: BasicBlock<'ctx>,
        exit: BasicBlock<'ctx>,
    ) {
        let label = self.loop_labels.remove(&body.span.start);
        self.loops.push(Loop {
            next,
            exit,
            defers: self.defers.len(),
            label,
            result: None,
        });
    }

    /// `[label:] loop { body }` (milestone 152): the value its breaks
    /// store, `Void`, or `Never` when no `break` leaves it.
    pub(super) fn loop_expr(
        &mut self,
        expr: &Expr,
        label: Option<&Ident>,
        body: &Block,
    ) -> Result<Value<'ctx>, CodegenError> {
        let ty = self.ty(expr);
        let result = match &ty {
            Type::Never | Type::Unit | Type::Error => None,
            ty => Some((self.entry_alloca(ty, "loop.value"), ty.clone())),
        };
        let head = self.append("loop.body");
        let exit = self.append("loop.end");
        let hoists = self.hoist_loop(&[], body)?;
        self.branch(head);

        self.builder.position_at_end(head);
        // A green thread's loop gives the others a turn now and then (v17).
        self.preempt_point();
        self.loops.push(Loop {
            next: head,
            exit,
            defers: self.defers.len(),
            label: label.map(|l| l.name.clone()),
            result: result.clone(),
        });
        self.block(body)?;
        self.loops.pop();
        self.branch(head);

        self.builder.position_at_end(exit);
        self.end_hoists(hoists);
        Ok(match (ty, result) {
            // Nothing breaks out: the exit has no way in.
            (Type::Never, _) => {
                self.builder.build_unreachable().expect(POSITIONED);
                self.start_dead_block("after.loop");
                Value::Never
            }
            (_, Some((ptr, ty))) => {
                let local = Local {
                    ptr,
                    ty,
                    borrowed: false,
                };
                Value::Val(self.load(&local, "loop.value"))
            }
            _ => Value::Void,
        })
    }

    /// `break [label] [value]` (`is_break`) or `continue [label]`: the
    /// value is stored first, then the defers of the blocks left run.
    pub(super) fn jump(
        &mut self,
        is_break: bool,
        label: Option<&Ident>,
        value: Option<&Expr>,
    ) -> Result<(), CodegenError> {
        let index = match label {
            Some(label) => self
                .loops
                .iter()
                .rposition(|l| l.label.as_deref() == Some(label.name.as_str())),
            None => self.loops.len().checked_sub(1),
        }
        .expect("checked: E0504 and E0512 reject jumps with no loop");
        if let (Some(value), Some((ptr, ty))) = (value, self.loops[index].result.clone()) {
            let stored = self.value_for_store(value, &ty)?;
            self.builder.build_store(ptr, stored).expect(POSITIONED);
        } else if let Some(value) = value {
            // A `()` value has nothing to store, but its effects still run.
            if matches!(self.expr(value)?, Value::Never) {
                return Ok(());
            }
        }
        let target = &self.loops[index];
        let (to, defers) = (
            if is_break { target.exit } else { target.next },
            target.defers,
        );
        self.run_defers(defers)?;
        self.branch(to);
        self.start_dead_block("after.jump");
        Ok(())
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

    pub(super) fn branch(&self, to: BasicBlock<'ctx>) {
        self.builder
            .build_unconditional_branch(to)
            .expect(POSITIONED);
    }
}

// `defer` (milestone 151) lowers at the exits this file and `function.rs` emit;
// declared here because `mod.rs` is at its 500-line limit.
#[path = "defer.rs"]
pub(super) mod defer;

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
