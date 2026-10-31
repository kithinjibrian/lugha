//! `defer` (v17, milestone 151; spec §6, §9). A `defer` statement emits no
//! code where it is written: it joins its block's list of pending
//! statements, with the names in scope there. Every way out of the block
//! lowers the pending ones inline, innermost block first and last first:
//! the block's end (after its tail), `return` and a failing `?` (every block
//! of the function), `break` and `continue` (the blocks inside the loop).
//! A statement reached by several exits is lowered at each, as Swift and Zig
//! do; its types come from the checker's side table, so each copy agrees.

use inkwell::values::BasicValueEnum;

use crate::ast::Expr;
use crate::check::Type;
use crate::codegen::CodegenError;
use crate::codegen::lower::Lowerer;
use crate::codegen::scope::Scopes;

/// A `defer` statement reached in the current block.
pub(in crate::codegen) struct Deferred<'ctx> {
    expr: Expr,
    /// The names in scope at the `defer`: a later `let` may shadow one
    /// before the statement runs.
    scopes: Scopes<'ctx>,
}

impl<'ctx> Lowerer<'ctx> {
    /// `defer e;`: remembered in the innermost block, lowered at its exits.
    pub(in crate::codegen) fn defer_stmt(&mut self, expr: &Expr) {
        let deferred = Deferred {
            expr: expr.clone(),
            scopes: self.scopes.clone(),
        };
        self.defers
            .last_mut()
            .expect("a statement is lowered inside a block")
            .push(deferred);
    }

    /// Whether leaving the blocks from depth `to` up runs anything.
    pub(in crate::codegen) fn defers_pending(&self, to: usize) -> bool {
        self.defers[to..].iter().any(|block| !block.is_empty())
    }

    /// Lowers the statements deferred in the blocks from depth `to` up,
    /// innermost first and last first, leaving them pending: code after an
    /// exit is dead, and the block's own end still runs them.
    pub(in crate::codegen) fn run_defers(&mut self, to: usize) -> Result<(), CodegenError> {
        let mut pending = Vec::new();
        for block in self.defers[to..].iter().rev() {
            for d in block.iter().rev() {
                pending.push((d.expr.clone(), d.scopes.clone()));
            }
        }
        for (expr, scopes) in pending {
            let saved = std::mem::replace(&mut self.scopes, scopes);
            let lowered = self.expr(&expr);
            self.scopes = saved;
            lowered?;
        }
        Ok(())
    }

    /// The value leaving the blocks from depth `to` up, held apart from the
    /// locals the deferred statements may still change: `defer xs.push(4);
    /// xs` gives `xs` as it was. A copy only marks arrays shared (spec §4).
    pub(in crate::codegen) fn hold(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
        to: usize,
    ) -> BasicValueEnum<'ctx> {
        if self.defers_pending(to) {
            self.deep_copy(value, ty)
        } else {
            value
        }
    }
}
