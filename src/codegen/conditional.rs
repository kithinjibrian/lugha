//! Conditional patterns in codegen (v9 draft §9, milestone 33): `if let`,
//! `while let` and `let … else` test one pattern with the arm chain's
//! `test`, jumping to the else branch, the loop's exit or the `else` block.

use super::CodegenError;
use super::control::Loop;
use super::hoist::Head;
use super::lower::{Lowerer, POSITIONED};
use super::patterns::Subject;
use super::value::{Value, llvm_type};
use crate::ast::{Block, Expr, IfLet, Pattern};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `scrutinee` evaluated as a pattern's subject: an enum or `Option` in
    /// a slot (payloads are tested in place), anything else as a value.
    fn subject_of(&mut self, scrutinee: &Expr) -> Result<(Subject<'ctx>, Type), CodegenError> {
        let ty = self.ty(scrutinee);
        let value = self.get(scrutinee, &ty)?;
        let subject = match &ty {
            Type::Enum(_, _) | Type::Option(_) => {
                let slot = self.entry_alloca(&ty, "scrutinee");
                self.builder.build_store(slot, value).expect(POSITIONED);
                Subject::At(slot)
            }
            _ => Subject::Value(value),
        };
        Ok((subject, ty))
    }

    /// `if let pattern = scrutinee { then } [else …]`; a value of the
    /// `if let`'s type goes through a slot, as for `match`.
    pub(super) fn if_let(
        &mut self,
        expr: &Expr,
        parts: &IfLet,
    ) -> Result<Value<'ctx>, CodegenError> {
        let (pattern, then, else_) = (&parts.pattern, &parts.then, parts.else_.as_deref());
        let (subject, sty) = self.subject_of(&parts.scrutinee)?;
        let rty = self.ty(expr);
        let result = (!matches!(rty, Type::Void | Type::Never))
            .then(|| self.entry_alloca(&rty, "iflet.value"));
        let (otherwise, merge) = (self.append("iflet.else"), self.append("iflet.end"));
        self.scopes.push();
        self.test(pattern, subject, &sty, otherwise, true);
        let then_value = self.block(then)?;
        self.scopes.pop();
        self.store_branch(result, then_value, merge);
        self.builder.position_at_end(otherwise);
        let else_value = match else_ {
            Some(e) => self.expr(e)?,
            None => Value::Void,
        };
        self.store_branch(result, else_value, merge);
        self.builder.position_at_end(merge);
        Ok(match result {
            _ if matches!((then_value, else_value), (Value::Never, Value::Never)) => Value::Never,
            Some(slot) => Value::Val(
                self.builder
                    .build_load(llvm_type(self.context, &rty), slot, "iflet")
                    .expect(POSITIONED),
            ),
            None => Value::Void,
        })
    }

    /// Stores a branch's value in `result` (if any) and ends the branch.
    fn store_branch(
        &self,
        result: Option<inkwell::values::PointerValue<'ctx>>,
        value: Value<'ctx>,
        merge: inkwell::basic_block::BasicBlock<'ctx>,
    ) {
        if let (Some(slot), Value::Val(v)) = (result, value) {
            self.builder.build_store(slot, v).expect(POSITIONED);
        }
        self.finish_branch(value, merge);
    }

    /// `while let pattern = scrutinee { body }`: the scrutinee is evaluated
    /// and tested before each iteration; `continue` goes back to it.
    pub(super) fn while_let(
        &mut self,
        pattern: &Pattern,
        scrutinee: &Expr,
        body: &Block,
    ) -> Result<(), CodegenError> {
        let (cond, exit) = (self.append("whilelet.cond"), self.append("whilelet.end"));
        let hoists = self.hoist_loop(&[Head::Pattern(pattern), Head::Read(scrutinee)], body)?;
        self.branch(cond);
        self.builder.position_at_end(cond);
        // A green thread's loop gives the others a turn now and then (v17).
        self.preempt_point();
        self.scopes.push();
        let (subject, sty) = self.subject_of(scrutinee)?;
        self.test(pattern, subject, &sty, exit, true);
        self.loops.push(Loop { next: cond, exit });
        self.block(body)?;
        self.loops.pop();
        self.scopes.pop();
        self.branch(cond);
        self.builder.position_at_end(exit);
        self.end_hoists(hoists);
        Ok(())
    }

    /// `let pattern = init else { … };`: the bindings land in the current
    /// scope; the `else` block always leaves (checked, E0429).
    pub(super) fn let_else(
        &mut self,
        pattern: &Pattern,
        init: &Expr,
        else_: Option<&Block>,
    ) -> Result<(), CodegenError> {
        let (subject, sty) = self.subject_of(init)?;
        let otherwise = self.append("letelse.else");
        self.test(pattern, subject, &sty, otherwise, true);
        let matched = self.current_block();
        self.builder.position_at_end(otherwise);
        // Without `else` (v14) the checker proved the pattern always matches.
        if let Some(else_) = else_ {
            self.block(else_)?;
        }
        if self.current_block().get_terminator().is_none() {
            self.builder.build_unreachable().expect(POSITIONED);
        }
        self.builder.position_at_end(matched);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn if_let_tests_the_tag_and_branches_to_else() {
        let ir = ir(
            "fun main() { let o = Some(2); if let Some(n) = o { println(n); } else { println(0); } }",
        );
        assert!(
            ir.contains("iflet.else") && ir.contains("icmp eq i32"),
            "{ir}"
        );
    }
}
