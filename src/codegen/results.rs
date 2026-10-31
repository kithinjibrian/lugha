//! `e?` in codegen (v4 draft §9, milestone 17): the operand goes in a slot,
//! its tag picks the branch, the failing branch builds the function's own
//! `Err(e)` (or `None`) and returns it, and the passing branch is the
//! `Ok`/`Some` payload.

use inkwell::IntPredicate;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::Value;
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `inner?`: its `Ok`/`Some` payload, or an early return.
    pub(super) fn try_value(&mut self, inner: &Expr) -> Result<Value<'ctx>, CodegenError> {
        let ty = self.ty(inner);
        let value = self.get(inner, &ty)?;
        let slot = self.entry_alloca(&ty, "try");
        self.builder.build_store(slot, value).expect(POSITIONED);
        let option = matches!(ty, Type::Option(_));
        let (pass, fail) = if option {
            ("Some", "None")
        } else {
            ("Ok", "Err")
        };
        let (pass_tag, pass_payloads) = self.variant(&ty, pass);
        let tag = self.tag(&ty, slot);
        let wanted = self
            .context
            .i32_type()
            .const_int(u64::from(pass_tag), false);
        let passed = self
            .builder
            .build_int_compare(IntPredicate::EQ, tag, wanted, "passed")
            .expect(POSITIONED);
        let function = self.function.expect("`?` is inside a function");
        let pass_block = self.context.append_basic_block(function, "try.pass");
        let fail_block = self.context.append_basic_block(function, "try.fail");
        self.builder
            .build_conditional_branch(passed, pass_block, fail_block)
            .expect(POSITIONED);
        self.builder.position_at_end(fail_block);
        let ret = self
            .ret
            .clone()
            .expect("checked: `?` is only where a Result or Option is returned");
        let out = if option {
            self.construct_values(&ret, "None", Vec::new())
        } else {
            let (_, fail_payloads) = self.variant(&ty, fail);
            let error = self.load_payload(&ty, slot, &fail_payloads, 0, &fail_payloads[0]);
            // `?` converting the error into an interface error (v5).
            let error = match self.try_conversions.get(&inner.id).cloned() {
                Some(to) => self.box_as(error, &self.concrete(&fail_payloads[0]), &to),
                None => error,
            };
            self.construct_values(&ret, "Err", vec![error])
        };
        // The failure is built, then every block's defers run (milestone 151).
        let out = self.hold(out, &ret, 0);
        self.run_defers(0)?;
        self.builder.build_return(Some(&out)).expect(POSITIONED);
        self.builder.position_at_end(pass_block);
        let value = self.load_payload(&ty, slot, &pass_payloads, 0, &pass_payloads[0]);
        Ok(Value::Val(value))
    }
}
