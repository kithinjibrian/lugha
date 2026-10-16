//! `assert`, `assert_eq` and `assert_ne` (v16 draft decision 83, milestone
//! 84): a branch, and on failure a panic at the call whose message shows
//! both values as they would print inside a built-in form (strings quoted).

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::Value;
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// One assertion call; the checker made the arguments fit.
    pub(super) fn assertion(
        &mut self,
        call: &Expr,
        name: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let at = call.span.start;
        if name == "assert" {
            let cond = self.get(&args[0], &Type::Bool)?.into_int_value();
            let failed = self.builder.build_not(cond, "failed").expect(POSITIONED);
            self.panic_if(failed, "assertion failed", at);
            return Ok(Value::Void);
        }
        let ty = self.ty(&args[0]);
        let a = self.get(&args[0], &ty)?;
        let b = self.get(&args[1], &ty)?;
        let equal = self.equal(a, b, &ty);
        let failed = match name {
            "assert_eq" => self.builder.build_not(equal, "failed").expect(POSITIONED),
            _ => equal,
        };
        let fail = self.append("assert.fail");
        let ok = self.append("assert.ok");
        self.builder
            .build_conditional_branch(failed, fail, ok)
            .expect(POSITIONED);
        self.builder.position_at_end(fail);
        let left = self.part(a, &ty);
        let message = match name {
            "assert_eq" => {
                let start = self.string_literal("assert_eq failed: left ").into();
                let with_left = self.concat(start, left);
                let with_label = self.join(with_left, ", right ");
                let right = self.part(b, &ty);
                self.concat(with_label, right)
            }
            _ => {
                let start = self.string_literal("assert_ne failed: both ").into();
                self.concat(start, left)
            }
        };
        self.call_panic(message, at);
        self.builder.position_at_end(ok);
        Ok(Value::Void)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn a_failed_assertion_panics_with_both_values() {
        let ir = ir("fun main() { assert(1 < 2); assert_eq(1, 2); assert_ne(\"a\", \"b\"); }");
        assert!(ir.contains("assertion failed"), "{ir}");
        assert!(ir.contains("assert_eq failed: left "), "{ir}");
        assert!(ir.contains("assert_ne failed: both "), "{ir}");
        assert!(ir.contains("call void @lugha_rt_panic"), "{ir}");
    }
}
