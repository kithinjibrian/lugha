//! `unwrap()` and `expect(msg)` in codegen (v9 draft §4, §9; milestone 32):
//! the value's tag is compared with `Some` or `Ok`; on a mismatch the
//! program panics at the method name, otherwise the payload is the result.

use inkwell::IntPredicate;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::Value;
use crate::ast::Expr;
use crate::check::Type;

/// The prelude's `Result` key.
const RESULT: &str = "$prelude.Result";

impl<'ctx> Lowerer<'ctx> {
    /// True if `name` on a value of type `ty` is the built-in unwrapping.
    pub(super) fn unwraps(&self, ty: &Type, name: &str) -> bool {
        let unwrappable = match ty {
            Type::Option(_) => true,
            Type::Enum(key, _) => key == RESULT,
            _ => false,
        };
        unwrappable && matches!(name, "unwrap" | "expect")
    }

    /// `base.unwrap()` or `base.expect(msg)`: the `Some`/`Ok` payload, or a
    /// panic at `at` with `unwrap on None`, `unwrap on Err` or `msg`.
    pub(super) fn unwrap_method(
        &mut self,
        base: &Expr,
        ty: &Type,
        name: &str,
        args: &[Expr],
        at: usize,
    ) -> Result<Value<'ctx>, CodegenError> {
        let value = self.get(base, ty)?;
        let slot = self.entry_alloca(ty, "unwrapped");
        self.builder.build_store(slot, value).expect(POSITIONED);
        let option = matches!(ty, Type::Option(_));
        // `expect`'s message is evaluated whether or not it is needed, as any argument.
        let message = match (name, args.first()) {
            ("expect", Some(message)) => self.get(message, &Type::String)?,
            _ => {
                let text = if option {
                    "unwrap on None"
                } else {
                    "unwrap on Err"
                };
                self.string_literal(text).into()
            }
        };
        let (wanted, payloads) = self.variant(ty, if option { "Some" } else { "Ok" });
        let actual = self.tag(ty, slot);
        let wanted = self.context.i32_type().const_int(u64::from(wanted), false);
        let ok = self
            .builder
            .build_int_compare(IntPredicate::EQ, actual, wanted, "is.ok")
            .expect(POSITIONED);
        let (pass, fail) = (self.append("unwrap.ok"), self.append("unwrap.fail"));
        self.builder
            .build_conditional_branch(ok, pass, fail)
            .expect(POSITIONED);
        self.builder.position_at_end(fail);
        // `unwrap` on `Err(e)` shows `e` (v12).
        let message = if name == "unwrap" && !option {
            let (_, errors) = self.variant(ty, "Err");
            let error = self.load_payload(ty, slot, &errors, 0, &errors[0]);
            let text = self.text(error, &errors[0]);
            let lead = self.string_literal("unwrap on Err: ").into();
            self.concat(lead, text)
        } else {
            message
        };
        self.call_panic(message, at);
        self.builder.position_at_end(pass);
        let payload = payloads[0].clone();
        Ok(Value::Val(
            self.load_payload(ty, slot, &payloads, 0, &payload),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn unwrap_tests_the_tag_and_panics_at_the_call() {
        let ir = ir("fun main() { let o = Some(3); let n = o.unwrap(); }");
        assert!(
            ir.contains("is.ok") && ir.contains("@lugha_rt_panic"),
            "{ir}"
        );
        assert!(ir.contains("unwrap on None"), "{ir}");
    }
}
