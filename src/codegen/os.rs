//! The operating system's services in codegen (v15): the `$os.<op>`
//! built-ins that std modules call, each a runtime function that returns
//! a flag and writes its results through out-pointers.

use inkwell::values::BasicValueEnum;

use super::CodegenError;
use super::lower::Lowerer;
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `$os.op(args)`, `call` being the whole call.
    pub(super) fn os_call(
        &mut self,
        call: &Expr,
        op: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let bytes = Type::Array(Box::new(Type::U8));
        Ok(match op {
            "seed" => self.os_seed(),
            // `Some(bytes)` of the variable, or `None` (milestone 64).
            "env" => {
                let name = self.get(&args[0], &Type::String)?;
                let out = self.out_slot(&bytes);
                let found = self.call_flag(
                    "lugha_rt_env",
                    &[Type::String, Type::String],
                    &[name, out.into()],
                );
                let value = self.load_slot(out, &bytes);
                let option = self.ty(call);
                let some = self.construct_values(&option, "Some", vec![value]);
                let none = self.construct_values(&option, "None", Vec::new());
                self.choose(found, some, none)
            }
            // `Ok(bytes)` of the current directory, or `Err(message)`.
            "cwd" => {
                let (out, error) = (self.out_slot(&bytes), self.out_slot(&Type::String));
                let ok = self.call_flag(
                    "lugha_rt_cwd",
                    &[Type::String, Type::String],
                    &[out.into(), error.into()],
                );
                self.result(call, ok, (out, &bytes), error)
            }
            _ => unreachable!("checked: `$os.{op}` is a known built-in"),
        })
    }
}
