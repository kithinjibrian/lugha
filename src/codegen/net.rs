//! `std.net`'s and `std.tls`'s built-ins in codegen (v17, milestones 110
//! and 112): each `$os.net_<op>` or `$os.tls_<op>` calls `lugha_rt_<op>`,
//! which returns a flag and writes its result or message through
//! out-pointers as `os.rs` does, or (a close, a socket number) just returns.

use inkwell::values::BasicValueEnum;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `$os.net_<op>(args)`, `call` being the whole call.
    pub(super) fn net_call(
        &mut self,
        call: &Expr,
        op: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let name = format!("lugha_rt_{op}");
        // The arguments, by the built-in's parameter types.
        let (params, _) = crate::check::os_signature(op).expect("checked: a known built-in");
        let mut values = Vec::with_capacity(params.len() + 2);
        for (arg, ty) in args.iter().zip(&params) {
            values.push(self.get(arg, ty)?);
        }
        // Anything but a `Result` is a plain call (v17): a close (`()`), a
        // socket number or flag (`i64`), a digest (`u8[]`), a hasher (`CPtr`).
        let (_, ret) = crate::check::os_signature(op).expect("checked: a known built-in");
        let is_result = matches!(&ret, Type::Enum(k, _) if k == crate::check::RESULT);
        if !is_result {
            let returns = (ret != Type::Unit).then(|| ret.clone());
            let function = self.runtime(&name, &params, returns);
            let values: Vec<_> = values.iter().map(|v| (*v).into()).collect();
            let site = self
                .builder
                .build_call(function, &values, "")
                .expect(POSITIONED);
            return Ok(match site.try_as_basic_value() {
                inkwell::values::ValueKind::Basic(value) => value,
                inkwell::values::ValueKind::Instruction(_) => {
                    super::value::unit_value(self.context)
                }
            });
        }
        // What `Ok` holds: a handle, bytes, a name, or the `0` of a write.
        let held = match &ret {
            Type::Enum(_, parts) => parts[0].clone(),
            _ => unreachable!("a `Result`, just checked"),
        };
        let (out, error) = (self.out_slot(&held), self.out_slot(&Type::String));
        let mut types = params.clone();
        if op != "net_write" && op != "tls_write" {
            types.push(Type::String);
            values.push(out.into());
        }
        types.push(Type::String);
        values.push(error.into());
        let ok = self.call_flag(&name, &types, &values);
        Ok(self.result(call, ok, (out, &held), error))
    }
}
