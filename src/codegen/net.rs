//! `std.net`'s built-ins in codegen (v17, milestone 110): each
//! `$os.net_<op>` calls `lugha_rt_net_<op>`, which returns a flag and
//! writes its result or message through out-pointers, as `os.rs` does.

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
        let bytes = Type::Array(Box::new(Type::U8));
        let name = format!("lugha_rt_{op}");
        // The arguments, by the built-in's parameter types.
        let (params, _) = crate::check::os_signature(op).expect("checked: a known built-in");
        let mut values = Vec::with_capacity(params.len() + 2);
        for (arg, ty) in args.iter().zip(&params) {
            values.push(self.get(arg, ty)?);
        }
        if op == "net_close" {
            let function = self.runtime(&name, &params, None);
            let values: Vec<_> = values.iter().map(|v| (*v).into()).collect();
            self.builder
                .build_call(function, &values, "")
                .expect(POSITIONED);
            return Ok(super::value::unit_value(self.context));
        }
        // What `Ok` holds: a descriptor, bytes, a name, or the `0` of a write.
        let held = match op {
            "net_read" => bytes,
            "net_name" => Type::String,
            _ => Type::I64,
        };
        let (out, error) = (self.out_slot(&held), self.out_slot(&Type::String));
        let mut types = params.clone();
        if op != "net_write" {
            types.push(Type::String);
            values.push(out.into());
        }
        types.push(Type::String);
        values.push(error.into());
        let ok = self.call_flag(&name, &types, &values);
        Ok(self.result(call, ok, (out, &held), error))
    }
}
