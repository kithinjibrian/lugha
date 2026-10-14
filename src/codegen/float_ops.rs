//! The float built-ins in codegen (v15 draft decision 18, milestone 53):
//! `$float.sqrt(x)` at `f64` is `llvm.sqrt.f64`, at `f32` `llvm.sqrt.f32`,
//! and likewise for the rest, so `std.math` needs no call into libm.

use inkwell::intrinsics::Intrinsic;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::ast::Expr;
use crate::check::Type;

/// The LLVM intrinsic behind built-in `op`.
fn intrinsic_name(op: &str) -> String {
    // `ln` is LLVM's `log`; the others share their names.
    let base = if op == "ln" { "log" } else { op };
    format!("llvm.{base}")
}

impl<'ctx> Lowerer<'ctx> {
    /// `$float.op(args)` at the call's float type.
    pub(super) fn float_call(
        &mut self,
        call: &Expr,
        op: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(call);
        // `$float.from_f64(x)` (v15): an `f64` at the call's float width.
        if op == "from_f64" {
            let value = self.get(&args[0], &Type::F64)?.into_float_value();
            if ty == Type::F64 {
                return Ok(value.into());
            }
            let f32_type = self.context.f32_type();
            let narrowed = self
                .builder
                .build_float_trunc(value, f32_type, "to_f32")
                .expect(POSITIONED);
            return Ok(narrowed.into());
        }
        let mut values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
        for arg in args {
            values.push(self.get(arg, &ty)?.into());
        }
        let llvm = llvm_type(self.context, &ty);
        let name = intrinsic_name(op);
        let declaration = Intrinsic::find(&name)
            .and_then(|i| i.get_declaration(&self.module, &[llvm]))
            .unwrap_or_else(|| panic!("LLVM 21 provides `{name}`"));
        let site = self
            .builder
            .build_call(declaration, &values, op)
            .expect(POSITIONED);
        match site.try_as_basic_value() {
            ValueKind::Basic(value) => Ok(value),
            ValueKind::Instruction(_) => unreachable!("float intrinsics return a float"),
        }
    }

    /// `$os.seed()` (v15): the runtime's 64 bits of OS entropy.
    pub(super) fn os_seed(&mut self) -> BasicValueEnum<'ctx> {
        let function = self.runtime("lugha_rt_os_seed", &[], Some(Type::U64));
        let site = self
            .builder
            .build_call(function, &[], "seed")
            .expect(POSITIONED);
        match site.try_as_basic_value() {
            ValueKind::Basic(value) => value,
            ValueKind::Instruction(_) => unreachable!("lugha_rt_os_seed returns a u64"),
        }
    }
}
