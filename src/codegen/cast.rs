//! `as` casts between numeric types (spec §4): integer narrowing truncates,
//! widening extends by the source's signedness, integer → float rounds to
//! nearest, and float → integer truncates toward zero and saturates, with
//! NaN becoming 0 (`llvm.fptosi.sat` / `llvm.fptoui.sat`, spec §9).

use inkwell::intrinsics::Intrinsic;
use inkwell::values::{BasicValueEnum, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::{int_type, is_signed, llvm_type};
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `inner as T`, where `cast` is the whole cast expression.
    pub(super) fn cast(
        &mut self,
        cast: &Expr,
        inner: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let (from, to) = (self.ty(inner), self.ty(cast));
        let from = if matches!(from, Type::Never | Type::Error) {
            to
        } else {
            from
        };
        let value = self.get(inner, from)?;
        let b = &self.builder;
        Ok(match (from, to) {
            _ if from == to => value,
            (Type::F64, _) => {
                let name = if is_signed(to) {
                    "llvm.fptosi.sat"
                } else {
                    "llvm.fptoui.sat"
                };
                let target = llvm_type(self.context, to);
                let source = self.context.f64_type().into();
                let saturate = Intrinsic::find(name)
                    .and_then(|i| i.get_declaration(&self.module, &[target, source]))
                    .expect("LLVM 21 provides the saturating float-to-int intrinsics");
                let call = b
                    .build_call(saturate, &[value.into()], "fptoi")
                    .expect(POSITIONED);
                match call.try_as_basic_value() {
                    ValueKind::Basic(result) => result,
                    ValueKind::Instruction(_) => unreachable!("the intrinsic returns an integer"),
                }
            }
            (_, Type::F64) => {
                let (v, f64_type) = (value.into_int_value(), self.context.f64_type());
                if is_signed(from) {
                    b.build_signed_int_to_float(v, f64_type, "sitofp")
                } else {
                    b.build_unsigned_int_to_float(v, f64_type, "uitofp")
                }
                .expect(POSITIONED)
                .into()
            }
            _ => {
                let (v, target) = (value.into_int_value(), int_type(self.context, to));
                let (from_bits, to_bits) = (v.get_type().get_bit_width(), target.get_bit_width());
                if to_bits < from_bits {
                    b.build_int_truncate(v, target, "trunc")
                } else if is_signed(from) {
                    b.build_int_s_extend(v, target, "sext")
                } else {
                    b.build_int_z_extend(v, target, "zext")
                }
                .expect(POSITIONED)
                .into()
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn integer_casts_follow_signedness() {
        let ir = ir(
            "fun main() { let a: i32 = -7; let b = a as i64; let c: u8 = 200; let d = c as i64; let e = d as u8; }",
        );
        assert!(ir.contains("sext i32"), "{ir}");
        assert!(ir.contains("zext i8"), "{ir}");
        assert!(ir.contains("trunc i64"), "{ir}");
    }

    #[test]
    fn float_casts_use_the_right_conversions() {
        let ir = ir(
            "fun main() { let c: u8 = 200; let f = c as f64; let x = 2.5; let i = x as i32; let u = x as u8; }",
        );
        assert!(ir.contains("uitofp i8"), "{ir}");
        assert!(ir.contains("@llvm.fptosi.sat.i32.f64"), "{ir}");
        assert!(ir.contains("@llvm.fptoui.sat.i8.f64"), "{ir}");
    }
}
