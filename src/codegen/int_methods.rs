//! Methods on integers in codegen (v7 draft §4, §9; milestone 29): the
//! wrapping operations are plain `add`, `sub` and `mul`; the counts are
//! `llvm.ctpop`, `llvm.ctlz` and `llvm.cttz`, widened to `i64`.

use inkwell::intrinsics::Intrinsic;
use inkwell::values::{IntValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::Value;
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `base.name(args)` on an integer `base` of type `ty`.
    pub(super) fn integer_method(
        &mut self,
        base: &Expr,
        ty: &Type,
        name: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let a = self.get(base, ty)?.into_int_value();
        let b = match args.first() {
            Some(arg) => Some(self.get(arg, ty)?.into_int_value()),
            None => None,
        };
        let builder = &self.builder;
        let result = match (name, b) {
            // Modulo 2^width: no overflow check (v7).
            ("wrapping_add", Some(b)) => builder.build_int_add(a, b, "wrap"),
            ("wrapping_sub", Some(b)) => builder.build_int_sub(a, b, "wrap"),
            ("wrapping_mul", Some(b)) => builder.build_int_mul(a, b, "wrap"),
            _ => return Ok(Value::Val(self.count(name, a).into())),
        };
        Ok(Value::Val(result.expect(POSITIONED).into()))
    }

    /// `count_ones`, `leading_zeros` or `trailing_zeros` of `a`, as `i64`.
    fn count(&self, name: &str, a: IntValue<'ctx>) -> IntValue<'ctx> {
        let intrinsic = match name {
            "count_ones" => "llvm.ctpop",
            "leading_zeros" => "llvm.ctlz",
            _ => "llvm.cttz",
        };
        let declaration = Intrinsic::find(intrinsic)
            .and_then(|i| i.get_declaration(&self.module, &[a.get_type().into()]))
            .expect("LLVM 21 provides the bit-counting intrinsics");
        let mut values = vec![a.into()];
        if intrinsic != "llvm.ctpop" {
            // Not poison for 0: the count of zero is the width.
            values.push(self.context.bool_type().const_zero().into());
        }
        let call = self
            .builder
            .build_call(declaration, &values, "count")
            .expect(POSITIONED);
        let ValueKind::Basic(count) = call.try_as_basic_value() else {
            unreachable!("the counting intrinsics return an integer")
        };
        let count = count.into_int_value();
        let i64_type = self.context.i64_type();
        if count.get_type().get_bit_width() == 64 {
            return count;
        }
        self.builder
            .build_int_z_extend(count, i64_type, "count.i64")
            .expect(POSITIONED)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn wrapping_is_plain_and_counts_use_the_intrinsics() {
        let ir = ir("fun main() {\n\
            let a: i64 = 3; let b = a.wrapping_mul(a);\n\
            let x: u32 = 6; let c = x.count_ones(); let d = x.leading_zeros(); let e = x.trailing_zeros();\n\
        }");
        assert!(ir.contains("mul i64"), "{ir}");
        assert!(!ir.contains("smul.with.overflow"), "{ir}");
        for needle in [
            "@llvm.ctpop.i32",
            "@llvm.ctlz.i32",
            "@llvm.cttz.i32",
            "zext i32",
        ] {
            assert!(ir.contains(needle), "{needle}: {ir}");
        }
    }
}
