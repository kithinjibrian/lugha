//! The v10 integer methods and constants in codegen (v10 draft §4, §9;
//! milestone 35): `checked_*` and `saturating_mul` read the overflow flag of
//! `llvm.*.with.overflow`, `saturating_add/sub` are `llvm.*.sat`, rotations
//! are `llvm.fshl`/`llvm.fshr` of the value with itself, and `I.MIN`/`I.MAX`
//! are constants.

use inkwell::IntPredicate;
use inkwell::intrinsics::Intrinsic;
use inkwell::values::{BasicValueEnum, IntValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, int_type, is_signed};
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `I.MIN` (`max` false) or `I.MAX` of integer type `ty`.
    pub(super) fn int_limit(&self, ty: &Type, max: bool) -> IntValue<'ctx> {
        let int = int_type(self.context, ty);
        let bits = int.get_bit_width();
        // The bit pattern of the constant; `const_int` keeps the low `bits`.
        let pattern = match (is_signed(ty), max) {
            (true, true) => (1u64 << (bits - 1)) - 1,
            (true, false) => 1u64 << (bits - 1),
            (false, true) => u64::MAX,
            (false, false) => 0,
        };
        int.const_int(pattern, false)
    }

    /// `base.name(args)` for a v10 method on an integer `base` of type
    /// `ty`; `None` for the v7 methods, which `integer_method` handles.
    pub(super) fn int_v10_method(
        &mut self,
        base: &Expr,
        ty: &Type,
        name: &str,
        args: &[Expr],
    ) -> Result<Option<Value<'ctx>>, CodegenError> {
        let family = name.split('_').next().unwrap_or(name);
        if !matches!(family, "checked" | "saturating" | "rotate") {
            return Ok(None);
        }
        let a = self.get(base, ty)?.into_int_value();
        let value = if family == "rotate" {
            let n = self.get(&args[0], &Type::I64)?.into_int_value();
            self.rotate(name, a, n).into()
        } else {
            let b = self.get(&args[0], ty)?.into_int_value();
            match name {
                "checked_div" => self.checked_div(ty, a, b),
                "saturating_add" | "saturating_sub" => self.saturate(name, ty, a, b).into(),
                "saturating_mul" => self.saturating_mul(ty, a, b).into(),
                _ => {
                    let (result, overflow) = self.with_overflow(&name[8..], ty, a, b);
                    self.optional(ty, overflow, result)
                }
            }
        };
        Ok(Some(Value::Val(value)))
    }

    /// `op` (`add`, `sub` or `mul`) of `a` and `b` through
    /// `llvm.{s,u}<op>.with.overflow`: the result and its overflow flag.
    fn with_overflow(
        &self,
        op: &str,
        ty: &Type,
        a: IntValue<'ctx>,
        b: IntValue<'ctx>,
    ) -> (IntValue<'ctx>, IntValue<'ctx>) {
        let sign = if is_signed(ty) { "s" } else { "u" };
        let pair = self.llvm_intrinsic(&format!("llvm.{sign}{op}.with.overflow"), a, &[a, b]);
        let pair = pair.into_struct_value();
        let get = |i, name| {
            self.builder
                .build_extract_value(pair, i, name)
                .expect(POSITIONED)
                .into_int_value()
        };
        (get(0, "value"), get(1, "overflow"))
    }

    /// `None` when `bad`, otherwise `Some(value)`, as an `Option<ty>`.
    fn optional(
        &mut self,
        ty: &Type,
        bad: IntValue<'ctx>,
        value: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let option = Type::Option(Box::new(ty.clone()));
        let some = self.construct_values(&option, "Some", vec![value.into()]);
        let none = self.construct_values(&option, "None", Vec::new());
        self.builder
            .build_select(bad, none, some, "checked")
            .expect(POSITIONED)
    }

    /// `a.checked_div(b)`: `None` for a zero divisor or a signed `MIN / -1`.
    /// The division always runs, by 1 in those cases, so it can't trap.
    fn checked_div(
        &mut self,
        ty: &Type,
        a: IntValue<'ctx>,
        b: IntValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let int = a.get_type();
        let b_ = &self.builder;
        let eq = |x, y, name| {
            b_.build_int_compare(IntPredicate::EQ, x, y, name)
                .expect(POSITIONED)
        };
        let mut bad = eq(b, int.const_zero(), "by.zero");
        if is_signed(ty) {
            let min = eq(a, self.int_limit(ty, false), "is.min");
            let minus_one = eq(b, int.const_all_ones(), "by.minus.one");
            let both = b_
                .build_and(min, minus_one, "min.by.minus.one")
                .expect(POSITIONED);
            bad = b_.build_or(bad, both, "bad").expect(POSITIONED);
        }
        let divisor = b_
            .build_select(bad, int.const_int(1, false), b, "divisor")
            .expect(POSITIONED)
            .into_int_value();
        let quotient = if is_signed(ty) {
            b_.build_int_signed_div(a, divisor, "quotient")
        } else {
            b_.build_int_unsigned_div(a, divisor, "quotient")
        }
        .expect(POSITIONED);
        self.optional(ty, bad, quotient)
    }

    /// `saturating_add` / `saturating_sub` through `llvm.{s,u}{add,sub}.sat`.
    fn saturate(
        &self,
        name: &str,
        ty: &Type,
        a: IntValue<'ctx>,
        b: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        let sign = if is_signed(ty) { "s" } else { "u" };
        let op = &name["saturating_".len()..];
        self.llvm_intrinsic(&format!("llvm.{sign}{op}.sat"), a, &[a, b])
            .into_int_value()
    }

    /// `a.saturating_mul(b)`: the product, or on overflow `MAX`, or `MIN`
    /// for a signed type when the signs of `a` and `b` differ.
    fn saturating_mul(&self, ty: &Type, a: IntValue<'ctx>, b: IntValue<'ctx>) -> IntValue<'ctx> {
        let (product, overflow) = self.with_overflow("mul", ty, a, b);
        let max = self.int_limit(ty, true);
        let bound = if is_signed(ty) {
            let builder = &self.builder;
            let signs = builder.build_xor(a, b, "signs").expect(POSITIONED);
            let zero = a.get_type().const_zero();
            let negative = builder
                .build_int_compare(IntPredicate::SLT, signs, zero, "negative")
                .expect(POSITIONED);
            builder
                .build_select(negative, self.int_limit(ty, false), max, "bound")
                .expect(POSITIONED)
                .into_int_value()
        } else {
            max
        };
        self.builder
            .build_select(overflow, bound, product, "saturated")
            .expect(POSITIONED)
            .into_int_value()
    }

    /// `a.rotate_left(n)` / `rotate_right(n)`: `n`, an `i64`, cut to `a`'s
    /// width; the funnel shift takes it modulo the width.
    fn rotate(&self, name: &str, a: IntValue<'ctx>, n: IntValue<'ctx>) -> IntValue<'ctx> {
        let n = if a.get_type().get_bit_width() < 64 {
            self.builder
                .build_int_truncate(n, a.get_type(), "count")
                .expect(POSITIONED)
        } else {
            n
        };
        let shift = if name == "rotate_left" {
            "llvm.fshl"
        } else {
            "llvm.fshr"
        };
        self.llvm_intrinsic(shift, a, &[a, a, n]).into_int_value()
    }

    /// Calls the LLVM intrinsic `name` overloaded on `like`'s type.
    fn llvm_intrinsic(
        &self,
        name: &str,
        like: IntValue<'ctx>,
        args: &[IntValue<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let declaration = Intrinsic::find(name)
            .and_then(|i| i.get_declaration(&self.module, &[like.get_type().into()]))
            .expect("LLVM 21 provides the integer intrinsics");
        let args: Vec<_> = args.iter().map(|a| (*a).into()).collect();
        let call = self
            .builder
            .build_call(declaration, &args, "int")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(value) => value,
            ValueKind::Instruction(_) => unreachable!("`{name}` returns a value"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn methods_use_the_intrinsics_and_limits_are_constants() {
        let ir = ir("fun main() {\n\
            let a: i64 = 3; let b = a.checked_add(a);\n\
            let c: u8 = 7; let d = c.saturating_add(c);\n\
            let e: i16 = 5; let f = e.rotate_left(1);\n\
            let g = i16.MAX;\n\
        }");
        for needle in [
            "@llvm.sadd.with.overflow.i64",
            "@llvm.uadd.sat.i8",
            "@llvm.fshl.i16",
            "i16 32767",
        ] {
            assert!(ir.contains(needle), "{needle}: {ir}");
        }
    }

    #[test]
    fn checked_div_never_divides_by_zero() {
        let ir = ir("fun main() { let a: i8 = 1; let b = a.checked_div(0); }");
        assert!(ir.contains("select i1 %bad, i8 1"), "{ir}");
        assert!(ir.contains("sdiv i8"), "{ir}");
    }
}
