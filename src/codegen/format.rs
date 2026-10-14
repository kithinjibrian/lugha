//! Format specifiers in codegen (v13 draft §9, milestone 44): `{x:spec}`
//! calls `lugha_rt_fmt_int`, `lugha_rt_fmt_float` or, for any other value,
//! `lugha_rt_pad` on its text, passing the parsed specifier as plain
//! numbers.

use inkwell::values::BasicValueEnum;

use super::lower::{Lowerer, POSITIONED};
use super::value::is_signed;
use crate::check::Type;
use crate::format::{self, Align, Kind};

impl<'ctx> Lowerer<'ctx> {
    /// `value`, of type `ty`, formatted by the specifier `text` (checked:
    /// E0431 rejects one that doesn't parse or fit).
    pub(super) fn formatted(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
        text: &str,
    ) -> BasicValueEnum<'ctx> {
        let spec = format::parse(text).expect("checked: E0431 rejects a bad specifier");
        let (i32_type, i64_type) = (self.context.i32_type(), self.context.i64_type());
        let int = |n: u64| -> BasicValueEnum<'ctx> { i32_type.const_int(n, false).into() };
        // Numbers align right unless told otherwise, everything else left.
        let align = match spec.align {
            Some(Align::Left) => 0,
            Some(Align::Right) => 1,
            Some(Align::Centre) => 2,
            None if ty.is_numeric() => 1,
            None => 0,
        };
        let (fill, align) = (int(u64::from(spec.fill)), int(align));
        let width: BasicValueEnum = i64_type.const_int(u64::from(spec.width), false).into();
        // `0` pads with zeros only when no alignment is given.
        let zero = int(u64::from(spec.zero && spec.align.is_none()));
        if ty.is_integer() {
            let v = value.into_int_value();
            let bits = v.get_type().get_bit_width();
            let wide = match bits {
                64 => v,
                _ if is_signed(ty) => self
                    .builder
                    .build_int_s_extend(v, i64_type, "wide")
                    .expect(POSITIONED),
                _ => self
                    .builder
                    .build_int_z_extend(v, i64_type, "wide")
                    .expect(POSITIONED),
            };
            let kind = match spec.kind {
                None => 0,
                Some(Kind::Hex) => 1,
                Some(Kind::UpperHex) => 2,
                Some(Kind::Binary) => 3,
                Some(Kind::Octal) => 4,
            };
            let args = [
                wide.into(),
                int(u64::from(bits)),
                int(u64::from(is_signed(ty))),
                int(kind),
                int(u64::from(spec.alternate)),
                fill,
                align,
                width,
                zero,
            ];
            let params = [
                Type::I64,
                Type::I32,
                Type::I32,
                Type::I32,
                Type::I32,
                Type::I32,
                Type::I32,
                Type::I64,
                Type::I32,
            ];
            return self.call_runtime_value("lugha_rt_fmt_int", &params, &args);
        }
        if ty.is_float() {
            let f = value.into_float_value();
            let wide = match ty {
                Type::F32 => self
                    .builder
                    .build_float_ext(f, self.context.f64_type(), "wide")
                    .expect(POSITIONED),
                _ => f,
            };
            // `-1` means no precision: the shortest round-tripping text.
            let precision = i32_type.const_int(spec.precision.map_or(u64::MAX, u64::from), true);
            let args = [
                wide.into(),
                int(u64::from(*ty == Type::F32)),
                precision.into(),
                fill,
                align,
                width,
                zero,
            ];
            let params = [
                Type::F64,
                Type::I32,
                Type::I32,
                Type::I32,
                Type::I32,
                Type::I64,
                Type::I32,
            ];
            return self.call_runtime_value("lugha_rt_fmt_float", &params, &args);
        }
        let text = self.text(value, ty);
        let params = [Type::String, Type::I32, Type::I32, Type::I64];
        self.call_runtime_value("lugha_rt_pad", &params, &[text, fill, align, width])
    }
}
