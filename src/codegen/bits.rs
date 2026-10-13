//! Bit operators in codegen (v7 draft §4, §9; milestone 28): `and`, `or`,
//! `xor`, and shifts after a range check of the amount, which panics.

use inkwell::IntPredicate;
use inkwell::values::IntValue;

use super::lower::{Lowerer, POSITIONED};
use crate::ast::BinOp;

impl<'ctx> Lowerer<'ctx> {
    /// `a op b` for a bit operator on integers; a shift by a negative amount
    /// or at least the width panics at `at` (`shift out of range`).
    pub(super) fn bitwise(
        &mut self,
        op: BinOp,
        signed: bool,
        a: IntValue<'ctx>,
        b: IntValue<'ctx>,
        at: usize,
    ) -> IntValue<'ctx> {
        if matches!(op, BinOp::Shl | BinOp::Shr) {
            // Unsigned, so a negative amount reads as huge and fails too.
            let width = b
                .get_type()
                .const_int(u64::from(b.get_type().get_bit_width()), false);
            let out = self
                .builder
                .build_int_compare(IntPredicate::UGE, b, width, "shift.range")
                .expect(POSITIONED);
            self.panic_if(out, "shift out of range", at);
        }
        let builder = &self.builder;
        match op {
            BinOp::BitAnd => builder.build_and(a, b, "and"),
            BinOp::BitOr => builder.build_or(a, b, "or"),
            BinOp::BitXor => builder.build_xor(a, b, "xor"),
            BinOp::Shl => builder.build_left_shift(a, b, "shl"),
            // `ashr` copies the sign bit for `i32`/`i64`; `lshr` fills zeros.
            _ => builder.build_right_shift(a, b, signed, "shr"),
        }
        .expect(POSITIONED)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn each_operator_has_its_instruction() {
        let ir = ir("fun main() {\n\
            let a: u32 = 6; let b = a & 3; let c = a | 1; let d = a ^ 5; let e = ~a;\n\
            let k: u32 = 2; let f = a << k; let g = a >> 1;\n\
            let s: i64 = -8; let t = s >> 1;\n\
        }");
        for needle in [
            "and i32",
            "or i32",
            "xor i32",
            "shl i32",
            "lshr i32",
            "ashr i64",
            "icmp uge i32",
        ] {
            assert!(ir.contains(needle), "{needle}: {ir}");
        }
    }
}
