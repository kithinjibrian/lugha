//! `$hash.text(s, h)` (v17, milestones 101 and 143): FNV-1a over a
//! string's bytes from state `h`, a loop in IR for speed. Every other type
//! hashes in Lugha, through the prelude's derivable `$Hash` (`prelude/hash.la`).
//! Not cryptographic.

use inkwell::values::{BasicValueEnum, PointerValue};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use crate::ast::Expr;
use crate::check::Type;

/// FNV-1a's 64-bit prime.
const PRIME: u64 = 0x0000_0100_0000_01b3;

impl<'ctx> Lowerer<'ctx> {
    /// `$hash.text(s, h)`: `s`'s bytes mixed into `h`, as a `u64`.
    pub(super) fn hash_call(
        &mut self,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let s = self.get(&args[0], &Type::String)?.into_pointer_value();
        let h = self.get(&args[1], &Type::U64)?;
        let state = self.entry_alloca(&Type::U64, "h");
        self.builder.build_store(state, h).expect(POSITIONED);
        self.hash_bytes(state, s);
        Ok(self
            .builder
            .build_load(self.context.i64_type(), state, "h")
            .expect(POSITIONED))
    }

    /// `h = (h ^ byte) * PRIME`, wrapping, for each of `s`'s bytes.
    fn hash_bytes(&mut self, state: PointerValue<'ctx>, s: PointerValue<'ctx>) {
        let len = self.length(s);
        self.count_loop(len, |this, i| {
            let byte = this
                .builder
                .build_load(this.context.i8_type(), this.byte_address(s, i), "byte")
                .expect(POSITIONED)
                .into_int_value();
            let i64_type = this.context.i64_type();
            let wide = this
                .builder
                .build_int_z_extend(byte, i64_type, "byte.wide")
                .expect(POSITIONED);
            let h = this
                .builder
                .build_load(i64_type, state, "h")
                .expect(POSITIONED)
                .into_int_value();
            let xored = this.builder.build_xor(h, wide, "h.xor").expect(POSITIONED);
            let next = this
                .builder
                .build_int_mul(xored, i64_type.const_int(PRIME, false), "h.mul")
                .expect(POSITIONED);
            this.builder.build_store(state, next).expect(POSITIONED);
            Ok(())
        })
        .expect("hash loops only lower loads");
    }
}
