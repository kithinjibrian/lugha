//! `$hash.value(x)` (v17, milestone 101): FNV-1a over the same structure deep
//! `==` compares (`equality.rs`), so equal values hash equal. Numbers,
//! `bool`, `char` and handles mix in their bits inline; strings, structs,
//! enums, `Option` and arrays go through one `lugha_hash_<type>` helper per
//! type. Not cryptographic.

use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, ValueKind};

use super::CodegenError;
use super::copy::mangle;
use super::enums::variants;
use super::heap::element_of;
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::ast::Expr;
use crate::check::Type;

/// FNV-1a's 64-bit offset basis and prime.
const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const PRIME: u64 = 0x0000_0100_0000_01b3;

impl<'ctx> Lowerer<'ctx> {
    /// `$hash.value(x)`: `x`'s hash as a `u64`.
    pub(super) fn hash_call(
        &mut self,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(&args[0]);
        let value = self.get(&args[0], &ty)?;
        Ok(self.hash(value, &ty).into())
    }

    /// The hash of `value`, of type `ty`, as an `i64`.
    fn hash(&mut self, value: BasicValueEnum<'ctx>, ty: &Type) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        let widen = |this: &Self, v: IntValue<'ctx>| {
            this.builder
                .build_int_z_extend_or_bit_cast(v, i64_type, "bits")
                .expect(POSITIONED)
        };
        match ty {
            Type::String | Type::Struct(..) | Type::Enum(..) | Type::Option(_) | Type::Array(_) => {
                let helper = self.hash_function(ty);
                let call = self
                    .builder
                    .build_call(helper, &[value.into()], "hash")
                    .expect(POSITIONED);
                match call.try_as_basic_value() {
                    ValueKind::Basic(result) => result.into_int_value(),
                    ValueKind::Instruction(_) => unreachable!("hash helpers return i64"),
                }
            }
            Type::Handle(_) => self
                .builder
                .build_ptr_to_int(value.into_pointer_value(), i64_type, "addr")
                .expect(POSITIONED),
            // Floats can't be keys (E0441); hashed by their bits all the same.
            Type::F32 | Type::F64 => {
                let float = value.into_float_value();
                let int = match ty {
                    Type::F32 => self.context.i32_type(),
                    _ => self.context.i64_type(),
                };
                let bits = self
                    .builder
                    .build_bit_cast(float, int, "bits")
                    .expect(POSITIONED)
                    .into_int_value();
                widen(self, bits)
            }
            _ if value.is_int_value() => widen(self, value.into_int_value()),
            _ => i64_type.const_zero(),
        }
    }

    /// `lugha_hash_<mangled>(T) -> i64`, generated once per type. Declared
    /// before its body, so a recursive enum's helper can call itself.
    fn hash_function(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_hash_{}", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let i64_type = self.context.i64_type();
        let function = self.module.add_function(
            &name,
            i64_type.fn_type(&[llvm_type(self.context, ty).into()], false),
            Some(Linkage::Internal),
        );
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let value = function.get_nth_param(0).expect("one parameter");
        let state = self.entry_alloca(&Type::I64, "h");
        let offset = i64_type.const_int(OFFSET, false);
        self.builder.build_store(state, offset).expect(POSITIONED);
        match ty {
            Type::String => self.hash_bytes(state, value.into_pointer_value()),
            Type::Struct(..) => self.hash_fields(state, value, &ty.to_string()),
            Type::Array(_) => self.hash_elements(state, value.into_pointer_value(), ty),
            _ => self.hash_variants(state, value, ty),
        }
        let h = self
            .builder
            .build_load(i64_type, state, "h")
            .expect(POSITIONED);
        self.builder.build_return(Some(&h)).expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }

    /// `h = (h ^ part) * PRIME`, wrapping.
    fn mix(&self, state: PointerValue<'ctx>, part: IntValue<'ctx>) {
        let i64_type = self.context.i64_type();
        let h = self
            .builder
            .build_load(i64_type, state, "h")
            .expect(POSITIONED)
            .into_int_value();
        let xored = self.builder.build_xor(h, part, "h.xor").expect(POSITIONED);
        let prime = i64_type.const_int(PRIME, false);
        let next = self
            .builder
            .build_int_mul(xored, prime, "h.mul")
            .expect(POSITIONED);
        self.builder.build_store(state, next).expect(POSITIONED);
    }

    /// A string's bytes, one at a time.
    fn hash_bytes(&mut self, state: PointerValue<'ctx>, s: PointerValue<'ctx>) {
        let len = self.length(s);
        self.count_loop(len, |this, i| {
            let byte = this
                .builder
                .build_load(this.context.i8_type(), this.byte_address(s, i), "byte")
                .expect(POSITIONED)
                .into_int_value();
            let wide = this
                .builder
                .build_int_z_extend(byte, this.context.i64_type(), "byte.wide")
                .expect(POSITIONED);
            this.mix(state, wide);
            Ok(())
        })
        .expect("hash loops only lower loads");
    }

    /// Each field's hash, in declaration order.
    fn hash_fields(&mut self, state: PointerValue<'ctx>, value: BasicValueEnum<'ctx>, name: &str) {
        let value = value.into_struct_value();
        for (index, (field, ty)) in self.structs[name].clone().iter().enumerate() {
            let index = u32::try_from(index).expect("field count fits in u32");
            let raw = self
                .builder
                .build_extract_value(value, index, field)
                .expect("field exists");
            let part = self.loaded_form(raw, ty);
            let h = self.hash(part, ty);
            self.mix(state, h);
        }
    }

    /// The length, then each element's hash.
    fn hash_elements(&mut self, state: PointerValue<'ctx>, array: PointerValue<'ctx>, ty: &Type) {
        let element = element_of(ty);
        let size = self.element_size(&element);
        let len = self.length(array);
        self.mix(state, len);
        self.count_loop(len, |this, i| {
            let x = this.load_element(this.element_address(array, i, size), &element);
            let h = this.hash(x, &element);
            this.mix(state, h);
            Ok(())
        })
        .expect("hash loops only lower loads");
    }

    /// The tag, then the payloads of its variant.
    fn hash_variants(&mut self, state: PointerValue<'ctx>, value: BasicValueEnum<'ctx>, ty: &Type) {
        let slot = self.entry_alloca(ty, "value");
        self.builder.build_store(slot, value).expect(POSITIONED);
        let tag = self.tag(ty, slot);
        let wide = self
            .builder
            .build_int_z_extend(tag, self.context.i64_type(), "tag.wide")
            .expect(POSITIONED);
        self.mix(state, wide);
        let done = self.append("payloads.hash");
        let i32_type = self.context.i32_type();
        let mut cases = Vec::new();
        for (index, (_, payloads)) in variants(ty, &self.enums).into_iter().enumerate() {
            if payloads.is_empty() {
                continue;
            }
            let block = self.append("variant");
            let tag = u64::try_from(index).expect("variant count fits in u64");
            cases.push((i32_type.const_int(tag, false), block));
            let resume = self.current_block();
            self.builder.position_at_end(block);
            for (i, payload) in payloads.iter().enumerate() {
                let i = u32::try_from(i).expect("payload count fits in u32");
                let x = self.load_payload(ty, slot, &payloads, i, payload);
                let h = self.hash(x, payload);
                self.mix(state, h);
            }
            self.branch(done);
            self.builder.position_at_end(resume);
        }
        self.builder
            .build_switch(tag, done, &cases)
            .expect(POSITIONED);
        self.builder.position_at_end(done);
    }
}
