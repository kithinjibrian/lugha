//! Heap objects: the length header, bounds checks, element addresses and
//! string operations (spec §4, §7, §9).
//!
//! Strings and arrays are pointers to `{ i64 len, data }`.
//! Run-time-indexed addresses are computed with integer arithmetic
//! (`ptrtoint` + `8 + i * size` + `inttoptr`) using only safe builder calls;
//! LLVM optimizes this a little less well, which v0 accepts (spec §1).

use inkwell::IntPredicate;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::ast::{BinOp, Expr, Ident};
use crate::check::Type;

/// Bytes before the data: the `i64` length (spec §7).
const HEADER: u64 = 8;

/// The element type of an array type.
pub(super) fn element_of(ty: &Type) -> Type {
    match ty {
        Type::Array(element) => (**element).clone(),
        _ => unreachable!("checked: {ty} is an array type"),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// How an element is held in memory: `bool` as `i8`, anything else as itself.
    pub(super) fn storage_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        if *ty == Type::Bool {
            self.context.i8_type().into()
        } else {
            llvm_type(self.context, ty)
        }
    }

    /// Loads the element of type `ty` at `address`.
    pub(super) fn load_element(
        &self,
        address: PointerValue<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        let raw = self
            .builder
            .build_load(self.storage_type(ty), address, "elem")
            .expect(POSITIONED);
        self.loaded_form(raw, ty)
    }

    /// Stores `value` of type `ty` as an element at `address`.
    pub(super) fn store_element(
        &self,
        address: PointerValue<'ctx>,
        ty: &Type,
        value: BasicValueEnum<'ctx>,
    ) {
        let value = self.stored_form(value, ty);
        self.builder.build_store(address, value).expect(POSITIONED);
    }

    /// The length stored in an object's header.
    pub(super) fn length(&self, object: PointerValue<'ctx>) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_load(i64_type, object, "len")
            .expect(POSITIONED)
            .into_int_value()
    }

    /// The address of element `index` (each `size` bytes) of `object`.
    pub(super) fn element_address(
        &self,
        object: PointerValue<'ctx>,
        index: IntValue<'ctx>,
        size: u64,
    ) -> PointerValue<'ctx> {
        let i64_type = self.context.i64_type();
        let b = &self.builder;
        let base = b
            .build_ptr_to_int(object, i64_type, "base")
            .expect(POSITIONED);
        let scaled = b
            .build_int_mul(index, i64_type.const_int(size, false), "scaled")
            .expect(POSITIONED);
        let offset = b
            .build_int_add(scaled, i64_type.const_int(HEADER, false), "offset")
            .expect(POSITIONED);
        let address = b.build_int_add(base, offset, "address").expect(POSITIONED);
        b.build_int_to_ptr(
            address,
            self.context.ptr_type(Default::default()),
            "element",
        )
        .expect(POSITIONED)
    }

    /// Panics at `at` unless `0 <= index < len`; one unsigned compare covers
    /// negative indexes too (spec §9).
    pub(super) fn bounds_check(&mut self, len: IntValue<'ctx>, index: IntValue<'ctx>, at: usize) {
        let in_bounds = self
            .builder
            .build_int_compare(IntPredicate::ULT, index, len, "in_bounds")
            .expect(POSITIONED);
        let ok = self.append("bounds.ok");
        let fail = self.append("bounds.fail");
        self.builder
            .build_conditional_branch(in_bounds, ok, fail)
            .expect(POSITIONED);
        self.builder.position_at_end(fail);
        let file = self.file_name();
        let (line, col) = self.source.line_col(at);
        let i64_type = self.context.i64_type();
        let panic = self.runtime(
            "lugha_rt_panic_bounds",
            &[Type::I64, Type::I64, Type::String, Type::I64, Type::I64],
            None,
        );
        let args = [
            len.into(),
            index.into(),
            file.into(),
            i64_type.const_int(line as u64, false).into(),
            i64_type.const_int(col as u64, false).into(),
        ];
        self.builder.build_call(panic, &args, "").expect(POSITIONED);
        self.builder.build_unreachable().expect(POSITIONED);
        self.builder.position_at_end(ok);
    }

    /// `+`, `==` and `!=` on strings, through the runtime.
    pub(super) fn string_binary(
        &self,
        op: BinOp,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        let call = |name: &str, returns: Type| {
            let function = self.runtime(name, &[Type::String, Type::String], Some(returns));
            let site = self
                .builder
                .build_call(function, &[a.into(), b.into()], "str")
                .expect(POSITIONED);
            match site.try_as_basic_value() {
                ValueKind::Basic(result) => result,
                ValueKind::Instruction(_) => unreachable!("{name} returns a value"),
            }
        };
        if op == BinOp::Add {
            return call("lugha_rt_str_concat", Type::String);
        }
        let equal = call("lugha_rt_str_eq", Type::I32).into_int_value();
        let zero = self.context.i32_type().const_zero();
        let predicate = if op == BinOp::Eq {
            IntPredicate::NE
        } else {
            IntPredicate::EQ
        };
        self.builder
            .build_int_compare(predicate, equal, zero, "str.eq")
            .expect(POSITIONED)
            .into()
    }

    /// `base.len` on a string or an array, or a struct's field.
    pub(super) fn field(
        &mut self,
        base: &Expr,
        field: &Ident,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(base);
        if matches!(ty, Type::Struct(_)) {
            return self.struct_field(base, field);
        }
        let object = self.get(base, &ty)?.into_pointer_value();
        Ok(self.length(object).into())
    }

    /// `base[index]` — a string's byte or an array's element, bounds-checked
    /// at the `[` (`at`).
    pub(super) fn index(
        &mut self,
        base: &Expr,
        at: usize,
        index: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let (address, ty) = self.element_place(base, at, index)?;
        Ok(self.load_element(address, &ty))
    }

    /// The bounds-checked address of `base[index]` and its type: `base`,
    /// then `index`, then the check (spec §5).
    pub(super) fn element_place(
        &mut self,
        base: &Expr,
        at: usize,
        index: &Expr,
    ) -> Result<(PointerValue<'ctx>, Type), CodegenError> {
        let base_type = self.ty(base);
        let element = match &base_type {
            Type::String => Type::U8,
            array => element_of(array),
        };
        let object = self.get(base, &base_type)?.into_pointer_value();
        let i = self.get(index, &Type::I64)?.into_int_value();
        let len = self.length(object);
        self.bounds_check(len, i, at);
        Ok((
            self.element_address(object, i, self.element_size(&element)),
            element,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn string_operators_call_the_runtime() {
        let ir = ir("fun main() { let a = \"x\" + \"y\"; let e = a == \"xy\"; }");
        assert!(ir.contains("call ptr @lugha_rt_str_concat("), "{ir}");
        assert!(ir.contains("call i32 @lugha_rt_str_eq("), "{ir}");
        assert!(ir.contains("icmp ne i32"), "{ir}");
    }

    #[test]
    fn indexing_is_bounds_checked_with_integer_addressing() {
        let ir = ir("fun main() {\n    let s = \"abc\";\n    let b = s[1];\n}");
        assert!(ir.contains("icmp ult i64"), "{ir}");
        assert!(
            ir.contains("@lugha_rt_panic_bounds(i64 %len, i64 1, ptr @file, i64 3, i64 14)"),
            "{ir}"
        );
        assert!(ir.contains("ptrtoint") && ir.contains("inttoptr"), "{ir}");
        assert!(ir.contains("load i8"), "{ir}");
        assert!(
            !ir.contains("getelementptr inbounds i8"),
            "no run-time-indexed GEP: {ir}"
        );
    }
}
