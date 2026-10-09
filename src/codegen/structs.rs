//! Structs in codegen (spec §4, §7): named LLVM types, the hand-computed
//! layout every element size comes from, literals, field reads and field
//! addresses.
//!
//! Struct values are SSA aggregates (`%S`); fields are stored like array
//! elements, so a `bool` field is an `i8`.

use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, PointerValue};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::ast::{Expr, Ident};
use crate::check::{Structs, Type};

/// `(size, alignment)` in bytes of a value of type `ty` held in memory: natural
/// alignment on a 64-bit target, matching LLVM's layout for `%S` there (spec §7).
pub(super) fn layout(ty: &Type, structs: &Structs) -> (u64, u64) {
    match ty {
        Type::U8 | Type::Bool => (1, 1),
        Type::I32 => (4, 4),
        Type::I64 | Type::F64 | Type::String | Type::Array(_) => (8, 8),
        Type::Struct(name) => {
            let (mut size, mut align) = (0u64, 1u64);
            for (_, field) in &structs[name] {
                let (field_size, field_align) = layout(field, structs);
                size = size.next_multiple_of(field_align) + field_size;
                align = align.max(field_align);
            }
            (size.next_multiple_of(align), align)
        }
        _ => unreachable!("checked: {ty} is not stored in memory"),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// Declares `%S` for every struct: all opaque first, then their bodies, so
    /// fields may name structs declared later (spec §6).
    pub(super) fn declare_structs(&self) {
        for name in self.structs.keys() {
            self.context.opaque_struct_type(name);
        }
        for (name, fields) in &self.structs {
            let body: Vec<BasicTypeEnum> =
                fields.iter().map(|(_, ty)| self.storage_type(ty)).collect();
            let named = self
                .context
                .get_struct_type(name)
                .expect("declared just above");
            named.set_body(&body, false);
        }
    }

    /// Bytes per array element of type `ty` (spec §7).
    pub(super) fn element_size(&self, ty: &Type) -> u64 {
        layout(ty, &self.structs).0
    }

    /// The position and type of `field` in struct type `ty`.
    fn field_index(&self, ty: &Type, field: &str) -> (u32, Type) {
        let Type::Struct(name) = ty else {
            unreachable!("checked: {ty} is a struct")
        };
        let (index, (_, field_ty)) = self.structs[name]
            .iter()
            .enumerate()
            .find(|(_, (f, _))| f == field)
            .unwrap_or_else(|| unreachable!("checked: `{name}` has `{field}`"));
        (
            u32::try_from(index).expect("field count fits in u32"),
            field_ty.clone(),
        )
    }

    /// `S { f: e, … }`: fields evaluated in source order (spec §5), places
    /// copied (spec §4), then assembled in declaration order.
    pub(super) fn struct_literal(
        &mut self,
        expr: &Expr,
        given: &[(Ident, Expr)],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(expr);
        let mut values = Vec::with_capacity(given.len());
        for (field, value) in given {
            let (index, field_ty) = self.field_index(&ty, &field.name);
            let value = self.value_for_store(value, &field_ty)?;
            values.push((index, self.stored_form(value, &field_ty)));
        }
        let Type::Struct(name) = &ty else {
            unreachable!("checked: a literal has its struct type")
        };
        let struct_type = self
            .context
            .get_struct_type(name)
            .expect("declared before lowering");
        let mut aggregate = struct_type.get_undef();
        for (index, value) in values {
            aggregate = self
                .builder
                .build_insert_value(aggregate, value, index, "lit")
                .expect(POSITIONED)
                .into_struct_value();
        }
        Ok(aggregate.into())
    }

    /// `base.field` on a struct value.
    pub(super) fn struct_field(
        &mut self,
        base: &Expr,
        field: &Ident,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(base);
        let (index, field_ty) = self.field_index(&ty, &field.name);
        let aggregate = self.get(base, &ty)?.into_struct_value();
        let raw = self
            .builder
            .build_extract_value(aggregate, index, &field.name)
            .expect("checked: the field exists");
        Ok(self.loaded_form(raw, &field_ty))
    }

    /// The address and type of a place (spec §3): a local's slot, a field of
    /// a place, or a bounds-checked element. Evaluated once, before the value
    /// (spec §5). `true` when the place is stored in element layout.
    pub(super) fn place_address(
        &mut self,
        place: &Expr,
    ) -> Result<(PointerValue<'ctx>, Type, bool), CodegenError> {
        use crate::ast::ExprKind;
        Ok(match &place.kind {
            ExprKind::Name(name) => {
                let local = self.scopes.lookup(name);
                (local.ptr, local.ty, false)
            }
            ExprKind::Field(base, field) => {
                let (base_address, base_ty, _) = self.place_address(base)?;
                let (index, field_ty) = self.field_index(&base_ty, &field.name);
                let llvm = llvm_type(self.context, &base_ty).into_struct_type();
                let address = self
                    .builder
                    .build_struct_gep(llvm, base_address, index, &field.name)
                    .expect("checked: the field exists");
                (address, field_ty, true)
            }
            ExprKind::Index(base, open, index) => {
                let (address, ty) = self.element_place(base, open.start, index)?;
                (address, ty, true)
            }
            _ => unreachable!("checked: E0502 rejects other places"),
        })
    }

    /// A value as it is held in memory: `bool` widened to `i8`.
    pub(super) fn stored_form(
        &self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        if *ty != Type::Bool {
            return value;
        }
        let i8_type = self.context.i8_type();
        self.builder
            .build_int_z_extend(value.into_int_value(), i8_type, "byte")
            .expect(POSITIONED)
            .into()
    }

    /// A value read from memory: an `i8` narrowed back to `bool`.
    pub(super) fn loaded_form(&self, raw: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        if *ty != Type::Bool {
            return raw;
        }
        let bool_type = self.context.bool_type();
        self.builder
            .build_int_truncate(raw.into_int_value(), bool_type, "bool")
            .expect(POSITIONED)
            .into()
    }
}

#[cfg(test)]
mod tests {
    use super::layout;
    use crate::check::{Structs, Type};
    use crate::codegen::test_util::ir;

    #[test]
    fn layouts_use_natural_alignment() {
        let s = |name: &str| Type::Struct(name.to_string());
        let structs: Structs = [
            (
                "A".to_string(),
                vec![("a".into(), Type::U8), ("b".into(), Type::I64)],
            ),
            (
                "B".to_string(),
                vec![("a".into(), Type::I32), ("b".into(), Type::U8)],
            ),
            (
                "C".to_string(),
                vec![("a".into(), Type::Bool), ("b".into(), s("B"))],
            ),
        ]
        .into();
        assert_eq!(layout(&s("A"), &structs), (16, 8));
        assert_eq!(layout(&s("B"), &structs), (8, 4));
        assert_eq!(layout(&s("C"), &structs), (12, 4));
    }

    #[test]
    fn structs_are_named_types_with_byte_bools() {
        let ir = ir("struct P { x: f64, y: f64 }\nstruct F { on: bool }\n\
                     fun main() { let p = P { y: 1.0, x: 2.0 }; let f = F { on: true }; let b = f.on; }");
        assert!(ir.contains("%P = type { double, double }"), "{ir}");
        assert!(ir.contains("%F = type { i8 }"), "{ir}");
        // Constant fields fold; `y` is given first but stored in declaration order.
        assert!(
            ir.contains("store %P { double 2.000000e+00, double 1.000000e+00 }"),
            "{ir}"
        );
        assert!(
            ir.contains("extractvalue %F") && ir.contains("trunc i8"),
            "{ir}"
        );
    }

    #[test]
    fn struct_arguments_pass_a_pointer_and_fields_use_constant_geps() {
        let ir = ir("struct P { x: f64, y: f64 }\nfun getx(p: P): f64 = p.x;\n\
                     fun main() { let mut p = P { x: 1.0, y: 2.0 }; p.y = 3.0; let x = getx(p); }");
        assert!(ir.contains("define double @lugha_fn_getx(ptr"), "{ir}");
        assert!(
            ir.contains("getelementptr inbounds nuw %P, ptr %p, i32 0, i32 1")
                || ir.contains("getelementptr inbounds %P, ptr %p, i32 0, i32 1"),
            "{ir}"
        );
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        assert!(
            main.contains("alloca %P") && main.contains("call double @lugha_fn_getx(ptr %arg"),
            "{ir}"
        );
    }
}
