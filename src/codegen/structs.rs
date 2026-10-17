//! Structs in codegen (spec §4, §7): named LLVM types, the hand-computed
//! layout every element size comes from, literals, field reads and field
//! addresses.
//!
//! Struct values are SSA aggregates (`%S`); fields are stored like array
//! elements, so a `bool` field is an `i8`.

use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use crate::ast::{Expr, Ident};
use crate::check::{Enums, Structs, Type};

/// True if a struct's fields in `Checked.structs` are an `extern union`'s:
/// they start with the checker's mark (v17, milestone 104).
pub(super) fn is_union_fields(fields: &[(String, Type)]) -> bool {
    fields
        .first()
        .is_some_and(|(f, _)| f == crate::check::UNION_MARK)
}

/// `(size, alignment)` in bytes of a value of type `ty` held in memory: natural
/// alignment on a 64-bit target, matching LLVM's layout for `%S` there (spec §7).
pub(super) fn layout(ty: &Type, structs: &Structs, enums: &Enums) -> (u64, u64) {
    match ty {
        Type::U8 | Type::I8 | Type::Bool => (1, 1),
        // `()` takes no memory (v17, milestone 107).
        Type::Unit => (0, 1),
        Type::I16 | Type::U16 => (2, 2),
        Type::I32 | Type::U32 | Type::F32 | Type::Char => (4, 4),
        Type::I64 | Type::U64 | Type::F64 | Type::String | Type::Array(_) | Type::Ref(_) => (8, 8),
        Type::Task(_) | Type::Chan(_) | Type::Actor(_) => (8, 8),
        // A handle is a C pointer (v16).
        Type::Handle(_) => (8, 8),
        // `{ code, env }` (v3 draft §7), `{ table, box }` (v5 draft §7).
        Type::Fun(..) | Type::Interface(..) => (16, 8),
        // An `extern union` (v17): its largest field, at its largest alignment.
        Type::Struct(..) if is_union_fields(&structs[&ty.to_string()]) => {
            let (mut size, mut align) = (0u64, 1u64);
            for (_, field) in &structs[&ty.to_string()][1..] {
                let (field_size, field_align) = layout(field, structs, enums);
                size = size.max(field_size);
                align = align.max(field_align);
            }
            (size.next_multiple_of(align), align)
        }
        Type::Struct(..) => {
            let (mut size, mut align) = (0u64, 1u64);
            for (_, field) in &structs[&ty.to_string()] {
                let (field_size, field_align) = layout(field, structs, enums);
                size = size.next_multiple_of(field_align) + field_size;
                align = align.max(field_align);
            }
            (size.next_multiple_of(align), align)
        }
        // `Option` of a handle is held as C's nullable pointer (v17, milestone 106).
        Type::Option(inner) if matches!(**inner, Type::Handle(_)) => (8, 8),
        Type::Enum(_, _) | Type::Option(_) => super::enums::enum_layout(ty, structs, enums),
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
            let body: Vec<BasicTypeEnum> = if is_union_fields(fields) {
                // A union: whole units of its alignment, as many as its size needs.
                let ty = Type::Struct(name.clone(), Vec::new());
                let (size, align) = layout(&ty, &self.structs, &self.enums);
                let unit = match align {
                    1 => self.context.i8_type(),
                    2 => self.context.i16_type(),
                    4 => self.context.i32_type(),
                    _ => self.context.i64_type(),
                };
                vec![
                    unit.array_type(u32::try_from(size / align).expect("small union"))
                        .into(),
                ]
            } else {
                fields.iter().map(|(_, ty)| self.storage_type(ty)).collect()
            };
            let named = self
                .context
                .get_struct_type(name)
                .expect("declared just above");
            named.set_body(&body, false);
        }
    }

    /// Bytes per array element of type `ty` (spec §7).
    pub(super) fn element_size(&self, ty: &Type) -> u64 {
        layout(ty, &self.structs, &self.enums).0
    }

    /// True if `ty` is an `extern union` (v17, milestone 104).
    pub(super) fn is_union(&self, ty: &Type) -> bool {
        matches!(ty, Type::Struct(..))
            && self
                .structs
                .get(&ty.to_string())
                .is_some_and(|f| is_union_fields(f))
    }

    /// The position and type of `field` in struct type `ty`.
    pub(super) fn field_index(&self, ty: &Type, field: &str) -> (u32, Type) {
        let Type::Struct(..) = ty else {
            unreachable!("checked: {ty} is a struct")
        };
        let name = ty.to_string();
        let (index, (_, field_ty)) = self.structs[&name]
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
        base: Option<&Expr>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(expr);
        let mut values = Vec::with_capacity(given.len());
        for (field, value) in given {
            let (index, field_ty) = self.field_index(&ty, &field.name);
            let value = self.value_for_store(value, &field_ty)?;
            values.push((index, self.stored_form(value, &field_ty)));
        }
        // `..base` (v17): evaluated once, after the written fields, and stored
        // like any value (copied from a place, moved on its last use); the
        // written fields then replace its own.
        let start = match base {
            Some(base) => Some(self.value_for_store(base, &ty)?.into_struct_value()),
            None => None,
        };
        let Type::Struct(..) = &ty else {
            unreachable!("checked: a literal has its struct type")
        };
        // A union's literal: zeroed bytes with its one field written (v17).
        if self.is_union(&ty) {
            let slot = self.entry_alloca(&ty, "union");
            let zero = super::value::llvm_type(self.context, &ty).const_zero();
            self.builder.build_store(slot, zero).expect(POSITIONED);
            // The value is already in element layout, as a field is stored.
            if let Some((_, value)) = values.first() {
                self.builder.build_store(slot, *value).expect(POSITIONED);
            }
            let llvm = super::value::llvm_type(self.context, &ty);
            return Ok(self
                .builder
                .build_load(llvm, slot, "union")
                .expect(POSITIONED));
        }
        let struct_type = self
            .context
            .get_struct_type(&ty.to_string())
            .expect("declared before lowering");
        let mut aggregate = start.unwrap_or_else(|| struct_type.get_undef());
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
        // A union's field is its bytes read as that field's type (v17).
        if self.is_union(&ty) {
            let value = self.get(base, &ty)?;
            let slot = self.entry_alloca(&ty, "union");
            self.builder.build_store(slot, value).expect(POSITIONED);
            return Ok(self.load_element(slot, &field_ty));
        }
        let aggregate = self.get(base, &ty)?.into_struct_value();
        let raw = self
            .builder
            .build_extract_value(aggregate, index, &field.name)
            .expect("checked: the field exists");
        Ok(self.loaded_form(raw, &field_ty))
    }

    /// A value as it is held in memory: `bool` widened to `i8`, `Option` of
    /// a handle as a nullable pointer (v17, milestone 106).
    pub(super) fn stored_form(
        &self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        if super::externs::is_option_handle(ty) {
            return self.option_to_pointer(value, ty);
        }
        if *ty != Type::Bool {
            return value;
        }
        let i8_type = self.context.i8_type();
        self.builder
            .build_int_z_extend(value.into_int_value(), i8_type, "byte")
            .expect(POSITIONED)
            .into()
    }

    /// A value read from memory: an `i8` narrowed back to `bool`, a
    /// nullable pointer back to `Option` of a handle.
    pub(super) fn loaded_form(&self, raw: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        if super::externs::is_option_handle(ty) {
            return self.pointer_to_option(raw, ty);
        }
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
        let s = |name: &str| Type::Struct(name.to_string(), Vec::new());
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
        assert_eq!(layout(&s("A"), &structs, &Default::default()), (16, 8));
        assert_eq!(layout(&s("B"), &structs, &Default::default()), (8, 4));
        assert_eq!(layout(&s("C"), &structs, &Default::default()), (12, 4));
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
