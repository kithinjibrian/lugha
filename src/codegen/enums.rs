//! Enums and `Option` in codegen (v1 §7): `{ i32 tag, [k x iA] payload }`
//! values, their layout, named types and payload addresses (construction
//! is in `construct.rs`).
//!
//! Variants are numbered in declaration order; `Option` is `None` = 0,
//! `Some` = 1. A variant's payloads are laid out as an anonymous struct at
//! the payload field, so they are reached with constant-index GEPs.

use std::collections::BTreeSet;

use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::PointerValue;

use super::lower::{Lowerer, POSITIONED};
use super::structs::layout;
use crate::check::{Enums, Structs, Type, indirect_slot};

/// The variants of an enum or `Option` type, in tag order.
pub(super) fn variants(ty: &Type, enums: &Enums) -> Vec<(String, Vec<Type>)> {
    match ty {
        Type::Enum(name, _) => enums[name].clone(),
        Type::Option(inner) => vec![
            ("None".into(), Vec::new()),
            ("Some".into(), vec![(**inner).clone()]),
        ],
        _ => unreachable!("checked: {ty} is an enum"),
    }
}

/// `(bytes, alignment)` of the payload area: the largest variant's payloads
/// laid out as a struct, rounded up to whole units of the largest alignment.
pub(super) fn payload_area(ty: &Type, structs: &Structs, enums: &Enums) -> (u64, u64) {
    let (mut size, mut align) = (0u64, 1u64);
    for (_, payloads) in variants(ty, enums) {
        let (mut s, mut a) = (0u64, 1u64);
        for p in &payloads {
            // A slot leading back to `ty` is a pointer to a heap copy (v1 §7).
            let (ps, pa) = if indirect_slot(ty, p, structs, enums) {
                (8, 8)
            } else {
                layout(p, structs, enums)
            };
            s = s.next_multiple_of(pa) + ps;
            a = a.max(pa);
        }
        size = size.max(s.next_multiple_of(a));
        align = align.max(a);
    }
    (size.next_multiple_of(align), align)
}

/// `(size, alignment)` of an enum value: an `i32` tag, then the payload area.
pub(super) fn enum_layout(ty: &Type, structs: &Structs, enums: &Enums) -> (u64, u64) {
    let (bytes, align) = payload_area(ty, structs, enums);
    let whole = align.max(4);
    (
        (4u64.next_multiple_of(align) + bytes).next_multiple_of(whole),
        whole,
    )
}

/// Every enum and `Option` type in `ty`, including `ty` itself.
fn collect(ty: &Type, out: &mut BTreeSet<String>, all: &mut Vec<Type>) {
    match ty {
        Type::Array(inner) => collect(inner, out, all),
        Type::Option(inner) => {
            collect(inner, out, all);
            if out.insert(ty.to_string()) {
                all.push(ty.clone());
            }
        }
        Type::Enum(name, _) if out.insert(name.clone()) => all.push(ty.clone()),
        _ => {}
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// Declares a named LLVM type for every enum and every `Option<T>` the
    /// program uses, before struct bodies, which may hold them.
    pub(super) fn declare_enums(&self) {
        let (mut names, mut all) = (BTreeSet::new(), Vec::new());
        for name in self.enums.keys() {
            collect(&Type::Enum(name.clone(), Vec::new()), &mut names, &mut all);
        }
        let mut seen: Vec<Type> = self.types.clone();
        seen.extend(self.structs.values().flatten().map(|(_, t)| t.clone()));
        seen.extend(self.enums.values().flatten().flat_map(|(_, ps)| ps.clone()));
        // Every written type, so a signature's `Option<T>` is declared too.
        seen.extend(self.annotations.values().cloned());
        for ty in &seen {
            collect(ty, &mut names, &mut all);
        }
        for ty in &all {
            let (bytes, align) = payload_area(ty, &self.structs, &self.enums);
            // Payload alignments are 1, 4 or 8 (spec §7 element sizes).
            let unit = match align {
                1 => self.context.i8_type(),
                4 => self.context.i32_type(),
                _ => self.context.i64_type(),
            };
            let area =
                unit.array_type(u32::try_from(bytes / align).expect("payload fits in u32 units"));
            let named = self.context.opaque_struct_type(&ty.to_string());
            named.set_body(&[self.context.i32_type().into(), area.into()], false);
        }
    }

    /// The anonymous struct a variant's payloads of enum `ty` are stored as.
    pub(super) fn variant_struct(&self, ty: &Type, payloads: &[Type]) -> StructType<'ctx> {
        let fields: Vec<BasicTypeEnum> = payloads
            .iter()
            .map(|p| {
                if self.indirect(ty, p) {
                    self.context.ptr_type(Default::default()).into()
                } else {
                    self.storage_type(p)
                }
            })
            .collect();
        self.context.struct_type(&fields, false)
    }

    /// True if payload `slot` of enum `ty` is stored behind a pointer.
    pub(super) fn indirect(&self, ty: &Type, slot: &Type) -> bool {
        indirect_slot(ty, slot, &self.structs, &self.enums)
    }

    /// The tag and payload types of `variant` in enum type `ty`.
    pub(super) fn variant(&self, ty: &Type, variant: &str) -> (u32, Vec<Type>) {
        let all = variants(ty, &self.enums);
        let (tag, (_, payloads)) = all
            .into_iter()
            .enumerate()
            .find(|(_, (v, _))| v == variant)
            .unwrap_or_else(|| unreachable!("checked: `{ty}` has `{variant}`"));
        (
            u32::try_from(tag).expect("variant count fits in u32"),
            payloads,
        )
    }

    /// Where payload `index` of the variant `payloads` lives inside the enum at `slot`.
    pub(super) fn payload_address(
        &self,
        ty: &Type,
        slot: PointerValue<'ctx>,
        payloads: &[Type],
        index: u32,
    ) -> PointerValue<'ctx> {
        let named = self
            .context
            .get_struct_type(&ty.to_string())
            .expect("declared before lowering");
        let area = self
            .builder
            .build_struct_gep(named, slot, 1, "payload")
            .expect("enums have two fields");
        let variant = self.variant_struct(ty, payloads);
        self.builder
            .build_struct_gep(variant, area, index, "field")
            .expect("checked payload index")
    }

    /// The tag of the enum value at `slot`.
    pub(super) fn tag(
        &self,
        ty: &Type,
        slot: PointerValue<'ctx>,
    ) -> inkwell::values::IntValue<'ctx> {
        let named = self
            .context
            .get_struct_type(&ty.to_string())
            .expect("declared before lowering");
        let at = self
            .builder
            .build_struct_gep(named, slot, 0, "tag.at")
            .expect("enums have two fields");
        self.builder
            .build_load(self.context.i32_type(), at, "tag")
            .expect(POSITIONED)
            .into_int_value()
    }
}

#[cfg(test)]
mod tests {
    use super::enum_layout;
    use crate::check::{Enums, Structs, Type};
    use crate::codegen::test_util::ir;

    #[test]
    fn enum_layouts_put_the_payload_after_the_tag() {
        let enums: Enums = [
            (
                "S".to_string(),
                vec![("Circle".into(), vec![Type::F64]), ("Empty".into(), vec![])],
            ),
            (
                "B".to_string(),
                vec![("A".into(), vec![Type::U8]), ("B".into(), vec![])],
            ),
        ]
        .into();
        let structs: Structs = [(
            "P".to_string(),
            vec![
                ("e".into(), Type::Enum("B".into(), Vec::new())),
                ("x".into(), Type::I64),
            ],
        )]
        .into();
        let e = |n: &str| Type::Enum(n.into(), Vec::new());
        assert_eq!(enum_layout(&e("S"), &structs, &enums), (16, 8));
        assert_eq!(enum_layout(&e("B"), &structs, &enums), (8, 4));
        assert_eq!(
            crate::codegen::structs::layout(
                &Type::Struct("P".into(), Vec::new()),
                &structs,
                &enums
            ),
            (16, 8)
        );
        assert_eq!(
            enum_layout(&Type::Option(Box::new(Type::I32)), &structs, &enums),
            (8, 4)
        );
    }

    #[test]
    fn enums_are_named_tagged_types() {
        let ir = ir(
            "enum Shape { Circle(f64), Rect(f64, f64), Empty }\nfun main() { let s = Shape.Rect(1.0, 2.0); let o = Some(3); }",
        );
        assert!(ir.contains("%Shape = type { i32, [2 x i64] }"), "{ir}");
        assert!(
            ir.contains("%\"Option<i64>\" = type { i32, [1 x i64] }"),
            "{ir}"
        );
    }
}
