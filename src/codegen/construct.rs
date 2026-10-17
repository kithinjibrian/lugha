//! Building enum and `Option` values in codegen (v1 §5, §7): `E.V(args)`,
//! `E.V`, `Some(x)` and `None`. Payloads are evaluated left to right and
//! stored like any value; a slot that leads back to the enum gets a heap
//! copy and holds its pointer.

use inkwell::values::{BasicValueEnum, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use crate::ast::{Expr, ExprKind};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// The variant a call's callee constructs (`E.V(..)` or `Some(..)`), if it is one.
    pub(super) fn constructed_variant(&self, callee: &Expr) -> Option<String> {
        match &callee.kind {
            // `Ok`/`Err` build the prelude's `Result` (v4).
            ExprKind::Name(name)
                if matches!(name.as_str(), "Some" | "Ok" | "Err")
                    && !self.functions.contains_key(name) =>
            {
                Some(name.clone())
            }
            ExprKind::Field(base, variant) if self.ty(base) == Type::Unit => {
                Some(variant.name.clone())
            }
            _ => None,
        }
    }

    /// `E.V(args)`, `E.V`, `Some(x)` or `None` of enum type `ty`: payloads
    /// are evaluated left to right and stored like any value (v1 §5).
    pub(super) fn construct(
        &mut self,
        ty: &Type,
        variant: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let (_, payloads) = self.variant(ty, variant);
        let mut values = Vec::with_capacity(args.len());
        for (arg, payload) in args.iter().zip(&payloads) {
            values.push(self.value_for_store(arg, payload)?);
        }
        Ok(self.construct_values(ty, variant, values))
    }

    /// Builds `variant` of enum `ty` from payload values already lowered.
    pub(super) fn construct_values(
        &mut self,
        ty: &Type,
        variant: &str,
        values: Vec<BasicValueEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let (tag, payloads) = self.variant(ty, variant);
        let slot = self.entry_alloca(ty, "enum");
        let named = self
            .context
            .get_struct_type(&ty.to_string())
            .expect("declared before lowering");
        let at = self
            .builder
            .build_struct_gep(named, slot, 0, "tag.at")
            .expect("enums have two fields");
        let tag = self.context.i32_type().const_int(u64::from(tag), false);
        self.builder.build_store(at, tag).expect(POSITIONED);
        for (i, (value, payload)) in values.into_iter().zip(&payloads).enumerate() {
            let index = u32::try_from(i).expect("payload count fits in u32");
            let address = self.payload_address(ty, slot, &payloads, index);
            if self.indirect(ty, payload) {
                // The slot holds a pointer to a heap copy, made once and never mutated.
                let size = self
                    .context
                    .i64_type()
                    .const_int(self.element_size(payload), false);
                let alloc = self.runtime("lugha_rt_alloc", &[Type::I64], Some(Type::String));
                let call = self
                    .builder
                    .build_call(alloc, &[size.into()], "boxed")
                    .expect(POSITIONED);
                let ValueKind::Basic(heap) = call.try_as_basic_value() else {
                    unreachable!("lugha_rt_alloc returns a pointer")
                };
                let heap = heap.into_pointer_value();
                self.store_element(heap, payload, value);
                self.builder.build_store(address, heap).expect(POSITIONED);
            } else {
                self.store_element(address, payload, value);
            }
        }
        self.builder
            .build_load(named, slot, "enum.value")
            .expect(POSITIONED)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn recursive_slots_are_heap_pointers() {
        let ir = ir("enum List { Nil, Cons(i64, List) }\n\
                     fun one(): List = List.Cons(1, List.Nil);\nfun none(): List = List.Nil;\nfun main() {}");
        assert!(ir.contains("%List = type { i32, [2 x i64] }"), "{ir}");
        let body = |name: &str| {
            let start = &ir[ir.find(&format!("@lugha_fn_{name}(")).unwrap()..];
            start[..start.find("\n}").unwrap()].to_string()
        };
        assert!(
            body("one").contains("call ptr @lugha_rt_alloc(i64 24)"),
            "{ir}"
        );
        assert!(!body("none").contains("lugha_rt_alloc"), "{ir}");
    }
}
