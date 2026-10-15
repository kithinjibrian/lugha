//! Using interface values in codegen (v5 draft §9, PRP-047): calls through
//! a value's table, and copies through its copy slot. Building them is in
//! `interfaces.rs`.

use inkwell::IntPredicate;
use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicValueEnum, PointerValue, StructValue, ValueKind};

use super::CodegenError;
use super::interfaces::SLOTS;
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, llvm_type};
use crate::ast::{Expr, Type as Annotation};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// A copy of interface value `pair`: the same table, and a new box from
    /// the table's copy slot.
    pub(super) fn copy_interface(&mut self, pair: StructValue<'ctx>) -> BasicValueEnum<'ctx> {
        let (table, boxed) = self.parts(pair);
        let ptr = self.context.ptr_type(Default::default());
        let copy = self.slot(table, 1);
        let fn_type = ptr.fn_type(&[ptr.into()], false);
        let call = self
            .builder
            .build_indirect_call(fn_type, copy, &[boxed.into()], "copy")
            .expect(POSITIONED);
        let ValueKind::Basic(copy) = call.try_as_basic_value() else {
            unreachable!("a box copy returns the new box")
        };
        self.pair(table, copy.into_pointer_value()).into()
    }

    /// The table and the box of an interface value.
    pub(super) fn parts(
        &self,
        pair: StructValue<'ctx>,
    ) -> (PointerValue<'ctx>, PointerValue<'ctx>) {
        let part = |i, name| {
            self.builder
                .build_extract_value(pair, i, name)
                .expect("a pair has 2 fields")
                .into_pointer_value()
        };
        (part(0, "table"), part(1, "box"))
    }

    /// The pointer in slot `index` of `table`. Every table starts with the
    /// identity and copy slots, so a two-pointer view reaches those; method
    /// slots use the interface's full table type.
    pub(super) fn slot(&self, table: PointerValue<'ctx>, index: u32) -> PointerValue<'ctx> {
        let ptr = self.context.ptr_type(Default::default());
        let fields = vec![ptr.into(); index as usize + 1];
        let view = self.context.struct_type(&fields, false);
        let at = self
            .builder
            .build_struct_gep(view, table, index, "slot.at")
            .expect("the slot exists");
        self.builder
            .build_load(ptr, at, "slot")
            .expect(POSITIONED)
            .into_pointer_value()
    }

    /// `base.name(args)` on an interface value: the method from its table,
    /// called with the box first, then the arguments as for any call.
    pub(super) fn dynamic_call(
        &mut self,
        base: &Expr,
        interface: &Type,
        name: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let Type::Interface(key, targs) = interface else {
            unreachable!("checked: a dynamic call is on an interface value")
        };
        // A generic interface's signatures at its arguments (v6).
        let methods = self.interfaces[key].methods_at(targs);
        let index = methods.iter().position(|m| m.name == name);
        let index = index.expect("checked: the interface declares the method");
        let method = methods[index].clone();
        let ty = self.ty(base);
        // A `mut self` method writes the box in place, so the receiver is
        // reached as a place, making each array on the way unique (v15):
        // an array cloned that way copies its interface values' boxes.
        // Like any `mut self` call (v15, milestone 57), the place's indexes
        // come first and the place itself after the arguments.
        let (plan, early) = if method.mutable {
            (Some(self.place_plan(base)?), None)
        } else {
            (None, Some(self.get(base, &ty)?.into_struct_value()))
        };
        let arguments = self.arguments(args, &method.params, false)?;
        let pair = if let Some(plan) = &plan {
            let (address, place_ty, element) = self.place_commit(base, plan);
            if element {
                self.load_element(address, &place_ty)
            } else {
                let llvm = llvm_type(self.context, &place_ty);
                self.builder
                    .build_load(llvm, address, "receiver")
                    .expect(POSITIONED)
            }
            .into_struct_value()
        } else {
            early.expect("a `self` receiver was read before the arguments")
        };
        let (table, boxed) = self.parts(pair);
        let slot = u32::try_from(index).expect("method count fits in u32") + SLOTS;
        let code = self.slot(table, slot);
        let mut values = vec![boxed.into()];
        values.extend(arguments);
        let ptr = self.context.ptr_type(Default::default());
        let mut types: Vec<BasicMetadataTypeEnum> = vec![ptr.into()];
        types.extend(method.params.iter().map(|p| self.param_type(p)));
        let fn_type = match &method.ret {
            Type::Void => self.context.void_type().fn_type(&types, false),
            ret => llvm_type(self.context, ret).fn_type(&types, false),
        };
        let site = self
            .builder
            .build_indirect_call(fn_type, code, &values, "dyn")
            .expect(POSITIONED);
        Ok(match site.try_as_basic_value() {
            ValueKind::Basic(result) => Value::Val(result),
            ValueKind::Instruction(_) => Value::Void,
        })
    }

    /// A type pattern `name: T` against interface value `pair`: continues
    /// when the table's identity is `T`'s, else jumps to `next`. Gives the
    /// box's value and `T`, for a binding (`_: T` ignores them).
    pub(super) fn type_test(
        &mut self,
        pair: StructValue<'ctx>,
        written: &Annotation,
        next: BasicBlock<'ctx>,
    ) -> (BasicValueEnum<'ctx>, Type) {
        let ty = self.annotation_type(written);
        let (table, boxed) = self.parts(pair);
        let identity = self.slot(table, 0);
        let same = self
            .builder
            .build_int_compare(IntPredicate::EQ, identity, self.type_id(&ty), "is.type")
            .expect(POSITIONED);
        self.or_next(same, next, "type.ok");
        (self.load_element(boxed, &ty), ty)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::interfaces::tests::{SHAPES, body};
    use crate::codegen::test_util::ir;

    #[test]
    fn calls_load_the_slot_and_pass_the_box() {
        let ir = ir(&format!(
            "{SHAPES}fun f(s: Shape): f64 = s.area();\nfun main() {{ let n = f(Circle {{ r: 1.0 }}); }}"
        ));
        let f = body(&ir, "f");
        assert!(f.contains("load ptr"), "{f}");
        assert!(f.contains("call double %"), "an indirect call: {f}");
    }

    #[test]
    fn copies_go_through_the_tables_copy_slot() {
        let ir = ir(&format!(
            "{SHAPES}fun main() {{ let a: Shape = Circle {{ r: 1.0 }}; let b = a; println(a.area() + b.area()); }}"
        ));
        assert!(body(&ir, "main").contains("@lugha_copy_5Shape("), "{ir}");
        let copy = body(&ir, "main");
        assert!(!copy.is_empty());
        let helper = &ir[ir.find("@lugha_copy_5Shape(").unwrap()..];
        assert!(
            helper.contains("call ptr %"),
            "the copy slot is called: {ir}"
        );
    }

    #[test]
    fn type_patterns_compare_the_identity_slot() {
        let ir = ir(&format!(
            "{SHAPES}fun f(s: Shape): f64 = match s {{ c: Circle => c.r, _: Square => 1.0, _ => 0.0 }};\nfun main() {{ let n = f(Circle {{ r: 1.0 }}); }}"
        ));
        let f = body(&ir, "f");
        assert!(
            f.contains("icmp eq ptr") && f.contains("@lugha_type_Circle"),
            "{f}"
        );
        assert!(f.contains("@lugha_type_Square"), "{f}");
        assert!(
            !f.contains("switch"),
            "type patterns use the arm chain: {f}"
        );
    }

    #[test]
    fn generic_interface_values_use_tables_named_with_their_arguments() {
        let ir = ir("interface Source<T> {\n\
                fun next(mut self): Option<T>;\n\
                fun skip(mut self): bool = match self.next() { Some(_) => true, None => false };\n\
            }\n\
            struct Count { n: i64 }\nimpl Count { fun next(mut self): Option<i64> = Some(self.n); }\n\
            struct Words { at: i64 }\nimpl Words { fun next(mut self): Option<string> = None; }\n\
            fun main() {\n\
                let mut s: Source<i64> = Count { n: 1 };\n\
                let b = s.skip();\n\
                let mut w: Source<string> = Words { at: 0 };\n\
                let x = w.next();\n\
            }");
        let table = ir
            .lines()
            .find(|l| l.starts_with("@\"lugha_vt_Count.Source<i64>\" = "))
            .unwrap_or_else(|| panic!("a table for Count as Source<i64>: {ir}"));
        assert!(table.contains("@lugha_fn_Count.next"), "{table}");
        assert!(
            table.contains("@\"lugha_fn_Source<i64>.skip<Count>\""),
            "the default's slot: {table}"
        );
        let main = body(&ir, "main");
        assert!(
            main.contains("call %\"Option<string>\" %"),
            "a dynamic call at the substituted signature: {main}"
        );
    }
}
