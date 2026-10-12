//! Growable arrays in codegen (v1 §4, §7): `push`, `pop` and `clear`.
//!
//! An array is `{ i64 len; i64 cap; T elems[cap] }`. The receiver is a place
//! (its address is evaluated once, first); growth replaces the array in that
//! place, which is its only owner, so no other pointer sees the old object.

use inkwell::IntPredicate;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue, ValueKind};

use super::CodegenError;
use super::heap::element_of;
use super::lower::{Lowerer, POSITIONED};
use super::value::Value;
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `xs.push(v)`, `xs.pop()` or `xs.clear()`.
    pub(super) fn array_builtin(
        &mut self,
        base: &Expr,
        name: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let ty = self.ty(base);
        let element = element_of(&ty);
        let (place, _, _) = self.place_address(base)?;
        match name {
            "push" => {
                let value = self.value_for_store(&args[0], &element)?;
                self.push(place, &element, value);
                Ok(Value::Void)
            }
            "pop" => Ok(Value::Val(self.pop(place, &element))),
            "clear" => {
                let array = self.array_in(place);
                let zero = self.context.i64_type().const_zero();
                self.builder.build_store(array, zero).expect(POSITIONED);
                Ok(Value::Void)
            }
            _ => unreachable!("checked: arrays have push, pop and clear"),
        }
    }

    /// The array currently held in `place`.
    fn array_in(&self, place: PointerValue<'ctx>) -> PointerValue<'ctx> {
        let ptr = self.context.ptr_type(Default::default());
        self.builder
            .build_load(ptr, place, "array")
            .expect(POSITIONED)
            .into_pointer_value()
    }

    /// Appends `value`, growing the array in `place` first if it is full.
    fn push(&mut self, place: PointerValue<'ctx>, element: &Type, value: BasicValueEnum<'ctx>) {
        let array = self.array_in(place);
        let len = self.length(array);
        let cap = self.load_i64(self.capacity_address(array), "cap");
        let full = self
            .builder
            .build_int_compare(IntPredicate::EQ, len, cap, "full")
            .expect(POSITIONED);
        let (grow, store) = (self.append("push.grow"), self.append("push.store"));
        self.builder
            .build_conditional_branch(full, grow, store)
            .expect(POSITIONED);
        self.builder.position_at_end(grow);
        let size = self
            .context
            .i64_type()
            .const_int(self.element_size(element), false);
        let grow_fn = self.runtime(
            "lugha_rt_array_grow",
            &[Type::String, Type::I64],
            Some(Type::String),
        );
        let call = self
            .builder
            .build_call(grow_fn, &[array.into(), size.into()], "grown")
            .expect(POSITIONED);
        let ValueKind::Basic(grown) = call.try_as_basic_value() else {
            unreachable!("lugha_rt_array_grow returns a pointer")
        };
        self.builder.build_store(place, grown).expect(POSITIONED);
        self.branch(store);
        self.builder.position_at_end(store);
        let array = self.array_in(place);
        let address = self.element_address(array, len, self.element_size(element));
        self.store_element(address, element, value);
        let one = self.context.i64_type().const_int(1, false);
        let longer = self
            .builder
            .build_int_add(len, one, "len")
            .expect(POSITIONED);
        self.builder.build_store(array, longer).expect(POSITIONED);
    }

    /// Removes the last element as `Some(x)`, or gives `None` when empty.
    fn pop(&mut self, place: PointerValue<'ctx>, element: &Type) -> BasicValueEnum<'ctx> {
        let option = Type::Option(Box::new(element.clone()));
        let result = self.entry_alloca(&option, "popped");
        let array = self.array_in(place);
        let len = self.length(array);
        let zero = self.context.i64_type().const_zero();
        let empty = self
            .builder
            .build_int_compare(IntPredicate::EQ, len, zero, "empty")
            .expect(POSITIONED);
        let (none, some, end) = (
            self.append("pop.none"),
            self.append("pop.some"),
            self.append("pop.end"),
        );
        self.builder
            .build_conditional_branch(empty, none, some)
            .expect(POSITIONED);
        self.builder.position_at_end(none);
        let value = self.construct_values(&option, "None", Vec::new());
        self.builder.build_store(result, value).expect(POSITIONED);
        self.branch(end);
        self.builder.position_at_end(some);
        let one = self.context.i64_type().const_int(1, false);
        let last = self
            .builder
            .build_int_sub(len, one, "last")
            .expect(POSITIONED);
        self.builder.build_store(array, last).expect(POSITIONED);
        // The element leaves the array, so it moves out without a copy.
        let item = self.load_element(
            self.element_address(array, last, self.element_size(element)),
            element,
        );
        let value = self.construct_values(&option, "Some", vec![item]);
        self.builder.build_store(result, value).expect(POSITIONED);
        self.branch(end);
        self.builder.position_at_end(end);
        let llvm = super::value::llvm_type(self.context, &option);
        self.builder
            .build_load(llvm, result, "pop")
            .expect(POSITIONED)
    }

    fn load_i64(&self, address: PointerValue<'ctx>, name: &str) -> IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        self.builder
            .build_load(i64_type, address, name)
            .expect(POSITIONED)
            .into_int_value()
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn push_grows_only_when_full() {
        let ir = ir("fun main() { let mut xs = [1]; xs.push(2); }");
        let grow = ir.find("push.grow:").expect("grow block");
        let call = ir
            .find("call ptr @lugha_rt_array_grow(")
            .expect("grow call");
        assert!(grow < call && ir.contains("icmp eq i64 %len, %cap"), "{ir}");
    }

    #[test]
    fn pop_builds_an_option() {
        let ir = ir("fun main() { let mut xs = [1]; let p = xs.pop(); }");
        assert!(ir.contains("%\"Option<i64>\""), "{ir}");
        assert!(ir.contains("pop.none:") && ir.contains("pop.some:"), "{ir}");
    }
}
