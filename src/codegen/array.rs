//! Arrays in codegen: allocation, list and repeat literals, and
//! `for x of xs` (spec §4, §5, §7).
//!
//! An array is a pointer to `{ i64 len, T elems[len] }` on the GC heap;
//! element sizes and storage live in `heap.rs`.

use inkwell::IntPredicate;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue, ValueKind};

use super::CodegenError;
use super::control::Loop;
use super::heap::element_of;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use crate::ast::{Block, Expr, Ident};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// A new array of `len` elements of type `element`, with its length stored.
    pub(super) fn new_array(&self, len: IntValue<'ctx>, element: &Type) -> PointerValue<'ctx> {
        let i64_type = self.context.i64_type();
        let size = i64_type.const_int(self.element_size(element), false);
        let data = self
            .builder
            .build_int_mul(len, size, "bytes")
            .expect(POSITIONED);
        let bytes = self
            .builder
            .build_int_add(data, i64_type.const_int(8, false), "bytes")
            .expect(POSITIONED);
        // The runtime returns a pointer; `string` is codegen's pointer-typed stand-in here.
        let alloc = self.runtime("lugha_rt_alloc", &[Type::I64], Some(Type::String));
        let call = self
            .builder
            .build_call(alloc, &[bytes.into()], "array")
            .expect(POSITIONED);
        let ValueKind::Basic(array) = call.try_as_basic_value() else {
            unreachable!("lugha_rt_alloc returns a pointer")
        };
        let array = array.into_pointer_value();
        self.builder.build_store(array, len).expect(POSITIONED);
        array
    }

    /// Runs `body(i)` for `i` in `0..len`.
    pub(super) fn count_loop(
        &mut self,
        len: IntValue<'ctx>,
        mut body: impl FnMut(&mut Self, IntValue<'ctx>) -> Result<(), CodegenError>,
    ) -> Result<(), CodegenError> {
        let i64_type = self.context.i64_type();
        let counter = self.entry_alloca(&Type::I64, "i");
        self.builder
            .build_store(counter, i64_type.const_zero())
            .expect(POSITIONED);
        let (cond, step_block, exit) = (
            self.append("each.cond"),
            self.append("each.body"),
            self.append("each.end"),
        );
        self.branch(cond);
        self.builder.position_at_end(cond);
        let i = self
            .builder
            .build_load(i64_type, counter, "i")
            .expect(POSITIONED)
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::SLT, i, len, "more")
            .expect(POSITIONED);
        self.builder
            .build_conditional_branch(more, step_block, exit)
            .expect(POSITIONED);
        self.builder.position_at_end(step_block);
        body(self, i)?;
        // Can't overflow: `i < len` held.
        let next = self
            .builder
            .build_int_add(i, i64_type.const_int(1, false), "next")
            .expect(POSITIONED);
        self.builder.build_store(counter, next).expect(POSITIONED);
        self.branch(cond);
        self.builder.position_at_end(exit);
        Ok(())
    }

    /// `[a, b, c]`: elements in order (spec §5); places are copied (spec §4).
    pub(super) fn array_literal(
        &mut self,
        expr: &Expr,
        elements: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let element = element_of(&self.ty(expr));
        let i64_type = self.context.i64_type();
        let array = self.new_array(i64_type.const_int(elements.len() as u64, false), &element);
        for (index, e) in elements.iter().enumerate() {
            let value = self.value_for_store(e, &element)?;
            let address = self.element_address(
                array,
                i64_type.const_int(index as u64, false),
                self.element_size(&element),
            );
            self.store_element(address, &element, value);
        }
        Ok(array.into())
    }

    /// `[value; count]`: `value` first, then `count`; a negative count panics;
    /// every element is an independent copy of `value` (spec §4, §5).
    pub(super) fn repeat(
        &mut self,
        expr: &Expr,
        value: &Expr,
        count: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let element = element_of(&self.ty(expr));
        let value = self.get(value, &element)?;
        let count = self.get(count, &Type::I64)?.into_int_value();
        let zero = self.context.i64_type().const_zero();
        let negative = self
            .builder
            .build_int_compare(IntPredicate::SLT, count, zero, "negative")
            .expect(POSITIONED);
        self.panic_if(negative, "negative array length", expr.span.start);
        let array = self.new_array(count, &element);
        let (deep, size) = (
            element.contains_array(&self.structs),
            self.element_size(&element),
        );
        self.count_loop(count, |this, i| {
            let fill = if deep {
                this.deep_copy(value, &element)
            } else {
                value
            };
            let address = this.element_address(array, i, size);
            this.store_element(address, &element, fill);
            Ok(())
        })?;
        Ok(array.into())
    }

    /// `for var of iter { body }`: `iter` evaluated once, uncopied, its length
    /// read once; `var` borrows each element (spec §5).
    pub(super) fn for_of(
        &mut self,
        var: &Ident,
        iter: &Expr,
        body: &Block,
    ) -> Result<(), CodegenError> {
        let array_type = self.ty(iter);
        let element = element_of(&array_type);
        let array = self.get(iter, &array_type)?.into_pointer_value();
        let len = self.length(array);
        let i64_type = self.context.i64_type();
        let counter = self.entry_alloca(&Type::I64, "of.i");
        self.builder
            .build_store(counter, i64_type.const_zero())
            .expect(POSITIONED);
        let (cond, body_block, step, exit) = (
            self.append("of.cond"),
            self.append("of.body"),
            self.append("of.step"),
            self.append("of.end"),
        );
        self.branch(cond);

        self.builder.position_at_end(cond);
        let i = self
            .builder
            .build_load(i64_type, counter, "i")
            .expect(POSITIONED)
            .into_int_value();
        let more = self
            .builder
            .build_int_compare(IntPredicate::SLT, i, len, "more")
            .expect(POSITIONED);
        self.builder
            .build_conditional_branch(more, body_block, exit)
            .expect(POSITIONED);

        self.builder.position_at_end(body_block);
        self.scopes.push();
        let address = self.element_address(array, i, self.element_size(&element));
        let item = self.load_element(address, &element);
        let slot = self.entry_alloca(&element, &var.name);
        self.builder.build_store(slot, item).expect(POSITIONED);
        // Borrowed: returning or storing it copies (spec §4, PRP-014).
        self.scopes.declare(
            &var.name,
            Local {
                ptr: slot,
                ty: element,
                borrowed: true,
            },
        );
        self.loops.push(Loop { next: step, exit });
        self.block(body)?;
        self.loops.pop();
        self.scopes.pop();
        self.branch(step);

        self.builder.position_at_end(step);
        let i = self
            .builder
            .build_load(i64_type, counter, "i")
            .expect(POSITIONED)
            .into_int_value();
        let next = self
            .builder
            .build_int_add(i, i64_type.const_int(1, false), "next")
            .expect(POSITIONED);
        self.builder.build_store(counter, next).expect(POSITIONED);
        self.branch(cond);
        self.builder.position_at_end(exit);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn literals_allocate_header_and_elements() {
        let ir = ir(
            "fun f(t: bool) { let xs: i32[] = [1, 2, 3]; let b = [t]; let x = b[0]; }\nfun main() {}",
        );
        assert!(
            ir.contains("@lugha_rt_alloc(i64 20)"),
            "8 + 3 * 4 bytes: {ir}"
        );
        assert!(
            ir.contains("store i64 3, ptr %array"),
            "length header: {ir}"
        );
        assert!(
            ir.contains("zext i1") && ir.contains("trunc i8"),
            "bool stored as i8: {ir}"
        );
    }

    #[test]
    fn negative_repeat_counts_panic() {
        let ir = ir("fun main() { let n = 2; let xs = [0; n]; }");
        assert!(
            ir.contains("icmp slt i64") && ir.contains("negative array length"),
            "{ir}"
        );
    }

    #[test]
    fn for_of_reads_the_length_once() {
        let ir = ir("fun main() { let xs = [1, 2]; for x of xs { } }");
        let body = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        assert_eq!(body.matches("%len = load i64").count(), 1, "{ir}");
        let cond = &body[body.find("of.cond:").unwrap()..];
        let cond = &cond[..cond.find("of.body:").unwrap()];
        assert!(
            cond.contains("icmp slt i64 %i, %len") && !cond.contains("load ptr"),
            "{ir}"
        );
    }
}
