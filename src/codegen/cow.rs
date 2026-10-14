//! Copy-on-write arrays (v15 draft decisions 22–24, milestone 55).
//!
//! A copy site doesn't copy an array: it sets the array's *shared* flag
//! (the top bit of its header's `cap` word) and shares the pointer. Before
//! anything writes into an array, `own` checks the flag; a shared array is
//! cloned one level deep (its own elements copied, any arrays inside them
//! marked shared in turn) and the clone is stored back into the slot that
//! held it. The flag is sticky, so no reference counts are needed: an
//! object once shared stays marked, and its other holders clone it if they
//! ever write. Programs behave exactly as with eager copies.

use inkwell::IntPredicate;
use inkwell::values::PointerValue;

use super::heap::element_of;
use super::lower::{Lowerer, POSITIONED};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// Marks `array` shared: from now on, whoever writes to it copies it first.
    pub(super) fn mark_shared(&self, array: PointerValue<'ctx>) {
        let i64_type = self.context.i64_type();
        let at = self.capacity_address(array);
        let cap = self
            .builder
            .build_load(i64_type, at, "cap")
            .expect(POSITIONED)
            .into_int_value();
        let shared_bit = i64_type.const_int(1 << 63, false);
        let marked = self
            .builder
            .build_or(cap, shared_bit, "shared")
            .expect(POSITIONED);
        self.builder.build_store(at, marked).expect(POSITIONED);
    }

    /// The array in `slot` (an array of type `ty`), made unique first: if it
    /// is shared, a one-level clone is stored back into `slot`.
    pub(super) fn own(&mut self, slot: PointerValue<'ctx>, ty: &Type) -> PointerValue<'ctx> {
        let ptr = self.context.ptr_type(Default::default());
        let array = self
            .builder
            .build_load(ptr, slot, "array")
            .expect(POSITIONED)
            .into_pointer_value();
        let i64_type = self.context.i64_type();
        let cap = self
            .builder
            .build_load(i64_type, self.capacity_address(array), "cap")
            .expect(POSITIONED)
            .into_int_value();
        // The flag is the sign bit, so "shared" is "cap < 0".
        let shared = self
            .builder
            .build_int_compare(IntPredicate::SLT, cap, i64_type.const_zero(), "is_shared")
            .expect(POSITIONED);
        let from = self.builder.get_insert_block().expect(POSITIONED);
        let (clone, done) = (self.append("cow.clone"), self.append("cow.done"));
        let branch = self
            .builder
            .build_conditional_branch(shared, clone, done)
            .expect(POSITIONED);
        // Writes to unique arrays are the rule: mark the clone path cold, so
        // the check costs a load and a predicted branch in hot loops.
        let i32_type = self.context.i32_type();
        let weights = self.context.metadata_node(&[
            self.context.metadata_string("branch_weights").into(),
            i32_type.const_int(1, false).into(),
            i32_type.const_int(2000, false).into(),
        ]);
        let kind = self.context.get_kind_id("prof");
        branch
            .set_metadata(weights, kind)
            .expect("a branch takes `prof` metadata");
        self.builder.position_at_end(clone);
        let copy = self.clone_array(array, ty);
        self.builder.build_store(slot, copy).expect(POSITIONED);
        let cloned = self.builder.get_insert_block().expect(POSITIONED);
        self.branch(done);
        self.builder.position_at_end(done);
        let phi = self.builder.build_phi(ptr, "owned").expect(POSITIONED);
        phi.add_incoming(&[(&array, from), (&copy, cloned)]);
        phi.as_basic_value().into_pointer_value()
    }

    /// A new, unshared array with `array`'s elements: plain elements by one
    /// `memcpy`, elements holding arrays each shared (so the arrays inside
    /// are marked, now that two arrays hold them).
    fn clone_array(&mut self, array: PointerValue<'ctx>, ty: &Type) -> PointerValue<'ctx> {
        let element = element_of(ty);
        if element.is_deep(&self.structs) {
            self.clone_deep(array, ty)
        } else {
            self.clone_flat(array, &element)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn a_copy_marks_instead_of_copying() {
        let ir = ir("fun main() { let xs = [1, 2]; let ys = xs; let n = xs[0] + ys[0]; }");
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        let main = &main[..main.find("\n}").unwrap()];
        assert!(main.contains("or i64 %cap"), "{main}");
        assert!(!main.contains("llvm.memcpy"), "{main}");
    }

    #[test]
    fn a_write_owns_its_array_first() {
        let ir = ir("fun main() { let mut xs = [1, 2]; let ys = xs; xs[0] = 5; let n = ys[0]; }");
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        let main = &main[..main.find("\n}").unwrap()];
        let check = main.find("cow.clone").expect("an own check");
        let store = main.rfind("store i64 5").expect("the element store");
        assert!(check < store, "{main}");
    }
}
