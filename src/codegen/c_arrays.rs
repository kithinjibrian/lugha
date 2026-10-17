//! Arrays in callbacks (v17, milestone 105, PRP-137): a callback parameter
//! `T[]` is C's `const T *, size_t` and arrives as a copy; `Ref<T[]>` is
//! C's `T *, size_t`, arrives as a box holding a copy, and the box's
//! elements are copied back to C when the callback returns (no more than C
//! gave: a callback can't grow C's memory).

use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use super::lower::{Lowerer, POSITIONED};
use crate::check::Type;

/// The element type of a counted callback parameter, and whether its
/// elements go back to C.
pub(super) fn counted(ty: &Type) -> Option<(Type, bool)> {
    match ty {
        Type::Array(element) => Some(((**element).clone(), false)),
        Type::Ref(inner) => match &**inner {
            Type::Array(element) => Some(((**element).clone(), true)),
            _ => None,
        },
        _ => None,
    }
}

/// C's parameters for a callback taking `params` with `data` inserted
/// before parameter `data`: each counted array is a pointer and a count.
pub(super) fn c_params(params: &[Type], data: Option<usize>) -> (Vec<Type>, Option<usize>) {
    let (mut out, mut at) = (Vec::new(), None);
    for (i, ty) in params.iter().enumerate() {
        if data == Some(i) {
            at = Some(out.len());
        }
        match counted(ty) {
            Some(_) => {
                out.push(Type::Handle("CPtr".to_string()));
                out.push(Type::U64);
            }
            None => out.push(ty.clone()),
        }
    }
    if data == Some(params.len()) {
        at = Some(out.len());
    }
    (out, at)
}

/// A `Ref<T[]>` parameter to copy back to C after the callback.
pub(super) struct CopyBack<'ctx> {
    boxed: PointerValue<'ctx>,
    to: PointerValue<'ctx>,
    count: IntValue<'ctx>,
    element: Type,
}

impl<'ctx> Lowerer<'ctx> {
    /// The Lugha value for counted parameter `ty` from C's pointer and
    /// count: an array, or for `Ref<T[]>` a box holding one, noted in `copies`.
    pub(super) fn counted_from_c(
        &mut self,
        ty: &Type,
        (pointer, count): (BasicValueEnum<'ctx>, BasicValueEnum<'ctx>),
        copies: &mut Vec<CopyBack<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let (element, back) = counted(ty).expect("checked: a counted parameter");
        let (pointer, count) = (pointer.into_pointer_value(), count.into_int_value());
        let array = self.new_array(count, &element);
        let size = self.element_size(&element);
        let bytes = self.byte_count(count, size);
        let first = self.element_address(array, self.context.i64_type().const_zero(), size);
        self.builder
            .build_memcpy(first, 1, pointer, 1, bytes)
            .expect(POSITIONED);
        if !back {
            return array.into();
        }
        let alloc = self.runtime("lugha_rt_alloc", &[Type::I64], Some(Type::String));
        let eight = self.context.i64_type().const_int(8, false);
        let site = self
            .builder
            .build_call(alloc, &[eight.into()], "box")
            .expect(POSITIONED);
        let inkwell::values::ValueKind::Basic(raw) = site.try_as_basic_value() else {
            unreachable!("lugha_rt_alloc returns a pointer")
        };
        let boxed = raw.into_pointer_value();
        let inner = Type::Array(Box::new(element.clone()));
        self.store_element(boxed, &inner, array.into());
        copies.push(CopyBack {
            boxed,
            to: pointer,
            count,
            element,
        });
        boxed.into()
    }

    /// After a callback: each `Ref<T[]>`'s elements copied back to C, as
    /// many as C gave or the array now has, whichever is fewer.
    pub(super) fn copy_back(&mut self, copies: Vec<CopyBack<'ctx>>) {
        for c in copies {
            let inner = Type::Array(Box::new(c.element.clone()));
            let array = self.load_element(c.boxed, &inner).into_pointer_value();
            let len = self.length(array);
            let fewer = self
                .builder
                .build_int_compare(inkwell::IntPredicate::ULT, len, c.count, "fewer")
                .expect(POSITIONED);
            let n = self
                .builder
                .build_select(fewer, len, c.count, "n")
                .expect(POSITIONED)
                .into_int_value();
            let size = self.element_size(&c.element);
            let bytes = self.byte_count(n, size);
            let first = self.element_address(array, self.context.i64_type().const_zero(), size);
            self.builder
                .build_memcpy(c.to, 1, first, 1, bytes)
                .expect(POSITIONED);
        }
    }

    /// `count * size` bytes.
    fn byte_count(&self, count: IntValue<'ctx>, size: u64) -> IntValue<'ctx> {
        let size = self.context.i64_type().const_int(size, false);
        self.builder
            .build_int_mul(count, size, "bytes")
            .expect(POSITIONED)
    }
}
