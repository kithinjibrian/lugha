//! Shared boxes in codegen (v16 draft decisions 1–4, milestone 67): a
//! `Ref<T>` is a pointer to one GC object holding a `T` in element layout.
//! Copying a `Ref` copies the pointer. `get` hands out a copy of the
//! contents and `set` stores one, so a box is a single holder of whatever
//! it contains, which keeps copy-on-write sound.

use inkwell::IntPredicate;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, is_aggregate};
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `Ref.new(value)`: a new box, `call` being the whole call.
    pub(super) fn ref_new(
        &mut self,
        call: &Expr,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let Type::Ref(inner) = self.ty(call) else {
            unreachable!("checked: `Ref.new` makes a `Ref`")
        };
        let value = self.value_for_store(&args[0], &inner)?;
        let size = self
            .context
            .i64_type()
            .const_int(self.element_size(&inner).max(1), false);
        let alloc = self.runtime("lugha_rt_alloc", &[Type::I64], Some(Type::String));
        let site = self
            .builder
            .build_call(alloc, &[size.into()], "box")
            .expect(POSITIONED);
        let ValueKind::Basic(raw) = site.try_as_basic_value() else {
            unreachable!("lugha_rt_alloc returns a pointer")
        };
        let box_ptr = raw.into_pointer_value();
        self.store_element(box_ptr, &inner, value);
        Ok(box_ptr.into())
    }

    /// `r.get()`, `r.set(v)`, `r.update(f)` or `r.is(s)` on `base`, a `Ref`.
    pub(super) fn ref_method(
        &mut self,
        base: &Expr,
        name: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let ty = self.ty(base);
        let Type::Ref(inner) = &ty else {
            unreachable!("checked: a `Ref` method has a `Ref` receiver")
        };
        let box_ptr = self.get(base, &ty)?.into_pointer_value();
        Ok(match name {
            "get" => Value::Val(self.ref_contents(box_ptr, inner)),
            "set" => {
                let value = self.value_for_store(&args[0], inner)?;
                self.store_element(box_ptr, inner, value);
                Value::Void
            }
            "update" => {
                let fun_ty = self.ty(&args[0]);
                let Type::Fun(params, ret) = &fun_ty else {
                    unreachable!("checked: `update` takes a function value")
                };
                let pair = self.get(&args[0], &fun_ty)?.into_struct_value();
                let current = self.ref_contents(box_ptr, inner);
                let arg = self.as_argument(current, inner);
                let Value::Val(next) = self.call_pair(pair, (params, ret), vec![arg]) else {
                    unreachable!("checked: `update`'s function returns the contents' type")
                };
                // A fresh result: stored as is, as any call's result is.
                self.store_element(box_ptr, inner, next);
                Value::Void
            }
            "is" => {
                let other = self.get(&args[0], &ty)?.into_pointer_value();
                let same = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, box_ptr, other, "same")
                    .expect(POSITIONED);
                Value::Val(same.into())
            }
            _ => unreachable!("checked: `Ref` has get, set, update and is"),
        })
    }

    /// `Ref(<contents>)`, or `Ref(…)` for a box that is already being
    /// printed further out (a cycle), so printing always ends (v16). The
    /// runtime keeps the stack of boxes being printed.
    pub(super) fn show_box(
        &mut self,
        box_ptr: PointerValue<'ctx>,
        inner: &Type,
    ) -> BasicValueEnum<'ctx> {
        let ptr = self.context.ptr_type(Default::default());
        let enter = self.runtime("lugha_rt_show_enter", &[Type::String], Some(Type::I32));
        let site = self
            .builder
            .build_call(enter, &[box_ptr.into()], "entered")
            .expect(POSITIONED);
        let ValueKind::Basic(entered) = site.try_as_basic_value() else {
            unreachable!("lugha_rt_show_enter returns a flag")
        };
        let zero = self.context.i32_type().const_zero();
        let fresh = self
            .builder
            .build_int_compare(IntPredicate::NE, entered.into_int_value(), zero, "fresh")
            .expect(POSITIONED);
        let (show, cut, done) = (
            self.append("box.show"),
            self.append("box.cut"),
            self.append("box.done"),
        );
        self.builder
            .build_conditional_branch(fresh, show, cut)
            .expect(POSITIONED);
        self.builder.position_at_end(show);
        let contents = self.load_element(box_ptr, inner);
        let open: BasicValueEnum = self.string_literal("Ref(").into();
        let part = self.part(contents, inner);
        let text = self.concat(open, part);
        let text = self.join(text, ")");
        let leave = self.runtime("lugha_rt_show_leave", &[], None);
        self.builder.build_call(leave, &[], "").expect(POSITIONED);
        let shown = self.builder.get_insert_block().expect(POSITIONED);
        self.branch(done);
        self.builder.position_at_end(cut);
        let short: BasicValueEnum = self.string_literal("Ref(…)").into();
        self.branch(done);
        self.builder.position_at_end(done);
        let phi = self.builder.build_phi(ptr, "text").expect(POSITIONED);
        phi.add_incoming(&[(&text, shown), (&short, cut)]);
        phi.as_basic_value()
    }

    /// A copy of the box's contents: a copy site, so arrays inside are
    /// marked shared before they leave the box.
    fn ref_contents(&mut self, box_ptr: PointerValue<'ctx>, inner: &Type) -> BasicValueEnum<'ctx> {
        let value = self.load_element(box_ptr, inner);
        self.deep_copy(value, inner)
    }

    /// `value` passed as an argument of type `ty`: a pointer to a copy on
    /// the stack for a struct, enum or other aggregate, the value otherwise.
    fn as_argument(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicMetadataValueEnum<'ctx> {
        if is_aggregate(ty) {
            let slot = self.entry_alloca(ty, "arg");
            self.builder.build_store(slot, value).expect(POSITIONED);
            slot.into()
        } else {
            value.into()
        }
    }
}
