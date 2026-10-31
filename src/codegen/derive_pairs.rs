//! The derive intrinsics that walk two values or thread one through the
//! fields (v17, milestones 143–144): `derive.all`, `derive.fold`,
//! `derive.index`, `derive.address`, and `==` calling `Eq`'s `eq`. Split
//! from `derive.rs` for the 500-line limit.

use inkwell::IntPredicate;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue, ValueKind};

use super::CodegenError;
use super::enums::variants;
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::value::is_aggregate;
use crate::ast::Expr;
use crate::check::{EQ, Type};

impl<'ctx> Lowerer<'ctx> {
    /// `a == b` through `Eq` (v17, milestone 144): the type's own `eq`, or
    /// its derive block's, called with `a` as the receiver.
    pub(super) fn eq_call(
        &mut self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> inkwell::values::IntValue<'ctx> {
        let own = match ty {
            Type::Struct(owner, targs) | Type::Enum(owner, targs)
                if self.derives.has_own(owner, "eq") =>
            {
                let key = format!("{owner}.eq");
                self.monos
                    .get(&instance_name(&key, targs))
                    .or_else(|| self.methods.get(&key))
                    .or_else(|| self.monos.get(&key))
                    .cloned()
            }
            _ => None,
        };
        let method = own.unwrap_or_else(|| {
            // A union-holding struct is never `Eq` (E0451), so none reaches here.
            let target = self
                .derives
                .target(EQ, "eq", ty, |_| false)
                .unwrap_or_else(|| panic!("checked: `{ty}` is `Eq`"));
            let key = instance_name(&target.key, &target.args);
            self.monos
                .get(&key)
                .unwrap_or_else(|| panic!("checked: `{key}` is listed"))
                .clone()
        });
        let receiver = self.entry_alloca(ty, "self");
        self.builder.build_store(receiver, a).expect(POSITIONED);
        let other: BasicMetadataValueEnum = if is_aggregate(ty) {
            let slot = self.entry_alloca(ty, "other");
            self.builder.build_store(slot, b).expect(POSITIONED);
            slot.into()
        } else {
            b.into()
        };
        let call = self
            .builder
            .build_call(method.signature.function, &[receiver.into(), other], "eq")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => v.into_int_value(),
            ValueKind::Instruction(_) => unreachable!("checked: `eq` returns a bool"),
        }
    }

    /// `derive.index(x)` (the tag, widened) or `derive.address(x)`.
    pub(super) fn index_or_address(
        &mut self,
        x: BasicValueEnum<'ctx>,
        ty: &Type,
        op: &str,
    ) -> inkwell::values::IntValue<'ctx> {
        let i64_type = self.context.i64_type();
        if op == "address" {
            return self
                .builder
                .build_ptr_to_int(x.into_pointer_value(), i64_type, "address")
                .expect(POSITIONED);
        }
        let held = self.entry_alloca(ty, "value");
        self.builder.build_store(held, x).expect(POSITIONED);
        let tag = self.tag(ty, held);
        self.builder
            .build_int_z_extend(tag, i64_type, "index")
            .expect(POSITIONED)
    }

    /// `derive.all(a, b, f)`: true if `f(name, x, y)` holds for every pair
    /// of fields (an enum's: the same variant, and every payload pair),
    /// stopping at the first false (v17, milestone 144).
    /// Where `e`, of type `ty`, is: a local's own storage (`self` and
    /// `other` in an `eq`, read in place rather than copied, v17, milestone
    /// 144), or else a fresh copy.
    pub(super) fn address_of(
        &mut self,
        e: &Expr,
        ty: &Type,
    ) -> Result<PointerValue<'ctx>, CodegenError> {
        if let crate::ast::ExprKind::Name(name) = &e.kind {
            let local = self.scopes.lookup(name);
            return Ok(self.local_address(local.ptr, false));
        }
        let value = self.get(e, ty)?;
        let slot = self.entry_alloca(ty, "value");
        self.builder.build_store(slot, value).expect(POSITIONED);
        Ok(slot)
    }

    /// Each field of the struct or tuple at `at` (of type `ty`), loaded one
    /// at a time: name, type, value.
    fn fields_at(
        &self,
        at: PointerValue<'ctx>,
        ty: &Type,
    ) -> Vec<(String, Type, BasicValueEnum<'ctx>)> {
        let struct_type = self
            .context
            .get_struct_type(&ty.to_string())
            .expect("declared before lowering");
        let fields = self.structs[&ty.to_string()].clone();
        let mut out = Vec::with_capacity(fields.len());
        for (index, (name, fty)) in fields.into_iter().enumerate() {
            let index = u32::try_from(index).expect("field count fits in u32");
            let address = self
                .builder
                .build_struct_gep(struct_type, at, index, &name)
                .expect("field exists");
            let stored = struct_type
                .get_field_type_at_index(index)
                .expect("field exists");
            let raw = self
                .builder
                .build_load(stored, address, &name)
                .expect(POSITIONED);
            out.push((name, fty.clone(), self.loaded_form(raw, &fty)));
        }
        out
    }

    pub(super) fn all(
        &mut self,
        f: &str,
        (ha, hb): (PointerValue<'ctx>, PointerValue<'ctx>),
        ty: &Type,
    ) -> inkwell::values::IntValue<'ctx> {
        let bool_type = self.context.bool_type();
        let result = self.entry_alloca(&Type::Bool, "all");
        self.builder
            .build_store(result, bool_type.const_zero())
            .expect(POSITIONED);
        let (done, fail) = (self.append("all.done"), self.append("all.no"));
        // `f(name, x, y)`, on to `fail` when false.
        let check = |this: &mut Self,
                     name: &str,
                     pair: (BasicValueEnum<'ctx>, BasicValueEnum<'ctx>),
                     part: &Type| {
            let name = this.string_literal(name).into();
            let args = [
                (name, Type::String),
                (pair.0, part.clone()),
                (pair.1, part.clone()),
            ];
            let same = this.field_call(f, part, &args).into_int_value();
            let next = this.append("all.next");
            this.builder
                .build_conditional_branch(same, next, fail)
                .expect(POSITIONED);
            this.builder.position_at_end(next);
        };
        if let Type::Enum(..) | Type::Option(_) = ty {
            let tag = self.tag(ty, ha);
            let same_tag = self
                .builder
                .build_int_compare(IntPredicate::EQ, tag, self.tag(ty, hb), "same.variant")
                .expect(POSITIONED);
            let payloads = self.append("all.payloads");
            self.builder
                .build_conditional_branch(same_tag, payloads, fail)
                .expect(POSITIONED);
            self.builder.position_at_end(payloads);
            let pass = self.append("all.yes");
            let mut cases = Vec::new();
            for (index, (name, parts)) in variants(ty, &self.enums).into_iter().enumerate() {
                let block = self.append("all.variant");
                cases.push((
                    self.context.i32_type().const_int(index as u64, false),
                    block,
                ));
                let resume = self.current_block();
                self.builder.position_at_end(block);
                for (i, p) in parts.iter().enumerate() {
                    let i = u32::try_from(i).expect("payload count fits in u32");
                    let x = self.load_payload(ty, ha, &parts, i, p);
                    let y = self.load_payload(ty, hb, &parts, i, p);
                    check(self, &name, (x, y), p);
                }
                self.branch(pass);
                self.builder.position_at_end(resume);
            }
            // Every tag has a case; the default is never taken.
            self.builder
                .build_switch(tag, pass, &cases)
                .expect(POSITIONED);
            self.builder.position_at_end(pass);
        } else {
            let xs = self.fields_at(ha, ty);
            let ys = self.fields_at(hb, ty);
            for ((name, part, x), (_, _, y)) in xs.into_iter().zip(ys) {
                check(self, &name, (x, y), &part);
            }
        }
        self.builder
            .build_store(result, bool_type.const_int(1, false))
            .expect(POSITIONED);
        self.branch(done);
        self.builder.position_at_end(fail);
        self.branch(done);
        self.builder.position_at_end(done);
        self.builder
            .build_load(bool_type, result, "all")
            .expect(POSITIONED)
            .into_int_value()
    }

    /// `derive.fold(x, init, f)`: `f(acc, name, part)` for each field, or
    /// each payload of the variant at hand, threading `acc` (v17, milestone 143).
    pub(super) fn fold(
        &mut self,
        f: &str,
        x: BasicValueEnum<'ctx>,
        ty: &Type,
        (init, acc_ty): (BasicValueEnum<'ctx>, Type),
    ) -> BasicValueEnum<'ctx> {
        let acc = self.entry_alloca(&acc_ty, "acc");
        self.builder.build_store(acc, init).expect(POSITIONED);
        let llvm = super::value::llvm_type(self.context, &acc_ty);
        let step = |this: &mut Self, name: &str, part: BasicValueEnum<'ctx>, part_ty: &Type| {
            let now = this.builder.build_load(llvm, acc, "acc").expect(POSITIONED);
            let name = this.string_literal(name).into();
            let args = [
                (now, acc_ty.clone()),
                (name, Type::String),
                (part, part_ty.clone()),
            ];
            let next = this.field_call(f, part_ty, &args);
            this.builder.build_store(acc, next).expect(POSITIONED);
        };
        if let Type::Enum(..) | Type::Option(_) = ty {
            let held = self.entry_alloca(ty, "value");
            self.builder.build_store(held, x).expect(POSITIONED);
            let tag = self.tag(ty, held);
            let done = self.append("fold.done");
            let mut cases = Vec::new();
            for (index, (name, payloads)) in variants(ty, &self.enums).into_iter().enumerate() {
                let block = self.append("fold.variant");
                cases.push((
                    self.context.i32_type().const_int(index as u64, false),
                    block,
                ));
                let resume = self.current_block();
                self.builder.position_at_end(block);
                for (i, p) in payloads.iter().enumerate() {
                    let i = u32::try_from(i).expect("payload count fits in u32");
                    let part = self.load_payload(ty, held, &payloads, i, p);
                    step(self, &name, part, p);
                }
                self.branch(done);
                self.builder.position_at_end(resume);
            }
            // Every tag has a case; the default is never taken.
            self.builder
                .build_switch(tag, done, &cases)
                .expect(POSITIONED);
            self.builder.position_at_end(done);
        } else {
            for (name, part_ty, part) in self.field_values(x, ty) {
                step(self, &name, part, &part_ty);
            }
        }
        self.builder
            .build_load(llvm, acc, "folded")
            .expect(POSITIONED)
    }
}
