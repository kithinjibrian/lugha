//! Derive blocks in codegen (v17, milestone 141, PRP-172). A call through a
//! derivable interface's bound reaches the item of the block covering the
//! concrete type, an ordinary instance (`lugha_fn_<interface>@<label>.<item>`).
//! Each field intrinsic is unrolled for the instance's `Self`: one call of
//! `f` per field, at that field's type, in declaration order.

use inkwell::IntPredicate;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, PointerValue, ValueKind};

use super::CodegenError;
use super::enums::variants;
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::methods::Method;
use super::value::is_aggregate;
use crate::ast::{BinOp, Expr};
use crate::check::{RESULT, SELF, Type};

impl<'ctx> Lowerer<'ctx> {
    /// The block item a call of `name` through a derivable bound reaches at
    /// `ty`, if the call is one.
    pub(super) fn derive_method(&self, call: &Expr, name: &str, ty: &Type) -> Option<Method<'ctx>> {
        let interface = self.derived.get(&call.id)?;
        // A struct holding a C union never meets a derivable interface
        // (E0450), so none reaches codegen.
        let target = self.derives.target(interface, name, ty, |_| false)?;
        let key = instance_name(&target.key, &target.args);
        Some(
            self.monos
                .get(&key)
                .unwrap_or_else(|| panic!("checked: `{key}` is listed"))
                .clone(),
        )
    }

    /// `derive.fields(x, ctx, f)` and the other field intrinsics.
    pub(super) fn derive_intrinsic(
        &mut self,
        expr: &Expr,
        op: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let me = self.concrete(&Type::Param(SELF.to_string()));
        let out = self.ty(expr);
        if op == "variants" {
            return Ok(self.variant_table(&me, &out));
        }
        // Two values compared field by field (v17, milestone 144).
        if op == "all" {
            let a = self.address_of(&args[0], &me)?;
            let b = self.address_of(&args[1], &me)?;
            let f = self.callees[&expr.id].clone();
            return Ok(self.all(&f, (a, b), &me).into());
        }
        // A variant's position, or a handle's address (v17, milestone 143).
        if op == "index" || op == "address" {
            let x = self.get(&args[0], &me)?;
            return Ok(self.index_or_address(x, &me, op).into());
        }
        let f = self.callees[&expr.id].clone();
        // The first argument (a value or a name) runs before `ctx`.
        let first = match op {
            "fields" | "variant" | "fold" => Some(self.get(&args[0], &me)?),
            "build_variant" => Some(self.get(&args[0], &Type::String)?),
            _ => None,
        };
        let ctx_expr = &args[args.len() - 2];
        let ctx_ty = self.ty(ctx_expr);
        let ctx = (self.get(ctx_expr, &ctx_ty)?, ctx_ty);
        let first = || first.expect("this intrinsic has a first argument");
        Ok(match op {
            "fields" => {
                let parts = self.field_values(first(), &me);
                let mut results = Vec::with_capacity(parts.len());
                for (name, ty, value) in parts {
                    let name = self.string_literal(&name).into();
                    let args = [ctx.clone(), (name, Type::String), (value, ty.clone())];
                    results.push(self.field_call(&f, &ty, &args));
                }
                let Type::Array(element) = &out else {
                    unreachable!("checked: `fields` gives an array")
                };
                self.array_of(&results, element).into()
            }
            "variant" => self.variant_parts(&f, first(), &me, &out, &ctx),
            "fold" => self.fold(&f, first(), &me, ctx),
            "build" => {
                let fields = self.structs[&me.to_string()].clone();
                let slot = self.entry_alloca(&out, "built");
                let done = self.append("derive.done");
                let values = self.made(&f, &fields, &ctx, (&out, slot, done), |e, l| {
                    l.construct_values(&out, "Err", vec![e])
                });
                let built = self.aggregate(&me, &fields, values);
                let ok = self.construct_values(&out, "Ok", vec![built]);
                self.finish(slot, ok, done, &out)
            }
            _ => self.build_variant(&f, first(), &me, &out, &ctx),
        })
    }

    /// A call of `f`'s instance at `ty`; aggregates go as a pointer to a
    /// copy, as every Lugha call passes them.
    pub(super) fn field_call(
        &mut self,
        f: &str,
        ty: &Type,
        args: &[(BasicValueEnum<'ctx>, Type)],
    ) -> BasicValueEnum<'ctx> {
        let key = instance_name(f, std::slice::from_ref(ty));
        let function = self
            .monos
            .get(&key)
            .unwrap_or_else(|| panic!("checked: `{key}` is listed for every field"))
            .signature
            .function;
        let mut values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
        for (value, ty) in args {
            if is_aggregate(ty) {
                let slot = self.entry_alloca(ty, "arg");
                self.builder.build_store(slot, *value).expect(POSITIONED);
                values.push(slot.into());
            } else {
                values.push((*value).into());
            }
        }
        let call = self
            .builder
            .build_call(function, &values, "field")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => v,
            ValueKind::Instruction(_) => unreachable!("checked: `f` returns a value"),
        }
    }

    /// Each field of struct or tuple `value` (of type `ty`): name, type, value.
    pub(super) fn field_values(
        &self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> Vec<(String, Type, BasicValueEnum<'ctx>)> {
        let value = value.into_struct_value();
        let fields = self.structs[&ty.to_string()].clone();
        let mut out = Vec::with_capacity(fields.len());
        for (index, (name, fty)) in fields.into_iter().enumerate() {
            let index = u32::try_from(index).expect("field count fits in u32");
            let raw = self
                .builder
                .build_extract_value(value, index, &name)
                .expect("field exists");
            let part = self.loaded_form(raw, &fty);
            out.push((name, fty, part));
        }
        out
    }

    /// A new array of `values`, each of type `element`.
    fn array_of(&self, values: &[BasicValueEnum<'ctx>], element: &Type) -> PointerValue<'ctx> {
        let i64_type = self.context.i64_type();
        let size = self.element_size(element);
        let array = self.new_array(i64_type.const_int(values.len() as u64, false), element);
        for (i, v) in values.iter().enumerate() {
            let at = i64_type.const_int(i as u64, false);
            self.store_element(self.element_address(array, at, size), element, *v);
        }
        array
    }

    /// Each of `fields` made by `f(ctx, name, at, of)`, in order. On the
    /// first `Err`, `fail` turns its error into the intrinsic's result,
    /// stored in `slot`, and control goes to `done`.
    fn made(
        &mut self,
        f: &str,
        fields: &[(String, Type)],
        ctx: &(BasicValueEnum<'ctx>, Type),
        (out, slot, done): (
            &Type,
            PointerValue<'ctx>,
            inkwell::basic_block::BasicBlock<'ctx>,
        ),
        fail: impl Fn(BasicValueEnum<'ctx>, &mut Self) -> BasicValueEnum<'ctx>,
    ) -> Vec<BasicValueEnum<'ctx>> {
        let error = match out {
            Type::Enum(_, a) => a[1].clone(),
            Type::Option(inner) => match &**inner {
                Type::Enum(_, a) => a[1].clone(),
                _ => unreachable!("checked: `Option<Result<Self, E>>`"),
            },
            _ => unreachable!("checked: a `Result`"),
        };
        let i64_type = self.context.i64_type();
        let of = i64_type.const_int(fields.len() as u64, false);
        let mut values = Vec::with_capacity(fields.len());
        for (i, (name, fty)) in fields.iter().enumerate() {
            let name = self.string_literal(name).into();
            let at = i64_type.const_int(i as u64, false);
            let args = [
                ctx.clone(),
                (name, Type::String),
                (at.into(), Type::I64),
                (of.into(), Type::I64),
            ];
            let result = self.field_call(f, fty, &args);
            let ty = Type::Enum(RESULT.to_string(), vec![fty.clone(), error.clone()]);
            let held = self.entry_alloca(&ty, "made");
            self.builder.build_store(held, result).expect(POSITIONED);
            let tag = self.tag(&ty, held);
            let (err, _) = self.variant(&ty, "Err");
            let failed = self
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    tag,
                    self.context.i32_type().const_int(u64::from(err), false),
                    "failed",
                )
                .expect(POSITIONED);
            let (bad, good) = (self.append("derive.err"), self.append("derive.ok"));
            self.builder
                .build_conditional_branch(failed, bad, good)
                .expect(POSITIONED);
            self.builder.position_at_end(bad);
            let e = self.load_payload(&ty, held, std::slice::from_ref(&error), 0, &error);
            let failure = fail(e, self);
            self.builder.build_store(slot, failure).expect(POSITIONED);
            self.branch(done);
            self.builder.position_at_end(good);
            values.push(self.load_payload(&ty, held, std::slice::from_ref(fty), 0, fty));
        }
        values
    }

    /// A struct or tuple of type `ty` holding `values`, one per field.
    fn aggregate(
        &self,
        ty: &Type,
        fields: &[(String, Type)],
        values: Vec<BasicValueEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let struct_type = self
            .context
            .get_struct_type(&ty.to_string())
            .expect("declared before lowering");
        let mut aggregate = struct_type.get_undef();
        for (index, (value, (_, fty))) in values.into_iter().zip(fields).enumerate() {
            let index = u32::try_from(index).expect("field count fits in u32");
            aggregate = self
                .builder
                .build_insert_value(aggregate, self.stored_form(value, fty), index, "field")
                .expect("field exists")
                .into_struct_value();
        }
        aggregate.into()
    }

    /// Stores `value` in `slot`, joins `done` and reads the result back.
    fn finish(
        &self,
        slot: PointerValue<'ctx>,
        value: BasicValueEnum<'ctx>,
        done: inkwell::basic_block::BasicBlock<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        self.builder.build_store(slot, value).expect(POSITIONED);
        self.branch(done);
        self.builder.position_at_end(done);
        let llvm = super::value::llvm_type(self.context, ty);
        self.builder
            .build_load(llvm, slot, "derived")
            .expect(POSITIONED)
    }

    /// `(name, results)` for enum value `value`: its variant's name and its
    /// payloads through `f(ctx, name, payload)`.
    fn variant_parts(
        &mut self,
        f: &str,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
        out: &Type,
        ctx: &(BasicValueEnum<'ctx>, Type),
    ) -> BasicValueEnum<'ctx> {
        let held = self.entry_alloca(ty, "value");
        self.builder.build_store(held, value).expect(POSITIONED);
        let tag = self.tag(ty, held);
        let slot = self.entry_alloca(out, "variant");
        let done = self.append("derive.done");
        let element = match out {
            Type::Struct(_, parts) => match &parts[1] {
                Type::Array(e) => (**e).clone(),
                _ => unreachable!("checked: `(string, R[])`"),
            },
            _ => unreachable!("checked: `(string, R[])`"),
        };
        let pair = [
            (String::from("0"), Type::String),
            ("1".into(), parts_ty(out)),
        ];
        let i32_type = self.context.i32_type();
        let mut cases = Vec::new();
        for (index, (name, payloads)) in variants(ty, &self.enums).into_iter().enumerate() {
            let block = self.append("variant");
            cases.push((i32_type.const_int(index as u64, false), block));
            let resume = self.current_block();
            self.builder.position_at_end(block);
            let literal: BasicValueEnum = self.string_literal(&name).into();
            let mut results = Vec::with_capacity(payloads.len());
            for (i, p) in payloads.iter().enumerate() {
                let i = u32::try_from(i).expect("payload count fits in u32");
                let x = self.load_payload(ty, held, &payloads, i, p);
                let args = [ctx.clone(), (literal, Type::String), (x, p.clone())];
                results.push(self.field_call(f, p, &args));
            }
            let array = self.array_of(&results, &element).into();
            let built = self.aggregate(out, &pair, vec![literal, array]);
            self.builder.build_store(slot, built).expect(POSITIONED);
            self.branch(done);
            self.builder.position_at_end(resume);
        }
        // Every tag has a case; the default is never taken.
        self.builder
            .build_switch(tag, done, &cases)
            .expect(POSITIONED);
        self.builder.position_at_end(done);
        let llvm = super::value::llvm_type(self.context, out);
        self.builder
            .build_load(llvm, slot, "variant")
            .expect(POSITIONED)
    }

    /// The variant of enum `ty` named `name`, its payloads made by
    /// `f(ctx, name, at, of)`: `Some(Ok(v))`, `Some(Err(e))` at the first
    /// failure, or `None` for an unknown name.
    fn build_variant(
        &mut self,
        f: &str,
        name: BasicValueEnum<'ctx>,
        ty: &Type,
        out: &Type,
        ctx: &(BasicValueEnum<'ctx>, Type),
    ) -> BasicValueEnum<'ctx> {
        let Type::Option(result) = out else {
            unreachable!("checked: `Option<Result<Self, E>>`")
        };
        let result = (**result).clone();
        let slot = self.entry_alloca(out, "built");
        let done = self.append("derive.done");
        for (vname, payloads) in variants(ty, &self.enums) {
            let literal: BasicValueEnum = self.string_literal(&vname).into();
            let same = self
                .string_binary(BinOp::Eq, name, literal)
                .into_int_value();
            let (this, next) = (self.append("variant"), self.append("variant.next"));
            self.builder
                .build_conditional_branch(same, this, next)
                .expect(POSITIONED);
            self.builder.position_at_end(this);
            let fields: Vec<(String, Type)> = payloads
                .iter()
                .map(|p| (vname.clone(), p.clone()))
                .collect();
            let values = self.made(f, &fields, ctx, (out, slot, done), |e, l| {
                let err = l.construct_values(&result, "Err", vec![e]);
                l.construct_values(out, "Some", vec![err])
            });
            let built = self.construct_values(ty, &vname, values);
            let ok = self.construct_values(&result, "Ok", vec![built]);
            let some = self.construct_values(out, "Some", vec![ok]);
            self.builder.build_store(slot, some).expect(POSITIONED);
            self.branch(done);
            self.builder.position_at_end(next);
        }
        let none = self.construct_values(out, "None", Vec::new());
        self.finish(slot, none, done, out)
    }

    /// `derive.variants()`: each variant's name and payload count, in order.
    fn variant_table(&mut self, ty: &Type, out: &Type) -> BasicValueEnum<'ctx> {
        let Type::Array(pair) = out else {
            unreachable!("checked: `(string, i64)[]`")
        };
        let fields = [(String::from("0"), Type::String), ("1".into(), Type::I64)];
        let mut rows = Vec::new();
        for (name, payloads) in variants(ty, &self.enums) {
            let name = self.string_literal(&name).into();
            let count = self
                .context
                .i64_type()
                .const_int(payloads.len() as u64, false);
            rows.push(self.aggregate(pair, &fields, vec![name, count.into()]));
        }
        self.array_of(&rows, pair).into()
    }
}

/// The `R[]` of a `(string, R[])` tuple type.
fn parts_ty(pair: &Type) -> Type {
    match pair {
        Type::Struct(_, parts) => parts[1].clone(),
        _ => unreachable!("checked: a tuple"),
    }
}
