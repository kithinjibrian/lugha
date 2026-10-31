//! `$json.decode(v, path)` (v17, milestone 140, PRP-171): a `json.Value` as
//! a `Result<T, DecodeError>`. Scalars and containers call `std.$json_from`'s
//! Lugha helpers, which the checker listed (`check/json.rs`); structs, enums
//! and tuples get one `lugha_json_dec_<type>` walker each, which returns the
//! first helper's or part's `Err` as its own.

use inkwell::IntPredicate;
use inkwell::module::Linkage;
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, ValueKind};

use super::CodegenError;
use super::copy::mangle;
use super::enums::variants;
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::value::{is_aggregate, llvm_type};
use crate::ast::{BinOp, Expr};
use crate::check::{
    DECODE_ERROR, JSON_FROM, Type, container_decoder, decode_result, json_value_type,
    scalar_decoder, tuple_elements,
};

/// `DecodeError`'s type.
fn decode_error() -> Type {
    Type::Struct(DECODE_ERROR.to_string(), Vec::new())
}

impl<'ctx> Lowerer<'ctx> {
    /// `$json.decode(v, path)`, its target the checker's `Result<T, _>`'s `T`.
    pub(super) fn json_decode_call(
        &mut self,
        expr: &Expr,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let Type::Enum(_, targs) = self.ty(expr) else {
            unreachable!("checked: `$json.decode` is a `Result`")
        };
        let v = self.get(&args[0], &json_value_type())?;
        let path = self.get(&args[1], &Type::String)?;
        Ok(self.json_decode(v, path, &targs[0]))
    }

    /// `v` decoded as `target`, with `path` for errors: a `Result`.
    fn json_decode(
        &mut self,
        v: BasicValueEnum<'ctx>,
        path: BasicValueEnum<'ctx>,
        target: &Type,
    ) -> BasicValueEnum<'ctx> {
        let args = [(v, json_value_type()), (path, Type::String)];
        if let Some(name) = scalar_decoder(target) {
            return self.decoding_call(name, &[], &args);
        }
        if let Some((name, targs)) = container_decoder(target) {
            return self.decoding_call(name, &targs, &args);
        }
        let walker = self.json_decoder(target);
        let call = self
            .builder
            .build_call(walker, &[v.into(), path.into()], "decoded")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(r) => r,
            ValueKind::Instruction(_) => unreachable!("walkers return a Result"),
        }
    }

    /// A call of `std.$json_from`'s `name` at `targs`; structs, enums and
    /// `Option`s go as a pointer to a copy, as every Lugha call passes them.
    fn decoding_call(
        &mut self,
        name: &str,
        targs: &[Type],
        args: &[(BasicValueEnum<'ctx>, Type)],
    ) -> BasicValueEnum<'ctx> {
        let key = instance_name(&format!("{JSON_FROM}{name}"), targs);
        let function = self
            .monos
            .get(&key)
            .unwrap_or_else(|| panic!("checked: `{key}` is listed for every `$json.decode`"))
            .signature
            .function;
        let mut values: Vec<BasicMetadataValueEnum> = Vec::new();
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
            .build_call(function, &values, "json")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => v,
            ValueKind::Instruction(_) => unreachable!("helpers return a value"),
        }
    }

    /// The `Ok` value of `result` (a `Result<ok, DecodeError>`); on `Err`,
    /// returns that error from the walker, whose `Result` is of `out`.
    fn json_try(
        &mut self,
        result: BasicValueEnum<'ctx>,
        ok: &Type,
        out: &Type,
    ) -> BasicValueEnum<'ctx> {
        let ty = decode_result(ok.clone());
        let slot = self.entry_alloca(&ty, "decoded");
        self.builder.build_store(slot, result).expect(POSITIONED);
        let tag = self.tag(&ty, slot);
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
        let (fail, pass) = (self.append("json.err"), self.append("json.ok"));
        self.builder
            .build_conditional_branch(failed, fail, pass)
            .expect(POSITIONED);
        self.builder.position_at_end(fail);
        let error = self.load_payload(&ty, slot, &[decode_error()], 0, &decode_error());
        self.json_return(out, "Err", error);
        self.builder.position_at_end(pass);
        self.load_payload(&ty, slot, std::slice::from_ref(ok), 0, ok)
    }

    /// `return Ok(value)` or `return Err(value)` from a walker for `out`.
    fn json_return(&mut self, out: &Type, variant: &str, value: BasicValueEnum<'ctx>) {
        let result = self.construct_values(&decode_result(out.clone()), variant, vec![value]);
        self.builder.build_return(Some(&result)).expect(POSITIONED);
    }

    /// `lugha_json_dec_<mangled>(Value, string) -> Result<T, DecodeError>`,
    /// generated once per type and declared before its body, so a recursive
    /// enum's walker calls itself.
    fn json_decoder(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_json_dec_{}", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let result = llvm_type(self.context, &decode_result(ty.clone()));
        let params = [
            llvm_type(self.context, &json_value_type()).into(),
            llvm_type(self.context, &Type::String).into(),
        ];
        let function = self.module.add_function(
            &name,
            result.fn_type(&params, false),
            Some(Linkage::Internal),
        );
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let v = function.get_nth_param(0).expect("two parameters");
        let path = function.get_nth_param(1).expect("two parameters");
        match ty {
            Type::Enum(..) => self.json_variant(v, path, ty),
            _ => self.json_struct(v, path, ty),
        }
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }

    /// A struct from an object (fields by name) or a tuple from an array
    /// of exactly its length (elements by position).
    fn json_struct(&mut self, v: BasicValueEnum<'ctx>, path: BasicValueEnum<'ctx>, ty: &Type) {
        let fields = self.structs[&ty.to_string()].clone();
        let i64_type = self.context.i64_type();
        let string = Type::String;
        let tuple = tuple_elements(ty).is_some();
        let source = if tuple {
            let n = i64_type.const_int(fields.len() as u64, false);
            let found = self.decoding_call(
                "$dec_items",
                &[],
                &[
                    (v, json_value_type()),
                    (n.into(), Type::I64),
                    (path, string.clone()),
                ],
            );
            let items_ty = Type::Array(Box::new(json_value_type()));
            self.json_try(found, &items_ty, ty)
        } else {
            let found = self.decoding_call(
                "$dec_object",
                &[],
                &[(v, json_value_type()), (path, string.clone())],
            );
            let map_ty = crate::check::json_object_type();
            self.json_try(found, &map_ty, ty)
        };
        let struct_type = self
            .context
            .get_struct_type(&ty.to_string())
            .expect("declared before lowering");
        let mut aggregate = struct_type.get_undef();
        for (index, (field, fty)) in fields.iter().enumerate() {
            let (member, at) = if tuple {
                let size = self.element_size(&json_value_type());
                let i = i64_type.const_int(index as u64, false);
                let address = self.element_address(source.into_pointer_value(), i, size);
                let member = self.load_element(address, &json_value_type());
                let at = self.decoding_call(
                    "$index_path",
                    &[],
                    &[(path, string.clone()), (i.into(), Type::I64)],
                );
                (member, at)
            } else {
                let name = self.string_literal(field);
                let optional = self
                    .context
                    .bool_type()
                    .const_int(u64::from(matches!(fty, Type::Option(_))), false);
                let map_ty = crate::check::json_object_type();
                let found = self.decoding_call(
                    "$dec_field",
                    &[],
                    &[
                        (source, map_ty),
                        (name.into(), string.clone()),
                        (path, string.clone()),
                        (optional.into(), Type::Bool),
                    ],
                );
                let member = self.json_try(found, &json_value_type(), ty);
                let at = self.decoding_call(
                    "$field_path",
                    &[],
                    &[(path, string.clone()), (name.into(), string.clone())],
                );
                (member, at)
            };
            let decoded = self.json_decode(member, at, fty);
            let value = self.json_try(decoded, fty, ty);
            let index = u32::try_from(index).expect("field count fits in u32");
            aggregate = self
                .builder
                .build_insert_value(aggregate, self.stored_form(value, fty), index, "field")
                .expect("field exists")
                .into_struct_value();
        }
        self.json_return(ty, "Ok", aggregate.into());
    }

    /// An enum from a variant's name, or a one-key object naming a variant
    /// that has a payload (several payloads as an array).
    fn json_variant(&mut self, v: BasicValueEnum<'ctx>, path: BasicValueEnum<'ctx>, ty: &Type) {
        let value_ty = json_value_type();
        let string = Type::String;
        let found = self.decoding_call(
            "$variant_name",
            &[],
            &[(v, value_ty.clone()), (path, string.clone())],
        );
        let name = self.json_try(found, &string, ty);
        let payload = self.decoding_call("$variant_payload", &[], &[(v, value_ty.clone())]);
        let bare = self.decoding_call("$variant_bare", &[], &[(v, value_ty.clone())]);
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
            let (fine, wrong) = (self.append("variant.shape"), self.append("variant.wrong"));
            // A bare name is right exactly when the variant has no payload.
            let (if_bare, if_object) = if payloads.is_empty() {
                (fine, wrong)
            } else {
                (wrong, fine)
            };
            self.builder
                .build_conditional_branch(bare.into_int_value(), if_bare, if_object)
                .expect(POSITIONED);
            self.builder.position_at_end(wrong);
            let error = self.decoding_call(
                "$payload_error",
                &[],
                &[
                    (path, string.clone()),
                    (literal, string.clone()),
                    (bare, Type::Bool),
                ],
            );
            self.json_return(ty, "Err", error);
            self.builder.position_at_end(fine);
            let at = self.decoding_call(
                "$field_path",
                &[],
                &[(path, string.clone()), (literal, string.clone())],
            );
            let mut values = Vec::new();
            if payloads.len() == 1 {
                let decoded = self.json_decode(payload, at, &payloads[0]);
                values.push(self.json_try(decoded, &payloads[0], ty));
            } else if payloads.len() > 1 {
                let n = self
                    .context
                    .i64_type()
                    .const_int(payloads.len() as u64, false);
                let found = self.decoding_call(
                    "$dec_items",
                    &[],
                    &[
                        (payload, value_ty.clone()),
                        (n.into(), Type::I64),
                        (at, string.clone()),
                    ],
                );
                let items_ty = Type::Array(Box::new(value_ty.clone()));
                let items = self.json_try(found, &items_ty, ty).into_pointer_value();
                let size = self.element_size(&value_ty);
                for (i, p) in payloads.iter().enumerate() {
                    let index = self.context.i64_type().const_int(i as u64, false);
                    let member =
                        self.load_element(self.element_address(items, index, size), &value_ty);
                    let place = self.decoding_call(
                        "$index_path",
                        &[],
                        &[(at, string.clone()), (index.into(), Type::I64)],
                    );
                    let decoded = self.json_decode(member, place, p);
                    values.push(self.json_try(decoded, p, ty));
                }
            }
            let built = self.construct_values(ty, &vname, values);
            self.json_return(ty, "Ok", built);
            self.builder.position_at_end(next);
        }
        let error = self.decoding_call(
            "$unknown_variant",
            &[],
            &[(path, string.clone()), (name, string)],
        );
        self.json_return(ty, "Err", error);
    }
}
