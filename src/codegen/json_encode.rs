//! `$json.encode(x)` (v17, milestone 139, PRP-171): `x` as a `json.Value`.
//! Scalars and containers call `std.$json_derive`'s Lugha helpers, which
//! the checker listed (`check/json.rs`); structs, enums and tuples get one
//! `lugha_json_enc_<type>` walker each, building the keys and values a
//! helper turns into an object or array.

use inkwell::module::Linkage;
use inkwell::types::BasicType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, ValueKind};

use super::CodegenError;
use super::copy::mangle;
use super::enums::variants;
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::value::{is_aggregate, llvm_type};
use crate::ast::Expr;
use crate::check::{
    JSON_DERIVE, JSON_VALUE, Type, container_helper, json_value_type, scalar_helper,
};

impl<'ctx> Lowerer<'ctx> {
    /// `$json.encode(x)`.
    pub(super) fn json_encode_call(
        &mut self,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(&args[0]);
        let value = self.get(&args[0], &ty)?;
        Ok(self.json_encode(value, &ty))
    }

    /// `value`, of type `ty`, as a `Value`.
    fn json_encode(&mut self, value: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        if let Some(name) = scalar_helper(ty) {
            let arg = self.json_scalar(value, ty);
            let args: Vec<BasicMetadataValueEnum> = arg.into_iter().map(Into::into).collect();
            return self.derive_call(name, &[], &args);
        }
        if let Some((name, targs)) = container_helper(ty) {
            // Maps, sets and `Option`s go to a Lugha function as a pointer
            // to the caller's copy, as every call passes them (`arguments`).
            let arg: BasicMetadataValueEnum = if is_aggregate(ty) {
                let slot = self.entry_alloca(ty, "arg");
                self.builder.build_store(slot, value).expect(POSITIONED);
                slot.into()
            } else {
                value.into()
            };
            return self.derive_call(name, &targs, &[arg]);
        }
        if matches!(ty, Type::Enum(key, _) if key == JSON_VALUE) {
            return value;
        }
        let walker = self.json_walker(ty);
        let call = self
            .builder
            .build_call(walker, &[value.into()], "json")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => v,
            ValueKind::Instruction(_) => unreachable!("walkers return a Value"),
        }
    }

    /// A scalar as its helper takes it: integers widened to `i64` (but
    /// `u64`, whose helper decides), `f32` to `f64`, `()` as nothing.
    fn json_scalar(&self, value: BasicValueEnum<'ctx>, ty: &Type) -> Option<BasicValueEnum<'ctx>> {
        let b = &self.builder;
        Some(match ty {
            Type::Unit => return None,
            Type::F32 => b
                .build_float_ext(value.into_float_value(), self.context.f64_type(), "wide")
                .expect(POSITIONED)
                .into(),
            Type::I8 | Type::I16 | Type::I32 => b
                .build_int_s_extend(value.into_int_value(), self.context.i64_type(), "wide")
                .expect(POSITIONED)
                .into(),
            Type::U8 | Type::U16 | Type::U32 => b
                .build_int_z_extend(value.into_int_value(), self.context.i64_type(), "wide")
                .expect(POSITIONED)
                .into(),
            _ => value,
        })
    }

    /// A call of `std.$json_derive`'s `name` at `targs`.
    fn derive_call(
        &self,
        name: &str,
        targs: &[Type],
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let key = instance_name(&format!("{JSON_DERIVE}{name}"), targs);
        let function = self
            .monos
            .get(&key)
            .unwrap_or_else(|| panic!("checked: `{key}` is listed for every `$json.encode`"))
            .signature
            .function;
        let call = self
            .builder
            .build_call(function, args, "json")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(v) => v,
            ValueKind::Instruction(_) => unreachable!("helpers return a Value"),
        }
    }

    /// `lugha_json_enc_<mangled>(T) -> Value`, generated once per type and
    /// declared before its body, so a recursive enum's walker calls itself.
    fn json_walker(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_json_enc_{}", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let result = llvm_type(self.context, &json_value_type());
        let function = self.module.add_function(
            &name,
            result.fn_type(&[llvm_type(self.context, ty).into()], false),
            Some(Linkage::Internal),
        );
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let value = function.get_nth_param(0).expect("one parameter");
        let out = match ty {
            Type::Enum(..) => self.json_variants(value, ty),
            _ if crate::check::tuple_elements(ty).is_some() => {
                let parts = self.json_fields(value, ty);
                let values = self.json_array_of(parts.into_iter().map(|(_, v)| v).collect());
                self.derive_call("$array", &[], &[values.into()])
            }
            _ => {
                let parts = self.json_fields(value, ty);
                self.json_object(parts)
            }
        };
        self.builder.build_return(Some(&out)).expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }

    /// Each field's name and encoded value, in declaration order.
    fn json_fields(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> Vec<(String, BasicValueEnum<'ctx>)> {
        let value = value.into_struct_value();
        let fields = self.structs[&ty.to_string()].clone();
        let mut out = Vec::new();
        for (index, (field, fty)) in fields.iter().enumerate() {
            let index = u32::try_from(index).expect("field count fits in u32");
            let raw = self
                .builder
                .build_extract_value(value, index, field)
                .expect("field exists");
            let part = self.loaded_form(raw, fty);
            let encoded = self.json_encode(part, fty);
            out.push((field.clone(), encoded));
        }
        out
    }

    /// A new array holding `values` (each a `Value`).
    fn json_array_of(
        &self,
        values: Vec<BasicValueEnum<'ctx>>,
    ) -> inkwell::values::PointerValue<'ctx> {
        let element = json_value_type();
        let size = self.element_size(&element);
        let len = self
            .context
            .i64_type()
            .const_int(values.len() as u64, false);
        let array = self.new_array(len, &element);
        for (i, v) in values.into_iter().enumerate() {
            let at = self.context.i64_type().const_int(i as u64, false);
            self.store_element(self.element_address(array, at, size), &element, v);
        }
        array
    }

    /// An object of `parts`, in order.
    fn json_object(&mut self, parts: Vec<(String, BasicValueEnum<'ctx>)>) -> BasicValueEnum<'ctx> {
        let names: Vec<BasicValueEnum<'ctx>> = parts
            .iter()
            .map(|(n, _)| self.string_literal(n).into())
            .collect();
        let size = self.element_size(&Type::String);
        let len = self.context.i64_type().const_int(names.len() as u64, false);
        let keys = self.new_array(len, &Type::String);
        for (i, n) in names.into_iter().enumerate() {
            let at = self.context.i64_type().const_int(i as u64, false);
            self.store_element(self.element_address(keys, at, size), &Type::String, n);
        }
        let values = self.json_array_of(parts.into_iter().map(|(_, v)| v).collect());
        self.derive_call("$object", &[], &[keys.into(), values.into()])
    }

    /// An enum value by its variant: its name as text, or a one-key object
    /// holding its payload (several payloads as an array).
    fn json_variants(&mut self, value: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        let slot = self.entry_alloca(ty, "value");
        self.builder.build_store(slot, value).expect(POSITIONED);
        let tag = self.tag(ty, slot);
        let result = self.entry_alloca(&json_value_type(), "json");
        let done = self.append("json.done");
        let i32_type = self.context.i32_type();
        let mut cases = Vec::new();
        for (index, (name, payloads)) in variants(ty, &self.enums).into_iter().enumerate() {
            let block = self.append("variant");
            let tag = u64::try_from(index).expect("variant count fits in u64");
            cases.push((i32_type.const_int(tag, false), block));
            let resume = self.current_block();
            self.builder.position_at_end(block);
            let encoded = if payloads.is_empty() {
                let text = self.string_literal(&name);
                self.derive_call("$text", &[], &[text.into()])
            } else {
                let mut parts = Vec::new();
                for (i, payload) in payloads.iter().enumerate() {
                    let i = u32::try_from(i).expect("payload count fits in u32");
                    let x = self.load_payload(ty, slot, &payloads, i, payload);
                    parts.push(self.json_encode(x, payload));
                }
                let inner = if parts.len() == 1 {
                    parts.remove(0)
                } else {
                    let values = self.json_array_of(parts);
                    self.derive_call("$array", &[], &[values.into()])
                };
                self.json_object(vec![(name.clone(), inner)])
            };
            self.builder.build_store(result, encoded).expect(POSITIONED);
            self.branch(done);
            self.builder.position_at_end(resume);
        }
        // Every tag has a case; the default is never taken.
        self.builder
            .build_switch(tag, done, &cases)
            .expect(POSITIONED);
        self.builder.position_at_end(done);
        self.builder
            .build_load(llvm_type(self.context, &json_value_type()), result, "json")
            .expect(POSITIONED)
    }
}
