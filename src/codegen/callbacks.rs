//! Callbacks into Lugha (v16, milestone 88, PRP-118): C gets plain function
//! pointers with C signatures, and these call the Lugha function value.
//!
//! - A callback without `data` gets `lugha_cfn_<n>`, one per call site. The
//!   checker allows only a function's name or a closure capturing nothing
//!   there (E0436), so the value at a site never changes: it is stored in
//!   the global `lugha_cbv_<n>` before the call, and the entry calls it.
//! - A callback with `data` gets the trampoline `lugha_cb_<signature>`, one
//!   per C signature, and the `data` argument is `lugha_rt_cb_pin(code, env)`:
//!   the trampoline reads the closure back from it.
//! - A `release` argument is `lugha_rt_cb_release`.

use inkwell::attributes::AttributeLoc;
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, FunctionType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue, StructValue, ValueKind,
};

use super::CodegenError;
use super::c_calls::{CKind, CSig, has_struct};
use super::declare::abi_attribute;
use super::externs::{converts_from_c, is_nullable_handle};
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, is_aggregate};
use crate::ast::Expr;
use crate::check::{CParam, Type};

/// The C type of a callback's parameter list with `data` at `data`, as text:
/// `fun(i64, data): i32`, naming its trampoline.
fn c_signature(params: &[Type], ret: &Type, data: Option<usize>) -> String {
    let mut parts: Vec<String> = params.iter().map(ToString::to_string).collect();
    if let Some(at) = data {
        parts.insert(at, "data".into());
    }
    match ret {
        Type::Unit => format!("fun({})", parts.join(", ")),
        ret => format!("fun({}): {ret}", parts.join(", ")),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// The LLVM type C uses for a value of type `ty` in an extern or callback.
    fn c_value_type(&self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        self.c_result_type(ty).into()
    }

    /// The parameter types C sees for an extern with plan `plan` and
    /// Lugha parameters `params` (the written ones, callbacks as `Fun`).
    pub(super) fn c_param_types(
        &self,
        plan: &[CParam],
        params: &[Type],
    ) -> Vec<(BasicMetadataTypeEnum<'ctx>, Option<Type>)> {
        let ptr = self.context.ptr_type(Default::default());
        let mut written = params.iter();
        plan.iter()
            .map(|c| match c {
                CParam::Value => {
                    let ty = written
                        .next()
                        .expect("checked: a value per written parameter");
                    (self.param_type(ty), Some(ty.clone()))
                }
                CParam::Callback { .. } | CParam::Pointer(_) | CParam::Elements { .. } => {
                    written.next();
                    (ptr.into(), None)
                }
                CParam::Data | CParam::Release => (ptr.into(), None),
            })
            .collect()
    }

    /// The arguments of a call to an extern with plan `plan`: the written
    /// ones left to right (spec §5), then what lughac adds for `data` and
    /// `release`.
    pub(super) fn callback_arguments(
        &mut self,
        args: &[Expr],
        params: &[Type],
        plan: &[CParam],
    ) -> Result<Vec<BasicValueEnum<'ctx>>, CodegenError> {
        let mut written = Vec::with_capacity(args.len());
        // The written parameters' kinds: `out` ones pass a slot (v17).
        let mut kinds = plan
            .iter()
            .filter(|c| !matches!(c, CParam::Data | CParam::Release));
        for (arg, ty) in args.iter().zip(params) {
            match kinds.next() {
                Some(CParam::Pointer(mode)) => {
                    written.push(self.pointer_argument(arg, ty, *mode)?);
                    continue;
                }
                Some(CParam::Elements { mutable }) => {
                    written.push(self.elements_argument(arg, ty, *mutable)?);
                    continue;
                }
                _ => {}
            }
            let value = self.get(arg, ty)?;
            written.push(match ty {
                Type::String => self.c_string(value),
                ty if is_nullable_handle(ty) => self.nullable_to_c(value, ty),
                _ => value,
            });
        }
        let mut written = written.into_iter();
        let (mut pinned, mut values) = (None, Vec::new());
        // `data` may come before its callback, so it is filled in afterwards.
        let mut data_at = None;
        for c in plan {
            let value: BasicValueEnum = match c {
                CParam::Value | CParam::Pointer(_) | CParam::Elements { .. } => {
                    written.next().expect("checked arity")
                }
                CParam::Callback { params, ret, data } => {
                    let pair = written.next().expect("checked arity").into_struct_value();
                    match data {
                        None => self.static_entry(pair, params, ret).into(),
                        Some(at) => {
                            pinned = Some(self.pin(pair));
                            self.trampoline(params, ret, *at).into()
                        }
                    }
                }
                CParam::Data => {
                    data_at = Some(values.len());
                    self.context
                        .ptr_type(Default::default())
                        .const_null()
                        .into()
                }
                CParam::Release => {
                    let release = self.runtime("lugha_rt_cb_release", &[Type::String], None);
                    release.as_global_value().as_pointer_value().into()
                }
            };
            values.push(value);
        }
        if let (Some(at), Some(pinned)) = (data_at, pinned) {
            values[at] = pinned.into();
        }
        Ok(values)
    }

    /// `lugha_rt_cb_pin(code, env)`: the closure, kept alive for C.
    fn pin(&mut self, pair: StructValue<'ctx>) -> PointerValue<'ctx> {
        let code = self
            .builder
            .build_extract_value(pair, 0, "code")
            .expect("a pair has 2 fields");
        let env = self
            .builder
            .build_extract_value(pair, 1, "env")
            .expect("a pair has 2 fields");
        let pin = self.runtime(
            "lugha_rt_cb_pin",
            &[Type::String, Type::String],
            Some(Type::String),
        );
        let call = self
            .builder
            .build_call(pin, &[code.into(), env.into()], "pinned")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(pinned) => pinned.into_pointer_value(),
            ValueKind::Instruction(_) => unreachable!("lugha_rt_cb_pin returns a pointer"),
        }
    }

    /// The C function type of a callback, `data` included at `data`.
    fn c_function_type(
        &self,
        params: &[Type],
        ret: &Type,
        data: Option<usize>,
    ) -> FunctionType<'ctx> {
        // Counted arrays are two C parameters (v17, milestone 105).
        let (params, data) = &super::c_arrays::c_params(params, data);
        let (params, data) = (params.as_slice(), *data);
        if let Some(sig) = self.entry_sig(params, ret, data) {
            return sig.fn_type;
        }
        let mut types: Vec<BasicMetadataTypeEnum> =
            params.iter().map(|p| self.c_value_type(p)).collect();
        if let Some(at) = data {
            types.insert(at, self.context.ptr_type(Default::default()).into());
        }
        match ret {
            Type::Unit => self.context.void_type().fn_type(&types, false),
            ret => self.c_result_type(ret).fn_type(&types, false),
        }
    }

    /// `lugha_cfn_<n>` for a callback without `data` at this call site:
    /// `pair` is stored in `lugha_cbv_<n>`, which the entry calls.
    fn static_entry(
        &mut self,
        pair: StructValue<'ctx>,
        params: &[Type],
        ret: &Type,
    ) -> PointerValue<'ctx> {
        let n = self.c_entries;
        self.c_entries += 1;
        let pair_type = pair.get_type();
        let global = self
            .module
            .add_global(pair_type, None, &format!("lugha_cbv_{n}"));
        global.set_linkage(Linkage::Internal);
        global.set_initializer(&pair_type.const_zero());
        let slot = global.as_pointer_value();
        self.builder.build_store(slot, pair).expect(POSITIONED);
        let fn_type = self.c_function_type(params, ret, None);
        let entry =
            self.module
                .add_function(&format!("lugha_cfn_{n}"), fn_type, Some(Linkage::Internal));
        self.c_body(entry, params, ret, None, |this, _| {
            this.builder
                .build_load(pair_type, slot, "pair")
                .expect(POSITIONED)
                .into_struct_value()
        });
        entry.as_global_value().as_pointer_value()
    }

    /// `lugha_cb_<signature>`: calls the closure pinned in its `data`
    /// parameter, at index `data`. Emitted once per signature.
    fn trampoline(&mut self, params: &[Type], ret: &Type, data: usize) -> PointerValue<'ctx> {
        let name = format!("lugha_cb_{}", c_signature(params, ret, Some(data)));
        if let Some(existing) = self.module.get_function(&name) {
            return existing.as_global_value().as_pointer_value();
        }
        let fn_type = self.c_function_type(params, ret, Some(data));
        let entry = self
            .module
            .add_function(&name, fn_type, Some(Linkage::Internal));
        self.c_body(entry, params, ret, Some(data), |this, pinned| {
            let ptr = this.context.ptr_type(Default::default());
            let pinned = pinned.expect("a trampoline has its data parameter");
            // The pin holds the function value itself: `{ code, env }`.
            let pair_type = this.context.struct_type(&[ptr.into(), ptr.into()], false);
            this.builder
                .build_load(pair_type, pinned, "pair")
                .expect(POSITIONED)
                .into_struct_value()
        });
        entry.as_global_value().as_pointer_value()
    }

    /// The body of a C entry `entry`: get the function value with `pair`,
    /// convert C's arguments (all but `data`), call it, convert its result.
    fn c_body(
        &mut self,
        entry: FunctionValue<'ctx>,
        params: &[Type],
        ret: &Type,
        data: Option<usize>,
        pair: impl FnOnce(&mut Self, Option<PointerValue<'ctx>>) -> StructValue<'ctx>,
    ) {
        // Counted arrays are two C parameters each (v17, milestone 105).
        let (c_params, c_data) = super::c_arrays::c_params(params, data);
        // Structs by value arrive as C passes them (v16, milestone 90).
        let sig = self.entry_sig(&c_params, ret, c_data);
        match &sig {
            Some(sig) => self.c_sig_attributes(sig, |at, a| entry.add_attribute(at, a)),
            None => self.c_abi_entry(entry, &c_params, ret, c_data),
        }
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(entry);
        self.builder
            .position_at_end(self.context.append_basic_block(entry, "entry"));
        let (all, sret): (Vec<BasicValueEnum>, _) = match &sig {
            Some(sig) => self.c_sig_params(sig, entry),
            None => (entry.get_param_iter().collect(), None),
        };
        let pinned = c_data.map(|at| all[at].into_pointer_value());
        let pair = pair(self, pinned);
        let mut incoming = all
            .into_iter()
            .enumerate()
            .filter(|(i, _)| Some(*i) != c_data)
            .map(|(_, p)| p);
        let mut values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(params.len());
        let mut copies = Vec::new();
        for ty in params {
            let value = incoming.next().expect("C passes every parameter");
            let value = if super::c_arrays::counted(ty).is_some() {
                let count = incoming.next().expect("a counted array has its count");
                self.counted_from_c(ty, (value, count), &mut copies)
            } else if converts_from_c(ty) {
                self.value_from_c(value, ty)
            } else {
                value
            };
            let value = if is_aggregate(ty) {
                // Function values take aggregates as a pointer to the caller's copy.
                let slot = self.entry_alloca(ty, "arg");
                self.builder.build_store(slot, value).expect(POSITIONED);
                slot.into()
            } else {
                value
            };
            values.push(value.into());
        }
        let result = self.call_pair(pair, (params, ret), values);
        self.copy_back(copies);
        if let Some(sig) = &sig {
            let result = match result {
                Value::Val(result) if is_nullable_handle(ret) => {
                    Some(self.nullable_to_c(result, ret))
                }
                Value::Val(result) => Some(result),
                Value::Void | Value::Never => None,
            };
            self.c_sig_return(sig, sret, result);
            self.function = saved_function;
            if let Some(block) = saved_block {
                self.builder.position_at_end(block);
            }
            return;
        }
        match result {
            Value::Val(result) if is_nullable_handle(ret) => {
                let result = self.nullable_to_c(result, ret);
                self.builder.build_return(Some(&result))
            }
            Value::Val(result) => self.builder.build_return(Some(&result)),
            Value::Void | Value::Never => self.builder.build_return(None),
        }
        .expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
    }

    /// The lowered signature of a C entry taking `params` (with the `data`
    /// pointer at `data`) and returning `ret`, when it passes structs.
    fn entry_sig(&self, params: &[Type], ret: &Type, data: Option<usize>) -> Option<CSig<'ctx>> {
        let mut kinds: Vec<CKind> = params.iter().cloned().map(CKind::Value).collect();
        if let Some(at) = data {
            kinds.insert(at, CKind::Pointer);
        }
        has_struct(&kinds, ret).then(|| self.c_sig(&kinds, ret))
    }

    /// C's extensions on an entry's `bool`, `u8`, `u16`, `i8` and `i16` (spec §8).
    fn c_abi_entry(
        &self,
        entry: FunctionValue<'ctx>,
        params: &[Type],
        ret: &Type,
        data: Option<usize>,
    ) {
        // C's parameter list is `params` with the `data` pointer inserted.
        let mut c_params: Vec<Option<&Type>> = params.iter().map(Some).collect();
        if let Some(at) = data {
            c_params.insert(at, None);
        }
        for (index, ty) in c_params.into_iter().enumerate() {
            if let Some(attribute) = ty.and_then(|ty| abi_attribute(self.context, ty)) {
                let index = u32::try_from(index).expect("parameter count fits in u32");
                entry.add_attribute(AttributeLoc::Param(index), attribute);
            }
        }
        if let Some(attribute) = abi_attribute(self.context, ret) {
            entry.add_attribute(AttributeLoc::Return, attribute);
        }
    }
}
