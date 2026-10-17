//! Function values in codegen (v3 draft §7, §9; milestone 14): a value is a
//! `{ code, env }` pair. A named function's code is its `lugha_ref_`
//! wrapper, which takes the environment first and calls the function; its
//! environment is null. Calling a value is an indirect call.

use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, ValueKind};

use super::CodegenError;
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, is_aggregate, llvm_type};
use crate::ast::Expr;
use crate::check::{Type, symbol};

impl<'ctx> Lowerer<'ctx> {
    /// A function or associated function used as a value: `{ wrapper, null }`.
    pub(super) fn function_value(&mut self, expr: &Expr) -> BasicValueEnum<'ctx> {
        let target = self.value_target(expr);
        let wrapper = self.wrapper(target);
        let ptr = self.context.ptr_type(Default::default());
        let pair = self.context.struct_type(&[ptr.into(), ptr.into()], false);
        let code = wrapper.as_global_value().as_pointer_value();
        pair.const_named_struct(&[code.into(), ptr.const_null().into()])
            .into()
    }

    /// A variant used as a function value (v17, milestone 94):
    /// `{ lugha_ref_<Enum>.<Variant>, null }`.
    pub(super) fn variant_function_value(&mut self, expr: &Expr) -> BasicValueEnum<'ctx> {
        let (ty, variant) = self.variant_values[&expr.id].clone();
        let ty = self.concrete(&ty);
        let wrapper = self.variant_wrapper(&ty, &variant);
        let ptr = self.context.ptr_type(Default::default());
        let pair = self.context.struct_type(&[ptr.into(), ptr.into()], false);
        let code = wrapper.as_global_value().as_pointer_value();
        pair.const_named_struct(&[code.into(), ptr.const_null().into()])
            .into()
    }

    /// `lugha_ref_<Enum>.<Variant>`: takes an environment, then the payloads,
    /// and builds the variant as a construction does. Emitted once.
    fn variant_wrapper(&mut self, ty: &Type, variant: &str) -> FunctionValue<'ctx> {
        let name = format!("lugha_ref_{}.{variant}", symbol(&ty.to_string()));
        if let Some(existing) = self.module.get_function(&name) {
            return existing;
        }
        let (_, payloads) = self.variant(ty, variant);
        let ptr = self.context.ptr_type(Default::default());
        let mut params: Vec<BasicMetadataTypeEnum> = vec![ptr.into()];
        params.extend(payloads.iter().map(|p| self.param_type(p)));
        let fn_type = llvm_type(self.context, ty).fn_type(&params, false);
        let wrapper = self
            .module
            .add_function(&name, fn_type, Some(Linkage::Internal));
        // Build the body elsewhere, then come back to where lowering was.
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(wrapper);
        self.builder
            .position_at_end(self.context.append_basic_block(wrapper, "entry"));
        let mut values = Vec::with_capacity(payloads.len());
        for (param, payload) in wrapper.get_param_iter().skip(1).zip(&payloads) {
            // Aggregates arrive as a pointer to the caller's copy, as for any call.
            let value = if is_aggregate(payload) {
                let llvm = llvm_type(self.context, payload);
                self.builder
                    .build_load(llvm, param.into_pointer_value(), "payload")
                    .expect(POSITIONED)
            } else {
                param
            };
            // Stored as a function body stores a parameter: deep values copied.
            let value = if payload.is_deep(&self.structs) {
                self.deep_copy(value, payload)
            } else {
                value
            };
            values.push(value);
        }
        let built = self.construct_values(ty, variant, values);
        self.builder.build_return(Some(&built)).expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        wrapper
    }

    /// The function `expr` names, as the checker recorded it: an instance,
    /// a function or an associated function.
    fn value_target(&self, expr: &Expr) -> FunctionValue<'ctx> {
        let key = &self.callees[&expr.id];
        if let Some(args) = self.instances.get(&expr.id) {
            let args: Vec<Type> = args.iter().map(|a| self.concrete(a)).collect();
            return self.monos[&instance_name(key, &args)].signature.function;
        }
        // A std function is a listed instance at no arguments (v15).
        match self
            .functions
            .get(key)
            .or_else(|| self.monos.get(key).map(|m| &m.signature))
        {
            Some(signature) => signature.function,
            None => self.methods[key].signature.function,
        }
    }

    /// `lugha_ref_<symbol>`: `target` with an environment pointer first,
    /// emitted once.
    fn wrapper(&mut self, target: FunctionValue<'ctx>) -> FunctionValue<'ctx> {
        let symbol = target.get_name().to_string_lossy().into_owned();
        let name = symbol.replacen("lugha_fn_", "lugha_ref_", 1);
        if let Some(existing) = self.module.get_function(&name) {
            return existing;
        }
        let target_type = target.get_type();
        let ptr = self.context.ptr_type(Default::default());
        let mut params: Vec<BasicMetadataTypeEnum> = vec![ptr.into()];
        params.extend(target_type.get_param_types());
        let fn_type = match target_type.get_return_type() {
            Some(ret) => ret.fn_type(&params, false),
            None => self.context.void_type().fn_type(&params, false),
        };
        let wrapper = self
            .module
            .add_function(&name, fn_type, Some(Linkage::Internal));
        // Build the body elsewhere, then come back to where lowering was.
        let saved = self.builder.get_insert_block();
        self.builder
            .position_at_end(self.context.append_basic_block(wrapper, "entry"));
        let args: Vec<_> = wrapper.get_param_iter().skip(1).map(Into::into).collect();
        let call = self
            .builder
            .build_call(target, &args, "call")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(result) => self.builder.build_return(Some(&result)),
            ValueKind::Instruction(_) => self.builder.build_return(None),
        }
        .expect(POSITIONED);
        if let Some(block) = saved {
            self.builder.position_at_end(block);
        }
        wrapper
    }

    /// `callee(args)` where `callee` is a function value: its code, called
    /// with its environment first, then the arguments as for any call.
    pub(super) fn call_value(
        &mut self,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let ty = self.ty(callee);
        let Type::Fun(params, ret) = &ty else {
            unreachable!("checked: only function values are called this way")
        };
        let pair = self.get(callee, &ty)?.into_struct_value();
        let values = self.arguments(args, params, false)?;
        Ok(self.call_pair(pair, (params, ret), values))
    }

    /// Calls function value `pair` (`{ code, env }`) of parameter types
    /// `params` and result `ret` with `values`, already lowered as arguments.
    pub(super) fn call_pair(
        &mut self,
        pair: inkwell::values::StructValue<'ctx>,
        (params, ret): (&[Type], &Type),
        args: Vec<BasicMetadataValueEnum<'ctx>>,
    ) -> Value<'ctx> {
        let code = self
            .builder
            .build_extract_value(pair, 0, "code")
            .expect("a pair has 2 fields");
        let env = self
            .builder
            .build_extract_value(pair, 1, "env")
            .expect("a pair has 2 fields");
        let mut values = vec![env.into()];
        values.extend(args);
        let ptr = self.context.ptr_type(Default::default());
        let mut types: Vec<BasicMetadataTypeEnum> = vec![ptr.into()];
        types.extend(params.iter().map(|p| self.param_type(p)));
        let fn_type = match ret {
            Type::Unit => self.context.void_type().fn_type(&types, false),
            ret => llvm_type(self.context, ret).fn_type(&types, false),
        };
        let site = self
            .builder
            .build_indirect_call(fn_type, code.into_pointer_value(), &values, "call")
            .expect(POSITIONED);
        match site.try_as_basic_value() {
            ValueKind::Basic(result) => Value::Val(result),
            ValueKind::Instruction(_) => Value::Void,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn named_functions_become_wrapper_pairs() {
        let ir = ir("fun double(x: i64): i64 = x * 2;\n\
                     fun max<T: Ord>(a: T, b: T): T = if a > b { a } else { b };\n\
                     fun main() { let f = double; let g = double; let m: fun(i64, i64): i64 = max; let n = f(1) + m(2, 3); }");
        assert_eq!(
            ir.matches("define internal i64 @lugha_ref_double(ptr")
                .count(),
            1,
            "{ir}"
        );
        assert!(ir.contains("{ ptr @lugha_ref_double, ptr null }"), "{ir}");
        assert!(ir.contains("@\"lugha_ref_max<i64>\""), "{ir}");
    }

    #[test]
    fn a_variant_value_has_one_wrapper_v17() {
        let ir = ir("enum Shape { Circle(f64), Empty }\n\
                     fun main() { let a = Shape.Circle; let b = Shape.Circle; let c = [Shape.Circle]; }");
        assert_eq!(ir.matches("@lugha_ref_Shape.Circle(ptr").count(), 1, "{ir}");
    }
}
