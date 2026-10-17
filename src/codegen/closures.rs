//! Closures in codegen (v3 draft §7, §9; milestone 15): a closure literal
//! copies its captures into one GC environment, written once, and its body
//! becomes `lugha_closure_<enclosing>.<n>`, which takes that environment
//! first, like every function value's code.

use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::{Value, is_aggregate, llvm_type};
use crate::ast::{Closure, Expr};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `fun(x) = e`: `{ lugha_closure_…, env }`, where `env` holds a copy
    /// of each capture (null when there are none).
    pub(super) fn closure_value(
        &mut self,
        expr: &Expr,
        c: &Closure,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        self.closure_value_with(expr, c, &[])
    }

    /// A closure value that also captures the locals named in `extra`: a
    /// spawned call's arguments, already evaluated (v17).
    pub(super) fn closure_value_with(
        &mut self,
        expr: &Expr,
        c: &Closure,
        extra: &[String],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let ty = self.ty(expr);
        let Type::Fun(params, ret) = &ty else {
            unreachable!("checked: a closure has a function type")
        };
        let mut names = self.captures.get(&expr.id).cloned().unwrap_or_default();
        names.extend(extra.iter().cloned());
        let captured: Vec<Local<'ctx>> = names.iter().map(|n| self.scopes.lookup(n)).collect();
        let layout = self.env_layout(&captured);
        let ptr = self.context.ptr_type(Default::default());
        let env = match layout {
            None => ptr.const_null(),
            Some(layout) => self.fill_env(layout, &names, &captured),
        };
        let function = self.closure_function(params, ret);
        let types: Vec<Type> = captured.iter().map(|l| l.ty.clone()).collect();
        self.lower_closure(c, function, (params, ret), (layout, &names, &types))?;
        let pair = self.context.struct_type(&[ptr.into(), ptr.into()], false);
        let code = function.as_global_value().as_pointer_value();
        let value = pair.get_undef();
        let value = self
            .builder
            .build_insert_value(value, code, 0, "code")
            .expect(POSITIONED);
        let value = self
            .builder
            .build_insert_value(value, env, 1, "env")
            .expect(POSITIONED);
        Ok(value.into_struct_value().into())
    }

    /// The environment's LLVM struct: one field per capture, in order.
    fn env_layout(&self, captured: &[Local<'ctx>]) -> Option<StructType<'ctx>> {
        if captured.is_empty() {
            return None;
        }
        let fields: Vec<BasicTypeEnum> = captured
            .iter()
            .map(|l| llvm_type(self.context, &l.ty))
            .collect();
        Some(self.context.struct_type(&fields, false))
    }

    /// A new environment holding a copy of each capture's current value.
    fn fill_env(
        &mut self,
        layout: StructType<'ctx>,
        names: &[String],
        captured: &[Local<'ctx>],
    ) -> PointerValue<'ctx> {
        let size = layout.size_of().expect("an environment is sized");
        let alloc = self.runtime("lugha_rt_alloc", &[Type::I64], Some(Type::String));
        let call = self
            .builder
            .build_call(alloc, &[size.into()], "env")
            .expect(POSITIONED);
        let ValueKind::Basic(env) = call.try_as_basic_value() else {
            unreachable!("lugha_rt_alloc returns a pointer")
        };
        let env = env.into_pointer_value();
        for (i, (name, local)) in names.iter().zip(captured).enumerate() {
            let value = self.load(local, name);
            // A copy, as at any copy site: the closure may outlive the local.
            let value = if local.ty.is_deep(&self.structs) {
                self.deep_copy(value, &local.ty)
            } else {
                value
            };
            let index = u32::try_from(i).expect("capture count fits in u32");
            let at = self
                .builder
                .build_struct_gep(layout, env, index, name)
                .expect("a field per capture");
            self.builder.build_store(at, value).expect(POSITIONED);
        }
        env
    }

    /// Declares `lugha_closure_<enclosing>.<n>`: environment first.
    fn closure_function(&mut self, params: &[Type], ret: &Type) -> FunctionValue<'ctx> {
        let enclosing = self.function.expect("closures are inside a function");
        let name = enclosing.get_name().to_string_lossy().into_owned();
        let base = name
            .strip_prefix("lugha_fn_")
            .or_else(|| name.strip_prefix("lugha_closure_"))
            .unwrap_or(&name)
            .to_string();
        let count = self.closure_counts.entry(base.clone()).or_insert(0);
        let symbol = format!("lugha_closure_{base}.{count}");
        *count += 1;
        let ptr = self.context.ptr_type(Default::default());
        let mut types: Vec<BasicMetadataTypeEnum> = vec![ptr.into()];
        types.extend(params.iter().map(|p| self.param_type(p)));
        let fn_type = match ret {
            Type::Unit => self.context.void_type().fn_type(&types, false),
            ret => llvm_type(self.context, ret).fn_type(&types, false),
        };
        self.module
            .add_function(&symbol, fn_type, Some(Linkage::Internal))
    }

    /// Lowers the closure's body into `function`, then returns to where the
    /// enclosing function was being lowered.
    fn lower_closure(
        &mut self,
        c: &Closure,
        function: FunctionValue<'ctx>,
        (params, ret): (&[Type], &Type),
        (layout, names, types): (Option<StructType<'ctx>>, &[String], &[Type]),
    ) -> Result<(), CodegenError> {
        let saved_block = self.builder.get_insert_block();
        let saved_function = self.function.replace(function);
        let saved_ret = std::mem::replace(&mut self.ret, (*ret != Type::Unit).then(|| ret.clone()));
        let saved_scopes = std::mem::take(&mut self.scopes);
        let saved_loops = std::mem::take(&mut self.loops);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        self.preempt_point();
        self.scopes.push();
        let env = function
            .get_nth_param(0)
            .expect("the environment comes first")
            .into_pointer_value();
        if let Some(layout) = layout {
            for (i, (name, ty)) in names.iter().zip(types).enumerate() {
                let index = u32::try_from(i).expect("capture count fits in u32");
                // Read in place: the environment is never written after creation.
                let ptr = self
                    .builder
                    .build_struct_gep(layout, env, index, name)
                    .expect("a field per capture");
                self.scopes.declare(
                    name,
                    Local {
                        ptr,
                        ty: ty.clone(),
                        borrowed: true,
                    },
                );
            }
        }
        for (i, ((name, _), ty)) in c.params.iter().zip(params).enumerate() {
            let index = u32::try_from(i + 1).expect("parameter count fits in u32");
            let arg = function
                .get_nth_param(index)
                .expect("declared with these parameters");
            let ptr = if is_aggregate(ty) {
                arg.into_pointer_value()
            } else {
                let ptr = self.entry_alloca(ty, &name.name);
                self.builder.build_store(ptr, arg).expect(POSITIONED);
                ptr
            };
            self.scopes.declare(
                &name.name,
                Local {
                    ptr,
                    ty: ty.clone(),
                    borrowed: true,
                },
            );
        }
        let value = self.block_inner(&c.body, true)?;
        match (*ret == Type::Unit, value) {
            (_, Value::Never) => self.builder.build_unreachable(),
            (true, _) => self.builder.build_return(None),
            (false, Value::Val(result)) => self.builder.build_return(Some(&result)),
            (false, Value::Void) => unreachable!("checked: a closure returning a value has one"),
        }
        .expect(POSITIONED);
        self.function = saved_function;
        self.ret = saved_ret;
        self.scopes = saved_scopes;
        self.loops = saved_loops;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn closures_get_their_own_functions_and_environments() {
        let ir = ir(
            "fun main() { let k = 2; let add = fun(x: i64): i64 = x + k; \
                     let one = fun(): i64 = 1; let n = add(one()); }",
        );
        assert!(
            ir.contains("define internal i64 @lugha_closure_main.0(ptr"),
            "{ir}"
        );
        assert!(
            ir.contains("define internal i64 @lugha_closure_main.1(ptr"),
            "{ir}"
        );
        let main = &ir[ir.find("define void @lugha_fn_main()").unwrap()..];
        assert_eq!(
            main.matches("call ptr @lugha_rt_alloc").count(),
            1,
            "{main}"
        );
        assert!(
            main.contains("ptr null"),
            "a closure capturing nothing has no environment: {main}"
        );
    }
}
