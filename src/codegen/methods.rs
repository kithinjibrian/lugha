//! Methods in codegen (v1 §6, §7, §9): `lugha_fn_<Type>.<name>` functions
//! whose first parameter is the receiver, and method and associated calls.
//!
//! A `self` receiver is passed like any struct argument: a pointer to a
//! caller-side copy. A `mut self` receiver is the address of the caller's
//! place itself, so the method's writes land there.

use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicMetadataValueEnum, ValueKind};

use super::CodegenError;
use super::function::Signature;
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, llvm_type};
use crate::ast::{Expr, ExprKind, FunDecl, ImplDecl, Item};
use crate::check::{Type, Unit, qualify, symbol};

/// A declared method or associated function.
#[derive(Debug, Clone)]
pub(super) struct Method<'ctx> {
    /// The function; `params` exclude the receiver.
    pub signature: Signature<'ctx>,
    /// `None` for an associated function; `Some(mutable)` for `self` / `mut self`.
    pub receiver: Option<bool>,
}

/// The key of a method: `Type.name`, with the type's key (`geometry.Vec2.len`).
pub(super) fn key(owner: &str, name: &str) -> String {
    format!("{owner}.{name}")
}

/// True for a method of a generic `impl` or with parameters of its own:
/// compiled per instance (`instances.rs`, PRP-036).
pub(super) fn generic(i: &ImplDecl, f: &FunDecl) -> bool {
    !i.params.is_empty() || !f.type_params.is_empty()
}

impl<'ctx> Lowerer<'ctx> {
    /// Declares every method of `unit` as `lugha_fn_<Type>.<name>`
    /// (`lugha_fn_geometry/Vec2.len` in a module, PRP-034), receiver first.
    pub(super) fn declare_methods(&mut self, unit: &Unit) {
        for item in &unit.program.items {
            let Item::Impl(i) = item else { continue };
            let owner = qualify(unit.path, &i.ty.name);
            for f in i.methods.iter().filter(|f| !generic(i, f)) {
                let params: Vec<Type> = f
                    .params
                    .iter()
                    .map(|p| self.annotation_type(&p.ty))
                    .collect();
                let ret = f.ret.as_ref().map(|t| self.annotation_type(t));
                let ptr = self.context.ptr_type(Default::default());
                let mut types: Vec<BasicMetadataTypeEnum> =
                    f.receiver.iter().map(|_| ptr.into()).collect();
                types.extend(params.iter().map(|ty| self.param_type(ty)));
                let fn_type = match &ret {
                    Some(ty) => llvm_type(self.context, ty).fn_type(&types, false),
                    None => self.context.void_type().fn_type(&types, false),
                };
                let key = key(&owner, &f.name.name);
                let function = self.module.add_function(
                    &format!("lugha_fn_{}.{}", symbol(&owner), f.name.name),
                    fn_type,
                    Some(Linkage::External),
                );
                let signature = Signature {
                    function,
                    params,
                    ret,
                    is_extern: false,
                };
                self.methods.insert(
                    key,
                    Method {
                        signature,
                        receiver: f.receiver.map(|r| r.mutable),
                    },
                );
            }
        }
    }

    /// Lowers every method body.
    pub(super) fn define_methods(&mut self, unit: &Unit) -> Result<(), CodegenError> {
        for item in &unit.program.items {
            let Item::Impl(i) = item else { continue };
            let name = qualify(unit.path, &i.ty.name);
            let owner = if self.enums.contains_key(&name) {
                Type::Enum(name.clone(), Vec::new())
            } else {
                Type::Struct(name.clone(), Vec::new())
            };
            for f in i.methods.iter().filter(|f| !generic(i, f)) {
                let method = self.methods[&key(&name, &f.name.name)].clone();
                let receiver = method.receiver.map(|_| owner.clone());
                self.define_with(f, &method.signature, receiver)?;
            }
        }
        Ok(())
    }

    /// A call whose callee is `T.f` or `x.m`, if it isn't a variant constructor.
    pub(super) fn method_call(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<Option<Value<'ctx>>, CodegenError> {
        let ExprKind::Field(base, name) = &callee.kind else {
            return Ok(None);
        };
        let base_type = self.ty(base);
        // `xs.map(f)`, `s.trim()`, `o.unwrap_or(d)`: helpers call the prelude
        // with the receiver first (v3 PRP-041, v4 PRP-043).
        let callee = self.callees.get(&call.id).cloned();
        let helper = callee.filter(|k| k.starts_with("$prelude."));
        if let Some(helper) = helper.and_then(|k| self.prelude_target(call, &k)) {
            let params = &helper.signature.params;
            let mut values = self.arguments(std::slice::from_ref(base), &params[..1], false)?;
            values.extend(self.arguments(args, &params[1..], false)?);
            let site = self
                .builder
                .build_call(helper.signature.function, &values, "helper");
            return Ok(Some(match site.expect(POSITIONED).try_as_basic_value() {
                ValueKind::Basic(result) => Value::Val(result),
                ValueKind::Instruction(_) => Value::Void,
            }));
        }
        if base_type == Type::String && name.name == "slice" {
            return self.string_slice(base, args, name.span.start).map(Some);
        }
        if let Type::Array(_) = base_type {
            return self.array_builtin(base, &name.name, args).map(Some);
        }
        // `s.area()` on an interface value: through its table (v5).
        if let Type::Interface(..) = &base_type {
            return self
                .dynamic_call(base, &base_type, &name.name, args)
                .map(Some);
        }
        // The checker gives a type name `void` (associated calls and variants).
        // An associated call's method key comes from the checker (PRP-034).
        let (method, receiver) = match &base_type {
            Type::Void => (self.callees.get(&call.id).cloned(), None),
            Type::Struct(owner, _) | Type::Enum(owner, _) => {
                (Some(key(owner, &name.name)), Some(base))
            }
            // Any other receiver through a bound: a default method (v5).
            _ if self.default_method(call, &base_type).is_some() => (None, Some(base)),
            _ => return Ok(None),
        };
        // Through a bound (milestone 12) the checker recorded no instance:
        // the receiver's own type arguments name it (`Stack.size<i64>`).
        let type_args = match &base_type {
            Type::Struct(_, a) | Type::Enum(_, a) if !a.is_empty() => Some(a.clone()),
            _ => None,
        };
        let found = method.and_then(|k| {
            let instance = self.instance_of(call, &k);
            let by_type =
                || type_args.and_then(|a| self.monos.get(&instance_name(&k, &a)).cloned());
            instance
                .or_else(by_type)
                .or_else(|| self.methods.get(&k).cloned())
        });
        let found = found.or_else(|| self.default_method(call, &base_type));
        let Some(method) = found else {
            return Ok(None); // a variant constructor, or a function of a module
        };
        let mut values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len() + 1);
        // `T.make(…)` through a bound names its type with `T`: no receiver.
        if let Some(base) = receiver.filter(|_| method.receiver.is_some()) {
            values.push(
                self.receiver(base, &base_type, method.receiver == Some(true))?
                    .into(),
            );
        }
        values.extend(self.arguments(args, &method.signature.params, false)?);
        let site = self
            .builder
            .build_call(method.signature.function, &values, "method")
            .expect(POSITIONED);
        Ok(Some(match site.try_as_basic_value() {
            ValueKind::Basic(result) => Value::Val(result),
            ValueKind::Instruction(_) => Value::Void,
        }))
    }

    /// The receiver pointer: the caller's place for `mut self` (evaluated
    /// once, before the arguments), else a pointer to a copy of the value.
    fn receiver(
        &mut self,
        base: &Expr,
        ty: &Type,
        mutable: bool,
    ) -> Result<inkwell::values::PointerValue<'ctx>, CodegenError> {
        if mutable {
            let (address, _, _) = self.place_address(base)?;
            return Ok(address);
        }
        let value = self.get(base, ty)?;
        let slot = self.entry_alloca(ty, "self");
        self.builder.build_store(slot, value).expect(POSITIONED);
        Ok(slot)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    const V: &str = "struct V { x: f64, y: f64 }\nimpl V {\n\
        fun new(x: f64, y: f64): V = V { x: x, y: y };\n\
        fun len_sq(self): f64 = self.x * self.x + self.y * self.y;\n\
        fun scale(mut self, k: f64) { self.x *= k; }\n}\n";

    #[test]
    fn methods_are_receiver_first_functions() {
        let ir = ir(&format!(
            "{V}fun main() {{ let v = V.new(1.0, 2.0); let n = v.len_sq(); }}"
        ));
        assert!(ir.contains("define double @lugha_fn_V.len_sq(ptr"), "{ir}");
        assert!(ir.contains("call %V @lugha_fn_V.new(double"), "{ir}");
    }

    #[test]
    fn mut_self_passes_the_callers_own_place() {
        let ir = ir(&format!(
            "{V}fun main() {{ let mut v = V.new(1.0, 2.0); v.scale(2.0); }}"
        ));
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        assert!(
            main.contains("call void @lugha_fn_V.scale(ptr %v, double"),
            "{ir}"
        );
    }

    #[test]
    fn element_receivers_are_bounds_checked_first() {
        let ir = ir(&format!(
            "{V}fun main() {{ let mut vs = [V.new(1.0, 1.0)]; vs[0].scale(2.0); }}"
        ));
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        // The call sits in the block the bounds check branches to on success.
        let ok = main.find("bounds.ok:").expect("bounds check");
        let call = main.find("call void @lugha_fn_V.scale").expect("call");
        assert!(ok < call && main.contains("icmp ult i64"), "{ir}");
    }
}
