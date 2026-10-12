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
use super::lower::{Lowerer, POSITIONED};
use super::value::{Value, llvm_type};
use crate::ast::{Expr, ExprKind, Item, Program};
use crate::check::Type;

/// A declared method or associated function.
#[derive(Debug, Clone)]
pub(super) struct Method<'ctx> {
    /// The function; `params` exclude the receiver.
    pub signature: Signature<'ctx>,
    /// `None` for an associated function; `Some(mutable)` for `self` / `mut self`.
    pub receiver: Option<bool>,
}

/// The key of a method: `Type.name`, which is also its symbol after `lugha_fn_`.
pub(super) fn key(owner: &str, name: &str) -> String {
    format!("{owner}.{name}")
}

impl<'ctx> Lowerer<'ctx> {
    /// Declares every method as `lugha_fn_<Type>.<name>`, receiver first.
    pub(super) fn declare_methods(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Impl(i) = item else { continue };
            for f in &i.methods {
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
                let key = key(&i.ty.name, &f.name.name);
                let function = self.module.add_function(
                    &format!("lugha_fn_{key}"),
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
    pub(super) fn define_methods(&mut self, program: &Program) -> Result<(), CodegenError> {
        for item in &program.items {
            let Item::Impl(i) = item else { continue };
            let owner = if self.enums.contains_key(&i.ty.name) {
                Type::Enum(i.ty.name.clone())
            } else {
                Type::Struct(i.ty.name.clone())
            };
            for f in &i.methods {
                let method = self.methods[&key(&i.ty.name, &f.name.name)].clone();
                let receiver = method.receiver.map(|_| owner.clone());
                self.define_with(f, &method.signature, receiver)?;
            }
        }
        Ok(())
    }

    /// A call whose callee is `T.f` or `x.m`, if it isn't a variant constructor.
    pub(super) fn method_call(
        &mut self,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<Option<Value<'ctx>>, CodegenError> {
        let ExprKind::Field(base, name) = &callee.kind else {
            return Ok(None);
        };
        let base_type = self.ty(base);
        // The checker gives a type name `void` (associated calls and variants).
        let (owner, receiver) = match (&base_type, &base.kind) {
            (Type::Void, ExprKind::Name(owner)) => (owner.clone(), None),
            (Type::Struct(owner) | Type::Enum(owner), _) => (owner.clone(), Some(base)),
            _ => return Ok(None),
        };
        let Some(method) = self.methods.get(&key(&owner, &name.name)).cloned() else {
            return Ok(None); // a variant constructor
        };
        let mut values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len() + 1);
        if let Some(base) = receiver {
            values.push(
                self.receiver(base, &base_type, method.receiver == Some(true))?
                    .into(),
            );
        }
        values.extend(self.arguments(args, &method.signature)?);
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
