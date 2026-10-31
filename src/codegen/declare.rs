//! Declaring functions (spec §6, §8): every signature before any body, so
//! calls may name functions defined later, in any module. Lugha functions
//! are `lugha_fn_<symbol>`; externs keep their C name and C ABI.

use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::FunctionValue;

use super::CodegenError;
use super::c_calls::{CKind, has_struct};
use super::function::Signature;
use super::lower::Lowerer;
use super::value::{is_aggregate, llvm_type};
use crate::ast::Item;
use crate::check::CParam;
use crate::check::{Type, Unit, qualify, symbol};

/// True for the modules compiled only as far as they are reached: the
/// prelude, and since milestone 60 (v15) every std module.
pub(super) fn is_lazy(unit: &Unit) -> bool {
    (unit.path.len() == 1 && unit.path[0] == crate::modules::PRELUDE_PATH)
        || unit.path.first().is_some_and(|p| p == "std")
}

/// How C passes a value of type `ty`: zero-extended for `bool`, `u8` and
/// `u16`, sign-extended for `i8` and `i16` (spec §8, v10), as a C compiler would.
pub(super) fn abi_attribute(context: &inkwell::context::Context, ty: &Type) -> Option<Attribute> {
    let name = match ty {
        Type::Bool | Type::U8 | Type::U16 => "zeroext",
        Type::I8 | Type::I16 => "signext",
        _ => return None,
    };
    let kind = Attribute::get_named_enum_kind_id(name);
    Some(context.create_enum_attribute(kind, 0))
}

impl<'ctx> Lowerer<'ctx> {
    /// Declares every function of `unit` as `lugha_fn_<symbol>` (spec §8;
    /// `lugha_fn_geometry/origin` in a module, PRP-034), in source order.
    pub(super) fn declare_functions(&mut self, unit: &Unit) -> Result<(), CodegenError> {
        for item in &unit.program.items {
            let (name, params, ret, is_extern, c_symbol) = match item {
                // Generic functions are declared per instance (`instances.rs`).
                Item::Fun(f) if !f.type_params.is_empty() => continue,
                // The prelude's and std's functions are compiled only when
                // reached (`monos`, PRP-043, v15).
                Item::Fun(_) if is_lazy(unit) => continue,
                // Tests are built only by `lughac test` (milestone 84).
                Item::Fun(f) if f.test.is_some() && self.tests.is_none() => continue,
                Item::Fun(f) => (&f.name.name, &f.params, f.ret.as_ref(), false, None),
                // Constants are globals (`consts.rs`, v17).
                Item::Const(_) => continue,
                Item::Extern(e) => (
                    &e.name.name,
                    &e.params,
                    e.ret.as_ref(),
                    true,
                    Some(e.c_symbol()),
                ),
                // Methods are declared by `declare_methods`; a handle is only a pointer.
                Item::Struct(_)
                | Item::Enum(_)
                | Item::Impl(_)
                | Item::Interface(_)
                | Item::Derive(_)
                | Item::ExternType(_) => continue,
            };
            let key = qualify(unit.path, name);
            // An extern's `data` and `release` aren't written by callers (v16):
            // its written parameters are the plan's values and callbacks.
            let plan = self.c_params.get(&key).cloned();
            let params: Vec<Type> = match &plan {
                // The plan has an entry per declared parameter, in order.
                Some(plan) => params
                    .iter()
                    .zip(plan)
                    .filter(|(_, c)| !matches!(c, CParam::Data | CParam::Release))
                    .map(|(p, _)| self.annotation_type(&p.ty))
                    .collect(),
                None => params.iter().map(|p| self.annotation_type(&p.ty)).collect(),
            };
            let ret = ret.map(|t| self.annotation_type(t));
            let c_params: Vec<(BasicMetadataTypeEnum, Option<Type>)> = match &plan {
                Some(plan) => self.c_param_types(plan, &params),
                None => params
                    .iter()
                    .map(|ty| (self.param_type(ty), Some(ty.clone())))
                    .collect(),
            };
            let param_types: Vec<BasicMetadataTypeEnum> =
                c_params.iter().map(|(t, _)| *t).collect();
            // An extern passing structs by value gets clang's lowering (v16, milestone 90).
            let kinds = self.c_kinds(plan.as_deref(), &params);
            let void = Type::Unit;
            let c_sig = (is_extern && has_struct(&kinds, ret.as_ref().unwrap_or(&void)))
                .then(|| self.c_sig(&kinds, ret.as_ref().unwrap_or(&void)));
            let fn_type = match &ret {
                _ if c_sig.is_some() => c_sig.as_ref().expect("checked just above").fn_type,
                // C returns `Option` of a handle as a nullable pointer (v16).
                Some(ty) if is_extern => self.c_result_type(ty).fn_type(&param_types, false),
                Some(ty) => llvm_type(self.context, ty).fn_type(&param_types, false),
                None => self.context.void_type().fn_type(&param_types, false),
            };
            // Externs keep their C name (spec §8), declared once however many
            // modules declare them (checked: E0306 makes plain ones agree).
            let symbol = match c_symbol {
                Some(c) => c.to_string(),
                None => format!("lugha_fn_{}", symbol(&key)),
            };
            let abi: Vec<Option<Type>> = c_params.into_iter().map(|(_, t)| t).collect();
            let (function, c_type) = match self.module.get_function(&symbol) {
                // A renamed extern may give one C function another type (v16):
                // its calls then go through their own declaration's type.
                Some(existing) if is_extern => {
                    let other = existing.get_type() != fn_type;
                    (
                        existing,
                        other.then_some((fn_type, abi.clone(), ret.clone())),
                    )
                }
                _ => {
                    let f = self
                        .module
                        .add_function(&symbol, fn_type, Some(Linkage::External));
                    match &c_sig {
                        Some(sig) => self.c_sig_attributes(sig, |at, a| f.add_attribute(at, a)),
                        None if is_extern => self.c_abi(f, &abi, ret.as_ref()),
                        None => {}
                    }
                    (f, None)
                }
            };
            if !is_extern {
                self.declare_owning(&key, function, &params, false);
            }
            let signature = Signature {
                function,
                params,
                ret,
                is_extern,
                // A struct-passing extern's calls go through `c_sig` alone.
                c_type: c_type.filter(|_| c_sig.is_none()),
                c_sig,
            };
            self.functions.insert(key, signature);
        }
        Ok(())
    }

    /// A struct parameter is a pointer to the caller's storage (spec §7, §9).
    pub(super) fn param_type(&self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        match ty {
            ty if is_aggregate(ty) => self.context.ptr_type(Default::default()).into(),
            _ => llvm_type(self.context, ty).into(),
        }
    }

    /// Marks C's extensions (`abi_attribute`) on an extern's parameters and
    /// result, so LLVM lowers calls the way a C compiler would. `None` for a parameter C gets as a plain pointer (v16: callbacks, `data`, `release`).
    fn c_abi(&self, function: FunctionValue<'ctx>, params: &[Option<Type>], ret: Option<&Type>) {
        for (index, ty) in params.iter().enumerate() {
            if let Some(attribute) = ty.as_ref().and_then(|ty| abi_attribute(self.context, ty)) {
                let index = u32::try_from(index).expect("parameter count fits in u32");
                function.add_attribute(AttributeLoc::Param(index), attribute);
            }
        }
        if let Some(attribute) = ret.and_then(|ty| abi_attribute(self.context, ty)) {
            function.add_attribute(AttributeLoc::Return, attribute);
        }
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// The C parameters of an extern with plan `plan` (or none) and written
    /// parameters `params`: values, and pointers for callbacks, `data` and `release`.
    pub(super) fn c_kinds(&self, plan: Option<&[CParam]>, params: &[Type]) -> Vec<CKind> {
        let Some(plan) = plan else {
            return params.iter().cloned().map(CKind::Value).collect();
        };
        let mut written = params.iter();
        plan.iter()
            .map(|c| match c {
                CParam::Value => CKind::Value(written.next().expect("checked arity").clone()),
                CParam::Callback { .. } | CParam::Pointer(_) | CParam::Elements { .. } => {
                    written.next();
                    CKind::Pointer
                }
                CParam::Data | CParam::Release => CKind::Pointer,
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn externs_keep_their_name_and_c_abi() {
        let src = "extern fun isspace(c: u8): bool;\nextern fun strlen(s: string): i64;\n\
                   fun main() { let b: u8 = 32; let w = isspace(b); let n = strlen(\"hi\"); }";
        let ir = ir(src);
        assert!(
            ir.contains("declare zeroext i1 @isspace(i8 zeroext)"),
            "{ir}"
        );
        assert!(
            ir.contains("call zeroext i1 @isspace(") || ir.contains("call i1 @isspace("),
            "{ir}"
        );
        // The string argument points at field 1 (the bytes), 8 bytes into the object;
        // for a constant literal LLVM folds the GEP into a constant expression.
        let call = &ir[ir
            .find("@strlen(ptr getelementptr")
            .or(ir.find("getelementptr"))
            .expect("a GEP for the string")..];
        assert!(
            call.contains("{ i64, [0 x i8] }") && call.contains("i32 1"),
            "{ir}"
        );
        assert!(!ir.contains("lugha_fn_strlen"), "{ir}");
    }
}
