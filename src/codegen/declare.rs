//! Declaring functions (spec §6, §8): every signature before any body, so
//! calls may name functions defined later, in any module. Lugha functions
//! are `lugha_fn_<symbol>`; externs keep their C name and C ABI.

use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::FunctionValue;

use super::CodegenError;
use super::function::Signature;
use super::lower::Lowerer;
use super::value::{is_aggregate, llvm_type};
use crate::ast::Item;
use crate::check::{Type, Unit, qualify, symbol};

impl<'ctx> Lowerer<'ctx> {
    /// Declares every function of `unit` as `lugha_fn_<symbol>` (spec §8;
    /// `lugha_fn_geometry/origin` in a module, PRP-034), in source order.
    pub(super) fn declare_functions(&mut self, unit: &Unit) -> Result<(), CodegenError> {
        for item in &unit.program.items {
            let (name, params, ret, is_extern) = match item {
                // Generic functions are declared per instance (`instances.rs`).
                Item::Fun(f) if !f.type_params.is_empty() => continue,
                Item::Fun(f) => (&f.name.name, &f.params, f.ret.as_ref(), false),
                Item::Extern(e) => (&e.name.name, &e.params, e.ret.as_ref(), true),
                // Methods are declared by `declare_methods`.
                Item::Struct(_) | Item::Enum(_) | Item::Impl(_) | Item::Interface(_) => continue,
            };
            let params: Vec<Type> = params.iter().map(|p| self.annotation_type(&p.ty)).collect();
            let ret = ret.map(|t| self.annotation_type(t));
            let param_types: Vec<BasicMetadataTypeEnum> =
                params.iter().map(|ty| self.param_type(ty)).collect();
            let fn_type = match &ret {
                Some(ty) => llvm_type(self.context, ty).fn_type(&param_types, false),
                None => self.context.void_type().fn_type(&param_types, false),
            };
            let key = qualify(unit.path, name);
            // Externs keep their C name (spec §8), declared once however many
            // modules declare them (checked: E0306 makes them agree).
            let symbol = if is_extern {
                name.clone()
            } else {
                format!("lugha_fn_{}", symbol(&key))
            };
            let function = match self.module.get_function(&symbol) {
                Some(existing) if is_extern => existing,
                _ => self
                    .module
                    .add_function(&symbol, fn_type, Some(Linkage::External)),
            };
            if is_extern {
                self.c_abi(function, &params, ret.as_ref());
            }
            let signature = Signature {
                function,
                params,
                ret,
                is_extern,
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

    /// C passes `bool` and `u8` zero-extended (spec §8): mark them so LLVM
    /// lowers the call the way a C compiler would.
    fn c_abi(&self, function: FunctionValue<'ctx>, params: &[Type], ret: Option<&Type>) {
        let zeroext = self
            .context
            .create_enum_attribute(Attribute::get_named_enum_kind_id("zeroext"), 0);
        for (index, ty) in params.iter().enumerate() {
            if matches!(ty, Type::Bool | Type::U8) {
                let index = u32::try_from(index).expect("parameter count fits in u32");
                function.add_attribute(AttributeLoc::Param(index), zeroext);
            }
        }
        if matches!(ret, Some(Type::Bool | Type::U8)) {
            function.add_attribute(AttributeLoc::Return, zeroext);
        }
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
