//! Functions: signatures, bodies, `return` and calls (spec §6).
//!
//! Every signature is declared before any body is lowered, so calls may name
//! functions defined later in the file, including mutual recursion.

use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::{Local, Scopes};
use super::value::{Value, annotation_type, llvm_type};
use crate::ast::{Expr, ExprKind, FunDecl, Item, Program};
use crate::check::Type;

/// A declared function.
#[derive(Debug, Clone)]
pub(super) struct Signature<'ctx> {
    pub function: FunctionValue<'ctx>,
    pub params: Vec<Type>,
    /// `None` for a void function.
    pub ret: Option<Type>,
    /// A C function declared with `extern fun`.
    pub is_extern: bool,
}

impl<'ctx> Lowerer<'ctx> {
    /// Declares every function as `lugha_fn_<name>` (spec §8), in source order.
    pub(super) fn declare_functions(&mut self, program: &Program) -> Result<(), CodegenError> {
        for item in &program.items {
            let (name, params, ret, is_extern) = match item {
                Item::Fun(f) => (&f.name.name, &f.params, f.ret.as_ref(), false),
                Item::Extern(e) => (&e.name.name, &e.params, e.ret.as_ref(), true),
                Item::Struct(_) => continue,
            };
            let params: Vec<Type> = params.iter().map(|p| annotation_type(&p.ty)).collect();
            let ret = ret.map(annotation_type);
            let param_types: Vec<BasicMetadataTypeEnum> =
                params.iter().map(|ty| self.param_type(ty)).collect();
            let fn_type = match &ret {
                Some(ty) => llvm_type(self.context, ty).fn_type(&param_types, false),
                None => self.context.void_type().fn_type(&param_types, false),
            };
            // Externs keep their C name (spec §8); Lugha functions get the prefix.
            let symbol = if is_extern {
                name.clone()
            } else {
                format!("lugha_fn_{name}")
            };
            let function = self
                .module
                .add_function(&symbol, fn_type, Some(Linkage::External));
            if is_extern {
                self.c_abi(function, &params, ret.as_ref());
            }
            let signature = Signature {
                function,
                params,
                ret,
                is_extern,
            };
            self.functions.insert(name.clone(), signature);
        }
        Ok(())
    }

    /// A struct parameter is a pointer to the caller's storage (spec §7, §9).
    fn param_type(&self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        match ty {
            Type::Struct(_) => self.context.ptr_type(Default::default()).into(),
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

    /// Lowers one function body. Parameters become locals (spec §6).
    pub(super) fn define(&mut self, f: &FunDecl) -> Result<(), CodegenError> {
        let signature = self.functions[&f.name.name].clone();
        self.function = Some(signature.function);
        self.ret = signature.ret.clone();
        self.scopes = Scopes::default();
        self.loops.clear();
        self.builder
            .position_at_end(self.context.append_basic_block(signature.function, "entry"));

        self.scopes.push();
        for (index, (param, ty)) in f.params.iter().zip(&signature.params).enumerate() {
            let index = u32::try_from(index).expect("parameter count fits in u32");
            let arg = signature
                .function
                .get_nth_param(index)
                .expect("declared with these parameters");
            // A struct arrives as a pointer to the caller's storage, which
            // serves as its slot: parameters are immutable (spec §7).
            let ptr = if matches!(ty, Type::Struct(_)) {
                arg.into_pointer_value()
            } else {
                let ptr = self.entry_alloca(ty, &param.name.name);
                self.builder.build_store(ptr, arg).expect(POSITIONED);
                ptr
            };
            self.scopes.declare(
                &param.name.name,
                Local {
                    ptr,
                    ty: ty.clone(),
                    borrowed: true,
                },
            );
        }
        let value = self.block_inner(&f.body, true)?;
        self.scopes.pop();

        match (&signature.ret, value) {
            // §6 guarantees control never reaches the end of a body that diverges.
            (_, Value::Never) => {
                self.builder.build_unreachable().expect(POSITIONED);
            }
            (None, _) => {
                self.builder.build_return(None).expect(POSITIONED);
            }
            (Some(_), Value::Val(result)) => {
                self.builder.build_return(Some(&result)).expect(POSITIONED);
            }
            (Some(_), Value::Void) => unreachable!("checked: E0503 rejects missing returns"),
        }
        Ok(())
    }

    /// `return [value];` — leaves the builder in a fresh dead block.
    pub(super) fn return_stmt(&mut self, value: Option<&Expr>) -> Result<(), CodegenError> {
        match (self.ret.clone(), value) {
            (Some(ty), Some(expr)) => {
                let result = self.value_for_return(expr, &ty)?;
                self.builder.build_return(Some(&result)).expect(POSITIONED);
            }
            _ => {
                self.builder.build_return(None).expect(POSITIONED);
            }
        }
        self.start_dead_block("after.return");
        Ok(())
    }

    /// The pointer C receives for a Lugha string: its first data byte, 8 bytes
    /// past the length header, NUL-terminated (spec §7, §8).
    fn c_string(&self, string: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        // The header layout `{ i64 len, [0 x i8] bytes }`; field 1 is the data, 8 bytes in.
        let i64_type = self.context.i64_type().into();
        let bytes = self.context.i8_type().array_type(0).into();
        let header = self.context.struct_type(&[i64_type, bytes], false);
        let data = self
            .builder
            .build_struct_gep(header, string.into_pointer_value(), 1, "cstr");
        data.expect("field 1 of a two-field struct exists").into()
    }

    /// A call to a declared function; arguments run left to right (spec §5).
    pub(super) fn call(&mut self, call: &Expr, args: &[Expr]) -> Result<Value<'ctx>, CodegenError> {
        let ExprKind::Call(callee, _) = &call.kind else {
            unreachable!("called with a call expression")
        };
        let ExprKind::Name(name) = &callee.kind else {
            unreachable!("checked: E0406 rejects non-name callees")
        };
        let Some(signature) = self.functions.get(name).cloned() else {
            // Not a declared function, so one of the intrinsics (checked: E0301 otherwise).
            return self.intrinsic(call, name, args);
        };
        let mut values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
        for (arg, ty) in args.iter().zip(&signature.params) {
            let value = self.get(arg, ty)?;
            let value = if signature.is_extern && *ty == Type::String {
                self.c_string(value)
            } else if matches!(ty, Type::Struct(_)) {
                // Passed as a pointer to caller storage, never copied (spec §4, §9).
                let slot = self.entry_alloca(ty, "arg");
                self.builder.build_store(slot, value).expect(POSITIONED);
                slot.into()
            } else {
                value
            };
            values.push(value.into());
        }
        let site = self
            .builder
            .build_call(signature.function, &values, "call")
            .expect(POSITIONED);
        Ok(match site.try_as_basic_value() {
            ValueKind::Basic(result) => Value::Val(result),
            ValueKind::Instruction(_) => Value::Void,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn functions_are_prefixed_and_typed_by_their_signature() {
        let ir = ir("fun half(x: u8): f64 = x as f64 / 2.0;\nfun main(): i32 { half(9) as i32 }");
        assert!(ir.contains("define double @lugha_fn_half(i8"), "{ir}");
        assert!(ir.contains("call double @lugha_fn_half(i8"), "{ir}");
    }

    #[test]
    fn diverging_bodies_end_unreachable() {
        let ir = ir("fun f(): i64 { return 1; }\nfun main() {}");
        let f = &ir[ir.find("define i64 @lugha_fn_f").unwrap()..];
        assert!(f[..f.find("\n}").unwrap()].contains("unreachable"), "{f}");
    }

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
