//! Functions: signatures, bodies, `return` and calls (spec §6).
//!
//! Every signature is declared before any body is lowered, so calls may name
//! functions defined later in the file, including mutual recursion.

use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED, unsupported};
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
}

impl<'ctx> Lowerer<'ctx> {
    /// Declares every function as `lugha_fn_<name>` (spec §8), in source order.
    pub(super) fn declare_functions(&mut self, program: &Program) -> Result<(), CodegenError> {
        for item in &program.items {
            let f = match item {
                Item::Fun(f) => f,
                Item::Extern(e) => return Err(unsupported("extern functions", 4, e.span)),
                Item::Struct(s) => return Err(unsupported("structs", 5, s.span)),
            };
            let params: Vec<Type> = f.params.iter().map(|p| annotation_type(&p.ty)).collect();
            let ret = f.ret.as_ref().map(annotation_type);
            let param_types: Vec<BasicMetadataTypeEnum> = params
                .iter()
                .map(|&ty| llvm_type(self.context, ty).into())
                .collect();
            let fn_type = match ret {
                Some(ty) => llvm_type(self.context, ty).fn_type(&param_types, false),
                None => self.context.void_type().fn_type(&param_types, false),
            };
            let function =
                self.module
                    .add_function(&format!("lugha_fn_{}", f.name.name), fn_type, None);
            self.functions.insert(
                f.name.name.clone(),
                Signature {
                    function,
                    params,
                    ret,
                },
            );
        }
        Ok(())
    }

    /// Lowers one function body. Parameters become locals (spec §6).
    pub(super) fn define(&mut self, f: &FunDecl) -> Result<(), CodegenError> {
        let signature = self.functions[&f.name.name].clone();
        self.function = Some(signature.function);
        self.ret = signature.ret;
        self.scopes = Scopes::default();
        self.loops.clear();
        self.builder
            .position_at_end(self.context.append_basic_block(signature.function, "entry"));

        self.scopes.push();
        for (index, (param, &ty)) in f.params.iter().zip(&signature.params).enumerate() {
            let ptr = self.entry_alloca(ty, &param.name.name);
            let index = u32::try_from(index).expect("parameter count fits in u32");
            let arg = signature
                .function
                .get_nth_param(index)
                .expect("declared with these parameters");
            self.builder.build_store(ptr, arg).expect(POSITIONED);
            self.scopes.declare(&param.name.name, Local { ptr, ty });
        }
        let value = self.block(&f.body)?;
        self.scopes.pop();

        match (signature.ret, value) {
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
        match (self.ret, value) {
            (Some(ty), Some(expr)) => {
                let result = self.get(expr, ty)?;
                self.builder.build_return(Some(&result)).expect(POSITIONED);
            }
            _ => {
                self.builder.build_return(None).expect(POSITIONED);
            }
        }
        self.start_dead_block("after.return");
        Ok(())
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
        for (arg, &ty) in args.iter().zip(&signature.params) {
            values.push(self.get(arg, ty)?.into());
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
}
