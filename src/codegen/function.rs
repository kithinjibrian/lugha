//! Functions: signatures, bodies, `return` and calls (spec §6).
//!
//! Every signature is declared before any body is lowered, so calls may name
//! functions defined later in the file, including mutual recursion.

use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED, unsupported};
use super::scope::{Local, Scopes};
use super::value::{Kind, Value, annotation_kind, type_error};
use crate::ast::{Expr, ExprKind, FunDecl, Item, Program};
use crate::span::Span;

/// Built into the compiler from milestone 4 (spec §5).
const INTRINSICS: [&str; 4] = ["print", "println", "panic", "to_string"];

/// A declared function.
#[derive(Debug, Clone)]
pub(super) struct Signature<'ctx> {
    pub function: FunctionValue<'ctx>,
    pub params: Vec<Kind>,
    /// `None` for a void function.
    pub ret: Option<Kind>,
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
            if self.functions.contains_key(&f.name.name) {
                return Err(unsupported("checking duplicate names", 3, f.name.span));
            }
            let params = f
                .params
                .iter()
                .map(|p| annotation_kind(&p.ty))
                .collect::<Result<Vec<_>, _>>()?;
            let ret = f.ret.as_ref().map(annotation_kind).transpose()?;
            let param_types: Vec<BasicMetadataTypeEnum> = params
                .iter()
                .map(|kind| kind.llvm(self.context).into())
                .collect();
            let fn_type = match ret {
                Some(kind) => kind.llvm(self.context).fn_type(&param_types, false),
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

    /// Lowers one function body. Parameters become immutable locals (spec §6).
    pub(super) fn define(&mut self, f: &FunDecl) -> Result<(), CodegenError> {
        let signature = self.functions[&f.name.name].clone();
        self.function = Some(signature.function);
        self.ret = signature.ret;
        self.scopes = Scopes::default();
        self.loops.clear();
        self.builder
            .position_at_end(self.context.append_basic_block(signature.function, "entry"));

        self.scopes.push();
        for (index, (param, &kind)) in f.params.iter().zip(&signature.params).enumerate() {
            let ptr = self.entry_alloca(kind, &param.name.name);
            let index = u32::try_from(index).expect("parameter count fits in u32");
            let arg = signature
                .function
                .get_nth_param(index)
                .expect("declared with these parameters");
            self.builder
                .build_store(ptr, arg.into_int_value())
                .expect(POSITIONED);
            self.scopes.declare(&param.name.name, Local { ptr, kind });
        }
        let value = self.block(&f.body)?;
        self.scopes.pop();

        match (signature.ret, value) {
            // §6 guarantees control never reaches the end of a body that diverges.
            (_, Value::Never { .. }) => {
                self.builder.build_unreachable().expect(POSITIONED);
            }
            (None, _) => {
                self.builder.build_return(None).expect(POSITIONED);
            }
            (Some(Kind::Int), Value::Int(result)) | (Some(Kind::Bool), Value::Bool(result)) => {
                self.builder.build_return(Some(&result)).expect(POSITIONED);
            }
            (Some(_), Value::Void) => {
                return Err(unsupported("checking missing returns", 3, f.name.span));
            }
            (Some(_), _) => {
                let span = f.body.tail.as_ref().map_or(f.body.span, |tail| tail.span);
                return Err(type_error(span));
            }
        }
        Ok(())
    }

    /// `return [value];` — leaves the builder in a fresh dead block.
    pub(super) fn return_stmt(
        &mut self,
        value: Option<&Expr>,
        span: Span,
    ) -> Result<(), CodegenError> {
        match (self.ret, value) {
            (Some(kind), Some(expr)) => {
                let result = match (kind, self.expr(expr)?) {
                    (Kind::Int, value) => value.int(expr.span)?,
                    (Kind::Bool, value) => value.bool(expr.span)?,
                };
                self.builder.build_return(Some(&result)).expect(POSITIONED);
            }
            (None, None) => {
                self.builder.build_return(None).expect(POSITIONED);
            }
            _ => return Err(type_error(span)),
        }
        let dead = self.append("after.return");
        self.builder.position_at_end(dead);
        Ok(())
    }

    /// `callee(args)`: locals shadow functions; arguments run left to right (spec §5, §6).
    pub(super) fn call(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let ExprKind::Name(name) = &callee.kind else {
            return Err(unsupported("checking calls", 3, callee.span));
        };
        if self.scopes.lookup(name).is_some() {
            return Err(unsupported("checking calls", 3, callee.span));
        }
        let Some(signature) = self.functions.get(name).cloned() else {
            return Err(if INTRINSICS.contains(&name.as_str()) {
                unsupported("intrinsics", 4, callee.span)
            } else {
                unsupported("checking undefined names", 3, callee.span)
            });
        };
        if args.len() != signature.params.len() {
            return Err(unsupported("checking calls", 3, call.span));
        }
        let mut values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
        for (arg, kind) in args.iter().zip(&signature.params) {
            let value = self.expr(arg)?;
            let value = match kind {
                Kind::Int => value.int(arg.span)?,
                Kind::Bool => value.bool(arg.span)?,
            };
            values.push(value.into());
        }
        let site = self
            .builder
            .build_call(signature.function, &values, "call")
            .expect(POSITIONED);
        Ok(match (signature.ret, site.try_as_basic_value()) {
            (Some(kind), ValueKind::Basic(result)) => Value::of(kind, result.into_int_value()),
            _ => Value::Void,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::{ir, unsupported};

    #[test]
    fn call_mistakes_wait_for_the_checker() {
        let cases = [
            (
                "fun f() {}\nfun f() {}\nfun main() {}",
                ("checking duplicate names", 3, "f"),
            ),
            ("fun main() { g(); }", ("checking undefined names", 3, "g")),
            (
                "fun f(a: i64) {}\nfun main() { f(); }",
                ("checking calls", 3, "f()"),
            ),
            (
                "fun f(a: i64) {}\nfun main() { f(true); }",
                ("type checking", 3, "true"),
            ),
            ("fun main() { let f = 1; f(); }", ("checking calls", 3, "f")),
            (
                "fun f() {}\nfun main() { let g = f; }",
                ("type checking", 3, "f"),
            ),
            ("fun main() { println(1); }", ("intrinsics", 4, "println")),
        ];
        for (src, want) in cases {
            assert_eq!(unsupported(src), want, "{src}");
        }
    }

    #[test]
    fn return_must_match_the_function() {
        assert_eq!(
            unsupported("fun f() { return 1; }\nfun main() {}"),
            ("type checking", 3, "return 1;")
        );
        assert_eq!(
            unsupported("fun f(): i64 { return; }\nfun main() {}"),
            ("type checking", 3, "return;")
        );
        assert_eq!(
            unsupported("fun f(c: bool): i64 { if c { return 1; } }\nfun main() {}"),
            ("checking missing returns", 3, "f")
        );
        // Spec §6: loops never definitely return.
        assert_eq!(
            unsupported("fun g(): i64 { while true { return 1; } }\nfun main() {}"),
            ("checking missing returns", 3, "g")
        );
    }

    #[test]
    fn a_returning_branch_adds_no_phi_edge() {
        let ir = ir("fun abs(x: i64): i64 { if x < 0 { return -x; } else { x } }\nfun main() {}");
        let body = &ir[ir.find("define i64 @lugha_fn_abs").unwrap()..];
        let body = &body[..body.find("\n}").unwrap()];
        assert!(!body.contains("phi"), "{body}");
        assert!(body.contains("unreachable"), "{body}");
    }

    #[test]
    fn functions_are_prefixed_and_diverging_bodies_end_unreachable() {
        let ir = ir("fun f(): i64 { return 1; }\nfun main(): i32 { f() }");
        assert!(ir.contains("call i64 @lugha_fn_f()"), "{ir}");
        assert!(ir.contains("define i64 @lugha_fn_main()"), "{ir}");
        let f = &ir[ir.find("define i64 @lugha_fn_f").unwrap()..];
        assert!(f[..f.find("\n}").unwrap()].contains("unreachable"), "{f}");
    }
}
