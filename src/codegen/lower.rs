//! The module: every function, then the C `main` that calls `lugha_fn_main`.

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, ValueKind};

use super::CodegenError;
use super::control::Loop;
use super::function::Signature;
use super::scope::Scopes;
use super::value::Kind;
use crate::ast::{FunDecl, Item, Program, TypeKind};
use crate::span::Span;

/// Every builder call below happens after `position_at_end`, so a
/// `BuilderError` can only mean a compiler bug.
pub(super) const POSITIONED: &str = "builder is positioned in a block";

/// Lowering state for one module.
pub(super) struct Lowerer<'ctx> {
    pub(super) context: &'ctx Context,
    pub(super) module: Module<'ctx>,
    pub(super) builder: Builder<'ctx>,
    /// Every declared function, by Lugha name.
    pub(super) functions: HashMap<String, Signature<'ctx>>,
    /// The function being emitted, for appending basic blocks.
    pub(super) function: Option<FunctionValue<'ctx>>,
    /// Its return kind; `None` for a void function.
    pub(super) ret: Option<Kind>,
    /// Local variables in scope.
    pub(super) scopes: Scopes<'ctx>,
    /// Enclosing loops, innermost last, for `break` and `continue`.
    pub(super) loops: Vec<Loop<'ctx>>,
}

/// Lowers `program` into a verified module.
pub(super) fn lower<'ctx>(
    context: &'ctx Context,
    program: &Program,
) -> Result<Module<'ctx>, CodegenError> {
    let mut lowerer = Lowerer {
        context,
        module: context.create_module("lugha"),
        builder: context.create_builder(),
        functions: HashMap::new(),
        function: None,
        ret: None,
        scopes: Scopes::default(),
        loops: Vec::new(),
    };
    lowerer.declare_functions(program)?;
    let main = check_main(program)?;
    for item in &program.items {
        if let Item::Fun(f) = item {
            lowerer.define(f)?;
        }
    }
    let user_main = lowerer.functions[&main.name.name].function;
    lowerer.c_main(user_main);
    lowerer
        .module
        .verify()
        .map_err(|e| CodegenError::Verify(e.to_string()))?;
    Ok(lowerer.module)
}

pub(super) fn unsupported(what: &'static str, milestone: u8, span: Span) -> CodegenError {
    CodegenError::Unsupported {
        what,
        milestone,
        span,
    }
}

/// `main` must exist, take no parameters, and return `i32` or nothing (spec §6).
fn check_main(program: &Program) -> Result<&FunDecl, CodegenError> {
    let main = program
        .items
        .iter()
        .find_map(|item| match item {
            Item::Fun(f) if f.name.name == "main" => Some(f),
            _ => None,
        })
        .ok_or_else(|| unsupported("programs without a `main` function", 3, Span::new(0, 0)))?;
    if let Some(param) = main.params.first() {
        return Err(unsupported("parameters for main", 3, param.name.span));
    }
    match &main.ret {
        Some(ty) if ty.kind != TypeKind::I32 => {
            Err(unsupported("this return type for main", 3, ty.span))
        }
        _ => Ok(main),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// Emits the C entry point: calls `lugha_fn_main` and returns its result
    /// truncated to `i32`, or 0 for a void `main`.
    fn c_main(&mut self, user_main: FunctionValue<'ctx>) {
        let i32_type = self.context.i32_type();
        let main = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        self.builder
            .position_at_end(self.context.append_basic_block(main, "entry"));
        let call = self
            .builder
            .build_call(user_main, &[], "result")
            .expect(POSITIONED);
        let code = match call.try_as_basic_value() {
            ValueKind::Basic(value) => {
                // Integers are i64 until milestone 3; the exit status is the low bits (spec §11).
                let value = value.into_int_value();
                self.builder
                    .build_int_truncate(value, i32_type, "exit")
                    .expect(POSITIONED)
            }
            ValueKind::Instruction(_) => i32_type.const_zero(),
        };
        self.builder.build_return(Some(&code)).expect(POSITIONED);
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::{ir, unsupported};

    #[test]
    fn main_is_prefixed_and_wrapped_by_a_c_main() {
        let ir = ir("fun main(): i32 { 2 + 3 * 4 }");
        assert!(ir.contains("define i32 @main()"), "{ir}");
        assert!(ir.contains("call i64 @lugha_fn_main()"), "{ir}");
        assert!(ir.contains("define i64 @lugha_fn_main()"), "{ir}");
    }

    #[test]
    fn void_main_exits_with_zero() {
        let ir = ir("fun main() { }");
        assert!(ir.contains("define void @lugha_fn_main()"), "{ir}");
        assert!(ir.contains("ret i32 0"), "{ir}");
    }

    #[test]
    fn items_and_main_rules_wait_for_later_milestones() {
        let cases = [
            (
                "extern fun abs(x: i32): i32;\nfun main() {}",
                ("extern functions", 4, "extern fun abs(x: i32): i32;"),
            ),
            (
                "struct P { x: i64 }\nfun main() {}",
                ("structs", 5, "struct P { x: i64 }"),
            ),
            ("fun main(x: i64) {}", ("parameters for main", 3, "x")),
            (
                "fun main(): i64 { 1 }",
                ("this return type for main", 3, "i64"),
            ),
            (
                "fun helper() {}",
                ("programs without a `main` function", 3, ""),
            ),
        ];
        for (src, want) in cases {
            assert_eq!(unsupported(src), want, "{src}");
        }
    }

    #[test]
    fn i32_main_needs_an_integer_result() {
        assert_eq!(
            unsupported("fun main(): i32 { let x = 1; }"),
            ("checking missing returns", 3, "main")
        );
        assert_eq!(
            unsupported("fun main(): i32 { true }"),
            ("type checking", 3, "true")
        );
    }
}
