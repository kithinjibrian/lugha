//! Items and functions: `lugha_fn_main` and the C `main` that calls it.

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, ValueKind};

use super::CodegenError;
use crate::ast::{FunDecl, Item, Program, Stmt, StmtKind, TypeKind};
use crate::span::Span;

/// Every builder call below happens after `position_at_end`, so a
/// `BuilderError` can only mean a compiler bug.
pub(super) const POSITIONED: &str = "builder is positioned in a block";

/// Lowering state for one module.
pub(super) struct Lowerer<'ctx> {
    pub(super) context: &'ctx Context,
    pub(super) module: Module<'ctx>,
    pub(super) builder: Builder<'ctx>,
    /// The function being emitted, for appending basic blocks.
    pub(super) function: Option<FunctionValue<'ctx>>,
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
        function: None,
    };
    let main = find_main(program)?;
    let user_main = lowerer.main_function(main)?;
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

/// Returns the only item milestone 1 can compile, `main`, or the first item it can't.
fn find_main(program: &Program) -> Result<&FunDecl, CodegenError> {
    let mut main = None;
    for item in &program.items {
        match item {
            Item::Fun(f) if f.name.name == "main" && main.is_none() => main = Some(f),
            Item::Fun(f) if f.name.name == "main" => {
                return Err(unsupported("duplicate functions", 3, f.name.span));
            }
            Item::Fun(f) => return Err(unsupported("functions other than main", 2, f.name.span)),
            Item::Extern(e) => return Err(unsupported("extern functions", 4, e.span)),
            Item::Struct(s) => return Err(unsupported("structs", 5, s.span)),
        }
    }
    main.ok_or_else(|| unsupported("programs without a `main` function", 3, Span::new(0, 0)))
}

impl<'ctx> Lowerer<'ctx> {
    /// Emits `main` as `lugha_fn_main` (spec §8): returns its `i64` tail
    /// truncated to `i32`, or `void`.
    fn main_function(&mut self, main: &FunDecl) -> Result<FunctionValue<'ctx>, CodegenError> {
        if let Some(param) = main.params.first() {
            return Err(unsupported("parameters", 2, param.name.span));
        }
        let returns_i32 = match &main.ret {
            None => false,
            Some(ty) if ty.kind == TypeKind::I32 => true,
            Some(ty) => return Err(unsupported("this return type for main", 3, ty.span)),
        };
        if let Some(stmt) = main.body.stmts.first() {
            return Err(stmt_unsupported(stmt));
        }
        let i32_type = self.context.i32_type();
        let fn_type = if returns_i32 {
            i32_type.fn_type(&[], false)
        } else {
            self.context.void_type().fn_type(&[], false)
        };
        let function = self.module.add_function("lugha_fn_main", fn_type, None);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let tail = match &main.body.tail {
            Some(tail) => Some(self.expr(tail)?),
            None => None,
        };
        match (returns_i32, tail) {
            (true, Some(value)) => {
                let value = self
                    .builder
                    .build_int_truncate(value, i32_type, "exit")
                    .expect(POSITIONED);
                self.builder.build_return(Some(&value)).expect(POSITIONED);
            }
            (true, None) => {
                return Err(unsupported(
                    "`main` without a result value",
                    3,
                    main.body.span,
                ));
            }
            // A void main's tail is evaluated for its effects (traps) and discarded.
            (false, _) => {
                self.builder.build_return(None).expect(POSITIONED);
            }
        }
        Ok(function)
    }

    /// Emits the C entry point, which calls `lugha_fn_main` and returns its result or 0.
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
            ValueKind::Basic(value) => value.into_int_value(),
            ValueKind::Instruction(_) => i32_type.const_zero(),
        };
        self.builder.build_return(Some(&code)).expect(POSITIONED);
    }
}

fn stmt_unsupported(stmt: &Stmt) -> CodegenError {
    let what = match &stmt.kind {
        StmtKind::Let { .. } => "`let` statements",
        StmtKind::Assign { .. } => "assignments",
        StmtKind::Expr { .. } => "expression statements",
        StmtKind::While { .. } => "`while` loops",
        StmtKind::For { .. } => "`for` loops",
        StmtKind::Return(_) => "`return`",
        StmtKind::Break => "`break`",
        StmtKind::Continue => "`continue`",
    };
    unsupported(what, 2, stmt.span)
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::{ir, unsupported};

    #[test]
    fn main_is_prefixed_and_wrapped_by_a_c_main() {
        let ir = ir("fun main(): i32 { 2 + 3 * 4 }");
        assert!(ir.contains("define i32 @main()"), "{ir}");
        assert!(ir.contains("call i32 @lugha_fn_main()"), "{ir}");
        assert!(ir.contains("define i32 @lugha_fn_main()"), "{ir}");
    }

    #[test]
    fn void_main_exits_with_zero() {
        let ir = ir("fun main() { }");
        assert!(ir.contains("define void @lugha_fn_main()"), "{ir}");
        assert!(ir.contains("ret i32 0"), "{ir}");
    }

    #[test]
    fn items_beyond_milestone_1_are_unsupported() {
        let cases = [
            (
                "fun helper() {}\nfun main() {}",
                ("functions other than main", 2, "helper"),
            ),
            (
                "extern fun abs(x: i32): i32;\nfun main() {}",
                ("extern functions", 4, "extern fun abs(x: i32): i32;"),
            ),
            (
                "struct P { x: i64 }\nfun main() {}",
                ("structs", 5, "struct P { x: i64 }"),
            ),
            ("fun main(x: i64) {}", ("parameters", 2, "x")),
            (
                "fun main(): i64 { 1 }",
                ("this return type for main", 3, "i64"),
            ),
        ];
        for (src, want) in cases {
            assert_eq!(unsupported(src), want, "{src}");
        }
    }

    #[test]
    fn statements_are_unsupported_until_milestone_2() {
        assert_eq!(
            unsupported("fun main(): i32 { let x = 1; x }"),
            ("`let` statements", 2, "let x = 1;")
        );
        assert_eq!(
            unsupported("fun main() { 1; }"),
            ("expression statements", 2, "1;")
        );
        assert_eq!(
            unsupported("fun main() { while 1 { } }"),
            ("`while` loops", 2, "while 1 { }")
        );
    }
}
