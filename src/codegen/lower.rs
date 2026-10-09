//! The module: every function, then the C `main` that calls `lugha_fn_main`.

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, ValueKind};

use super::control::Loop;
use super::function::Signature;
use super::runtime::Constants;
use super::scope::Scopes;
use super::{CodegenError, SourceInfo};
use crate::ast::{Item, Program};
use crate::check::{Checked, Structs, Type};

/// Every builder call below happens after `position_at_end`, so a
/// `BuilderError` can only mean a compiler bug.
pub(super) const POSITIONED: &str = "builder is positioned in a block";

/// Lowering state for one module.
pub(super) struct Lowerer<'ctx> {
    pub(super) context: &'ctx Context,
    pub(super) module: Module<'ctx>,
    pub(super) builder: Builder<'ctx>,
    /// The checker's type for every expression, by `ExprId`.
    pub(super) types: Vec<Type>,
    /// Every struct's fields in declaration order.
    pub(super) structs: Structs,
    /// Every declared function, by Lugha name.
    pub(super) functions: HashMap<String, Signature<'ctx>>,
    /// The function being emitted, for appending basic blocks.
    pub(super) function: Option<FunctionValue<'ctx>>,
    /// Its return type; `None` for a void function.
    pub(super) ret: Option<Type>,
    /// Local variables in scope.
    pub(super) scopes: Scopes<'ctx>,
    /// Enclosing loops, innermost last, for `break` and `continue`.
    pub(super) loops: Vec<Loop<'ctx>>,
    /// String literals and the file name, emitted once each.
    pub(super) constants: Constants<'ctx>,
    /// The source file, for panic locations.
    pub(super) source: OwnedSource,
}

/// The source name and text, owned so the lowerer has no extra lifetime.
pub(super) struct OwnedSource {
    pub name: String,
    text: String,
}

impl OwnedSource {
    pub(super) fn line_col(&self, offset: usize) -> (usize, usize) {
        SourceInfo {
            name: &self.name,
            text: &self.text,
        }
        .line_col(offset)
    }
}

/// Lowers a checked `program` into a verified module.
pub(super) fn lower<'ctx>(
    context: &'ctx Context,
    program: &Program,
    checked: &Checked,
    source: &SourceInfo,
) -> Result<Module<'ctx>, CodegenError> {
    let mut lowerer = Lowerer {
        context,
        module: context.create_module("lugha"),
        builder: context.create_builder(),
        types: checked.types.clone(),
        structs: checked.structs.clone(),
        functions: HashMap::new(),
        function: None,
        ret: None,
        scopes: Scopes::default(),
        loops: Vec::new(),
        constants: Constants::default(),
        source: OwnedSource {
            name: source.name.to_string(),
            text: source.text.to_string(),
        },
    };
    lowerer.declare_structs();
    lowerer.declare_functions(program)?;
    for item in &program.items {
        if let Item::Fun(f) = item {
            lowerer.define(f)?;
        }
    }
    let user_main = lowerer
        .functions
        .get("main")
        .expect("checked: E0303 requires `main`")
        .function;
    lowerer.c_main(user_main);
    lowerer
        .module
        .verify()
        .map_err(|e| CodegenError::Verify(e.to_string()))?;
    Ok(lowerer.module)
}

impl<'ctx> Lowerer<'ctx> {
    /// Emits the C entry point: returns `lugha_fn_main`'s `i32`, or 0 for a
    /// void `main` (spec §6).
    fn c_main(&mut self, user_main: FunctionValue<'ctx>) {
        let i32_type = self.context.i32_type();
        let main = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        self.builder
            .position_at_end(self.context.append_basic_block(main, "entry"));
        self.call_runtime_init();
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

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn main_returns_a_real_i32() {
        let ir = ir("fun main(): i32 { 2 + 3 * 4 }");
        assert!(ir.contains("define i32 @lugha_fn_main()"), "{ir}");
        assert!(ir.contains("define i32 @main()"), "{ir}");
        assert!(ir.contains("call i32 @lugha_fn_main()"), "{ir}");
        assert!(!ir.contains("trunc"), "no truncation in the C main: {ir}");
    }

    #[test]
    fn void_main_exits_with_zero() {
        let ir = ir("fun main() { }");
        assert!(ir.contains("define void @lugha_fn_main()"), "{ir}");
        assert!(ir.contains("ret i32 0"), "{ir}");
    }
}
