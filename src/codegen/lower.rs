//! The module: every function, then the C `main` that calls `lugha_fn_main`.

use std::collections::{HashMap, HashSet};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, ValueKind};

use super::control::Loop;
use super::function::Signature;
use super::runtime::Constants;
use super::scope::Scopes;
use super::{CodegenError, ModuleSource, SourceInfo};
use crate::ast::{ExprId, Item};
use crate::check::{Checked, Enums, Structs, Type, Unit};
use crate::span::Span;

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
    /// Every enum's variants and payload types in declaration order.
    pub(super) enums: Enums,
    /// Values read from a place for the last time, stored without a copy.
    pub(super) moves: HashSet<ExprId>,
    /// Every declared function, by key (`geometry.origin`; PRP-034).
    pub(super) functions: HashMap<String, Signature<'ctx>>,
    /// The checker's key for what each call calls (PRP-034).
    pub(super) callees: HashMap<ExprId, String>,
    /// The checker's type for every written type, by span.
    pub(super) annotations: HashMap<Span, Type>,
    /// Every method and associated function, by `Type.name` (v1).
    pub(super) methods: HashMap<String, super::methods::Method<'ctx>>,
    /// Every concrete type the program uses (`Checked::used`).
    pub(super) used: Vec<Type>,
    /// The type arguments of each generic use (`Checked::instances`).
    pub(super) instances: HashMap<ExprId, Vec<Type>>,
    /// Every generic function and method instance, by `key<args>` (PRP-036).
    pub(super) monos: HashMap<String, super::methods::Method<'ctx>>,
    /// The type parameters of the instance being lowered, and their types.
    pub(super) subst: HashMap<String, Type>,
    /// What each closure literal captures (`Checked::captures`).
    pub(super) captures: HashMap<ExprId, Vec<String>>,
    /// Closures emitted so far per enclosing symbol, for `lugha_closure_<f>.<n>`.
    pub(super) closure_counts: HashMap<String, usize>,
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
    /// Every module's source, for panic locations.
    pub(super) source: OwnedSource,
}

/// One module's source, owned so the lowerer has no extra lifetime.
pub(super) struct OwnedFile {
    pub name: String,
    base: usize,
    text: String,
}

/// Every module's source, in offset order.
pub(super) struct OwnedSource {
    pub files: Vec<OwnedFile>,
}

impl OwnedSource {
    /// The file holding `offset`, and `offset` within it.
    pub(super) fn file_at(&self, offset: usize) -> (&OwnedFile, usize) {
        let file = self
            .files
            .iter()
            .rev()
            .find(|f| f.base <= offset)
            .unwrap_or(&self.files[0]);
        (file, offset - file.base.min(offset))
    }

    pub(super) fn line_col(&self, offset: usize) -> (usize, usize) {
        let (file, local) = self.file_at(offset);
        SourceInfo {
            name: &file.name,
            text: &file.text,
        }
        .line_col(local)
    }
}

/// Lowers a checked program of modules into one verified LLVM module.
pub(super) fn lower<'ctx>(
    context: &'ctx Context,
    units: &[Unit],
    checked: &Checked,
    sources: &[ModuleSource],
) -> Result<Module<'ctx>, CodegenError> {
    let mut lowerer = Lowerer {
        context,
        module: context.create_module("lugha"),
        builder: context.create_builder(),
        types: checked.types.clone(),
        structs: checked.structs.clone(),
        enums: checked.enums.clone(),
        moves: checked.moves.clone(),
        functions: HashMap::new(),
        callees: checked.callees.clone(),
        annotations: checked.annotations.clone(),
        methods: HashMap::new(),
        used: checked.used.clone(),
        instances: checked.instances.clone(),
        monos: HashMap::new(),
        subst: HashMap::new(),
        captures: checked.captures.clone(),
        closure_counts: HashMap::new(),
        function: None,
        ret: None,
        scopes: Scopes::default(),
        loops: Vec::new(),
        constants: Constants::default(),
        source: OwnedSource {
            files: sources
                .iter()
                .map(|s| OwnedFile {
                    name: s.name.to_string(),
                    base: s.base,
                    text: s.text.to_string(),
                })
                .collect(),
        },
    };
    lowerer.declare_enums();
    lowerer.declare_structs();
    for unit in units {
        lowerer.declare_functions(unit)?;
    }
    for unit in units {
        lowerer.declare_methods(unit);
    }
    lowerer.declare_instances(units, &checked.monos);
    for unit in units {
        for item in &unit.program.items {
            if let Item::Fun(f) = item
                && f.type_params.is_empty()
                && !super::declare::is_prelude(unit)
            {
                lowerer.define(unit.path, f)?;
            }
        }
    }
    for unit in units {
        lowerer.define_methods(unit)?;
    }
    lowerer.define_instances(units, &checked.monos)?;
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
        // `main(int argc, char **argv)`: the runtime keeps both for `args()` (v3).
        let ptr = self.context.ptr_type(Default::default());
        let main_type = i32_type.fn_type(&[i32_type.into(), ptr.into()], false);
        let main = self.module.add_function("main", main_type, None);
        self.builder
            .position_at_end(self.context.append_basic_block(main, "entry"));
        let argc = main.get_nth_param(0).expect("main takes argc");
        let argv = main.get_nth_param(1).expect("main takes argv");
        let init = self.runtime("lugha_rt_init", &[Type::I32, Type::String], None);
        let init_args = [argc.into(), argv.into()];
        self.builder
            .build_call(init, &init_args, "")
            .expect(POSITIONED);
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
        assert!(ir.contains("define i32 @main(i32 %0, ptr %1)"), "{ir}");
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
