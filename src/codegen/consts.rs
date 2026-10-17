//! Global constants (v17, milestone 109): one global `lugha_const_<key>`
//! each, filled by `lugha_const_init` before `main` in `Checked::consts`
//! order; a read loads the global (and copies it as any place is copied).

use inkwell::values::{BasicValueEnum, GlobalValue};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Scopes;
use super::value::llvm_type;
use crate::ast::{Expr, Item};
use crate::check::{Type, Unit, qualify, symbol};

/// The init function's symbol.
const INIT: &str = "lugha_const_init";

impl<'ctx> Lowerer<'ctx> {
    /// A zeroed global for every constant.
    pub(super) fn declare_consts(&mut self, consts: &[(String, Type)]) {
        for (key, ty) in consts {
            let llvm = llvm_type(self.context, ty);
            let name = format!("lugha_const_{}", symbol(key));
            let global = self.module.add_global(llvm, None, &name);
            global.set_initializer(&llvm.const_zero());
            self.const_globals.insert(key.clone(), (global, ty.clone()));
        }
    }

    /// `lugha_const_init()`: every constant's value, in order. `None` when
    /// the program has no constants.
    pub(super) fn define_const_init(
        &mut self,
        units: &[Unit],
        consts: &[(String, Type)],
    ) -> Result<Option<inkwell::values::FunctionValue<'ctx>>, CodegenError> {
        if consts.is_empty() {
            return Ok(None);
        }
        let function =
            self.module
                .add_function(INIT, self.context.void_type().fn_type(&[], false), None);
        self.function = Some(function);
        self.ret = None;
        self.scopes = Scopes::default();
        self.loops.clear();
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        self.scopes.push();
        for (key, ty) in consts {
            let value = declared(units, key).expect("checked: every constant is declared");
            let value = self.value_for_store(value, ty)?;
            let (global, _) = self.const_globals[key];
            self.builder
                .build_store(global.as_pointer_value(), value)
                .expect(POSITIONED);
        }
        self.scopes.pop();
        self.builder.build_return(None).expect(POSITIONED);
        Ok(Some(function))
    }

    /// A read of the constant `expr` names (`NAME` or `m.NAME`).
    pub(super) fn const_value(&mut self, expr: &Expr) -> BasicValueEnum<'ctx> {
        let (global, ty): (GlobalValue<'ctx>, Type) =
            self.const_globals[&self.const_uses[&expr.id]].clone();
        self.builder
            .build_load(
                llvm_type(self.context, &ty),
                global.as_pointer_value(),
                "const",
            )
            .expect(POSITIONED)
    }
}

/// The value of the constant `key` as written.
fn declared<'a>(units: &'a [Unit], key: &str) -> Option<&'a Expr> {
    units.iter().find_map(|unit| {
        unit.program.items.iter().find_map(|item| match item {
            Item::Const(c) if qualify(unit.path, &c.name.name) == key => Some(&c.value),
            _ => None,
        })
    })
}
