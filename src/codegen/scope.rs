//! Local variables: block scopes, and stack slots in the entry block (spec §7).

use std::collections::HashMap;

use inkwell::values::{BasicValueEnum, PointerValue};

use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::check::Type;

/// A local variable's stack slot.
#[derive(Debug, Clone)]
pub(super) struct Local<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub ty: Type,
    /// A parameter or `for … of` variable: part of someone else's data, so
    /// returning it copies (spec §4).
    pub borrowed: bool,
}

/// Nested block scopes; inner names shadow outer ones (spec §5).
#[derive(Default)]
pub(super) struct Scopes<'ctx> {
    stack: Vec<HashMap<String, Local<'ctx>>>,
}

impl<'ctx> Scopes<'ctx> {
    pub(super) fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub(super) fn pop(&mut self) {
        self.stack.pop();
    }

    /// Declares `name` in the innermost scope, shadowing any earlier binding.
    pub(super) fn declare(&mut self, name: &str, local: Local<'ctx>) {
        let scope = self
            .stack
            .last_mut()
            .expect("locals are declared inside a block");
        scope.insert(name.to_string(), local);
    }

    /// The innermost binding of `name`; the checker guarantees one exists.
    pub(super) fn lookup(&self, name: &str) -> Local<'ctx> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
            .unwrap_or_else(|| unreachable!("checked: `{name}` is in scope"))
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// Allocates a stack slot at the top of the entry block, so `mem2reg` can
    /// promote it and loops don't allocate once per iteration (spec §7).
    pub(super) fn entry_alloca(&self, ty: &Type, name: &str) -> PointerValue<'ctx> {
        let function = self
            .function
            .expect("locals are allocated inside a function");
        let entry = function
            .get_first_basic_block()
            .expect("functions start with an entry block");
        let builder = self.context.create_builder();
        match entry.get_first_instruction() {
            Some(first) => builder.position_before(&first),
            None => builder.position_at_end(entry),
        }
        builder
            .build_alloca(llvm_type(self.context, ty), name)
            .expect(POSITIONED)
    }

    /// Loads a local's current value.
    pub(super) fn load(&self, local: &Local<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        let ty = llvm_type(self.context, &local.ty);
        self.builder
            .build_load(ty, local.ptr, name)
            .expect(POSITIONED)
    }
}
