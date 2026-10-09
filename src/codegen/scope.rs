//! Local variables: block scopes, and stack slots in the entry block (spec §7).

use std::collections::HashMap;

use inkwell::values::{IntValue, PointerValue};

use super::lower::{Lowerer, POSITIONED};
use super::value::Kind;

/// A local variable's stack slot.
#[derive(Debug, Clone, Copy)]
pub(super) struct Local<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub kind: Kind,
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

    /// The innermost binding of `name`.
    pub(super) fn lookup(&self, name: &str) -> Option<Local<'ctx>> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// Allocates a stack slot at the top of the entry block, so `mem2reg` can
    /// promote it and loops don't allocate once per iteration (spec §7).
    pub(super) fn entry_alloca(&self, kind: Kind, name: &str) -> PointerValue<'ctx> {
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
            .build_alloca(kind.llvm(self.context), name)
            .expect(POSITIONED)
    }

    /// Loads a local's current value.
    pub(super) fn load(&self, local: Local<'ctx>, name: &str) -> IntValue<'ctx> {
        let ty = local.kind.llvm(self.context);
        self.builder
            .build_load(ty, local.ptr, name)
            .expect(POSITIONED)
            .into_int_value()
    }
}
