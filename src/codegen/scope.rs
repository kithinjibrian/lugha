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

/// Nested block scopes; inner names shadow outer ones (spec §5). Cloned
/// for a `defer`, which runs with the names in scope where it is written.
#[derive(Default, Clone)]
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
        self.find(name)
            .unwrap_or_else(|| unreachable!("checked: `{name}` is in scope"))
    }

    /// `name`'s binding in the innermost scope only: an or-pattern's later
    /// alternatives reuse the local the first one declared (v9).
    pub(super) fn in_innermost(&self, name: &str) -> Option<Local<'ctx>> {
        self.stack.last().and_then(|scope| scope.get(name).cloned())
    }

    /// The innermost binding of `name`, if there is one.
    pub(super) fn find(&self, name: &str) -> Option<Local<'ctx>> {
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).cloned())
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
    pub(super) fn load(&mut self, local: &Local<'ctx>, name: &str) -> BasicValueEnum<'ctx> {
        let ty = llvm_type(self.context, &local.ty);
        // A `for … of mut` variable is its element, reached afresh (v15).
        let at = self.local_address(local.ptr, false);
        self.builder.build_load(ty, at, name).expect(POSITIONED)
    }
}
