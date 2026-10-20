//! Giving a defined function a new type (v17, milestones 125–126): LLVM
//! can't change a function's type, so the passes that change Lugha's
//! calling convention (`sret.rs`, `split.rs`) build a new function under
//! the old name, move the old body into it, and delete the old one.

use inkwell::attributes::AttributeLoc;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::FunctionType;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};

/// A new function of type `fn_type` replacing `old`: same name, linkage
/// and function attributes, and `old`'s body. `params[i]` is where old
/// parameter `i`'s attributes go (`None`: nowhere). `remap`, positioned at
/// the start of the entry block, gives the value standing for each old
/// parameter. Every use of `old` (function values, tables) then names the
/// new function, and `old` is deleted.
pub(super) fn rebuild<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    (old, fn_type): (FunctionValue<'ctx>, FunctionType<'ctx>),
    params: &[Option<u32>],
    remap: impl FnOnce(&Builder<'ctx>, FunctionValue<'ctx>) -> Vec<BasicValueEnum<'ctx>>,
) -> FunctionValue<'ctx> {
    let name = old.get_name().to_string_lossy().into_owned();
    // Until the old one is gone, two functions can't share a name.
    old.as_global_value().set_name(&format!("lugha_old_{name}"));
    let new = module.add_function(&name, fn_type, Some(old.get_linkage()));
    for attribute in old.attributes(AttributeLoc::Function) {
        new.add_attribute(AttributeLoc::Function, attribute);
    }
    for (i, at) in params.iter().enumerate() {
        let Some(at) = at else { continue };
        for attribute in old.attributes(AttributeLoc::Param(i as u32)) {
            new.add_attribute(AttributeLoc::Param(*at), attribute);
        }
    }
    // The body moves block by block behind a placeholder, then the placeholder goes.
    let anchor = context.append_basic_block(new, "anchor");
    let mut last = anchor;
    for block in old.get_basic_blocks() {
        block
            .move_after(last)
            .expect("both blocks are in functions");
        last = block;
    }
    anchor
        .remove_from_function()
        .expect("the placeholder is in the new function");
    let entry = new
        .get_first_basic_block()
        .expect("a defined function has an entry block");
    match entry.get_first_instruction() {
        Some(first) => builder.position_before(&first),
        None => builder.position_at_end(entry),
    }
    let values = remap(builder, new);
    for (param, value) in old.get_param_iter().zip(values) {
        if let Some(name) = param.get_name().to_str().ok().filter(|n| !n.is_empty()) {
            value.set_name(name);
        }
        replace_uses(param, value);
    }
    old.replace_all_uses_with(new);
    debug_assert!(
        old.as_global_value()
            .as_pointer_value()
            .get_first_use()
            .is_none(),
        "every use of `{name}` was redirected"
    );
    // SAFETY: `old` has no basic blocks left (all moved into `new`), every
    // use of its parameters was replaced, and `replace_all_uses_with`
    // redirected every use of `old` itself, so nothing refers to it: deleting
    // it can't leave a dangling reference. The user approved this one
    // `unsafe` call (PRP-158, decision 300).
    unsafe { old.delete() };
    new
}

/// Every use of `old` becomes a use of `new`, a value of the same type.
pub(super) fn replace_uses<'ctx>(old: BasicValueEnum<'ctx>, new: BasicValueEnum<'ctx>) {
    match (old, new) {
        (BasicValueEnum::IntValue(o), BasicValueEnum::IntValue(n)) => o.replace_all_uses_with(n),
        (BasicValueEnum::FloatValue(o), BasicValueEnum::FloatValue(n)) => {
            o.replace_all_uses_with(n)
        }
        (BasicValueEnum::PointerValue(o), BasicValueEnum::PointerValue(n)) => {
            o.replace_all_uses_with(n)
        }
        (BasicValueEnum::StructValue(o), BasicValueEnum::StructValue(n)) => {
            o.replace_all_uses_with(n)
        }
        (BasicValueEnum::ArrayValue(o), BasicValueEnum::ArrayValue(n)) => {
            o.replace_all_uses_with(n)
        }
        (BasicValueEnum::VectorValue(o), BasicValueEnum::VectorValue(n)) => {
            o.replace_all_uses_with(n)
        }
        _ => unreachable!("a value is replaced by one of its own type"),
    }
}
