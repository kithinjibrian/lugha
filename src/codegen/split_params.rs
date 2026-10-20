//! Struct parameters split into one parameter per leaf (v17, milestone
//! 126, PRP-158): a Lugha function taking a struct by value (an equality
//! helper's `%P`, a function value's `{ ptr, ptr }`) takes its leaves
//! instead, and every call of it, direct or indirect, passes them. Calls
//! are rewritten with `extractvalue`s and definitions rebuild the struct
//! with `insertvalue`s; the body pass (`split.rs`) then folds both away.
//! Entries C calls and calls into C keep C's convention.

use inkwell::attributes::AttributeLoc;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, InstructionOpcode,
    InstructionValue, ValueKind,
};

use super::lower::POSITIONED;
use super::rebuild::{rebuild, replace_uses};
use super::split::{extract, insert, leaf_types};

/// Splits every Lugha function's struct parameters, and its calls' arguments.
pub(super) fn split_params<'ctx>(context: &'ctx Context, module: &Module<'ctx>) {
    let builder = context.create_builder();
    let targets: Vec<FunctionValue> = module.get_functions().filter(|f| rewritten(*f)).collect();
    for function in module.get_functions() {
        for block in function.get_basic_blocks() {
            let calls: Vec<InstructionValue> = block
                .get_instructions()
                .filter(|i| i.get_opcode() == InstructionOpcode::Call)
                .collect();
            for call in calls {
                rewrite_call(context, &builder, call, &targets);
            }
        }
    }
    for function in targets {
        rewrite_definition(context, module, &builder, function);
    }
}

/// True for a Lugha function defined here taking a struct by value.
fn rewritten(f: FunctionValue<'_>) -> bool {
    let name = f.get_name().to_string_lossy();
    let c_entry = name == "main" || name.starts_with("lugha_cb_") || name.starts_with("lugha_cfn_");
    f.count_basic_blocks() > 0 && !c_entry && splits(f.get_type())
}

/// True if a function of type `ty` has a struct parameter to split.
fn splits(ty: FunctionType<'_>) -> bool {
    ty.get_param_types()
        .into_iter()
        .any(|t| split_type(t).is_some())
}

/// The leaf types of a parameter of type `t`, if it is split.
fn split_type(t: BasicMetadataTypeEnum<'_>) -> Option<Vec<BasicTypeEnum<'_>>> {
    let basic = BasicTypeEnum::try_from(t).ok()?;
    leaf_types(basic).map(|leaves| leaves.into_iter().map(|(_, ty)| ty).collect())
}

/// `ty` with each split parameter replaced by its leaves; and, for each old
/// parameter, the index of its first new one.
fn new_type<'ctx>(
    context: &'ctx Context,
    ty: FunctionType<'ctx>,
) -> (FunctionType<'ctx>, Vec<u32>) {
    let mut params: Vec<BasicMetadataTypeEnum> = Vec::new();
    let mut starts = Vec::new();
    for t in ty.get_param_types() {
        starts.push(params.len() as u32);
        match split_type(t) {
            Some(leaves) => params.extend(leaves.into_iter().map(BasicMetadataTypeEnum::from)),
            None => params.push(t),
        }
    }
    let fn_type = match ty.get_return_type() {
        Some(ret) => ret.fn_type(&params, ty.is_var_arg()),
        None => context.void_type().fn_type(&params, ty.is_var_arg()),
    };
    (fn_type, starts)
}

/// `call` of a rewritten function, or through a function value, passing
/// each struct argument's leaves; calls into C are left alone.
fn rewrite_call<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    call: InstructionValue<'ctx>,
    targets: &[FunctionValue<'ctx>],
) {
    let site = CallSiteValue::try_from(call).expect("a call instruction is a call site");
    let fn_type = site.get_called_fn_type();
    if !splits(fn_type) {
        return;
    }
    // Indirect calls are always to Lugha function values (C is only called directly).
    if let Some(callee) = site.get_called_fn_value()
        && !targets.contains(&callee)
    {
        return;
    }
    let callee = call
        .get_operand(call.get_num_operands() - 1)
        .and_then(|o| o.value())
        .expect("a call's last operand is its callee")
        .into_pointer_value();
    let (new_fn_type, starts) = new_type(context, fn_type);
    builder.position_before(&call);
    let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
    for (i, t) in fn_type.get_param_types().into_iter().enumerate() {
        let arg = call
            .get_operand(i as u32)
            .and_then(|o| o.value())
            .expect("a call's arguments are values");
        match BasicTypeEnum::try_from(t).ok().and_then(leaf_types) {
            Some(leaves) => {
                for (path, _) in leaves {
                    args.push(extract(builder, arg, &path).into());
                }
            }
            None => args.push(arg.into()),
        }
    }
    let new = builder
        .build_indirect_call(new_fn_type, callee, &args, "")
        .expect(POSITIONED);
    for (i, start) in starts.iter().enumerate() {
        if split_type(fn_type.get_param_types()[i]).is_none() {
            for attribute in site.attributes(AttributeLoc::Param(i as u32)) {
                new.add_attribute(AttributeLoc::Param(*start), attribute);
            }
        }
    }
    if let (ValueKind::Basic(old), ValueKind::Basic(result)) =
        (site.try_as_basic_value(), new.try_as_basic_value())
    {
        replace_uses(old, result);
    }
    call.erase_from_basic_block();
}

/// `function`, taking a struct by value: rebuilt (`rebuild.rs`) to take its
/// leaves, the whole struct put back together at the top of the body.
fn rewrite_definition<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    old: FunctionValue<'ctx>,
) {
    let fn_type = old.get_type();
    let (new_fn_type, starts) = new_type(context, fn_type);
    let types = fn_type.get_param_types();
    // Attributes stay on unsplit parameters (the `sret` slot among them).
    let moved: Vec<Option<u32>> = types
        .iter()
        .zip(&starts)
        .map(|(t, start)| split_type(*t).is_none().then_some(*start))
        .collect();
    let names: Vec<String> = old
        .get_param_iter()
        .map(|p| p.get_name().to_string_lossy().into_owned())
        .collect();
    rebuild(
        context,
        module,
        builder,
        (old, new_fn_type),
        &moved,
        |b, new| {
            let params: Vec<BasicValueEnum> = new.get_param_iter().collect();
            types
                .iter()
                .zip(&starts)
                .zip(&names)
                .map(|((t, &start), name)| {
                    let basic = BasicTypeEnum::try_from(*t).expect("a parameter has a basic type");
                    let Some(leaves) = leaf_types(basic) else {
                        return params[start as usize];
                    };
                    let mut whole = match basic {
                        BasicTypeEnum::StructType(s) => s.get_undef().into(),
                        BasicTypeEnum::ArrayType(a) => a.get_undef().into(),
                        _ => unreachable!("only structs and arrays have leaves"),
                    };
                    for (k, (path, _)) in leaves.into_iter().enumerate() {
                        let leaf = params[start as usize + k];
                        if !name.is_empty() {
                            leaf.set_name(&format!("{name}.{k}"));
                        }
                        whole = insert(b, whole, &path, leaf);
                    }
                    whole
                })
                .collect()
        },
    );
}
