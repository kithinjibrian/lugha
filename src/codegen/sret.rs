//! Results through a hidden pointer (v17, milestone 125, PRP-157). At
//! `-O0`, LLVM's fast instruction selector can't lower a function that
//! returns a struct as a value, and sends the whole function to the slow
//! one. So once a module is lowered, every Lugha function returning a
//! struct (a Lugha struct, enum, `Option`, tuple or function value) is
//! rewritten to take a first parameter `ptr sret(T)` and return nothing,
//! and every call of one passes a slot in its caller's entry block and
//! reads the result from it. Functions C calls (`main`, the `lugha_cb_`
//! and `lugha_cfn_` entries) and calls into C keep C's own convention.

use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicTypeEnum, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, InstructionOpcode,
    InstructionValue, ValueKind,
};

use super::lower::POSITIONED;
use super::rebuild::rebuild;

/// Rewrites `module`'s struct results to go through a hidden pointer.
pub(super) fn results_through_pointers<'ctx>(context: &'ctx Context, module: &Module<'ctx>) {
    let builder = context.create_builder();
    let targets: Vec<FunctionValue> = module.get_functions().filter(|f| rewritten(*f)).collect();
    // Calls first, while every callee still has its old type.
    for function in module.get_functions() {
        for block in function.get_basic_blocks() {
            let calls: Vec<InstructionValue> = block
                .get_instructions()
                .filter(|i| i.get_opcode() == InstructionOpcode::Call)
                .collect();
            for call in calls {
                rewrite_call(context, &builder, function, call, &targets);
            }
        }
    }
    for function in targets {
        rewrite_definition(context, module, &builder, function);
    }
}

/// The struct a function of this result type returns through a pointer.
fn struct_result(ret: Option<BasicTypeEnum<'_>>) -> Option<StructType<'_>> {
    match ret {
        Some(BasicTypeEnum::StructType(s)) => Some(s),
        _ => None,
    }
}

/// True for a Lugha function defined here that returns a struct: not an
/// extern's declaration, and not an entry C calls.
fn rewritten(f: FunctionValue<'_>) -> bool {
    let name = f.get_name().to_string_lossy();
    let c_entry = name == "main" || name.starts_with("lugha_cb_") || name.starts_with("lugha_cfn_");
    f.count_basic_blocks() > 0
        && !c_entry
        && struct_result(f.get_type().get_return_type()).is_some()
}

/// The `sret(T)` attribute for a result of type `ty`.
fn sret<'ctx>(context: &'ctx Context, ty: StructType<'ctx>) -> Attribute {
    let kind = Attribute::get_named_enum_kind_id("sret");
    context.create_type_attribute(kind, ty.as_any_type_enum())
}

/// `call`, returning a struct from a rewritten function or through a
/// function value: a call passing a fresh entry-block slot, then a load of
/// the result from it. Calls into C (to declarations) are left alone.
fn rewrite_call<'ctx>(
    context: &'ctx Context,
    builder: &Builder<'ctx>,
    caller: FunctionValue<'ctx>,
    call: InstructionValue<'ctx>,
    targets: &[FunctionValue<'ctx>],
) {
    let site = CallSiteValue::try_from(call).expect("a call instruction is a call site");
    let fn_type = site.get_called_fn_type();
    let Some(ty) = struct_result(fn_type.get_return_type()) else {
        return;
    };
    // A direct call reaches a rewritten function, or C; an indirect one is
    // always a Lugha function value (C is only ever called directly).
    if let Some(callee) = site.get_called_fn_value()
        && !targets.contains(&callee)
    {
        return;
    }
    let operands = call.get_num_operands();
    let callee = call
        .get_operand(operands - 1)
        .and_then(|o| o.value())
        .expect("a call's last operand is its callee")
        .into_pointer_value();
    let args: Vec<BasicValueEnum> = (0..site.count_arguments())
        .map(|i| {
            call.get_operand(i)
                .and_then(|o| o.value())
                .expect("a call's arguments are values")
        })
        .collect();
    let entry = caller
        .get_first_basic_block()
        .expect("a caller has an entry block");
    match entry.get_first_instruction() {
        Some(first) => builder.position_before(&first),
        None => builder.position_at_end(entry),
    }
    let slot = builder.build_alloca(ty, "result").expect(POSITIONED);
    builder.position_before(&call);
    let mut params: Vec<BasicMetadataTypeEnum> = vec![context.ptr_type(Default::default()).into()];
    params.extend(fn_type.get_param_types());
    let new_type = context.void_type().fn_type(&params, fn_type.is_var_arg());
    let mut values: Vec<BasicMetadataValueEnum> = vec![slot.into()];
    values.extend(args.iter().map(|a| BasicMetadataValueEnum::from(*a)));
    let new = builder
        .build_indirect_call(new_type, callee, &values, "")
        .expect(POSITIONED);
    new.add_attribute(AttributeLoc::Param(0), sret(context, ty));
    // The call's own attributes on its arguments move one place along.
    for (i, _) in args.iter().enumerate() {
        for attribute in site.attributes(AttributeLoc::Param(i as u32)) {
            new.add_attribute(AttributeLoc::Param(i as u32 + 1), attribute);
        }
    }
    let result = builder.build_load(ty, slot, "result").expect(POSITIONED);
    if let ValueKind::Basic(old) = site.try_as_basic_value() {
        old.into_struct_value()
            .replace_all_uses_with(result.into_struct_value());
    }
    call.erase_from_basic_block();
}

/// `function`, returning a struct: a new function taking `ptr sret(T)`
/// first, with the old body moved in (`rebuild.rs`), its parameters one
/// place along, and each `ret v` a store into the slot and `ret void`.
fn rewrite_definition<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    old: FunctionValue<'ctx>,
) {
    let fn_type = old.get_type();
    let ty = struct_result(fn_type.get_return_type()).expect("checked: a struct result");
    let mut params: Vec<BasicMetadataTypeEnum> = vec![context.ptr_type(Default::default()).into()];
    params.extend(fn_type.get_param_types());
    let new_type = context.void_type().fn_type(&params, fn_type.is_var_arg());
    let moved: Vec<Option<u32>> = (1..=old.count_params()).map(Some).collect();
    let new = rebuild(
        context,
        module,
        builder,
        (old, new_type),
        &moved,
        |_, new| new.get_param_iter().skip(1).collect(),
    );
    new.add_attribute(AttributeLoc::Param(0), sret(context, ty));
    let out = new
        .get_first_param()
        .expect("the result slot is the first parameter")
        .into_pointer_value();
    out.set_name("result");
    for block in new.get_basic_blocks() {
        let rets: Vec<InstructionValue> = block
            .get_instructions()
            .filter(|i| i.get_opcode() == InstructionOpcode::Return)
            .collect();
        for ret in rets {
            builder.position_before(&ret);
            if let Some(value) = ret.get_operand(0).and_then(|o| o.value()) {
                builder.build_store(out, value).expect(POSITIONED);
            }
            builder.build_return(None).expect(POSITIONED);
            ret.erase_from_basic_block();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    /// The line defining `symbol` in `ir`.
    fn define<'a>(ir: &'a str, symbol: &str) -> &'a str {
        ir.lines()
            .find(|l| l.starts_with("define") && l.contains(symbol))
            .unwrap_or_else(|| panic!("no {symbol} in {ir}"))
    }

    const P: &str = "struct P { x: i64, y: i64 }\n";

    #[test]
    fn a_struct_result_goes_through_a_hidden_pointer() {
        let ir = ir(&format!(
            "{P}fun make(n: i64): P = P {{ x: n, y: 2 }};\n\
             fun main() {{ let p = make(1); println(p.x); }}"
        ));
        let def = define(&ir, "@lugha_fn_make(");
        assert!(
            def.starts_with("define void @lugha_fn_make(ptr sret(%P)"),
            "{def}"
        );
        assert!(ir.contains("call void @lugha_fn_make(ptr"), "{ir}");
        assert!(!ir.contains("ret %P"), "{ir}");
    }

    #[test]
    fn enums_options_tuples_and_methods_too() {
        let ir = ir(&format!(
            "{P}enum E {{ A(i64), B }}\n\
             impl P {{ fun flip(self): P = P {{ x: self.y, y: self.x }}; }}\n\
             fun e(): E = E.A(1);\nfun o(): Option<i64> = Some(1);\nfun t(): (i64, bool) = (1, true);\n\
             fun main() {{ let p = P {{ x: 1, y: 2 }}.flip(); let a = e(); let b = o(); let c = t(); println(p.x); }}"
        ));
        for symbol in [
            "@lugha_fn_e(",
            "@lugha_fn_o(",
            "@lugha_fn_t(",
            "@lugha_fn_P.flip(",
        ] {
            let def = define(&ir, symbol);
            assert!(def.contains("void") && def.contains("sret("), "{def}");
        }
    }

    #[test]
    fn calls_through_values_and_interfaces_pass_the_slot() {
        let ir = ir(&format!(
            "{P}interface Make {{ fun make(self): P; }}\n\
             struct M {{ n: i64 }}\nimpl M {{ fun make(self): P = P {{ x: self.n, y: 0 }}; }}\n\
             fun via(m: Make): P = m.make();\n\
             fun main() {{ let f = fun(n: i64): P = P {{ x: n, y: n }}; let p = f(3); let q = via(M {{ n: 1 }}); println(p.x + q.x); }}"
        ));
        // Every call returning `%P`, direct or not, returns nothing now.
        assert!(!ir.contains("call %P"), "{ir}");
        assert!(
            ir.contains("call void %"),
            "an indirect call with the slot: {ir}"
        );
        let def = define(&ir, "@lugha_fn_M.make(");
        assert!(def.contains("sret(%P)"), "{def}");
    }

    #[test]
    fn other_results_and_main_are_unchanged() {
        let ir = ir(
            "fun n(): i64 = 1;\nfun s(): string = \"a\";\nfun xs(): i64[] = [1];\n\
             fun main() { println(n()); println(s()); println(xs()); }",
        );
        assert!(
            define(&ir, "@lugha_fn_n(").starts_with("define i64"),
            "{ir}"
        );
        assert!(
            define(&ir, "@lugha_fn_s(").starts_with("define ptr"),
            "{ir}"
        );
        assert!(
            define(&ir, "@lugha_fn_xs(").starts_with("define ptr"),
            "{ir}"
        );
        assert!(
            define(&ir, "@main(").starts_with("define i32 @main"),
            "{ir}"
        );
    }
}
