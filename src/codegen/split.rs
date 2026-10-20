//! Struct values split into their fields (v17, milestone 126, PRP-158).
//! At `-O0`, LLVM's fast instruction selector gives up on a block at its
//! first whole-struct instruction and hands it to the slow one. So once a
//! module is lowered (and results go through pointers, `sret.rs`), every
//! struct-typed value is replaced by its *leaves* — its fields, recursively
//! through nested structs and arrays, down to integers, floats and
//! pointers: a struct `load` or `store` becomes one per leaf, `insertvalue`
//! and `extractvalue` pick leaves without any instruction, and `phi` and
//! `select` work leaf by leaf. Struct parameters become one parameter per
//! leaf (`split_params.rs`). Where a whole struct must still exist — given
//! to C, or of a type with more than [`MAX_LEAVES`] leaves — it is rebuilt
//! from its leaves just there.

use std::collections::{HashMap, HashSet};

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{
    AnyValue, BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode, InstructionValue,
    PhiValue,
};

use super::lower::POSITIONED;
use super::rebuild::replace_uses;

/// Types with more leaves than this stay whole (their instructions keep
/// the slow path); kivuli's largest has 11.
pub(super) const MAX_LEAVES: usize = 64;

/// Where a leaf sits: field and element indexes from the outer type in.
pub(super) type Path = Vec<u32>;

/// A struct value as its leaves, in order.
type Leaves<'ctx> = Vec<(Path, BasicValueEnum<'ctx>)>;

/// The leaves of `ty` with their types, when `ty` is a struct or array of
/// at most [`MAX_LEAVES`] leaves; `None` for anything else.
pub(super) fn leaf_types(ty: BasicTypeEnum<'_>) -> Option<Vec<(Path, BasicTypeEnum<'_>)>> {
    if !matches!(
        ty,
        BasicTypeEnum::StructType(_) | BasicTypeEnum::ArrayType(_)
    ) {
        return None;
    }
    let mut out = Vec::new();
    collect(ty, &mut Vec::new(), &mut out).then_some(out)
}

/// Appends `ty`'s leaves under `path`; false once there are too many.
fn collect<'ctx>(
    ty: BasicTypeEnum<'ctx>,
    path: &mut Path,
    out: &mut Vec<(Path, BasicTypeEnum<'ctx>)>,
) -> bool {
    let children: Vec<BasicTypeEnum> = match ty {
        BasicTypeEnum::StructType(s) => s.get_field_types(),
        BasicTypeEnum::ArrayType(a) if a.len() as usize <= MAX_LEAVES => {
            vec![a.get_element_type(); a.len() as usize]
        }
        BasicTypeEnum::ArrayType(_) => return false,
        leaf => {
            out.push((path.clone(), leaf));
            return out.len() <= MAX_LEAVES;
        }
    };
    for (i, child) in children.into_iter().enumerate() {
        path.push(i as u32);
        let fits = collect(child, path, out);
        path.pop();
        if !fits {
            return false;
        }
    }
    true
}

/// The value at `path` inside `value`, through `extractvalue`s (folded
/// away later, or for a constant at once).
pub(super) fn extract<'ctx>(
    builder: &Builder<'ctx>,
    value: BasicValueEnum<'ctx>,
    path: &[u32],
) -> BasicValueEnum<'ctx> {
    let mut v = value;
    for &i in path {
        v = match v {
            BasicValueEnum::StructValue(s) => builder.build_extract_value(s, i, "leaf"),
            BasicValueEnum::ArrayValue(a) => builder.build_extract_value(a, i, "leaf"),
            _ => unreachable!("a path leads through structs and arrays"),
        }
        .expect(POSITIONED);
    }
    v
}

/// `whole` with the value at `path` replaced by `leaf`.
pub(super) fn insert<'ctx>(
    builder: &Builder<'ctx>,
    whole: BasicValueEnum<'ctx>,
    path: &[u32],
    leaf: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let Some((&first, rest)) = path.split_first() else {
        return leaf;
    };
    let inner = match rest.is_empty() {
        true => leaf,
        false => insert(builder, extract(builder, whole, &[first]), rest, leaf),
    };
    let built = match whole {
        BasicValueEnum::StructValue(s) => builder.build_insert_value(s, inner, first, "whole"),
        BasicValueEnum::ArrayValue(a) => builder.build_insert_value(a, inner, first, "whole"),
        _ => unreachable!("a path leads through structs and arrays"),
    }
    .expect(POSITIONED);
    built.as_basic_value_enum()
}

/// Splits every struct value of every function `module` defines.
pub(super) fn split_structs<'ctx>(context: &'ctx Context, module: &Module<'ctx>) {
    super::split_params::split_params(context, module);
    let builder = context.create_builder();
    for function in module.get_functions() {
        if function.count_basic_blocks() > 0 {
            Splitter::new(context, &builder, function).run();
        }
    }
}

/// One function's rewrite.
struct Splitter<'a, 'ctx> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    function: FunctionValue<'ctx>,
    /// Each split value's leaves.
    leaves: HashMap<BasicValueEnum<'ctx>, Leaves<'ctx>>,
    /// The struct instructions replaced, in the order they were met.
    replaced: Vec<InstructionValue<'ctx>>,
    /// Each struct `phi` with its leaf `phi`s, filled in at the end.
    phis: Vec<(PhiValue<'ctx>, Vec<PhiValue<'ctx>>)>,
}

impl<'a, 'ctx> Splitter<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        function: FunctionValue<'ctx>,
    ) -> Self {
        Splitter {
            context,
            builder,
            function,
            leaves: HashMap::new(),
            replaced: Vec::new(),
            phis: Vec::new(),
        }
    }

    fn run(mut self) {
        for block in order(self.function) {
            let instructions: Vec<InstructionValue> = block.get_instructions().collect();
            for inst in instructions {
                self.instruction(inst);
            }
        }
        for (phi, leaf_phis) in std::mem::take(&mut self.phis) {
            for i in 0..phi.count_incoming() {
                let (value, from) = phi.get_incoming(i).expect("counted");
                let leaves = self.leaves_of(value);
                for (leaf_phi, (_, leaf)) in leaf_phis.iter().zip(leaves) {
                    leaf_phi.add_incoming(&[(&leaf, from)]);
                }
            }
        }
        // Users before what they use: what still has a use is rebuilt whole.
        for inst in std::mem::take(&mut self.replaced).into_iter().rev() {
            if inst.get_first_use().is_some() {
                self.rebuild_whole(inst);
            }
            inst.erase_from_basic_block();
        }
    }

    /// Splits `inst` if it is a struct instruction with leaves.
    fn instruction(&mut self, inst: InstructionValue<'ctx>) {
        let result = result_leaves(inst);
        match inst.get_opcode() {
            InstructionOpcode::Load if let Some(types) = result => {
                let ptr = operand(inst, 0).into_pointer_value();
                let ty = basic_type(inst);
                let align = inst.get_alignment().unwrap_or(1);
                self.builder.position_before(&inst);
                let leaves = types
                    .into_iter()
                    .map(|(path, leaf_ty)| {
                        let at = self.address(ptr, ty, &path);
                        let load = self.builder.build_load(leaf_ty, at, "f").expect(POSITIONED);
                        aligned(load.as_instruction_value().expect("a load"), align, leaf_ty);
                        (path, load)
                    })
                    .collect();
                self.split(inst, leaves);
            }
            InstructionOpcode::Store => {
                let value = operand(inst, 0);
                if leaf_types(value.get_type()).is_none() {
                    return;
                }
                let ptr = operand(inst, 1).into_pointer_value();
                let align = inst.get_alignment().unwrap_or(1);
                let leaves = self.leaves_of(value);
                self.builder.position_before(&inst);
                for (path, leaf) in leaves {
                    let at = self.address(ptr, value.get_type(), &path);
                    let store = self.builder.build_store(at, leaf).expect(POSITIONED);
                    aligned(store, align, leaf.get_type());
                }
                self.replaced.push(inst);
            }
            InstructionOpcode::InsertValue if result.is_some() => {
                let base = self.leaves_of(operand(inst, 0));
                let element = operand(inst, 1);
                let at = inst.get_indices();
                let inserted = match leaf_types(element.get_type()) {
                    Some(_) => self.leaves_of(element),
                    None => vec![(Vec::new(), element)],
                };
                let leaves = base
                    .into_iter()
                    .map(|(path, leaf)| match path.strip_prefix(at.as_slice()) {
                        Some(rest) => {
                            let found = inserted.iter().find(|(p, _)| p == rest);
                            (
                                path.clone(),
                                found.expect("the inserted value has this leaf").1,
                            )
                        }
                        None => (path, leaf),
                    })
                    .collect();
                self.split(inst, leaves);
            }
            InstructionOpcode::ExtractValue => {
                let whole = operand(inst, 0);
                // A field of a call's result (C's, or an overflow intrinsic's
                // `{ i64, i1 }`, which the fast selector handles) stays as written.
                let from_call = !self.leaves.contains_key(&whole)
                    && whole
                        .as_instruction_value()
                        .is_some_and(|d| d.get_opcode() == InstructionOpcode::Call);
                if leaf_types(whole.get_type()).is_none() || from_call {
                    return;
                }
                let at = inst.get_indices();
                let inner: Leaves = self
                    .leaves_of(whole)
                    .into_iter()
                    .filter_map(|(path, leaf)| {
                        path.strip_prefix(at.as_slice()).map(|r| (r.to_vec(), leaf))
                    })
                    .collect();
                match result {
                    Some(_) => self.split(inst, inner),
                    None => {
                        let leaf = inner.first().expect("a scalar field is one leaf").1;
                        replace_uses(value_of(inst), leaf);
                        self.replaced.push(inst);
                    }
                }
            }
            InstructionOpcode::Phi if let Some(types) = result => {
                self.builder.position_before(&inst);
                let leaf_phis: Vec<PhiValue> = types
                    .iter()
                    .map(|(_, ty)| self.builder.build_phi(*ty, "f").expect(POSITIONED))
                    .collect();
                let leaves = types
                    .into_iter()
                    .zip(&leaf_phis)
                    .map(|((path, _), p)| (path, p.as_basic_value()))
                    .collect();
                let phi = PhiValue::try_from(inst).expect("a phi instruction");
                self.phis.push((phi, leaf_phis));
                self.split(inst, leaves);
            }
            InstructionOpcode::Select if result.is_some() => {
                let condition = operand(inst, 0).into_int_value();
                let (a, b) = (
                    self.leaves_of(operand(inst, 1)),
                    self.leaves_of(operand(inst, 2)),
                );
                self.builder.position_before(&inst);
                let leaves = a
                    .into_iter()
                    .zip(b)
                    .map(|((path, x), (_, y))| {
                        let s = self
                            .builder
                            .build_select(condition, x, y, "f")
                            .expect(POSITIONED);
                        (path, s)
                    })
                    .collect();
                self.split(inst, leaves);
            }
            _ => {}
        }
    }

    /// Records `inst`'s leaves; `inst` goes once its users are rewritten.
    fn split(&mut self, inst: InstructionValue<'ctx>, leaves: Leaves<'ctx>) {
        self.leaves.insert(value_of(inst), leaves);
        self.replaced.push(inst);
    }

    /// `value`'s leaves: those recorded, a constant's (folded), or for a
    /// value split nowhere else (a parameter or a C call's result) the
    /// `extractvalue`s just after it. It moves the builder: callers
    /// position it afterwards.
    fn leaves_of(&mut self, value: BasicValueEnum<'ctx>) -> Leaves<'ctx> {
        if let Some(found) = self.leaves.get(&value) {
            return found.clone();
        }
        let types = leaf_types(value.get_type()).expect("a split value has leaves");
        match value.as_instruction_value() {
            Some(def) => self.position_after(def),
            // A parameter's leaves come first; a constant's fold (no
            // instruction), but the builder still needs a position.
            None => {
                let entry = self
                    .function
                    .get_first_basic_block()
                    .expect("an entry block");
                self.position_at_start(entry);
            }
        }
        let leaves: Leaves = types
            .into_iter()
            .map(|(path, _)| {
                let leaf = extract(self.builder, value, &path);
                (path, leaf)
            })
            .collect();
        self.leaves.insert(value, leaves.clone());
        leaves
    }

    /// The whole value of `inst`, rebuilt from its leaves for a use that
    /// needs it (C), replacing `inst`'s remaining uses.
    fn rebuild_whole(&mut self, inst: InstructionValue<'ctx>) {
        let value = value_of(inst);
        let leaves = self
            .leaves
            .get(&value)
            .cloned()
            .expect("a split value has leaves");
        match inst.get_opcode() {
            InstructionOpcode::Phi => {
                let block = inst.get_parent().expect("in a block");
                let first = block
                    .get_instructions()
                    .find(|i| i.get_opcode() != InstructionOpcode::Phi)
                    .expect("a block ends with a terminator");
                self.builder.position_before(&first);
            }
            _ => self.builder.position_before(&inst),
        }
        let mut whole = undef(value.get_type());
        for (path, leaf) in leaves {
            whole = insert(self.builder, whole, &path, leaf);
        }
        replace_uses(value, whole);
    }

    /// The address of the leaf at `path` inside the `ty` at `ptr`: struct
    /// fields by `getelementptr`, array elements by their byte offset
    /// (inkwell's array `getelementptr` is `unsafe`).
    fn address(
        &self,
        ptr: inkwell::values::PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        path: &[u32],
    ) -> inkwell::values::PointerValue<'ctx> {
        let (mut at, mut ty) = (ptr, ty);
        for &i in path {
            match ty {
                BasicTypeEnum::StructType(s) => {
                    at = self
                        .builder
                        .build_struct_gep(s, at, i, "f")
                        .expect("a field");
                    ty = s.get_field_type_at_index(i).expect("a field");
                }
                BasicTypeEnum::ArrayType(a) => {
                    let element = a.get_element_type();
                    at = self.offset(at, element, i);
                    ty = element;
                }
                _ => unreachable!("a path leads through structs and arrays"),
            }
        }
        at
    }

    /// `at` moved `i` elements of type `element` along.
    fn offset(
        &self,
        at: inkwell::values::PointerValue<'ctx>,
        element: BasicTypeEnum<'ctx>,
        i: u32,
    ) -> inkwell::values::PointerValue<'ctx> {
        if i == 0 {
            return at;
        }
        let i64_type = self.context.i64_type();
        let b = self.builder;
        let size = match byte_size(element) {
            Some(n) => i64_type.const_int(n * i as u64, false),
            None => {
                let one = element.size_of().expect("a sized element");
                b.build_int_mul(one, i64_type.const_int(i as u64, false), "size")
                    .expect(POSITIONED)
            }
        };
        let base = b.build_ptr_to_int(at, i64_type, "base").expect(POSITIONED);
        let address = b.build_int_add(base, size, "address").expect(POSITIONED);
        b.build_int_to_ptr(
            address,
            self.context.ptr_type(Default::default()),
            "element",
        )
        .expect(POSITIONED)
    }

    fn position_after(&self, def: InstructionValue<'ctx>) {
        let block = def.get_parent().expect("in a block");
        let next = match def.get_opcode() {
            InstructionOpcode::Phi => block
                .get_instructions()
                .find(|i| i.get_opcode() != InstructionOpcode::Phi),
            _ => def.get_next_instruction(),
        };
        match next {
            Some(next) => self.builder.position_before(&next),
            None => self.builder.position_at_end(block),
        }
    }

    fn position_at_start(&self, block: BasicBlock<'ctx>) {
        match block.get_first_instruction() {
            Some(first) => self.builder.position_before(&first),
            None => self.builder.position_at_end(block),
        }
    }
}
/// The function's blocks, reachable ones in reverse post-order (each
/// value's definition before its uses), then any unreachable ones.
fn order(function: FunctionValue<'_>) -> Vec<BasicBlock<'_>> {
    let Some(entry) = function.get_first_basic_block() else {
        return Vec::new();
    };
    let mut seen = HashSet::new();
    let mut post = Vec::new();
    // An explicit stack: (block, successors still to visit).
    let mut stack = vec![(entry, successors(entry))];
    seen.insert(entry);
    while let Some((block, next)) = stack.last_mut() {
        match next.pop() {
            Some(succ) if seen.insert(succ) => {
                let succs = successors(succ);
                stack.push((succ, succs));
            }
            Some(_) => {}
            None => {
                post.push(*block);
                stack.pop();
            }
        }
    }
    post.reverse();
    for block in function.get_basic_blocks() {
        if !seen.contains(&block) {
            post.push(block);
        }
    }
    post
}

/// The blocks `block`'s terminator may branch to, in reverse order (popped last first).
fn successors(block: BasicBlock<'_>) -> Vec<BasicBlock<'_>> {
    let Some(term) = block.get_terminator() else {
        return Vec::new();
    };
    let mut out: Vec<BasicBlock> = (0..term.get_num_operands())
        .filter_map(|i| term.get_operand(i).and_then(|o| o.block()))
        .collect();
    out.reverse();
    out
}

/// The leaves of `inst`'s result type, if it has a struct result to split.
fn result_leaves(inst: InstructionValue<'_>) -> Option<Vec<(Path, BasicTypeEnum<'_>)>> {
    let ty = BasicTypeEnum::try_from(inst.get_type()).ok()?;
    leaf_types(ty)
}

/// `inst`'s result type, for an instruction with a value.
fn basic_type(inst: InstructionValue<'_>) -> BasicTypeEnum<'_> {
    BasicTypeEnum::try_from(inst.get_type()).expect("an instruction with a value")
}

/// `inst`'s result as a value.
fn value_of(inst: InstructionValue<'_>) -> BasicValueEnum<'_> {
    BasicValueEnum::try_from(inst.as_any_value_enum()).expect("an instruction with a value")
}

/// Operand `i` of `inst`, a value.
fn operand(inst: InstructionValue<'_>, i: u32) -> BasicValueEnum<'_> {
    inst.get_operand(i)
        .and_then(|o| o.value())
        .expect("the operand is a value")
}

/// An undefined value of the struct or array type `ty`, to build on.
fn undef(ty: BasicTypeEnum<'_>) -> BasicValueEnum<'_> {
    match ty {
        BasicTypeEnum::StructType(s) => s.get_undef().into(),
        BasicTypeEnum::ArrayType(a) => a.get_undef().into(),
        _ => unreachable!("only structs and arrays are rebuilt"),
    }
}

/// Sets a leaf access's alignment: the whole access's, at most the
/// leaf's size (a leaf's offset is a multiple of its size).
fn aligned(access: InstructionValue<'_>, whole: u32, leaf: BasicTypeEnum<'_>) {
    let own = byte_size(leaf).map_or(whole, |n| n.max(1) as u32);
    let align = whole.min(own).max(1);
    access
        .set_alignment(align.next_power_of_two())
        .expect("loads and stores take an alignment");
}

/// The size in bytes of a scalar type on x86-64 (`None` for aggregates).
fn byte_size(ty: BasicTypeEnum<'_>) -> Option<u64> {
    match ty {
        BasicTypeEnum::IntType(i) => Some(u64::from(i.get_bit_width()).div_ceil(8)),
        BasicTypeEnum::FloatType(f) => Some(if f == f.get_context().f32_type() {
            4
        } else {
            8
        }),
        BasicTypeEnum::PointerType(_) => Some(8),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    /// The body of the function defining `symbol`.
    fn body<'a>(ir: &'a str, symbol: &str) -> &'a str {
        let start = ir
            .find(&format!("define {symbol}"))
            .or_else(|| {
                ir.lines()
                    .find(|l| l.starts_with("define") && l.contains(symbol))
                    .and_then(|l| ir.find(l))
            })
            .unwrap_or_else(|| panic!("no {symbol} in {ir}"));
        let end = ir[start..].find("\n}\n").map_or(ir.len(), |e| start + e);
        &ir[start..end]
    }

    /// Struct-typed instructions in `text`: whole-value loads, stores,
    /// inserts, extracts, `phi`s and `select`s.
    fn struct_ops(text: &str) -> Vec<&str> {
        text.lines()
            .filter(|l| {
                let l = l.trim();
                // Overflow intrinsics' `{ iN, i1 }` results are read as LLVM expects.
                let overflow = l.contains("extractvalue { i") && l.contains(", i1 }");
                !overflow
                    && (l.contains("insertvalue ")
                        || l.contains("extractvalue ")
                        || l.contains(" = load %")
                        || l.contains(" = load {")
                        || l.starts_with("store %")
                        || l.starts_with("store {")
                        || l.contains(" = phi %")
                        || l.contains(" = phi {")
                        || (l.contains(" = select ") && (l.contains(", %\"") || l.contains("{ "))))
            })
            .collect()
    }

    const P: &str = "struct P { x: i64, y: i64 }\n";

    #[test]
    fn fields_are_read_built_and_copied_without_struct_values() {
        let ir = ir(&format!(
            "{P}fun make(n: i64): P = P {{ x: n, y: n + 1 }};\n\
             fun main() {{ let p = make(1); let q = p; println(q.x + q.y); }}"
        ));
        assert_eq!(
            struct_ops(body(&ir, "void @lugha_fn_main")),
            Vec::<&str>::new(),
            "{ir}"
        );
        assert_eq!(
            struct_ops(body(&ir, "void @lugha_fn_make")),
            Vec::<&str>::new(),
            "{ir}"
        );
    }

    #[test]
    fn an_if_producing_a_struct_joins_each_field() {
        let ir = ir(&format!(
            "{P}fun pick(c: bool): P = if c {{ P {{ x: 1, y: 2 }} }} else {{ P {{ x: 3, y: 4 }} }};\n\
             fun main() {{ println(pick(true).x); }}"
        ));
        let pick = body(&ir, "void @lugha_fn_pick");
        assert_eq!(struct_ops(pick), Vec::<&str>::new(), "{pick}");
    }

    #[test]
    fn enum_payloads_and_options_are_fields_too() {
        let ir = ir("enum E { A(i64), B(i64, i64), C }\n\
             fun e(n: i64): E = if n > 0 { E.B(n, n) } else { E.C };\n\
             fun o(n: i64): Option<i64> = if n > 0 { Some(n) } else { None };\n\
             fun main() { let s = match e(2) { B(a, b) => a + b, A(a) => a, C => 0 }; \
             let t = match o(1) { Some(v) => v, None => 0 }; println(s + t); }");
        for f in [
            "void @lugha_fn_e",
            "void @lugha_fn_o",
            "void @lugha_fn_main",
        ] {
            let b = body(&ir, f);
            assert_eq!(struct_ops(b), Vec::<&str>::new(), "{b}");
        }
    }

    #[test]
    fn a_struct_parameter_becomes_its_fields() {
        // Function values are `{ ptr, ptr }`: a function taking one takes two pointers.
        let ir = ir(&format!(
            "{P}fun apply(f: fun(i64): i64, n: i64): i64 = f(n);\n\
             fun main() {{ println(apply(fun(x: i64): i64 = x + 1, 2)); println(P {{ x: 1, y: 2 }} == P {{ x: 1, y: 2 }}); }}"
        ));
        let apply = ir
            .lines()
            .find(|l| l.starts_with("define") && l.contains("@lugha_fn_apply("))
            .expect("apply is defined");
        // The function value's code and environment, then `n`.
        let params =
            &apply[apply.find('(').expect("a parameter list")..=apply.find(')').expect("closed")];
        assert!(
            params.starts_with("(ptr %") && params.contains(", ptr %"),
            "{apply}"
        );
        assert!(
            params.contains(", i64 %") && !params.contains('{'),
            "{apply}"
        );
        assert!(
            !ir.contains("{ ptr, ptr } %"),
            "no function value passed whole: {ir}"
        );
    }

    #[test]
    fn rewritten_functions_leave_no_old_declarations() {
        let ir = ir(&format!(
            "{P}fun make(): P = P {{ x: 1, y: 2 }};\nfun main() {{ println(make().x); }}"
        ));
        assert!(!ir.contains("lugha_old_"), "{ir}");
    }
}
