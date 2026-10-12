//! Deep `==` (v2 draft §4, §5): numbers and `bool` compare inline, strings
//! through the runtime, and structs, enums, `Option` and arrays through one
//! `lugha_eq_<type>` helper per type, which stops at the first difference.

use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, ValueKind};

use super::copy::mangle;
use super::enums::variants;
use super::heap::element_of;
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::ast::BinOp;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `a == b` for two values of type `ty`, as an `i1`.
    pub(super) fn equal(
        &mut self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> IntValue<'ctx> {
        match ty {
            Type::String => self.string_binary(BinOp::Eq, a, b).into_int_value(),
            Type::Struct(_) | Type::Enum(_) | Type::Option(_) | Type::Array(_) => {
                let helper = self.eq_function(ty);
                let call = self
                    .builder
                    .build_call(helper, &[a.into(), b.into()], "eq")
                    .expect(POSITIONED);
                match call.try_as_basic_value() {
                    ValueKind::Basic(result) => result.into_int_value(),
                    ValueKind::Instruction(_) => unreachable!("eq helpers return i1"),
                }
            }
            // Numbers and `bool`: `f64` is ordered `==`, so `NaN != NaN`.
            _ => self.compare(BinOp::Eq, ty.clone(), a, b),
        }
    }

    /// `lugha_eq_<mangled>(T, T) -> i1`, generated once per type. Declared
    /// before its body, so a recursive enum's helper can call itself.
    fn eq_function(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_eq_{}", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let llvm = llvm_type(self.context, ty);
        let bool_type = self.context.bool_type();
        let function = self.module.add_function(
            &name,
            bool_type.fn_type(&[llvm.into(), llvm.into()], false),
            Some(Linkage::Internal),
        );
        // Build the body elsewhere, then come back to where lowering was.
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let param = |i| function.get_nth_param(i).expect("two parameters");
        let (a, b) = (param(0), param(1));
        let unequal = self.append("unequal");
        match ty {
            Type::Struct(name) => self.eq_fields(a, b, name, unequal),
            Type::Array(_) => self.eq_elements(a, b, ty, unequal),
            _ => self.eq_variants(a, b, ty, unequal),
        }
        // Every check passed.
        let (yes, no) = (bool_type.const_int(1, false), bool_type.const_zero());
        self.builder.build_return(Some(&yes)).expect(POSITIONED);
        self.builder.position_at_end(unequal);
        self.builder.build_return(Some(&no)).expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }

    /// Continues in a new block if `equal`, else jumps to `unequal`.
    fn or_unequal(&self, equal: IntValue<'ctx>, unequal: BasicBlock<'ctx>, name: &str) {
        let next = self.append(name);
        self.builder
            .build_conditional_branch(equal, next, unequal)
            .expect(POSITIONED);
        self.builder.position_at_end(next);
    }

    /// Field by field, in declaration order.
    fn eq_fields(
        &mut self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        name: &str,
        unequal: BasicBlock<'ctx>,
    ) {
        let (a, b) = (a.into_struct_value(), b.into_struct_value());
        for (index, (field, ty)) in self.structs[name].clone().iter().enumerate() {
            let index = u32::try_from(index).expect("field count fits in u32");
            let read = |this: &Self, value: inkwell::values::StructValue<'ctx>| {
                let raw = this
                    .builder
                    .build_extract_value(value, index, field)
                    .expect("field exists");
                // Fields hold `bool` as `i8`, like elements.
                this.loaded_form(raw, ty)
            };
            let (x, y) = (read(self, a), read(self, b));
            let equal = self.equal(x, y, ty);
            self.or_unequal(equal, unequal, "field.eq");
        }
    }

    /// The lengths, then each element up to `.len`; spare capacity is never read.
    fn eq_elements(
        &mut self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        ty: &Type,
        unequal: BasicBlock<'ctx>,
    ) {
        let (a, b) = (a.into_pointer_value(), b.into_pointer_value());
        let element = element_of(ty);
        let size = self.element_size(&element);
        let len = self.length(a);
        let same = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, len, self.length(b), "same.len")
            .expect(POSITIONED);
        self.or_unequal(same, unequal, "elements");
        self.count_loop(len, |this, i| {
            let x = this.load_element(this.element_address(a, i, size), &element);
            let y = this.load_element(this.element_address(b, i, size), &element);
            let equal = this.equal(x, y, &element);
            this.or_unequal(equal, unequal, "element.eq");
            Ok(())
        })
        .expect("equality loops only lower comparisons");
    }

    /// The tags, then the payloads of the shared variant.
    fn eq_variants(
        &mut self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
        ty: &Type,
        unequal: BasicBlock<'ctx>,
    ) {
        let slot = |this: &Self, value, name| {
            let slot = this.entry_alloca(ty, name);
            this.builder.build_store(slot, value).expect(POSITIONED);
            slot
        };
        let (a, b) = (slot(self, a, "lhs"), slot(self, b, "rhs"));
        let tag = self.tag(ty, a);
        let same = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, tag, self.tag(ty, b), "same.tag")
            .expect(POSITIONED);
        self.or_unequal(same, unequal, "variants");
        let done = self.append("payloads.eq");
        let i32_type = self.context.i32_type();
        let mut cases = Vec::new();
        for (index, (_, payloads)) in variants(ty, &self.enums).into_iter().enumerate() {
            if payloads.is_empty() {
                continue;
            }
            let block = self.append("variant");
            let tag = u64::try_from(index).expect("variant count fits in u64");
            cases.push((i32_type.const_int(tag, false), block));
            let resume = self.current_block();
            self.builder.position_at_end(block);
            for (i, payload) in payloads.iter().enumerate() {
                let i = u32::try_from(i).expect("payload count fits in u32");
                let x = self.load_payload(ty, a, &payloads, i, payload);
                let y = self.load_payload(ty, b, &payloads, i, payload);
                let equal = self.equal(x, y, payload);
                self.or_unequal(equal, unequal, "payload.eq");
            }
            self.branch(done);
            self.builder.position_at_end(resume);
        }
        self.builder
            .build_switch(tag, done, &cases)
            .expect(POSITIONED);
        self.builder.position_at_end(done);
    }

    /// Payload `index` of the enum at `slot`, through its pointer if indirect.
    fn load_payload(
        &self,
        ty: &Type,
        slot: PointerValue<'ctx>,
        payloads: &[Type],
        index: u32,
        payload: &Type,
    ) -> BasicValueEnum<'ctx> {
        let mut address = self.payload_address(ty, slot, payloads, index);
        if self.indirect(ty, payload) {
            let ptr = self.context.ptr_type(Default::default());
            address = self
                .builder
                .build_load(ptr, address, "boxed")
                .expect(POSITIONED)
                .into_pointer_value();
        }
        self.load_element(address, payload)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    /// The definition of the function `name`, up to its closing brace.
    fn body<'a>(ir: &'a str, name: &str) -> &'a str {
        let call = format!("@{name}(");
        let header = ir
            .match_indices("define ")
            .map(|(at, _)| at)
            .find(|&at| ir[at..].lines().next().is_some_and(|l| l.contains(&call)))
            .unwrap_or_else(|| panic!("{name} is not defined: {ir}"));
        let start = &ir[header..];
        &start[..start.find("\n}").expect("function ends")]
    }

    #[test]
    fn structs_compare_through_one_helper() {
        let ir = ir("struct P { xs: i64[], s: string, ok: bool, f: f64 }\n\
             fun main() { let p = P { xs: [1], s: \"a\", ok: true, f: 1.5 }; let a = p == p; let b = p != p; }");
        assert_eq!(
            ir.matches("define internal i1 @lugha_eq_1P(").count(),
            1,
            "{ir}"
        );
        let main = body(&ir, "lugha_fn_main");
        assert_eq!(main.matches("call i1 @lugha_eq_1P(").count(), 2, "{ir}");
        assert!(
            main.contains("xor i1") || main.contains("icmp eq i1"),
            "!= negates: {ir}"
        );
        let helper = body(&ir, "lugha_eq_1P");
        assert!(helper.contains("call i1 @lugha_eq_i64_arr("), "{ir}");
        assert!(
            helper.contains("@lugha_rt_str_eq(") && helper.contains("fcmp oeq"),
            "{ir}"
        );
    }

    #[test]
    fn arrays_compare_lengths_before_elements() {
        let ir = ir("fun main() { let a = [1, 2]; let e = a == [1, 3]; }");
        let helper = body(&ir, "lugha_eq_i64_arr");
        let lengths = helper.find("icmp eq i64").expect("lengths compared");
        let looped = helper.find("each.cond").expect("element loop");
        assert!(lengths < looped, "{ir}");
    }

    #[test]
    fn recursive_enums_and_options_call_themselves() {
        let ir = ir("enum L { N, C(i64, L) }\n\
             fun main() { let l = L.C(1, L.N); let e = l == l; let o: Option<L> = None; let f = o == None; }");
        assert!(
            body(&ir, "lugha_eq_1L").contains("call i1 @lugha_eq_1L("),
            "{ir}"
        );
        assert!(
            body(&ir, "lugha_eq_1L_opt").contains("call i1 @lugha_eq_1L("),
            "{ir}"
        );
        assert!(body(&ir, "lugha_eq_1L").contains("switch i32"), "{ir}");
    }

    #[test]
    fn strings_order_through_the_runtime() {
        let ir = ir("fun main() { let a = \"a\" < \"b\"; let b = \"a\" >= \"b\"; }");
        assert_eq!(ir.matches("call i32 @lugha_rt_str_cmp(").count(), 2, "{ir}");
        assert!(
            ir.contains("icmp slt i32") && ir.contains("icmp sge i32"),
            "{ir}"
        );
    }
}
