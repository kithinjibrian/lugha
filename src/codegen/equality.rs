//! `==` (v2 draft §4, §5; v17, milestone 144): numbers, `bool`, `char` and
//! handles compare inline and strings through the runtime; structs, enums,
//! tuples, `Option` and arrays call `Eq`'s `eq` for their type, its own or
//! its derive block's (`prelude/eq.la`), which stops at the first difference.

use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use super::lower::{Lowerer, POSITIONED};
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
            // `()` has one value (v17, milestone 107).
            Type::Unit => self.context.bool_type().const_int(1, false),
            Type::String => self.string_binary(BinOp::Eq, a, b).into_int_value(),
            // Handles are equal when they are the same C object (v16).
            Type::Handle(_) => self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::EQ,
                    a.into_pointer_value(),
                    b.into_pointer_value(),
                    "eq",
                )
                .expect(POSITIONED),
            // Everything else through `Eq` (v17, milestone 144): the type's
            // own `eq`, or its derive block's.
            Type::Struct(_, _) | Type::Enum(_, _) | Type::Option(_) | Type::Array(_) => {
                self.eq_call(a, b, ty)
            }
            // Numbers and `bool`: `f64` is ordered `==`, so `NaN != NaN`.
            _ => self.compare(BinOp::Eq, ty.clone(), a, b),
        }
    }

    /// Payload `index` of the enum at `slot`, through its pointer if indirect.
    pub(super) fn load_payload(
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
    fn structs_compare_through_their_eq_block() {
        let ir = ir("struct P { xs: i64[], s: string, ok: bool, f: f64 }\n\
             fun main() { let p = P { xs: [1], s: \"a\", ok: true, f: 1.5 }; let a = p == p; let b = p != p; }");
        let eq = "@\"lugha_fn_$prelude/Eq@struct.eq<P>\"(";
        assert_eq!(
            ir.matches(&format!("define internal zeroext i1 {eq}"))
                .count(),
            1,
            "{ir}"
        );
        let main = body(&ir, "lugha_fn_main");
        assert_eq!(
            main.matches(&format!("call zeroext i1 {eq}")).count(),
            2,
            "{ir}"
        );
        assert!(
            main.contains("xor i1") || main.contains("icmp eq i1"),
            "!= negates: {ir}"
        );
        // Each field through its own `eq`: the array's, then inline ones.
        assert!(
            ir.contains("define internal zeroext i1 @\"lugha_fn_$prelude/Eq@array.eq<i64>\"("),
            "{ir}"
        );
        assert!(
            ir.contains("@lugha_rt_str_eq(") && ir.contains("fcmp oeq"),
            "{ir}"
        );
    }

    #[test]
    fn arrays_compare_lengths_before_elements() {
        let ir = ir("fun main() { let a = [1, 2]; let e = a == [1, 3]; }");
        let helper = body(&ir, "\"lugha_fn_$prelude/Eq@array.eq<i64>\"");
        let lengths = helper
            .find("icmp ne i64")
            .or_else(|| helper.find("icmp eq i64"));
        let looped = helper.find("br label").expect("a loop");
        assert!(lengths.expect("lengths compared") < looped, "{ir}");
    }

    #[test]
    fn enums_compare_their_variants_first() {
        let ir = ir("enum L { N, C(i64, L) }\n\
             fun main() { let l = L.C(1, L.N); let e = l == l; let o: Option<L> = None; let f = o == None; }");
        let enum_eq = body(&ir, "\"lugha_fn_$prelude/Eq@enum.eq<L>\"");
        assert!(
            enum_eq.contains("same.variant") && enum_eq.contains("switch i32"),
            "{ir}"
        );
        // A payload of type `L` compares through `L`'s `eq` again.
        assert!(
            body(&ir, "\"lugha_fn_$prelude/$same<L>\"").contains("Eq@enum.eq<L>"),
            "{ir}"
        );
        assert!(
            ir.contains("define internal zeroext i1 @\"lugha_fn_$prelude/Eq@enum.eq<Option<L>>\"("),
            "{ir}"
        );
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
