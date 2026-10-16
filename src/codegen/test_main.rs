//! The entry point of a test executable (v16 draft decision 84, milestone
//! 84): `main` runs the test named by `LUGHA_TEST` and returns 0, or says
//! no test has that name and returns 2. A panic exits with 101 as always.

use inkwell::IntPredicate;
use inkwell::values::ValueKind;

use super::lower::{Lowerer, POSITIONED};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `main(argc, argv)` for the tests `names` of the main file, in order.
    pub(super) fn c_test_main(&mut self, names: &[String]) {
        let i32_type = self.context.i32_type();
        let ptr = self.context.ptr_type(Default::default());
        let main_type = i32_type.fn_type(&[i32_type.into(), ptr.into()], false);
        let main = self.module.add_function("main", main_type, None);
        self.builder
            .position_at_end(self.context.append_basic_block(main, "entry"));
        let argc = main.get_nth_param(0).expect("main takes argc");
        let argv = main.get_nth_param(1).expect("main takes argv");
        let init = self.runtime("lugha_rt_init", &[Type::I32, Type::String], None);
        self.builder
            .build_call(init, &[argc.into(), argv.into()], "")
            .expect(POSITIONED);
        // `ptr` stands for a C string: opaque pointers are all alike.
        let is = self.runtime("lugha_rt_test_is", &[Type::String], Some(Type::I32));
        for name in names {
            let test = self
                .functions
                .get(name)
                .expect("checked: a listed test is a declared function")
                .function;
            let text = self
                .builder
                .build_global_string_ptr(name, "test")
                .expect(POSITIONED)
                .as_pointer_value();
            let site = self
                .builder
                .build_call(is, &[text.into()], "is")
                .expect(POSITIONED);
            let ValueKind::Basic(matched) = site.try_as_basic_value() else {
                unreachable!("lugha_rt_test_is returns an int")
            };
            let matched = matched.into_int_value();
            let yes = self
                .builder
                .build_int_compare(IntPredicate::NE, matched, i32_type.const_zero(), "yes")
                .expect(POSITIONED);
            let run = self.context.append_basic_block(main, "run");
            let next = self.context.append_basic_block(main, "next");
            self.builder
                .build_conditional_branch(yes, run, next)
                .expect(POSITIONED);
            self.builder.position_at_end(run);
            self.builder.build_call(test, &[], "").expect(POSITIONED);
            self.builder
                .build_return(Some(&i32_type.const_zero()))
                .expect(POSITIONED);
            self.builder.position_at_end(next);
        }
        let unknown = self.runtime("lugha_rt_test_unknown", &[], Some(Type::I32));
        let site = self
            .builder
            .build_call(unknown, &[], "code")
            .expect(POSITIONED);
        let ValueKind::Basic(code) = site.try_as_basic_value() else {
            unreachable!("lugha_rt_test_unknown returns an int")
        };
        let code = code.into_int_value();
        self.builder.build_return(Some(&code)).expect(POSITIONED);
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::{ir, test_ir};

    #[test]
    fn a_test_executable_runs_the_named_test() {
        let src = "@test fun first() { assert(true); }\n@test fun second() {}\nfun helper() {}";
        let ir = test_ir(src);
        assert!(ir.contains("define void @lugha_fn_first()"), "{ir}");
        assert!(ir.contains("c\"second\\00\""), "{ir}");
        assert!(ir.contains("call i32 @lugha_rt_test_is"), "{ir}");
        assert!(ir.contains("call i32 @lugha_rt_test_unknown"), "{ir}");
    }

    #[test]
    fn other_builds_leave_tests_out() {
        let ir = ir("fun main() {}\n@test fun first() {}");
        assert!(!ir.contains("lugha_fn_first"), "{ir}");
        assert!(!ir.contains("lugha_rt_test_is"), "{ir}");
    }
}
