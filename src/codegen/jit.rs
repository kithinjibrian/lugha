use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;
use crate::codegen::Codegen;

type MainFn = unsafe extern "C" fn() -> i64;

pub fn run_jit(codegen: &Codegen) -> i64 {
    let engine = codegen
        .module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("JIT engine creation failed");

    unsafe {
        let main: JitFunction<MainFn> =
            engine.get_function("main").expect("main function not found");
        main.call()
    }
}