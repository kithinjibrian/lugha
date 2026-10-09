//! Calls into the C runtime (`runtime/lugha_rt.c`, spec §9): intrinsics,
//! panics, and string literals laid out as the runtime expects (spec §7).

use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue, ValueKind,
};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::Value;
use crate::ast::Expr;
use crate::check::Type;

/// Every runtime function codegen may call; `lugha_rt.c` defines them all.
pub const RUNTIME_SYMBOLS: [&str; 15] = [
    "lugha_rt_init",
    "lugha_rt_alloc",
    "lugha_rt_panic",
    "lugha_rt_print_i32",
    "lugha_rt_print_i64",
    "lugha_rt_print_u8",
    "lugha_rt_print_f64",
    "lugha_rt_print_bool",
    "lugha_rt_print_str",
    "lugha_rt_print_newline",
    "lugha_rt_to_string_i32",
    "lugha_rt_to_string_i64",
    "lugha_rt_to_string_u8",
    "lugha_rt_to_string_f64",
    "lugha_rt_to_string_bool",
];

/// Interned string-literal globals and the source file name, per module.
#[derive(Default)]
pub(super) struct Constants<'ctx> {
    strings: HashMap<String, PointerValue<'ctx>>,
    file: Option<PointerValue<'ctx>>,
}

/// The runtime's name for a primitive or string type, as in `lugha_rt_print_<name>`.
fn suffix(ty: Type) -> &'static str {
    match ty {
        Type::I32 => "i32",
        Type::I64 => "i64",
        Type::U8 => "u8",
        Type::F64 => "f64",
        Type::Bool => "bool",
        Type::String => "str",
        _ => unreachable!("checked: intrinsics take primitives or strings"),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// The runtime function `name`, declared on first use. `bool` and `u8`
    /// cross as zero-extended `i32`, so no C parameter-extension rules apply.
    fn runtime(&self, name: &str, params: &[Type], returns: Option<Type>) -> FunctionValue<'ctx> {
        if let Some(function) = self.module.get_function(name) {
            return function;
        }
        let abi = |ty: Type| -> BasicMetadataTypeEnum<'ctx> {
            match ty {
                Type::Bool | Type::U8 => self.context.i32_type().into(),
                Type::String => self.context.ptr_type(AddressSpace::default()).into(),
                _ => super::value::llvm_type(self.context, ty).into(),
            }
        };
        let params: Vec<_> = params.iter().map(|&ty| abi(ty)).collect();
        let fn_type = match returns {
            Some(Type::String) => self
                .context
                .ptr_type(AddressSpace::default())
                .fn_type(&params, false),
            Some(ty) => super::value::llvm_type(self.context, ty).fn_type(&params, false),
            None => self.context.void_type().fn_type(&params, false),
        };
        self.module
            .add_function(name, fn_type, Some(Linkage::External))
    }

    /// Calls `lugha_rt_init` (it runs `GC_INIT()`), first thing in the C `main`.
    pub(super) fn call_runtime_init(&self) {
        let init = self.runtime("lugha_rt_init", &[], None);
        self.builder.build_call(init, &[], "").expect(POSITIONED);
    }

    /// Widens `bool`/`u8` to the runtime's `i32` parameters.
    fn abi_value(&self, ty: Type, value: BasicValueEnum<'ctx>) -> BasicMetadataValueEnum<'ctx> {
        match ty {
            Type::Bool | Type::U8 => {
                let i32_type = self.context.i32_type();
                self.builder
                    .build_int_z_extend(value.into_int_value(), i32_type, "abi")
                    .expect(POSITIONED)
                    .into()
            }
            _ => value.into(),
        }
    }

    /// A string literal: a private constant `{ i64 len, [len+1 x i8] }` with a
    /// trailing NUL; the value is a pointer to it (spec §7). Shared per text.
    pub(super) fn string_literal(&mut self, text: &str) -> PointerValue<'ctx> {
        if let Some(&ptr) = self.constants.strings.get(text) {
            return ptr;
        }
        let len = self.context.i64_type().const_int(text.len() as u64, false);
        let bytes = self.context.const_string(text.as_bytes(), true);
        let value = self
            .context
            .const_struct(&[len.into(), bytes.into()], false);
        let global = self.module.add_global(value.get_type(), None, "str");
        global.set_initializer(&value);
        global.set_constant(true);
        global.set_linkage(Linkage::Private);
        global.set_unnamed_addr(true);
        let ptr = global.as_pointer_value();
        self.constants.strings.insert(text.to_string(), ptr);
        ptr
    }

    /// `print`, `println`, `to_string` or `panic` (spec §5).
    pub(super) fn intrinsic(
        &mut self,
        call: &Expr,
        name: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let arg = match args.first() {
            Some(arg) => {
                let ty = self.ty(arg);
                Some((ty, self.get(arg, ty)?))
            }
            None => None,
        };
        match (name, arg) {
            ("print" | "println", arg) => {
                if let Some((ty, value)) = arg {
                    let print =
                        self.runtime(&format!("lugha_rt_print_{}", suffix(ty)), &[ty], None);
                    let value = self.abi_value(ty, value);
                    self.builder
                        .build_call(print, &[value], "")
                        .expect(POSITIONED);
                }
                if name == "println" {
                    let newline = self.runtime("lugha_rt_print_newline", &[], None);
                    self.builder.build_call(newline, &[], "").expect(POSITIONED);
                }
                Ok(Value::Void)
            }
            ("to_string", Some((ty, value))) => {
                let convert = self.runtime(
                    &format!("lugha_rt_to_string_{}", suffix(ty)),
                    &[ty],
                    Some(Type::String),
                );
                let value = self.abi_value(ty, value);
                let call = self
                    .builder
                    .build_call(convert, &[value], "str")
                    .expect(POSITIONED);
                match call.try_as_basic_value() {
                    ValueKind::Basic(result) => Ok(Value::Val(result)),
                    ValueKind::Instruction(_) => unreachable!("to_string returns a string"),
                }
            }
            ("panic", Some((_, message))) => {
                self.panic_at(message, call.span.start);
                Ok(Value::Never)
            }
            _ => unreachable!("checked: E0405 enforces intrinsic arity"),
        }
    }

    /// Calls `lugha_rt_panic(message, file, line, col)` for the source offset
    /// `at`, then continues in a dead block: the call never returns.
    pub(super) fn panic_at(&mut self, message: BasicValueEnum<'ctx>, at: usize) {
        let file = match self.constants.file {
            Some(file) => file,
            None => {
                let name = self.source.name.clone();
                let file = self
                    .builder
                    .build_global_string_ptr(&name, "file")
                    .expect(POSITIONED)
                    .as_pointer_value();
                self.constants.file = Some(file);
                file
            }
        };
        let (line, col) = self.source.line_col(at);
        let i64_type = self.context.i64_type();
        let panic = self.runtime(
            "lugha_rt_panic",
            &[Type::String, Type::String, Type::I64, Type::I64],
            None,
        );
        let args = [
            message.into(),
            file.into(),
            i64_type.const_int(line as u64, false).into(),
            i64_type.const_int(col as u64, false).into(),
        ];
        self.builder.build_call(panic, &args, "").expect(POSITIONED);
        self.builder.build_unreachable().expect(POSITIONED);
        self.start_dead_block("after.panic");
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn string_literals_are_length_prefixed_constants() {
        let ir = ir("fun main() { let s = \"hey\"; let t = \"hey\"; }");
        assert!(
            ir.contains(
                "private unnamed_addr constant { i64, [4 x i8] } { i64 3, [4 x i8] c\"hey\\00\" }"
            ),
            "{ir}"
        );
        assert_eq!(
            ir.matches("c\"hey\\00\"").count(),
            1,
            "identical literals share one global: {ir}"
        );
    }

    #[test]
    fn println_calls_the_typed_printer_then_a_newline() {
        let ir = ir("fun main() { println(1); let b: u8 = 2; print(b); }");
        let i64_call = ir
            .find("call void @lugha_rt_print_i64(i64 1)")
            .expect("print_i64");
        let newline = ir
            .find("call void @lugha_rt_print_newline()")
            .expect("newline");
        assert!(i64_call < newline, "{ir}");
        assert!(ir.contains("zext i8"), "u8 widens to i32: {ir}");
    }

    #[test]
    fn panic_passes_its_location_and_main_initialises_the_runtime() {
        let ir = ir("fun main() {\n    panic(\"boom\");\n}");
        assert!(
            ir.contains("@lugha_rt_panic(ptr @str, ptr @file, i64 2, i64 5)"),
            "{ir}"
        );
        let main = &ir[ir.find("define i32 @main()").unwrap()..];
        let init = main.find("call void @lugha_rt_init()").expect("init");
        assert!(
            init < main.find("call void @lugha_fn_main()").unwrap(),
            "{main}"
        );
    }
}
