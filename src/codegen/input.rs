//! Input and interpolation in codegen (v3 draft §5, §9; milestone 13):
//! `"a {x} b"` as a chain of `lugha_rt_str_concat`, and the intrinsics
//! `args`, `parse_i64` and `parse_f64` through the runtime.

use inkwell::IntPredicate;
use inkwell::values::{BasicValueEnum, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::Value;
use crate::ast::{Expr, Part};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `"a {x} b"`: each part as a string, left to right, concatenated.
    pub(super) fn interpolation(
        &mut self,
        parts: &[Part],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let mut joined: Option<BasicValueEnum<'ctx>> = None;
        for part in parts {
            let piece: BasicValueEnum<'ctx> = match part {
                Part::Text(text) if text.is_empty() => continue,
                Part::Text(text) => self.string_literal(text).into(),
                Part::Expr(e) => {
                    let ty = self.ty(e);
                    let value = self.get(e, &ty)?;
                    if ty == Type::String {
                        value
                    } else {
                        self.text_of(&ty, value)
                    }
                }
            };
            joined = Some(match joined {
                None => piece,
                Some(left) => self.call_runtime_value(
                    "lugha_rt_str_concat",
                    &[Type::String, Type::String],
                    &[left, piece],
                ),
            });
        }
        Ok(joined.unwrap_or_else(|| self.string_literal("").into()))
    }

    /// A number or `bool` as text, exactly as `to_string` gives it.
    fn text_of(&mut self, ty: &Type, value: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        let suffix = match ty {
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::U8 => "u8",
            Type::F64 => "f64",
            _ => "bool",
        };
        let name = format!("lugha_rt_to_string_{suffix}");
        let convert = self.runtime(&name, std::slice::from_ref(ty), Some(Type::String));
        let arg = self.abi_value(ty, value);
        let call = self
            .builder
            .build_call(convert, &[arg], "text")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(text) => text,
            ValueKind::Instruction(_) => unreachable!("to_string returns a string"),
        }
    }

    /// Calls runtime function `name`, which returns a string-like pointer.
    fn call_runtime_value(
        &mut self,
        name: &str,
        params: &[Type],
        args: &[BasicValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let function = self.runtime(name, params, Some(Type::String));
        let args: Vec<_> = args.iter().map(|a| (*a).into()).collect();
        let call = self
            .builder
            .build_call(function, &args, "rt")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(value) => value,
            ValueKind::Instruction(_) => unreachable!("`{name}` returns a value"),
        }
    }

    /// `args()`, `parse_i64(s)` or `parse_f64(s)`.
    pub(super) fn input_intrinsic(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        if name == "args" {
            return Ok(Value::Val(self.call_runtime_value(
                "lugha_rt_args",
                &[],
                &[],
            )));
        }
        let text = self.get(&args[0], &Type::String)?;
        let ty = if name == "parse_i64" {
            Type::I64
        } else {
            Type::F64
        };
        // The runtime writes the number here and returns whether it parsed.
        let out = self.entry_alloca(&ty, "parsed");
        let zero: BasicValueEnum = match ty {
            Type::I64 => self.context.i64_type().const_zero().into(),
            _ => self.context.f64_type().const_zero().into(),
        };
        self.builder.build_store(out, zero).expect(POSITIONED);
        let parse = self.runtime(
            &format!("lugha_rt_{name}"),
            &[Type::String, Type::String],
            Some(Type::I32),
        );
        let call = self
            .builder
            .build_call(parse, &[text.into(), out.into()], "ok")
            .expect(POSITIONED);
        let ValueKind::Basic(ok) = call.try_as_basic_value() else {
            unreachable!("the parsers return a flag")
        };
        let llvm = super::value::llvm_type(self.context, &ty);
        let number = self
            .builder
            .build_load(llvm, out, "number")
            .expect(POSITIONED);
        let option = Type::Option(Box::new(ty));
        let some = self.construct_values(&option, "Some", vec![number]);
        let none = self.construct_values(&option, "None", Vec::new());
        let zero32 = self.context.i32_type().const_zero();
        let parsed = self
            .builder
            .build_int_compare(IntPredicate::NE, ok.into_int_value(), zero32, "parsed")
            .expect(POSITIONED);
        let value = self
            .builder
            .build_select(parsed, some, none, "option")
            .expect(POSITIONED);
        Ok(Value::Val(value))
    }

    /// `s.slice(from, to)` (v4): a new string through `lugha_rt_str_slice`,
    /// which panics at `at` unless `0 <= from <= to <= s.len`.
    pub(super) fn string_slice(
        &mut self,
        base: &Expr,
        args: &[Expr],
        at: usize,
    ) -> Result<Value<'ctx>, CodegenError> {
        let text = self.get(base, &Type::String)?;
        let from = self.get(&args[0], &Type::I64)?;
        let to = self.get(&args[1], &Type::I64)?;
        let file = self.file_name(at);
        let (line, col) = self.source.line_col(at);
        let i64_type = self.context.i64_type();
        let params = [
            Type::String,
            Type::I64,
            Type::I64,
            Type::String,
            Type::I64,
            Type::I64,
        ];
        let slice = self.runtime("lugha_rt_str_slice", &params, Some(Type::String));
        let args = [
            text.into(),
            from.into(),
            to.into(),
            file.into(),
            i64_type.const_int(line as u64, false).into(),
            i64_type.const_int(col as u64, false).into(),
        ];
        let call = self
            .builder
            .build_call(slice, &args, "slice")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(value) => Ok(Value::Val(value)),
            ValueKind::Instruction(_) => unreachable!("lugha_rt_str_slice returns a string"),
        }
    }
}
