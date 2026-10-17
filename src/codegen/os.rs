//! The operating system's services in codegen (v15): the `$os.<op>`
//! built-ins that std modules call, each a runtime function that returns
//! a flag and writes its results through out-pointers.

use inkwell::values::BasicValueEnum;

use super::CodegenError;
use super::lower::Lowerer;
use crate::ast::Expr;
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `$os.op(args)`, `call` being the whole call.
    pub(super) fn os_call(
        &mut self,
        call: &Expr,
        op: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let bytes = Type::Array(Box::new(Type::U8));
        Ok(match op {
            // `std.net` (v17, milestone 110).
            _ if op.starts_with("net_") => self.net_call(call, op, args)?,
            "seed" => self.os_seed(),
            // `Some(bytes)` of the variable, or `None` (milestone 64).
            "env" => {
                let name = self.get(&args[0], &Type::String)?;
                let out = self.out_slot(&bytes);
                let found = self.call_flag(
                    "lugha_rt_env",
                    &[Type::String, Type::String],
                    &[name, out.into()],
                );
                let value = self.load_slot(out, &bytes);
                let option = self.ty(call);
                let some = self.construct_values(&option, "Some", vec![value]);
                let none = self.construct_values(&option, "None", Vec::new());
                self.choose(found, some, none)
            }
            // `Ok(bytes)` of the current directory, or `Err(message)`.
            "cwd" => {
                let (out, error) = (self.out_slot(&bytes), self.out_slot(&Type::String));
                let ok = self.call_flag(
                    "lugha_rt_cwd",
                    &[Type::String, Type::String],
                    &[out.into(), error.into()],
                );
                self.result(call, ok, (out, &bytes), error)
            }
            // `std.fs` (milestone 65): the names' bytes, or `Err(message)`.
            "list_dir" => {
                let names = Type::Array(Box::new(bytes.clone()));
                let path = self.get(&args[0], &Type::String)?;
                let (out, error) = (self.out_slot(&names), self.out_slot(&Type::String));
                let params = [Type::String, Type::String, Type::String];
                let ok = self.call_flag(
                    "lugha_rt_list_dir",
                    &params,
                    &[path, out.into(), error.into()],
                );
                self.result(call, ok, (out, &names), error)
            }
            // `Ok(0)` or `Err(message)`.
            "make_dir" | "remove_file" | "remove_dir" | "chdir" => {
                let path = self.get(&args[0], &Type::String)?;
                let (zero, error) = (self.out_slot(&Type::I64), self.out_slot(&Type::String));
                let params = [Type::String, Type::String];
                let ok = self.call_flag(&format!("lugha_rt_{op}"), &params, &[path, error.into()]);
                self.result(call, ok, (zero, &Type::I64), error)
            }
            // 0 nothing, 1 file, 2 folder, 3 anything else.
            "stat_kind" => {
                let path = self.get(&args[0], &Type::String)?;
                let function = self.runtime("lugha_rt_stat_kind", &[Type::String], Some(Type::I64));
                let site = self
                    .builder
                    .build_call(function, &[path.into()], "kind")
                    .expect(super::lower::POSITIONED);
                match site.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(kind) => kind,
                    inkwell::values::ValueKind::Instruction(_) => {
                        unreachable!("stat_kind returns an i64")
                    }
                }
            }
            // `std.process` (milestone 66): `Ok((code, stdout, stderr))` or `Err(reason)`.
            "start" => {
                let cmd = self.get(&args[0], &Type::String)?;
                let list = self.get(&args[1], &Type::Array(Box::new(Type::String)))?;
                let input = self.get(&args[2], &Type::String)?;
                let mode = self.get(&args[3], &Type::I32)?;
                let code = self.out_slot(&Type::I64);
                let (out, err) = (self.out_slot(&bytes), self.out_slot(&bytes));
                let error = self.out_slot(&Type::String);
                let params = [
                    Type::String,
                    Type::String,
                    Type::String,
                    Type::I32,
                    Type::String,
                    Type::String,
                    Type::String,
                    Type::String,
                ];
                let values = [
                    cmd,
                    list,
                    input,
                    mode,
                    code.into(),
                    out.into(),
                    err.into(),
                    error.into(),
                ];
                let ok = self.call_flag("lugha_rt_spawn", &params, &values);
                let result = self.ty(call);
                let crate::check::Type::Enum(_, parts) = &result else {
                    unreachable!("checked: `$os.spawn` gives a `Result`")
                };
                let tuple = parts[0].clone();
                let code64 = self.load_slot(code, &Type::I64).into_int_value();
                let code32 = self
                    .builder
                    .build_int_truncate(code64, self.context.i32_type(), "code")
                    .expect(super::lower::POSITIONED);
                let stdout = self.load_slot(out, &bytes);
                let stderr = self.load_slot(err, &bytes);
                let llvm = super::value::llvm_type(self.context, &tuple).into_struct_type();
                let mut value = llvm.get_undef();
                for (i, part) in [code32.into(), stdout, stderr].into_iter().enumerate() {
                    let index = u32::try_from(i).expect("three elements");
                    value = self
                        .builder
                        .build_insert_value(value, part, index, "outcome")
                        .expect(super::lower::POSITIONED)
                        .into_struct_value();
                }
                let slot = self.entry_alloca(&tuple, "outcome");
                self.builder
                    .build_store(slot, value)
                    .expect(super::lower::POSITIONED);
                self.result(call, ok, (slot, &tuple), error)
            }
            _ => unreachable!("checked: `$os.{op}` is a known built-in"),
        })
    }
}
