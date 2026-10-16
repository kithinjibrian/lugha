//! Channels and `select` in codegen (v17, milestone 91). A `Chan<T>` is a
//! pointer to the runtime's channel; values cross it as `T` in element
//! layout, copied here (a copy site) and moved byte for byte by the
//! runtime. A `select` fills a table of `LughaCase { chan, kind, data, ok }`
//! on the stack, one per `recv`/`send` arm, and `lugha_rt_task_select`
//! says which arm to run: its case index, -1 for `default`, -2 for `after`.

use inkwell::IntPredicate;
use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::{BasicValueEnum, IntValue, PointerValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::Value;
use crate::ast::{Expr, ExprKind, SelectArm, SelectKind};
use crate::check::Type;

/// The case table on the stack and its type.
type Table<'ctx> = (PointerValue<'ctx>, StructType<'ctx>);

/// `lugha_rt_task_select`'s answer for the `default` arm.
const DEFAULT_ARM: i64 = -1;
/// Its answer for the `after` arm.
const TIMED_OUT: i64 = -2;

impl<'ctx> Lowerer<'ctx> {
    /// `Chan.new()` or `Chan.buffered(n)`, `call` being the whole call.
    pub(super) fn chan_new(
        &mut self,
        call: &Expr,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let Type::Chan(element) = self.ty(call) else {
            unreachable!("checked: `Chan.new` makes a `Chan`")
        };
        let i64_type = self.context.i64_type();
        let cap = match args.first() {
            Some(size) => self.get(size, &Type::I64)?,
            None => i64_type.const_zero().into(),
        };
        let size = i64_type.const_int(self.element_size(&element).max(1), false);
        let mut values = vec![cap, size.into()];
        values.extend(self.location(call.span.start));
        Ok(self.call_runtime("lugha_rt_chan_new", &values, true))
    }

    /// `c.send(v)`, `c.recv()`, `c.try_recv()`, `c.close()` or `c.is(d)`.
    pub(super) fn chan_method(
        &mut self,
        call: &Expr,
        base: &Expr,
        name: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        let ty = self.ty(base);
        let Type::Chan(element) = &ty else {
            unreachable!("checked: a channel method has a `Chan` receiver")
        };
        let chan = self.get(base, &ty)?;
        Ok(match name {
            "send" => {
                let data = self.element_value(&args[0], element)?;
                let mut values = vec![chan, data.into()];
                values.extend(self.location(call.span.start));
                self.call_runtime("lugha_rt_chan_send", &values, false);
                Value::Void
            }
            "recv" | "try_recv" => {
                let data = self.element_slot(element);
                let function = format!("lugha_rt_chan_{name}");
                let flag = self.call_runtime(&function, &[chan, data.into()], true);
                let got = self.nonzero(flag);
                Value::Val(self.received(got, data, element))
            }
            "close" => {
                let mut values = vec![chan];
                values.extend(self.location(call.span.start));
                self.call_runtime("lugha_rt_chan_close", &values, false);
                Value::Void
            }
            "is" => {
                let other = self.get(&args[0], &ty)?;
                let same = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        chan.into_pointer_value(),
                        other.into_pointer_value(),
                        "same",
                    )
                    .expect(POSITIONED);
                Value::Val(same.into())
            }
            _ => unreachable!("checked: `Chan` has send, recv, try_recv, close and is"),
        })
    }

    /// `select { arms }`: every channel, sent value and timeout evaluated
    /// top to bottom, then the runtime's pick, then that arm.
    pub(super) fn select(
        &mut self,
        expr: &Expr,
        arms: &[SelectArm],
    ) -> Result<Value<'ctx>, CodegenError> {
        let i64_type = self.context.i64_type();
        let waits = arms.iter().filter(|a| a.kind.channel().is_some()).count();
        // A struct of `waits` cases is laid out as C's array of them.
        let table_type = self.case_table_type(waits);
        let table = self.raw_alloca(table_type.into(), "cases");
        // `after(ms)` sets it; below 0 the runtime waits for ever.
        let mut timeout = i64_type.const_int(-1i64 as u64, true);
        let mut has_default = false;
        // Each arm's case index (or answer), and for `recv` its data slot.
        let mut answers: Vec<(i64, Option<PointerValue<'ctx>>)> = Vec::new();
        for arm in arms {
            let answer = match &arm.kind {
                SelectKind::Recv { .. } | SelectKind::Send(_) => {
                    let index = answers.iter().filter(|(a, _)| *a >= 0).count();
                    let slot = self.case(arm, (table, table_type), index)?;
                    answers.push((i64::try_from(index).expect("arms fit in i64"), slot));
                    continue;
                }
                SelectKind::After(ms) => {
                    timeout = self.get(ms, &Type::I64)?.into_int_value();
                    TIMED_OUT
                }
                SelectKind::Default(_) => {
                    has_default = true;
                    DEFAULT_ARM
                }
            };
            answers.push((answer, None));
        }
        let first = self
            .builder
            .build_pointer_cast(table, self.context.ptr_type(Default::default()), "cases")
            .expect(POSITIONED);
        let mut values: Vec<BasicValueEnum<'ctx>> = vec![
            first.into(),
            i64_type.const_int(waits as u64, false).into(),
            timeout.into(),
            self.context
                .i32_type()
                .const_int(u64::from(has_default), false)
                .into(),
        ];
        values.extend(self.location(expr.span.start));
        let chosen = self
            .call_runtime("lugha_rt_task_select", &values, true)
            .into_int_value();
        self.select_arms(expr, arms, (chosen, (table, table_type), &answers))
    }

    /// The `switch` on the runtime's answer, and each arm's body.
    fn select_arms(
        &mut self,
        expr: &Expr,
        arms: &[SelectArm],
        (chosen, table, answers): (
            IntValue<'ctx>,
            Table<'ctx>,
            &[(i64, Option<PointerValue<'ctx>>)],
        ),
    ) -> Result<Value<'ctx>, CodegenError> {
        let i64_type = self.context.i64_type();
        let rty = self.ty(expr);
        let result = (!matches!(rty, Type::Void | Type::Never))
            .then(|| self.entry_alloca(&rty, "select.value"));
        let end = self.append("select.end");
        let impossible = self.append("select.impossible");
        let blocks: Vec<BasicBlock<'ctx>> =
            arms.iter().map(|_| self.append("select.arm")).collect();
        let cases: Vec<(IntValue<'ctx>, BasicBlock<'ctx>)> = answers
            .iter()
            .zip(&blocks)
            .map(|((answer, _), block)| (i64_type.const_int(*answer as u64, true), *block))
            .collect();
        self.builder
            .build_switch(chosen, impossible, &cases)
            .expect(POSITIONED);
        self.builder.position_at_end(impossible);
        self.builder.build_unreachable().expect(POSITIONED);
        let mut diverges = true;
        for ((arm, block), (answer, slot)) in arms.iter().zip(&blocks).zip(answers) {
            self.builder.position_at_end(*block);
            self.scopes.push();
            if let (SelectKind::Recv { binding, call }, Some(data)) = (&arm.kind, slot)
                && binding.name != "_"
            {
                let option = self.ty(call);
                let Type::Option(element) = &option else {
                    unreachable!("checked: `recv` gives an `Option`")
                };
                let ok = self.case_ok(table, *answer);
                let value = self.received(ok, *data, element);
                let ptr = self.entry_alloca(&option, &binding.name);
                self.builder.build_store(ptr, value).expect(POSITIONED);
                let local = Local {
                    ptr,
                    ty: option.clone(),
                    borrowed: false,
                };
                self.scopes.declare(&binding.name, local);
            }
            let value = self.expr(&arm.body)?;
            self.scopes.pop();
            if let (Some(result), Value::Val(v)) = (result, value) {
                self.builder.build_store(result, v).expect(POSITIONED);
            }
            diverges &= matches!(value, Value::Never);
            self.finish_branch(value, end);
        }
        self.builder.position_at_end(end);
        Ok(match result {
            _ if diverges => Value::Never,
            Some(result) => Value::Val(
                self.builder
                    .build_load(
                        super::value::llvm_type(self.context, &rty),
                        result,
                        "select",
                    )
                    .expect(POSITIONED),
            ),
            None => Value::Void,
        })
    }

    /// Fills case `index` of `table` for a `recv` or `send` arm; for a
    /// `recv`, the slot its value arrives in.
    fn case(
        &mut self,
        arm: &SelectArm,
        table: Table<'ctx>,
        index: usize,
    ) -> Result<Option<PointerValue<'ctx>>, CodegenError> {
        let chan_expr = arm.kind.channel().expect("a recv or send arm");
        let chan_type = self.ty(chan_expr);
        let Type::Chan(element) = &chan_type else {
            unreachable!("checked: E0438 unless on a channel")
        };
        let chan = self.get(chan_expr, &chan_type)?;
        let (kind, data, slot) = match &arm.kind {
            SelectKind::Send(call) => {
                let ExprKind::Call(_, args) = &call.kind else {
                    unreachable!("the parser checked the arm")
                };
                (1, self.element_value(&args[0], element)?, None)
            }
            _ => {
                let data = self.element_slot(element);
                (0, data, Some(data))
            }
        };
        let i64_type = self.context.i64_type();
        let case_type = self.case_type();
        let at = self.case_at(table, index);
        let fields: [BasicValueEnum<'ctx>; 4] = [
            chan,
            i64_type.const_int(kind, false).into(),
            data.into(),
            self.context.i32_type().const_zero().into(),
        ];
        for (i, value) in (0u32..).zip(fields) {
            let field = self
                .builder
                .build_struct_gep(case_type, at, i, "case.field")
                .expect("a case has four fields");
            self.builder.build_store(field, value).expect(POSITIONED);
        }
        Ok(slot)
    }

    /// Whether case `index` received a value rather than a close.
    fn case_ok(&mut self, table: Table<'ctx>, index: i64) -> IntValue<'ctx> {
        let at = self.case_at(table, usize::try_from(index).expect("a case index"));
        let field = self
            .builder
            .build_struct_gep(self.case_type(), at, 3, "case.ok")
            .expect("a case has four fields");
        let ok = self
            .builder
            .build_load(self.context.i32_type(), field, "ok")
            .expect(POSITIONED);
        self.nonzero(ok)
    }

    fn case_at(&self, (table, table_type): Table<'ctx>, index: usize) -> PointerValue<'ctx> {
        let index = u32::try_from(index).expect("arms fit in u32");
        self.builder
            .build_struct_gep(table_type, table, index, "case")
            .expect("a table has a case per arm")
    }

    /// The case table: a struct of `count` cases (at least one).
    fn case_table_type(&self, count: usize) -> StructType<'ctx> {
        let case: BasicTypeEnum = self.case_type().into();
        self.context.struct_type(&vec![case; count.max(1)], false)
    }

    /// `{ ptr chan, i64 kind, ptr data, i32 ok }`, C's `LughaCase`.
    fn case_type(&self) -> StructType<'ctx> {
        let ptr = self.context.ptr_type(Default::default());
        let fields = [
            ptr.into(),
            self.context.i64_type().into(),
            ptr.into(),
            self.context.i32_type().into(),
        ];
        self.context.struct_type(&fields, false)
    }

    /// `Some(value at data)` when `got`, else `None`.
    fn received(
        &mut self,
        got: IntValue<'ctx>,
        data: PointerValue<'ctx>,
        element: &Type,
    ) -> BasicValueEnum<'ctx> {
        let option = Type::Option(Box::new(element.clone()));
        let value = self.load_element(data, element);
        let some = self.construct_values(&option, "Some", vec![value]);
        let none = self.construct_values(&option, "None", Vec::new());
        self.choose(got, some, none)
    }

    /// A slot holding a copy of `value` in element layout, to send.
    fn element_value(
        &mut self,
        value: &Expr,
        element: &Type,
    ) -> Result<PointerValue<'ctx>, CodegenError> {
        let value = self.value_for_store(value, element)?;
        let slot = self.element_slot(element);
        self.store_element(slot, element, value);
        Ok(slot)
    }

    /// A stack slot for one `element` in element layout.
    fn element_slot(&self, element: &Type) -> PointerValue<'ctx> {
        self.raw_alloca(self.storage_type(element), "elem")
    }

    /// An alloca of `ty` at the top of the entry block.
    fn raw_alloca(&self, ty: BasicTypeEnum<'ctx>, name: &str) -> PointerValue<'ctx> {
        let function = self.function.expect("inside a function");
        let entry = function
            .get_first_basic_block()
            .expect("functions start with an entry block");
        let builder = self.context.create_builder();
        match entry.get_first_instruction() {
            Some(first) => builder.position_before(&first),
            None => builder.position_at_end(entry),
        }
        builder.build_alloca(ty, name).expect(POSITIONED)
    }

    fn nonzero(&self, flag: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        let flag = flag.into_int_value();
        self.builder
            .build_int_compare(IntPredicate::NE, flag, flag.get_type().const_zero(), "got")
            .expect(POSITIONED)
    }

    /// `file, line, col` of source offset `at`, for the runtime's panics.
    fn location(&mut self, at: usize) -> [BasicValueEnum<'ctx>; 3] {
        let file = self.file_name(at);
        let (line, col) = self.source.line_col(at);
        let i64_type = self.context.i64_type();
        [
            file.into(),
            i64_type.const_int(line as u64, false).into(),
            i64_type.const_int(col as u64, false).into(),
        ]
    }

    /// Calls the runtime function `name` with `values`, declared from them;
    /// its result when `returns` (a pointer for `chan_new`, else an
    /// integer of the C function's width).
    fn call_runtime(
        &mut self,
        name: &str,
        values: &[BasicValueEnum<'ctx>],
        returns: bool,
    ) -> BasicValueEnum<'ctx> {
        let function = match self.module.get_function(name) {
            Some(function) => function,
            None => {
                let params: Vec<_> = values.iter().map(|v| v.get_type().into()).collect();
                let fn_type = match (returns, name) {
                    (false, _) => self.context.void_type().fn_type(&params, false),
                    (true, "lugha_rt_chan_new") => self
                        .context
                        .ptr_type(Default::default())
                        .fn_type(&params, false),
                    (true, "lugha_rt_task_select") => {
                        self.context.i64_type().fn_type(&params, false)
                    }
                    (true, _) => self.context.i32_type().fn_type(&params, false),
                };
                self.module.add_function(name, fn_type, None)
            }
        };
        let args: Vec<_> = values.iter().map(|v| (*v).into()).collect();
        let site = self
            .builder
            .build_call(function, &args, "rt")
            .expect(POSITIONED);
        match site.try_as_basic_value() {
            ValueKind::Basic(value) => value,
            // A void call's value is never used.
            ValueKind::Instruction(_) => self.context.i32_type().const_zero().into(),
        }
    }
}
