//! Green threads in codegen (v17, milestone 91). `spawn call` lowers the
//! closure the parser wrapped the call in, and starts it through
//! `lugha_rt_task_spawn` with an entry `lugha_task_<enclosing>.<n>` that
//! calls it and stores the result. The call's arguments are evaluated
//! first, by the spawning thread (draft decision 2): each becomes a hidden
//! capture, and the closure body reads it instead of evaluating the
//! argument again. `join` copies the stored result out. In a program with
//! `spawn`, every function entry and loop iteration counts down
//! `lugha_rt_task_budget` and calls `lugha_rt_task_preempt` at zero.

use inkwell::IntPredicate;
use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::Value;
use crate::ast::{Expr, ExprKind, spawned_call};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `spawn call`: a new green thread, its `Task` handle the value.
    pub(super) fn spawn(
        &mut self,
        expr: &Expr,
        closure: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // `spawn Counter { … }` starts an actor (milestone 92).
        if let Type::Actor(key) = self.ty(expr) {
            return self.spawn_actor(&key, spawned_call(closure));
        }
        let Type::Task(result) = self.ty(expr) else {
            unreachable!("checked: `spawn` makes a `Task` or an actor")
        };
        let ExprKind::Closure(c) = &closure.kind else {
            unreachable!("the parser wraps a spawned call in a closure")
        };
        self.scopes.push();
        let mut hidden = Vec::new();
        for early in evaluated_first(spawned_call(closure)) {
            let ty = self
                .conversions
                .get(&early.id)
                .cloned()
                .unwrap_or_else(|| self.ty(early));
            let value = self.get(early, &ty)?;
            let name = format!("$spawn.{}", early.id.0);
            let ptr = self.entry_alloca(&ty, &name);
            self.builder.build_store(ptr, value).expect(POSITIONED);
            let local = Local {
                ptr,
                ty,
                borrowed: false,
            };
            self.scopes.declare(&name, local);
            hidden.push((early.id, name));
        }
        // Only now: while they were evaluated above, they were themselves.
        self.presets.extend(hidden.iter().cloned());
        let names: Vec<String> = hidden.iter().map(|(_, n)| n.clone()).collect();
        let pair = self.closure_value_with(closure, c, &names);
        for (id, _) in &hidden {
            self.presets.remove(id);
        }
        self.scopes.pop();
        let pair = pair?.into_struct_value();
        let entry = self.task_entry(&result);
        let code = self
            .builder
            .build_extract_value(pair, 0, "code")
            .expect("a pair has 2 fields");
        let env = self
            .builder
            .build_extract_value(pair, 1, "env")
            .expect("a pair has 2 fields");
        let size = match *result {
            Type::Unit | Type::Never => 1,
            ref ty => self.element_size(ty).max(1),
        };
        let ptr = self.context.ptr_type(Default::default());
        let spawn = self.runtime(
            "lugha_rt_task_spawn",
            &[Type::String, Type::String, Type::String, Type::I64],
            Some(Type::String),
        );
        let args = [
            entry.as_global_value().as_pointer_value().into(),
            code.into(),
            env.into(),
            self.context.i64_type().const_int(size, false).into(),
        ];
        let site = self
            .builder
            .build_call(spawn, &args, "task")
            .expect(POSITIONED);
        let ValueKind::Basic(task) = site.try_as_basic_value() else {
            unreachable!("lugha_rt_task_spawn returns the task")
        };
        debug_assert!(task.get_type() == ptr.into());
        Ok(task)
    }

    /// The value of a spawned call's argument the spawning thread already
    /// evaluated, inside the closure the new thread runs.
    pub(super) fn preset(&mut self, expr: &Expr) -> Option<BasicValueEnum<'ctx>> {
        let name = self.presets.get(&expr.id)?.clone();
        let local = self.scopes.lookup(&name);
        Some(self.load(&local, &name))
    }

    /// `lugha_task_<enclosing>.<n>(code, env, result)`: calls the closure
    /// `{ code, env }` and stores what it returns at `result`.
    fn task_entry(&mut self, result: &Type) -> FunctionValue<'ctx> {
        let enclosing = self.function.expect("`spawn` is inside a function");
        let name = enclosing.get_name().to_string_lossy().into_owned();
        let base = name
            .strip_prefix("lugha_fn_")
            .or_else(|| name.strip_prefix("lugha_closure_"))
            .unwrap_or(&name)
            .to_string();
        let count = self
            .closure_counts
            .entry(format!("task {base}"))
            .or_insert(0);
        let symbol = format!("lugha_task_{base}.{count}");
        *count += 1;
        let ptr = self.context.ptr_type(Default::default());
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[ptr.into(), ptr.into(), ptr.into()], false);
        let entry = self
            .module
            .add_function(&symbol, fn_type, Some(Linkage::Internal));
        let saved_block = self.builder.get_insert_block();
        self.builder
            .position_at_end(self.context.append_basic_block(entry, "entry"));
        let param = |i: u32| {
            entry
                .get_nth_param(i)
                .expect("declared with three parameters")
        };
        let pair_type = self.context.struct_type(&[ptr.into(), ptr.into()], false);
        let pair = pair_type.get_undef();
        let pair = self
            .builder
            .build_insert_value(pair, param(0), 0, "code")
            .expect(POSITIONED);
        let pair = self
            .builder
            .build_insert_value(pair, param(1), 1, "env")
            .expect(POSITIONED)
            .into_struct_value();
        let ret = match result {
            Type::Never => Type::Unit,
            ty => ty.clone(),
        };
        if let Value::Val(value) = self.call_pair(pair, (&[], &ret), Vec::new()) {
            self.store_element(param(2).into_pointer_value(), &ret, value);
        }
        self.builder.build_return(None).expect(POSITIONED);
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        entry
    }

    /// `t.join()`: waits for the task, then a copy of its result.
    pub(super) fn task_method(&mut self, base: &Expr) -> Result<Value<'ctx>, CodegenError> {
        let ty = self.ty(base);
        let Type::Task(result) = &ty else {
            unreachable!("checked: `join` has a `Task` receiver")
        };
        let task = self.get(base, &ty)?;
        let join = self.runtime("lugha_rt_task_join", &[Type::String], Some(Type::String));
        let site = self
            .builder
            .build_call(join, &[task.into()], "result")
            .expect(POSITIONED);
        let ValueKind::Basic(at) = site.try_as_basic_value() else {
            unreachable!("lugha_rt_task_join returns the result's address")
        };
        if matches!(**result, Type::Unit | Type::Never) {
            return Ok(Value::Void);
        }
        // A copy site: joining again gives another copy.
        let value = self.load_element(at.into_pointer_value(), result);
        Ok(Value::Val(self.deep_copy(value, result)))
    }

    /// `$os.sleep(ms)` and `$os.yield()` in `std.task`.
    pub(super) fn task_os_call(
        &mut self,
        op: &str,
        args: &[Expr],
    ) -> Result<Value<'ctx>, CodegenError> {
        match op {
            "sleep" => {
                let ms = self.get(&args[0], &Type::I64)?;
                let sleep = self.runtime("lugha_rt_task_sleep", &[Type::I64], None);
                self.builder
                    .build_call(sleep, &[ms.into()], "")
                    .expect(POSITIONED);
            }
            _ => {
                let yield_ = self.runtime("lugha_rt_task_yield", &[], None);
                self.builder.build_call(yield_, &[], "").expect(POSITIONED);
            }
        }
        Ok(Value::Void)
    }

    /// One step of the countdown, at a function's entry or a loop's head,
    /// in a program that spawns (draft decision 10).
    pub(super) fn preempt_point(&mut self) {
        if !self.uses_tasks {
            return;
        }
        let i64_type = self.context.i64_type();
        let budget = match self.module.get_global("lugha_rt_task_budget") {
            Some(global) => global,
            None => {
                let global = self
                    .module
                    .add_global(i64_type, None, "lugha_rt_task_budget");
                global.set_linkage(Linkage::External);
                global
            }
        };
        let at = budget.as_pointer_value();
        let left = self
            .builder
            .build_load(i64_type, at, "budget")
            .expect(POSITIONED)
            .into_int_value();
        let left = self
            .builder
            .build_int_sub(left, i64_type.const_int(1, false), "budget")
            .expect(POSITIONED);
        self.builder.build_store(at, left).expect(POSITIONED);
        let spent = self
            .builder
            .build_int_compare(IntPredicate::SLE, left, i64_type.const_zero(), "spent")
            .expect(POSITIONED);
        let (preempt, go_on) = (self.append("preempt"), self.append("preempt.done"));
        self.builder
            .build_conditional_branch(spent, preempt, go_on)
            .expect(POSITIONED);
        self.builder.position_at_end(preempt);
        let function = self.runtime("lugha_rt_task_preempt", &[], None);
        self.builder
            .build_call(function, &[], "")
            .expect(POSITIONED);
        self.branch(go_on);
        self.builder.position_at_end(go_on);
    }
}

/// The parts of a spawned call that the spawning thread evaluates first
/// (each is lowered through `expr`, where `preset` finds it): every
/// argument that computes something, a receiver that is a call
/// (`make().run()`), and a callee that is neither a name, a member nor a
/// closure literal (`fs[i](x)`, `pick()(x)`). Literals, names and closure
/// literals evaluate the same in either thread (captures are copies), so
/// they stay where they are.
fn evaluated_first(call: &Expr) -> Vec<&Expr> {
    let ExprKind::Call(callee, args) = &call.kind else {
        unreachable!("the parser checked that `spawn` has a call")
    };
    let mut early = Vec::new();
    match &callee.kind {
        ExprKind::Field(base, _) if matches!(base.kind, ExprKind::Call(..)) => early.push(&**base),
        ExprKind::Field(..) | ExprKind::Name(_) | ExprKind::Closure(_) => {}
        _ => early.push(&**callee),
    }
    early.extend(args.iter().filter(|a| computed(a)));
    early
}

/// Whether evaluating `e` could do something or differ by when it runs.
fn computed(e: &Expr) -> bool {
    match &e.kind {
        ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::Char(_)
        | ExprKind::Str(_)
        | ExprKind::Name(_)
        | ExprKind::Closure(_) => false,
        // `-1` is one literal (spec §4 rule 6).
        ExprKind::Unary(crate::ast::UnOp::Neg, inner) => {
            !matches!(inner.kind, ExprKind::Int(_) | ExprKind::Float(_))
        }
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn spawn_starts_an_entry_that_calls_the_closure() {
        let ir = ir("fun work(n: i64): i64 = n * n;
            fun main() { let t = spawn work(3); let n = t.join(); }");
        assert!(
            ir.contains("define internal void @lugha_task_main.0(ptr"),
            "{ir}"
        );
        assert!(
            ir.contains("call ptr @lugha_rt_task_spawn(ptr @lugha_task_main.0"),
            "{ir}"
        );
        assert!(ir.contains("call ptr @lugha_rt_task_join("), "{ir}");
    }

    #[test]
    fn arguments_are_evaluated_by_the_spawning_thread() {
        let ir = ir("fun next(): i64 = 1;\nfun work(n: i64): i64 = n;
            fun main() { let t = spawn work(next()); }");
        let main = &ir[ir.find("define void @lugha_fn_main()").unwrap()..];
        let main = &main[..main.find("\n}\n").unwrap()];
        assert!(main.contains("call i64 @lugha_fn_next()"), "{main}");
        let body = &ir[ir
            .find("define internal i64 @lugha_closure_main.0")
            .unwrap()..];
        let body = &body[..body.find("\n}\n").unwrap()];
        assert!(!body.contains("@lugha_fn_next"), "{body}");
    }

    #[test]
    fn only_programs_that_spawn_count_down() {
        let plain = ir("fun main() { let mut i = 0; while i < 3 { i += 1; } }");
        assert!(!plain.contains("lugha_rt_task_budget"), "{plain}");
        let spawning =
            ir("fun f() {}\nfun main() { spawn f(); let mut i = 0; while i < 3 { i += 1; } }");
        assert!(
            spawning.contains("call void @lugha_rt_task_preempt()"),
            "{spawning}"
        );
    }
}
