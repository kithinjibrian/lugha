//! `match` in codegen (v1 §5, §9): the scrutinee is evaluated once; enums
//! and integers branch with `switch`, strings with a chain of comparisons.
//! Each arm stores its value into one result slot, loaded at the join.
//! Matches with nested patterns or guards go arm by arm (`patterns.rs`).

use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::{Value, int_type};
use crate::ast::{Arm, BinOp, Expr, Pattern, PatternKind};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// Lowers `match scrutinee { arms }`.
    pub(super) fn match_expr(
        &mut self,
        expr: &Expr,
        scrutinee: &Expr,
        arms: &[Arm],
    ) -> Result<Value<'ctx>, CodegenError> {
        let sty = self.ty(scrutinee);
        if !self.is_flat(&sty, arms) {
            return self.match_chain(expr, scrutinee, arms);
        }
        let value = self.get(scrutinee, &sty)?;
        let rty = self.ty(expr);
        let result = (!matches!(rty, Type::Void | Type::Never))
            .then(|| self.entry_alloca(&rty, "match.value"));
        let end = self.append("match.end");
        // Arms that can never run (W0102) are not lowered: a `switch` can't
        // repeat a case value.
        let live = live_arms(arms);
        let blocks: Vec<BasicBlock<'ctx>> = live.iter().map(|_| self.append("match.arm")).collect();
        let fallback = live
            .iter()
            .position(|a| a.pattern.kind == PatternKind::Wildcard);
        let default = match fallback {
            Some(i) => blocks[i],
            None => self.append("match.impossible"),
        };
        let slot = match &sty {
            Type::Enum(_, _) | Type::Option(_) => {
                let slot = self.entry_alloca(&sty, "scrutinee");
                self.builder.build_store(slot, value).expect(POSITIONED);
                Some(slot)
            }
            _ => None,
        };
        if sty == Type::String {
            self.string_tests(value, &live, &blocks, default);
        } else {
            let on = match slot {
                Some(slot) => self.tag(&sty, slot),
                None => value.into_int_value(),
            };
            let cases: Vec<(IntValue<'ctx>, BasicBlock<'ctx>)> = live
                .iter()
                .zip(&blocks)
                .filter_map(|(arm, block)| {
                    self.case_value(&sty, &arm.pattern.kind)
                        .map(|v| (v, *block))
                })
                .collect();
            self.builder
                .build_switch(on, default, &cases)
                .expect(POSITIONED);
        }
        if fallback.is_none() {
            // Exhaustive without `_` (checked): no other value can arrive.
            self.builder.position_at_end(default);
            self.builder.build_unreachable().expect(POSITIONED);
        }
        let mut diverges = true;
        for (arm, block) in live.iter().zip(&blocks) {
            self.builder.position_at_end(*block);
            self.scopes.push();
            if let (Some(slot), PatternKind::Variant(name, Some(slots))) = (slot, &arm.pattern.kind)
            {
                self.bind_payloads(&sty, slot, &name.name, slots);
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
            _ if diverges && !live.is_empty() => Value::Never,
            Some(result) => Value::Val(
                self.builder
                    .build_load(self.llvm(&rty), result, "match")
                    .expect(POSITIONED),
            ),
            None if rty == Type::Never => Value::Never,
            None => Value::Void,
        })
    }

    /// The `switch` case value a pattern stands for; `None` for `_`.
    pub(super) fn case_value(&self, sty: &Type, pattern: &PatternKind) -> Option<IntValue<'ctx>> {
        match pattern {
            PatternKind::Wildcard | PatternKind::Str(_) | PatternKind::Type(..) => None,
            // Never in a flat match: `is_flat` sends these to the arm chain (v9).
            PatternKind::Or(_) | PatternKind::Mut(_) | PatternKind::Tuple(_) => None,
            PatternKind::Variant(name, _) => {
                let (tag, _) = self.variant(sty, &name.name);
                Some(self.context.i32_type().const_int(u64::from(tag), false))
            }
            PatternKind::Int { value, negative } => {
                // Two's complement, truncated to the scrutinee's width.
                let bits = if *negative {
                    value.wrapping_neg()
                } else {
                    *value
                };
                Some(int_type(self.context, sty).const_int(bits, false))
            }
            PatternKind::Bool(b) => Some(self.context.bool_type().const_int(u64::from(*b), false)),
        }
    }

    /// String arms are tested in order with `lugha_rt_str_eq`.
    fn string_tests(
        &mut self,
        value: BasicValueEnum<'ctx>,
        arms: &[&Arm],
        blocks: &[BasicBlock<'ctx>],
        default: BasicBlock<'ctx>,
    ) {
        for (arm, block) in arms.iter().zip(blocks) {
            let PatternKind::Str(text) = &arm.pattern.kind else {
                continue;
            };
            let literal = self.string_literal(text).into();
            let equal = self
                .string_binary(BinOp::Eq, value, literal)
                .into_int_value();
            let next = self.append("match.next");
            self.builder
                .build_conditional_branch(equal, *block, next)
                .expect(POSITIONED);
            self.builder.position_at_end(next);
        }
        self.branch(default);
    }

    /// Binds each named payload of `variant` as a borrowed local (v1 §4).
    fn bind_payloads(
        &mut self,
        sty: &Type,
        slot: PointerValue<'ctx>,
        variant: &str,
        slots: &[Pattern],
    ) {
        let (_, payloads) = self.variant(sty, variant);
        for (i, (slot_pattern, ty)) in slots.iter().zip(&payloads).enumerate() {
            // A flat match's slots are `_` or bindings (`is_flat`).
            let PatternKind::Variant(name, None) = &slot_pattern.kind else {
                continue;
            };
            let index = u32::try_from(i).expect("payload count fits in u32");
            let mut address = self.payload_address(sty, slot, &payloads, index);
            if self.indirect(sty, ty) {
                let ptr = self.context.ptr_type(Default::default());
                address = self
                    .builder
                    .build_load(ptr, address, "boxed")
                    .expect(POSITIONED)
                    .into_pointer_value();
            }
            let value = self.load_element(address, ty);
            let local = self.entry_alloca(ty, &name.name);
            self.builder.build_store(local, value).expect(POSITIONED);
            self.scopes.declare(
                &name.name,
                Local {
                    ptr: local,
                    ty: ty.clone(),
                    borrowed: true,
                },
            );
        }
    }

    fn llvm(&self, ty: &Type) -> inkwell::types::BasicTypeEnum<'ctx> {
        super::value::llvm_type(self.context, ty)
    }
}

/// The arms that can run: up to the first `_`, skipping repeated cases.
fn live_arms(arms: &[Arm]) -> Vec<&Arm> {
    let mut live: Vec<&Arm> = Vec::new();
    for arm in arms {
        if live.iter().any(|a| {
            a.pattern.kind == arm.pattern.kind || same_case(&a.pattern.kind, &arm.pattern.kind)
        }) {
            continue;
        }
        let wildcard = arm.pattern.kind == PatternKind::Wildcard;
        live.push(arm);
        if wildcard {
            break;
        }
    }
    live
}

/// Two variant patterns name the same variant, whatever their bindings.
fn same_case(a: &PatternKind, b: &PatternKind) -> bool {
    matches!((a, b), (PatternKind::Variant(x, _), PatternKind::Variant(y, _)) if x.name == y.name)
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn enum_and_integer_matches_switch() {
        let ir = ir(
            "enum S { A(f64), B }\nfun f(s: S): f64 = match s { A(x) => x, B => 0.0 };\n\
                     fun g(n: i64): i64 = match n { 1 => 10, -1 => 20, _ => 0 };\nfun main() {}",
        );
        assert!(
            ir.contains("define double @lugha_fn_f(ptr"),
            "enums pass by pointer: {ir}"
        );
        assert!(ir.contains("switch i32"), "{ir}");
        assert!(
            ir.contains("switch i64") && ir.contains("i64 -1, label"),
            "{ir}"
        );
    }

    #[test]
    fn string_matches_compare_in_order() {
        let ir = ir(
            "fun f(s: string): i64 = match s { \"a\" => 1, \"b\" => 2, _ => 0 };\nfun main() {}",
        );
        assert_eq!(ir.matches("call i32 @lugha_rt_str_eq(").count(), 2, "{ir}");
    }

    #[test]
    fn enum_values_share_payloads_and_constructions_copy_places() {
        let src = "enum J { B(i64[]), I }\nfun main() { let xs = [1]; let a = J.B(xs); let b = a; println(xs[0]); }";
        let ir = ir(src);
        // `xs` is used later, so building the variant shares it (v15: marks,
        // not copies); `let b = a` copies nothing more.
        assert_eq!(ir.matches("or i64 %cap").count(), 1, "{ir}");
        assert!(!ir.contains("call void @llvm.memcpy"), "{ir}");
    }
}
