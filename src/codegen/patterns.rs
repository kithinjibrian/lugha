//! `match` with nested patterns or guards (v2 draft §5): lowered arm by arm.
//! Each arm tests its whole pattern outside in, binds, runs its guard, and
//! on any failure falls through to the next arm. Flat, unguarded matches
//! keep the single `switch` in `matching.rs`.

use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use super::CodegenError;
use super::enums::variants;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::Value;
use crate::ast::{Arm, BinOp, Expr, Ident, Pattern, PatternKind};
use crate::check::{Type, slot_names_variant};

/// Where a value being matched is: loaded already, or at an address in
/// storage form (payloads; `bool` as `i8`).
#[derive(Clone, Copy)]
enum Subject<'ctx> {
    Value(BasicValueEnum<'ctx>),
    At(PointerValue<'ctx>),
}

impl<'ctx> Lowerer<'ctx> {
    /// The variants of `ty` if it is an enum or `Option`.
    fn variants_of(&self, ty: &Type) -> Option<Vec<(String, Vec<Type>)>> {
        matches!(ty, Type::Enum(_, _) | Type::Option(_)).then(|| variants(ty, &self.enums))
    }

    /// True if no arm has a guard and every payload slot is `_` or a binding:
    /// one `switch` on the scrutinee then decides (v1's lowering).
    pub(super) fn is_flat(&self, sty: &Type, arms: &[Arm]) -> bool {
        arms.iter().all(|arm| {
            let PatternKind::Variant(name, Some(slots)) = &arm.pattern.kind else {
                return arm.guard.is_none();
            };
            let (_, payloads) = self.variant(sty, &name.name);
            arm.guard.is_none()
                && slots
                    .iter()
                    .zip(&payloads)
                    .all(|(slot, ty)| match &slot.kind {
                        PatternKind::Wildcard => true,
                        PatternKind::Variant(n, None) => {
                            !slot_names_variant(self.variants_of(ty).as_deref(), &n.name)
                        }
                        _ => false,
                    })
        })
    }

    /// `match` arm by arm: test, bind, guard; any failure tries the next arm.
    pub(super) fn match_chain(
        &mut self,
        expr: &Expr,
        scrutinee: &Expr,
        arms: &[Arm],
    ) -> Result<Value<'ctx>, CodegenError> {
        let sty = self.ty(scrutinee);
        let value = self.get(scrutinee, &sty)?;
        let subject = match &sty {
            Type::Enum(_, _) | Type::Option(_) => {
                let slot = self.entry_alloca(&sty, "scrutinee");
                self.builder.build_store(slot, value).expect(POSITIONED);
                Subject::At(slot)
            }
            _ => Subject::Value(value),
        };
        let rty = self.ty(expr);
        let result = (!matches!(rty, Type::Void | Type::Never))
            .then(|| self.entry_alloca(&rty, "match.value"));
        let end = self.append("match.end");
        let mut diverges = true;
        for arm in arms {
            let next = self.append("arm.next");
            self.scopes.push();
            self.test(&arm.pattern, subject, &sty, next, true);
            if let Some(guard) = &arm.guard {
                let pass = self.get(guard, &Type::Bool)?.into_int_value();
                self.or_next(pass, next, "arm.body");
            }
            let value = self.expr(&arm.body)?;
            self.scopes.pop();
            if let (Some(result), Value::Val(v)) = (result, value) {
                self.builder.build_store(result, v).expect(POSITIONED);
            }
            diverges &= matches!(value, Value::Never);
            self.finish_branch(value, end);
            self.builder.position_at_end(next);
        }
        // Exhaustive (checked): no value gets past the last arm.
        self.builder.build_unreachable().expect(POSITIONED);
        self.builder.position_at_end(end);
        Ok(match result {
            _ if diverges && !arms.is_empty() => Value::Never,
            Some(result) => Value::Val(
                self.builder
                    .build_load(super::value::llvm_type(self.context, &rty), result, "match")
                    .expect(POSITIONED),
            ),
            None if rty == Type::Never => Value::Never,
            None => Value::Void,
        })
    }

    /// Continues in a new block if `pass`, else jumps to `next`.
    fn or_next(&self, pass: IntValue<'ctx>, next: BasicBlock<'ctx>, name: &str) {
        let ok = self.append(name);
        self.builder
            .build_conditional_branch(pass, ok, next)
            .expect(POSITIONED);
        self.builder.position_at_end(ok);
    }

    fn load_subject(&self, subject: Subject<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        match subject {
            Subject::Value(value) => value,
            Subject::At(address) => self.load_element(address, ty),
        }
    }

    /// Tests `pattern` against `subject`, binding as it goes; on a mismatch
    /// jumps to `next`. A bare name in a slot binds unless it names a variant.
    fn test(
        &mut self,
        pattern: &Pattern,
        subject: Subject<'ctx>,
        ty: &Type,
        next: BasicBlock<'ctx>,
        top: bool,
    ) {
        match &pattern.kind {
            PatternKind::Wildcard => {}
            PatternKind::Variant(name, None)
                if !top && !slot_names_variant(self.variants_of(ty).as_deref(), &name.name) =>
            {
                let value = self.load_subject(subject, ty);
                self.bind(name, value, ty);
            }
            PatternKind::Variant(name, slots) => {
                let slot = match subject {
                    Subject::At(slot) => slot,
                    Subject::Value(value) => {
                        let slot = self.entry_alloca(ty, "subject");
                        self.builder.build_store(slot, value).expect(POSITIONED);
                        slot
                    }
                };
                let (tag, payloads) = self.variant(ty, &name.name);
                let wanted = self.context.i32_type().const_int(u64::from(tag), false);
                let actual = self.tag(ty, slot);
                let same = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::EQ, actual, wanted, "is")
                    .expect(POSITIONED);
                self.or_next(same, next, "variant.ok");
                for (i, (sub, payload)) in slots.iter().flatten().zip(&payloads).enumerate() {
                    let index = u32::try_from(i).expect("payload count fits in u32");
                    let mut address = self.payload_address(ty, slot, &payloads, index);
                    if self.indirect(ty, payload) {
                        let ptr = self.context.ptr_type(Default::default());
                        address = self
                            .builder
                            .build_load(ptr, address, "boxed")
                            .expect(POSITIONED)
                            .into_pointer_value();
                    }
                    self.test(sub, Subject::At(address), payload, next, false);
                }
            }
            PatternKind::Str(text) => {
                let value = self.load_subject(subject, ty);
                let literal = self.string_literal(text).into();
                let same = self
                    .string_binary(BinOp::Eq, value, literal)
                    .into_int_value();
                self.or_next(same, next, "literal.ok");
            }
            kind => {
                let value = self.load_subject(subject, ty).into_int_value();
                let wanted = self
                    .case_value(ty, kind)
                    .expect("integer and bool patterns");
                let same = self
                    .builder
                    .build_int_compare(inkwell::IntPredicate::EQ, value, wanted, "is")
                    .expect(POSITIONED);
                self.or_next(same, next, "literal.ok");
            }
        }
    }

    /// A borrowed local for a payload binding (v1 §4).
    fn bind(&mut self, name: &Ident, value: BasicValueEnum<'ctx>, ty: &Type) {
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

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    /// The definition of `lugha_fn_<name>`, up to its closing brace.
    fn body<'a>(ir: &'a str, name: &str) -> &'a str {
        let start = &ir[ir
            .find(&format!("define i64 @lugha_fn_{name}("))
            .expect("defined")..];
        &start[..start.find("\n}").expect("function ends")]
    }

    const SHAPE: &str = "enum Shape { Circle(f64), Rect(f64, f64), Empty }\n";

    #[test]
    fn nested_patterns_test_outside_in_and_guards_fall_through() {
        let ir = ir(&format!(
            "{SHAPE}fun f(s: Option<Shape>): i64 = match s {{ Some(Circle(r)) if r > 1.0 => 1, Some(_) => 2, None => 3 }};\nfun main() {{}}"
        ));
        let f = body(&ir, "f");
        assert!(!f.contains("switch"), "{ir}");
        // `Some`, then `Circle`, in the first arm; `Some` again; `None`.
        assert!(f.matches("icmp eq i32").count() >= 4, "{ir}");
        let guard = f.find("fcmp ogt").expect("guard");
        assert!(
            f[guard..].contains("label %arm.next"),
            "a false guard tries the next arm: {ir}"
        );
        assert!(f.contains("unreachable"), "{ir}");
    }

    #[test]
    fn indirect_payloads_are_loaded_before_the_inner_test() {
        let ir = ir(
            "enum L { N, C(i64, L) }\nfun f(l: L): i64 = match l { C(_, C(x, _)) => x, _ => 0 };\nfun main() {}",
        );
        let f = body(&ir, "f");
        let boxed = f.find("load ptr").expect("indirect slot loaded");
        let tags: Vec<usize> = f.match_indices("icmp eq i32").map(|(i, _)| i).collect();
        assert!(
            tags.len() >= 2 && tags[0] < boxed && boxed < tags[1],
            "{ir}"
        );
    }

    #[test]
    fn flat_matches_keep_their_switch() {
        let ir = ir(&format!(
            "{SHAPE}fun f(s: Shape): i64 = match s {{ Circle(_) => 1, Rect(w, _) => w as i64, Empty => 3 }};\nfun main() {{}}"
        ));
        assert!(body(&ir, "f").contains("switch i32"), "{ir}");
    }
}
