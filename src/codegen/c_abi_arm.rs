//! Passing `extern struct`s to and from C on AArch64 Linux (v17, milestone
//! 134, PRP-166) as AAPCS64 does, the way clang lowers it:
//!
//! - a homogeneous floating-point aggregate — 1 to 4 scalars, all `f32` or
//!   all `f64`, nested structs flattened — is `[N x float]`/`[N x double]`;
//! - any other struct of up to 16 bytes is `i64` or `[2 x i64]`;
//! - a bigger one is an argument in a copy the caller makes, passed as a
//!   plain pointer (no `byval`), and a result through `sret` (in `x8`).
//!
//! Unlike x86-64 nothing counts registers: LLVM's AArch64 lowering puts a
//! whole array in registers or on the stack itself.

use inkwell::types::BasicTypeEnum;

use crate::check::{Enums, Structs, Type};

use super::c_abi::{Piece, scalars};
use super::c_calls::{Arg, CKind, CSig, Ret};
use super::lower::Lowerer;
use super::structs::layout;

/// How a struct travels on AArch64.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ArmPassing {
    /// `[count x float]` or `[count x double]` in consecutive FP registers.
    Floats(u32, Piece),
    /// `i64` (one) or `[2 x i64]` (two) in general registers.
    Ints(u32),
    /// Over 16 bytes: by a pointer to a copy (argument), or `sret` (result).
    Indirect,
}

/// How the extern struct `ty` is passed on AArch64.
pub(super) fn classify_arm(ty: &Type, structs: &Structs, enums: &Enums) -> ArmPassing {
    let (size, _) = layout(ty, structs, enums);
    assert!(size > 0, "the checker refuses empty extern structs (E0437)");
    let mut all = Vec::new();
    scalars(ty, 0, structs, enums, &mut all);
    let floats = |t: &Type, width: u64| {
        all.len() <= 4 && all.iter().all(|(_, s)| s == t) && size == width * all.len() as u64
    };
    let count = u32::try_from(all.len()).expect("four at most");
    if floats(&Type::F32, 4) {
        ArmPassing::Floats(count, Piece::Float)
    } else if floats(&Type::F64, 8) {
        ArmPassing::Floats(count, Piece::Double)
    } else if size <= 8 {
        ArmPassing::Ints(1)
    } else if size <= 16 {
        ArmPassing::Ints(2)
    } else {
        ArmPassing::Indirect
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// An AArch64 struct's one LLVM value: `[N x float]`, `i64` or `[2 x i64]`.
    fn arm_piece(&self, passing: ArmPassing) -> Vec<BasicTypeEnum<'ctx>> {
        let i64 = self.context.i64_type();
        vec![match passing {
            ArmPassing::Floats(n, piece) => match self.piece_type(piece) {
                BasicTypeEnum::FloatType(f) => f.array_type(n).into(),
                other => unreachable!("an FP aggregate is of floats, not {other:?}"),
            },
            ArmPassing::Ints(1) => i64.into(),
            ArmPassing::Ints(n) => i64.array_type(n).into(),
            ArmPassing::Indirect => unreachable!("an indirect struct has no registers"),
        }]
    }

    /// The lowered signature of a C function taking `kinds` and returning
    /// `ret`, by AAPCS64 on AArch64 (milestone 134).
    pub(super) fn c_sig_arm(&self, kinds: &[CKind], ret: &Type) -> CSig<'ctx> {
        let classified = |ty: &Type| classify_arm(ty, &self.structs, &self.enums);
        let ret = match ret {
            Type::Unit => Ret::Void,
            Type::Struct(..) => match classified(ret) {
                ArmPassing::Indirect => Ret::Memory(ret.clone()),
                regs => Ret::Split(ret.clone(), self.arm_piece(regs)),
            },
            other => Ret::One(self.c_result_type(other), other.clone()),
        };
        let ptr: BasicTypeEnum = self.context.ptr_type(Default::default()).into();
        let args = kinds
            .iter()
            .map(|kind| match kind {
                CKind::Pointer => Arg::One(ptr, None),
                CKind::Value(ty @ Type::Struct(..)) => match classified(ty) {
                    ArmPassing::Indirect => Arg::Indirect(ty.clone()),
                    regs => Arg::Split(ty.clone(), self.arm_piece(regs)),
                },
                CKind::Value(ty) => Arg::One(self.c_result_type(ty), Some(ty.clone())),
            })
            .collect();
        self.c_sig_of(args, ret)
    }
}

#[cfg(test)]
mod tests {
    use super::{ArmPassing, Piece, classify_arm};
    use crate::check::{Enums, Structs, Type};

    /// One struct per name, with these fields.
    fn structs(defs: &[(&str, &[Type])]) -> Structs {
        defs.iter()
            .map(|(name, fields)| {
                let named = fields
                    .iter()
                    .enumerate()
                    .map(|(i, t)| (format!("f{i}"), t.clone()))
                    .collect();
                (name.to_string(), named)
            })
            .collect()
    }

    fn pass(name: &str, defs: &[(&str, &[Type])]) -> ArmPassing {
        classify_arm(
            &Type::Struct(name.into(), Vec::new()),
            &structs(defs),
            &Enums::new(),
        )
    }

    use ArmPassing::{Floats, Indirect, Ints};
    use Type::{Bool, F32, F64, I32, I64, U8};

    fn named(name: &str) -> Type {
        Type::Struct(name.into(), Vec::new())
    }

    #[test]
    fn float_aggregates_of_one_type_go_in_fp_registers() {
        assert_eq!(pass("V2", &[("V2", &[F32, F32])]), Floats(2, Piece::Float));
        assert_eq!(
            pass("D4", &[("D4", &[F64, F64, F64, F64])]),
            Floats(4, Piece::Double)
        );
        assert_eq!(pass("One", &[("One", &[F64])]), Floats(1, Piece::Double));
        let nested = [
            ("V2", &[F32, F32][..]),
            ("Q", &[named("V2"), named("V2")][..]),
        ];
        assert_eq!(pass("Q", &nested), Floats(4, Piece::Float));
    }

    #[test]
    fn mixed_or_too_many_floats_are_not_float_aggregates() {
        assert_eq!(pass("M", &[("M", &[F32, F64])]), Ints(2));
        assert_eq!(pass("F5", &[("F5", &[F32, F32, F32, F32, F32])]), Indirect);
        assert_eq!(pass("FB", &[("FB", &[F32, Bool])]), Ints(1));
    }

    #[test]
    fn other_small_structs_go_in_one_or_two_general_registers() {
        assert_eq!(pass("C", &[("C", &[U8, U8, U8, U8])]), Ints(1));
        assert_eq!(pass("P", &[("P", &[I64, I32])]), Ints(2));
        assert_eq!(pass("W", &[("W", &[I64, I64])]), Ints(2));
        assert_eq!(pass("B", &[("B", &[I64, I64, I64])]), Indirect);
    }

    const SHAPES: &str = "extern struct V2 { x: f32, y: f32 }
extern struct Big { a: i64, b: i64, c: i64 }
extern struct Rgba { r: u8, g: u8, b: u8, a: u8 }
extern fun len(v: V2): f32;
extern fun big(b: Big): i64;
extern fun make(): Big;
extern fun tint(c: Rgba): Rgba;
fun main() {
    println(len(V2 { x: 1.0, y: 2.0 }));
    println(big(make()));
    println(tint(Rgba { r: 1, g: 2, b: 3, a: 4 }).a);
}
";

    #[test]
    fn signatures_are_aapcs64_on_arm_and_unchanged_on_x86_64() {
        use crate::codegen::Target;
        use crate::codegen::test_util::ir_for;
        let arm = ir_for(SHAPES, Target::Aarch64Linux);
        assert!(arm.contains("declare float @len([2 x float]"), "{arm}");
        // A big argument is a plain pointer to the caller's copy: no `byval`.
        assert!(arm.contains("declare i64 @big(ptr)"), "{arm}");
        assert!(arm.contains("declare void @make(ptr sret("), "{arm}");
        // A 4-byte struct travels in a whole 64-bit register.
        assert!(arm.contains("declare i64 @tint(i64)"), "{arm}");
        let x86 = ir_for(SHAPES, Target::X86_64Linux);
        assert!(x86.contains("declare float @len(<2 x float>)"), "{x86}");
        assert!(x86.contains("byval("), "{x86}");
        assert!(x86.contains("declare i32 @tint(i32)"), "{x86}");
    }
}
