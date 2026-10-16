//! Passing `extern struct`s to and from C (v16, milestone 90, PRP-120) as
//! the x86-64 System V ABI does, the way clang lowers them: LLVM itself
//! does no C ABI lowering, so a frontend must.
//!
//! A struct over 16 bytes goes in memory: an argument as a `byval` pointer,
//! a result through a hidden `sret` pointer. A smaller one is split into
//! eightbytes, each INTEGER or SSE by its fields, and travels as one LLVM
//! value per eightbyte (`{u8 × 4}` → `i32`, `{f32, f32}` → `<2 x float>`,
//! `{i64, f64}` → `i64` and `double`). An argument that needs more integer
//! or SSE registers than are left goes in memory as a whole.

use crate::check::{Enums, Structs, Type};

use super::structs::layout;

/// One eightbyte of a struct passed in registers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Piece {
    /// An integer register: `i<bits>`, as many bits as the eightbyte's data spans.
    Int(u32),
    /// An SSE register holding one `float`.
    Float,
    /// An SSE register holding two `float`s: `<2 x float>`.
    TwoFloats,
    /// An SSE register holding one `double`.
    Double,
}

impl Piece {
    /// True for a piece in an SSE register.
    pub(super) fn is_sse(self) -> bool {
        !matches!(self, Piece::Int(_))
    }
}

/// How a struct travels between C functions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Passing {
    /// In registers, one piece per eightbyte.
    Registers(Vec<Piece>),
    /// In memory: `byval` for an argument, `sret` for a result.
    Memory,
}

/// Every scalar of `ty` with its byte offset, nested structs flattened.
fn scalars(ty: &Type, base: u64, structs: &Structs, enums: &Enums, out: &mut Vec<(u64, Type)>) {
    match ty {
        Type::Struct(..) => {
            let mut offset = 0u64;
            for (_, field) in &structs[&ty.to_string()] {
                let (size, align) = layout(field, structs, enums);
                offset = offset.next_multiple_of(align);
                scalars(field, base + offset, structs, enums, out);
                offset += size;
            }
        }
        scalar => out.push((base, scalar.clone())),
    }
}

/// How the extern struct `ty` is passed, ignoring how many registers are left.
pub(super) fn classify(ty: &Type, structs: &Structs, enums: &Enums) -> Passing {
    let (size, _) = layout(ty, structs, enums);
    if size > 16 || size == 0 {
        return Passing::Memory;
    }
    let mut all = Vec::new();
    scalars(ty, 0, structs, enums, &mut all);
    let pieces = (0..size.div_ceil(8))
        .map(|i| {
            let start = i * 8;
            let here: Vec<&(u64, Type)> = all
                .iter()
                .filter(|(offset, _)| *offset >= start && *offset < start + 8)
                .collect();
            // Only the bytes that hold data, as clang: `{ptr, i32}`'s tail is `i32`.
            let end = here
                .iter()
                .map(|(offset, t)| offset + layout(t, structs, enums).0)
                .max()
                .unwrap_or(start);
            let types: Vec<&Type> = here.iter().map(|(_, t)| t).collect();
            match types.as_slice() {
                [Type::F64] => Piece::Double,
                [Type::F32] => Piece::Float,
                [Type::F32, Type::F32] => Piece::TwoFloats,
                _ => Piece::Int(u32::try_from((end - start) * 8).expect("an eightbyte is 64 bits")),
            }
        })
        .collect::<Vec<_>>();
    Passing::Registers(widen(pieces))
}

/// With two pieces, the low one fills its whole eightbyte, so the high one
/// starts at offset 8 in the coerced pair, as clang widens it.
fn widen(mut pieces: Vec<Piece>) -> Vec<Piece> {
    if pieces.len() == 2 {
        pieces[0] = match pieces[0] {
            Piece::Int(_) => Piece::Int(64),
            Piece::Float => Piece::Double,
            other => other,
        };
    }
    pieces
}

/// The registers still free while laying out a C call's arguments.
#[derive(Debug, Clone, Copy)]
pub(super) struct Free {
    /// `rdi rsi rdx rcx r8 r9`.
    pub ints: u32,
    /// `xmm0`–`xmm7`.
    pub sses: u32,
}

impl Free {
    /// All six integer and eight SSE registers, less one integer register
    /// for a hidden `sret` pointer when the result goes in memory.
    pub(super) fn new(sret: bool) -> Free {
        Free {
            ints: 6 - u32::from(sret),
            sses: 8,
        }
    }

    /// Takes the register a scalar argument of type `ty` needs, if one is left.
    pub(super) fn scalar(&mut self, ty: &Type) {
        if matches!(ty, Type::F32 | Type::F64) {
            self.sses = self.sses.saturating_sub(1);
        } else {
            self.ints = self.ints.saturating_sub(1);
        }
    }

    /// How a struct argument passed as `passing` actually travels: in
    /// registers if all its pieces fit (taking them), else in memory.
    pub(super) fn argument(&mut self, passing: Passing) -> Passing {
        let Passing::Registers(pieces) = &passing else {
            return Passing::Memory;
        };
        let sses =
            u32::try_from(pieces.iter().filter(|p| p.is_sse()).count()).expect("two at most");
        let ints = u32::try_from(pieces.len()).expect("two at most") - sses;
        if ints > self.ints || sses > self.sses {
            return Passing::Memory;
        }
        self.ints -= ints;
        self.sses -= sses;
        passing
    }
}

#[cfg(test)]
mod tests {
    use super::{Free, Passing, Piece, classify};
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

    fn pass(name: &str, defs: &[(&str, &[Type])]) -> Passing {
        classify(
            &Type::Struct(name.into(), Vec::new()),
            &structs(defs),
            &Enums::new(),
        )
    }

    use Type::{Bool, F32, F64, Handle, I32, I64, U8};

    #[test]
    fn small_structs_split_into_eightbytes_as_clang_does() {
        let regs = Passing::Registers;
        // raylib's Color: one integer register, 32 bits of data.
        assert_eq!(
            pass("Color", &[("Color", &[U8, U8, U8, U8])]),
            regs(vec![Piece::Int(32)])
        );
        // Vector2: two floats share one SSE register.
        assert_eq!(
            pass("V2", &[("V2", &[F32, F32])]),
            regs(vec![Piece::TwoFloats])
        );
        // Rectangle: four floats, two SSE registers.
        assert_eq!(
            pass("R", &[("R", &[F32, F32, F32, F32])]),
            regs(vec![Piece::TwoFloats, Piece::TwoFloats])
        );
        assert_eq!(
            pass("M", &[("M", &[I64, F64])]),
            regs(vec![Piece::Int(64), Piece::Double])
        );
        // A float and an int in one eightbyte: integer.
        assert_eq!(
            pass("FI", &[("FI", &[F32, I32])]),
            regs(vec![Piece::Int(64)])
        );
        assert_eq!(
            pass("DD", &[("DD", &[F64, F64])]),
            regs(vec![Piece::Double, Piece::Double])
        );
        // Three floats: the tail is one float, never read past the struct's 12 bytes.
        assert_eq!(
            pass("V3", &[("V3", &[F32, F32, F32])]),
            regs(vec![Piece::TwoFloats, Piece::Float])
        );
        // Three bytes: `i24`.
        assert_eq!(
            pass("B3", &[("B3", &[U8, Bool, U8])]),
            regs(vec![Piece::Int(24)])
        );
        // A low piece is widened when a high one follows: `{u8, f64}` → `i64`, `double`.
        assert_eq!(
            pass("TD", &[("TD", &[U8, F64])]),
            regs(vec![Piece::Int(64), Piece::Double])
        );
        assert_eq!(
            pass("FD", &[("FD", &[F32, F64])]),
            regs(vec![Piece::Double, Piece::Double])
        );
        // A handle is a pointer: integer.
        assert_eq!(
            pass("H", &[("H", &[Handle("W".into()), I32])]),
            regs(vec![Piece::Int(64), Piece::Int(32)])
        );
    }

    #[test]
    fn nested_structs_flatten_and_large_ones_go_in_memory() {
        let defs: &[(&str, &[Type])] = &[
            ("V2", &[F32, F32]),
            (
                "Pair",
                &[
                    Type::Struct("V2".into(), vec![]),
                    Type::Struct("V2".into(), vec![]),
                ],
            ),
            (
                "Cam",
                &[
                    Type::Struct("V2".into(), vec![]),
                    Type::Struct("V2".into(), vec![]),
                    F32,
                    F32,
                ],
            ),
            ("Big", &[I64, I64, I32]),
            ("Exactly16", &[I64, I64]),
        ];
        assert_eq!(
            pass("Pair", defs),
            Passing::Registers(vec![Piece::TwoFloats, Piece::TwoFloats])
        );
        assert_eq!(pass("Cam", defs), Passing::Memory);
        assert_eq!(pass("Big", defs), Passing::Memory);
        assert_eq!(
            pass("Exactly16", defs),
            Passing::Registers(vec![Piece::Int(64), Piece::Int(64)])
        );
    }

    #[test]
    fn a_struct_that_doesnt_fit_the_registers_left_goes_in_memory() {
        let two_ints = Passing::Registers(vec![Piece::Int(64), Piece::Int(64)]);
        let mut free = Free::new(false);
        for _ in 0..5 {
            free.scalar(&I64);
        }
        // One integer register left: the whole struct goes on the stack.
        assert_eq!(free.argument(two_ints.clone()), Passing::Memory);
        // and the register stays free for a later scalar.
        assert_eq!(free.ints, 1);
        let mut free = Free::new(true);
        assert_eq!(free.ints, 5);
        assert_eq!(free.argument(two_ints.clone()), two_ints);
        assert_eq!(free.ints, 3);
        let mut sse = Free::new(false);
        for _ in 0..8 {
            sse.scalar(&F64);
        }
        assert_eq!(
            sse.argument(Passing::Registers(vec![Piece::TwoFloats])),
            Passing::Memory
        );
    }
}
