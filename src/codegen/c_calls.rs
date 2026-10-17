//! C signatures that carry `extern struct`s (v16, milestone 90, PRP-120):
//! the LLVM function type clang would give the C function, and moving
//! values into it (a call into C) and out of it (an entry C calls).
//!
//! A struct in registers becomes one LLVM parameter per piece (`c_abi`),
//! read from or written to the struct's memory one piece at a time, so
//! nothing past the struct's own bytes is touched. A struct in memory is a
//! `byval` pointer; a result in memory comes back through a hidden `sret`
//! pointer, the first parameter. Signatures without structs never come here,
//! so their IR is unchanged.

use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::types::{
    AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType,
};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue, ValueKind,
};

use super::c_abi::{Free, Passing, Piece, classify};
use super::declare::abi_attribute;
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::check::Type;

/// One parameter of a C function, as Lugha sees it.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum CKind {
    /// A Lugha value of this type.
    Value(Type),
    /// A plain pointer lughac fills in: a callback, `data`, `release`.
    Pointer,
}

/// How one parameter travels.
#[derive(Debug, Clone)]
enum Arg<'ctx> {
    /// One LLVM parameter; the Lugha type when it has one, for C's extensions.
    One(BasicTypeEnum<'ctx>, Option<Type>),
    /// An extern struct in registers: one LLVM parameter per piece.
    Split(Type, Vec<BasicTypeEnum<'ctx>>),
    /// An extern struct in memory: a `byval` pointer.
    ByVal(Type),
}

/// How the result travels.
#[derive(Debug, Clone)]
enum Ret<'ctx> {
    Void,
    One(BasicTypeEnum<'ctx>, Type),
    /// An extern struct in registers: its piece, or a pair of pieces.
    Split(Type, Vec<BasicTypeEnum<'ctx>>),
    /// An extern struct in memory, written through the `sret` pointer.
    Memory(Type),
}

/// A C function's signature with its structs lowered.
#[derive(Debug, Clone)]
pub(super) struct CSig<'ctx> {
    args: Vec<Arg<'ctx>>,
    ret: Ret<'ctx>,
    /// The LLVM type of the C function.
    pub fn_type: FunctionType<'ctx>,
}

/// True if a C signature carries a struct by value: only then does it need lowering.
pub(super) fn has_struct(kinds: &[CKind], ret: &Type) -> bool {
    matches!(ret, Type::Struct(..))
        || kinds
            .iter()
            .any(|k| matches!(k, CKind::Value(Type::Struct(..))))
}

impl<'ctx> Lowerer<'ctx> {
    fn piece_type(&self, piece: Piece) -> BasicTypeEnum<'ctx> {
        match piece {
            Piece::Int(bits) => {
                let bits = std::num::NonZeroU32::new(bits).expect("a piece holds data");
                self.context
                    .custom_width_int_type(bits)
                    .expect("1 to 64 bits is a valid integer width")
                    .into()
            }
            Piece::Float => self.context.f32_type().into(),
            Piece::TwoFloats => self.context.f32_type().vec_type(2).into(),
            Piece::Double => self.context.f64_type().into(),
        }
    }

    fn piece_types(&self, pieces: &[Piece]) -> Vec<BasicTypeEnum<'ctx>> {
        pieces.iter().map(|p| self.piece_type(*p)).collect()
    }

    /// The lowered signature of a C function taking `kinds` and returning `ret`.
    pub(super) fn c_sig(&self, kinds: &[CKind], ret: &Type) -> CSig<'ctx> {
        let classified = |ty: &Type| classify(ty, &self.structs, &self.enums);
        let ret = match ret {
            Type::Unit => Ret::Void,
            Type::Struct(..) => match classified(ret) {
                Passing::Memory => Ret::Memory(ret.clone()),
                Passing::Registers(pieces) => Ret::Split(ret.clone(), self.piece_types(&pieces)),
            },
            other => Ret::One(self.c_result_type(other), other.clone()),
        };
        let ptr: BasicTypeEnum = self.context.ptr_type(Default::default()).into();
        let mut free = Free::new(matches!(ret, Ret::Memory(_)));
        let args: Vec<Arg> = kinds
            .iter()
            .map(|kind| match kind {
                CKind::Pointer => {
                    free.scalar(&Type::I64);
                    Arg::One(ptr, None)
                }
                CKind::Value(ty @ Type::Struct(..)) => match free.argument(classified(ty)) {
                    Passing::Memory => Arg::ByVal(ty.clone()),
                    Passing::Registers(pieces) => Arg::Split(ty.clone(), self.piece_types(&pieces)),
                },
                CKind::Value(ty) => {
                    free.scalar(ty);
                    Arg::One(self.c_result_type(ty), Some(ty.clone()))
                }
            })
            .collect();
        let mut params: Vec<BasicMetadataTypeEnum> = Vec::new();
        if let Ret::Memory(_) = ret {
            params.push(ptr.into());
        }
        for arg in &args {
            match arg {
                Arg::One(t, _) => params.push((*t).into()),
                Arg::Split(_, pieces) => {
                    params.extend(pieces.iter().map(|p| BasicMetadataTypeEnum::from(*p)))
                }
                Arg::ByVal(_) => params.push(ptr.into()),
            }
        }
        let fn_type = match &ret {
            Ret::Void | Ret::Memory(_) => self.context.void_type().fn_type(&params, false),
            Ret::One(t, _) => t.fn_type(&params, false),
            Ret::Split(_, pieces) => self.pair_type(pieces).fn_type(&params, false),
        };
        CSig { args, ret, fn_type }
    }

    /// One piece, or an anonymous pair `{ p0, p1 }` laid over the struct's memory.
    fn pair_type(&self, pieces: &[BasicTypeEnum<'ctx>]) -> BasicTypeEnum<'ctx> {
        match pieces {
            [one] => *one,
            many => self.context.struct_type(many, false).into(),
        }
    }

    /// `byval`, `sret`, their alignment and C's extensions, for a C function
    /// declared or entered with `sig`, or a call through it: `add` puts each.
    pub(super) fn c_sig_attributes(
        &self,
        sig: &CSig<'ctx>,
        mut add: impl FnMut(AttributeLoc, Attribute),
    ) {
        let kind = Attribute::get_named_enum_kind_id;
        let align = self.context.create_enum_attribute(kind("align"), 8);
        let mut index = 0u32;
        if let Ret::Memory(ty) = &sig.ret {
            let named = self.named_struct(ty);
            add(
                AttributeLoc::Param(0),
                self.context
                    .create_type_attribute(kind("sret"), named.as_any_type_enum()),
            );
            add(AttributeLoc::Param(0), align);
            index = 1;
        }
        for arg in &sig.args {
            match arg {
                Arg::One(_, ty) => {
                    if let Some(attribute) =
                        ty.as_ref().and_then(|t| abi_attribute(self.context, t))
                    {
                        add(AttributeLoc::Param(index), attribute);
                    }
                    index += 1;
                }
                Arg::Split(_, pieces) => index += u32::try_from(pieces.len()).expect("two at most"),
                Arg::ByVal(ty) => {
                    let named = self.named_struct(ty);
                    add(
                        AttributeLoc::Param(index),
                        self.context
                            .create_type_attribute(kind("byval"), named.as_any_type_enum()),
                    );
                    add(AttributeLoc::Param(index), align);
                    index += 1;
                }
            }
        }
        if let Ret::One(_, ty) = &sig.ret
            && let Some(attribute) = abi_attribute(self.context, ty)
        {
            add(AttributeLoc::Return, attribute);
        }
    }

    fn named_struct(&self, ty: &Type) -> StructType<'ctx> {
        llvm_type(self.context, ty).into_struct_type()
    }

    /// Stores `value` of extern struct type `ty` and loads it back as `pieces`.
    fn split_pieces(
        &mut self,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
        pieces: &[BasicTypeEnum<'ctx>],
    ) -> Vec<BasicValueEnum<'ctx>> {
        let slot = self.entry_alloca(ty, "c.struct");
        self.builder.build_store(slot, value).expect(POSITIONED);
        let pair = self.context.struct_type(pieces, false);
        pieces
            .iter()
            .enumerate()
            .map(|(i, piece)| {
                let at = self
                    .builder
                    .build_struct_gep(
                        pair,
                        slot,
                        u32::try_from(i).expect("two at most"),
                        "c.piece.at",
                    )
                    .expect("a piece of the pair");
                self.builder
                    .build_load(*piece, at, "c.piece")
                    .expect(POSITIONED)
            })
            .collect()
    }

    /// Writes `values`, the pieces of an extern struct of type `ty`, into a
    /// new slot and loads the struct.
    fn join_pieces(
        &mut self,
        values: &[BasicValueEnum<'ctx>],
        ty: &Type,
        pieces: &[BasicTypeEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let slot = self.entry_alloca(ty, "c.struct");
        let pair = self.context.struct_type(pieces, false);
        for (i, value) in values.iter().enumerate() {
            let at = self
                .builder
                .build_struct_gep(
                    pair,
                    slot,
                    u32::try_from(i).expect("two at most"),
                    "c.piece.at",
                )
                .expect("a piece of the pair");
            self.builder.build_store(at, *value).expect(POSITIONED);
        }
        self.builder
            .build_load(self.named_struct(ty), slot, "c.value")
            .expect(POSITIONED)
    }

    /// Calls `code` through `sig` with `values`, one per kind, lowered for C;
    /// the result as a Lugha value (`None` for `void`).
    pub(super) fn c_sig_call(
        &mut self,
        sig: &CSig<'ctx>,
        code: PointerValue<'ctx>,
        values: Vec<BasicValueEnum<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
        let sret = match &sig.ret {
            Ret::Memory(ty) => {
                let slot = self.entry_alloca(ty, "c.result");
                args.push(slot.into());
                Some((slot, ty.clone()))
            }
            _ => None,
        };
        for (arg, value) in sig.args.iter().zip(values) {
            match arg {
                Arg::One(..) => args.push(value.into()),
                Arg::Split(ty, pieces) => args.extend(
                    self.split_pieces(value, ty, pieces)
                        .into_iter()
                        .map(BasicMetadataValueEnum::from),
                ),
                Arg::ByVal(ty) => {
                    // C gets its own copy: `byval` copies from this slot.
                    let slot = self.entry_alloca(ty, "c.byval");
                    self.builder.build_store(slot, value).expect(POSITIONED);
                    args.push(slot.into());
                }
            }
        }
        let site = self
            .builder
            .build_indirect_call(sig.fn_type, code, &args, "c.call")
            .expect(POSITIONED);
        self.c_sig_attributes(sig, |at, attribute| site.add_attribute(at, attribute));
        let result = match site.try_as_basic_value() {
            ValueKind::Basic(result) => Some(result),
            ValueKind::Instruction(_) => None,
        };
        match (&sig.ret, result) {
            (Ret::Memory(_), _) => {
                let (slot, ty) = sret.expect("an sret slot");
                Some(
                    self.builder
                        .build_load(self.named_struct(&ty), slot, "c.value")
                        .expect(POSITIONED),
                )
            }
            (Ret::Split(ty, pieces), Some(result)) => {
                let parts: Vec<BasicValueEnum> = match pieces.as_slice() {
                    [_] => vec![result],
                    _ => (0..pieces.len())
                        .map(|i| {
                            self.builder
                                .build_extract_value(
                                    result.into_struct_value(),
                                    u32::try_from(i).expect("two at most"),
                                    "c.piece",
                                )
                                .expect("a piece of the pair")
                        })
                        .collect(),
                };
                Some(self.join_pieces(&parts, ty, pieces))
            }
            (_, result) => result,
        }
    }

    /// In an entry C calls through `sig`: its parameters as Lugha values,
    /// one per kind, and the `sret` pointer if the result goes in memory.
    pub(super) fn c_sig_params(
        &mut self,
        sig: &CSig<'ctx>,
        entry: FunctionValue<'ctx>,
    ) -> (Vec<BasicValueEnum<'ctx>>, Option<PointerValue<'ctx>>) {
        let mut incoming = entry.get_param_iter();
        let sret = match sig.ret {
            Ret::Memory(_) => Some(
                incoming
                    .next()
                    .expect("the sret parameter")
                    .into_pointer_value(),
            ),
            _ => None,
        };
        let mut values = Vec::new();
        for arg in &sig.args {
            values.push(match arg {
                Arg::One(..) => incoming.next().expect("a parameter per piece"),
                Arg::Split(ty, pieces) => {
                    let parts: Vec<BasicValueEnum> = (0..pieces.len())
                        .map(|_| incoming.next().expect("a parameter per piece"))
                        .collect();
                    self.join_pieces(&parts, ty, pieces)
                }
                Arg::ByVal(ty) => {
                    let from = incoming
                        .next()
                        .expect("the byval pointer")
                        .into_pointer_value();
                    self.builder
                        .build_load(self.named_struct(ty), from, "c.value")
                        .expect(POSITIONED)
                }
            });
        }
        (values, sret)
    }

    /// In an entry C calls through `sig`: returns `value` the way C expects.
    pub(super) fn c_sig_return(
        &mut self,
        sig: &CSig<'ctx>,
        sret: Option<PointerValue<'ctx>>,
        value: Option<BasicValueEnum<'ctx>>,
    ) {
        match (&sig.ret, value) {
            (Ret::Memory(_), Some(value)) => {
                let to = sret.expect("an sret parameter");
                self.builder.build_store(to, value).expect(POSITIONED);
                self.builder.build_return(None)
            }
            (Ret::Split(ty, pieces), Some(value)) => {
                let parts = self.split_pieces(value, ty, pieces);
                match parts.as_slice() {
                    [one] => self.builder.build_return(Some(one)),
                    _ => {
                        let pair = self.context.struct_type(pieces, false);
                        let mut agg = pair.get_undef();
                        for (i, part) in parts.into_iter().enumerate() {
                            agg = self
                                .builder
                                .build_insert_value(
                                    agg,
                                    part,
                                    u32::try_from(i).expect("two at most"),
                                    "c.pair",
                                )
                                .expect(POSITIONED)
                                .into_struct_value();
                        }
                        self.builder.build_return(Some(&agg))
                    }
                }
            }
            (_, Some(value)) => self.builder.build_return(Some(&value)),
            (_, None) => self.builder.build_return(None),
        }
        .expect(POSITIONED);
    }
}
