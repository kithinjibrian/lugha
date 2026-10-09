//! The two kinds of value codegen tracks until the checker exists (PRP-006).
//!
//! Not a type system: just enough to emit valid IR in milestone 2, where every
//! integer is `i64` (CLAUDE.md rule 9) and conditions need `i1`. Mixing the
//! kinds stops compilation with "type checking (milestone 3)". Milestone 3
//! replaces this with real types.

use inkwell::context::Context;
use inkwell::types::IntType;
use inkwell::values::IntValue;

use super::CodegenError;
use super::lower::{Lowerer, unsupported};
use crate::ast::{Type, TypeKind};
use crate::span::Span;

/// What a non-void value is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Kind {
    /// Any integer, lowered as `i64`.
    Int,
    /// A boolean, lowered as `i1`.
    Bool,
}

impl Kind {
    /// The LLVM type for values of this kind.
    pub(super) fn llvm(self, context: &Context) -> IntType<'_> {
        match self {
            Kind::Int => context.i64_type(),
            Kind::Bool => context.bool_type(),
        }
    }
}

/// The result of lowering an expression.
#[derive(Debug, Clone, Copy)]
pub(super) enum Value<'ctx> {
    Int(IntValue<'ctx>),
    Bool(IntValue<'ctx>),
    /// Statements, blocks without a tail, `if` without `else`, loops.
    Void,
    /// Control never gets here: the code after `return`, `break` or
    /// `continue`, or an `if` whose branches all do that (spec §6). It stands in
    /// for any kind, so it carries `undef` placeholders; they only ever appear
    /// in unreachable blocks.
    Never {
        int: IntValue<'ctx>,
        bool: IntValue<'ctx>,
    },
}

impl<'ctx> Value<'ctx> {
    /// Wraps an LLVM value of the given kind.
    pub(super) fn of(kind: Kind, value: IntValue<'ctx>) -> Self {
        match kind {
            Kind::Int => Value::Int(value),
            Kind::Bool => Value::Bool(value),
        }
    }

    /// The integer, or a type-checking error at `span`.
    pub(super) fn int(self, span: Span) -> Result<IntValue<'ctx>, CodegenError> {
        match self {
            Value::Int(value) | Value::Never { int: value, .. } => Ok(value),
            _ => Err(type_error(span)),
        }
    }

    /// The boolean, or a type-checking error at `span`.
    pub(super) fn bool(self, span: Span) -> Result<IntValue<'ctx>, CodegenError> {
        match self {
            Value::Bool(value) | Value::Never { bool: value, .. } => Ok(value),
            _ => Err(type_error(span)),
        }
    }

    /// The kind and value of a non-void value, or a type-checking error at `span`.
    pub(super) fn typed(self, span: Span) -> Result<(Kind, IntValue<'ctx>), CodegenError> {
        match self {
            Value::Int(value) => Ok((Kind::Int, value)),
            Value::Bool(value) => Ok((Kind::Bool, value)),
            Value::Never { int, .. } => Ok((Kind::Int, int)),
            Value::Void => Err(type_error(span)),
        }
    }
}

/// A mistake only the milestone 3 checker can report properly.
pub(super) fn type_error(span: Span) -> CodegenError {
    unsupported("type checking", 3, span)
}

/// The kind an annotation requires; `i32` and `u8` are `i64` until milestone 3.
pub(super) fn annotation_kind(ty: &Type) -> Result<Kind, CodegenError> {
    match &ty.kind {
        TypeKind::I32 | TypeKind::I64 | TypeKind::U8 => Ok(Kind::Int),
        TypeKind::Bool => Ok(Kind::Bool),
        TypeKind::F64 => Err(unsupported("floats", 3, ty.span)),
        TypeKind::String => Err(unsupported("strings", 4, ty.span)),
        TypeKind::Named(_) => Err(unsupported("structs", 5, ty.span)),
        TypeKind::Array(_) => Err(unsupported("arrays", 5, ty.span)),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// The value of code that never runs to completion.
    pub(super) fn never(&self) -> Value<'ctx> {
        Value::Never {
            int: self.context.i64_type().get_undef(),
            bool: self.context.bool_type().get_undef(),
        }
    }
}
