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
use super::lower::unsupported;
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
            Value::Int(value) => Ok(value),
            _ => Err(type_error(span)),
        }
    }

    /// The boolean, or a type-checking error at `span`.
    pub(super) fn bool(self, span: Span) -> Result<IntValue<'ctx>, CodegenError> {
        match self {
            Value::Bool(value) => Ok(value),
            _ => Err(type_error(span)),
        }
    }

    /// The kind and value of a non-void value, or a type-checking error at `span`.
    pub(super) fn typed(self, span: Span) -> Result<(Kind, IntValue<'ctx>), CodegenError> {
        match self {
            Value::Int(value) => Ok((Kind::Int, value)),
            Value::Bool(value) => Ok((Kind::Bool, value)),
            Value::Void => Err(type_error(span)),
        }
    }
}

/// A mistake only the milestone 3 checker can report properly.
pub(super) fn type_error(span: Span) -> CodegenError {
    unsupported("type checking", 3, span)
}
