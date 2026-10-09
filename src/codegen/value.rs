//! Values and types: the checker's `Type` mapped to LLVM (spec §4).
//!
//! Codegen never infers types (CLAUDE.md rule 4): every expression's type
//! comes from the checker's table. `Value` only remembers whether lowering
//! produced a value, nothing, or code that never finishes — blocks have no
//! `ExprId`, and divergence decides phi edges (spec §9).

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::{BasicTypeEnum, IntType};
use inkwell::values::BasicValueEnum;

use super::CodegenError;
use super::lower::Lowerer;
use crate::ast::{Expr, Type as Annotation, TypeKind};
use crate::check::Type;

/// The result of lowering an expression.
#[derive(Debug, Clone, Copy)]
pub(super) enum Value<'ctx> {
    Val(BasicValueEnum<'ctx>),
    /// Statements, blocks without a tail, `if` without `else`, void calls.
    Void,
    /// Control never gets here: after `return`, `break` or `continue`, or an
    /// `if` whose branches all do that (spec §6).
    Never,
}

/// The LLVM type of a value of type `ty` (spec §4 lowering table).
pub(super) fn llvm_type(context: &Context, ty: Type) -> BasicTypeEnum<'_> {
    match ty {
        Type::I32 => context.i32_type().into(),
        Type::I64 => context.i64_type().into(),
        Type::U8 => context.i8_type().into(),
        Type::F64 => context.f64_type().into(),
        Type::Bool => context.bool_type().into(),
        // Strings are pointers to runtime objects (spec §7).
        Type::String => context.ptr_type(AddressSpace::default()).into(),
        Type::Void | Type::Never | Type::Error => unreachable!("checked: {ty} is not a value type"),
    }
}

/// The LLVM integer type of `i32`, `i64`, `u8` or `bool`.
pub(super) fn int_type(context: &Context, ty: Type) -> IntType<'_> {
    llvm_type(context, ty).into_int_type()
}

/// `i32` and `i64` are signed; `u8` is not.
pub(super) fn is_signed(ty: Type) -> bool {
    matches!(ty, Type::I32 | Type::I64)
}

/// The type an annotation declares. The checker has already resolved it, so
/// only milestone 3 types reach codegen.
pub(super) fn annotation_type(ty: &Annotation) -> Type {
    match ty.kind {
        TypeKind::I32 => Type::I32,
        TypeKind::I64 => Type::I64,
        TypeKind::U8 => Type::U8,
        TypeKind::F64 => Type::F64,
        TypeKind::Bool => Type::Bool,
        TypeKind::String => Type::String,
        _ => unreachable!("checked: arrays and structs stop before codegen"),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// The checker's type for `expr`.
    pub(super) fn ty(&self, expr: &Expr) -> Type {
        self.types[expr.id.0 as usize]
    }

    /// Lowers `expr` where a value of type `ty` is needed. In code that never
    /// finishes there is no value; an `undef` keeps the IR well-formed.
    pub(super) fn get(
        &mut self,
        expr: &Expr,
        ty: Type,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        Ok(match self.expr(expr)? {
            Value::Val(value) => value,
            Value::Never => undef(self.context, ty),
            Value::Void => unreachable!("checked: E0407 rejects void values"),
        })
    }
}

fn undef(context: &Context, ty: Type) -> BasicValueEnum<'_> {
    match llvm_type(context, ty) {
        BasicTypeEnum::FloatType(t) => t.get_undef().into(),
        BasicTypeEnum::PointerType(t) => t.get_undef().into(),
        other => other.into_int_type().get_undef().into(),
    }
}
