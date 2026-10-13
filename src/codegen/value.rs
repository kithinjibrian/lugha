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
use crate::ast::{Expr, Type as Annotation};
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
pub(super) fn llvm_type<'ctx>(context: &'ctx Context, ty: &Type) -> BasicTypeEnum<'ctx> {
    match ty {
        Type::I32 => context.i32_type().into(),
        Type::I64 => context.i64_type().into(),
        Type::U8 => context.i8_type().into(),
        Type::F64 => context.f64_type().into(),
        Type::Bool => context.bool_type().into(),
        // Strings and arrays are pointers to heap objects (spec §7).
        Type::String | Type::Array(_) => context.ptr_type(AddressSpace::default()).into(),
        // Declared by `declare_enums` before anything is lowered.
        Type::Enum(_, _) | Type::Option(_) => context
            .get_struct_type(&ty.to_string())
            .expect("enums are declared before lowering")
            .into(),
        // Declared by `declare_structs` before anything is lowered.
        Type::Struct(name, _) => context
            .get_struct_type(name)
            .expect("structs are declared before lowering")
            .into(),
        Type::Void | Type::Never | Type::Error => unreachable!("checked: {ty} is not a value type"),
        // The gate stops generic programs before codegen (PRP-035).
        Type::Param(_) => unreachable!("codegen never sees a type parameter"),
    }
}

/// The LLVM integer type of `i32`, `i64`, `u8` or `bool`.
pub(super) fn int_type<'ctx>(context: &'ctx Context, ty: &Type) -> IntType<'ctx> {
    llvm_type(context, ty).into_int_type()
}

/// `i32` and `i64` are signed; `u8` is not.
pub(super) fn is_signed(ty: &Type) -> bool {
    matches!(ty, Type::I32 | Type::I64)
}

/// True for values held in memory and passed to functions as a pointer to
/// the caller's storage: structs and enums (spec §7, v1 §7).
pub(super) fn is_aggregate(ty: &Type) -> bool {
    matches!(ty, Type::Struct(_, _) | Type::Enum(_, _) | Type::Option(_))
}

impl<'ctx> Lowerer<'ctx> {
    /// The type an annotation declares, as the checker resolved it
    /// (architecture rule 4; PRP-034: names depend on the module).
    pub(super) fn annotation_type(&self, ty: &Annotation) -> Type {
        self.annotations
            .get(&ty.span)
            .cloned()
            .expect("checked: every written type is resolved")
    }

    /// The checker's type for `expr`.
    pub(super) fn ty(&self, expr: &Expr) -> Type {
        self.types[expr.id.0 as usize].clone()
    }

    /// Lowers `expr` where a value of type `ty` is needed. In code that never
    /// finishes there is no value; an `undef` keeps the IR well-formed.
    pub(super) fn get(
        &mut self,
        expr: &Expr,
        ty: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        Ok(match self.expr(expr)? {
            Value::Val(value) => value,
            Value::Never => undef(self.context, ty),
            Value::Void => unreachable!("checked: E0407 rejects void values"),
        })
    }
}

fn undef<'ctx>(context: &'ctx Context, ty: &Type) -> BasicValueEnum<'ctx> {
    match llvm_type(context, ty) {
        BasicTypeEnum::FloatType(t) => t.get_undef().into(),
        BasicTypeEnum::PointerType(t) => t.get_undef().into(),
        BasicTypeEnum::StructType(t) => t.get_undef().into(),
        other => other.into_int_type().get_undef().into(),
    }
}
