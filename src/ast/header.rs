//! v2 declaration headers: imports, visibility, fields and type parameters
//! (v2 draft §3, §6).

use super::{Ident, Param, Receiver, Type};
use crate::span::Span;

/// `import a.b.c;` — a module path, bound by its last segment (v2 draft §6).
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    /// The path's segments in order; never empty.
    pub path: Vec<Ident>,
    /// From `import` to the `;`.
    pub span: Span,
}

/// A type parameter and its bounds, `T` or `T: Eq + Ord` (v2 draft §4).
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    /// The parameter's name.
    pub name: Ident,
    /// Bounds in order, resolved by the checker (E0315).
    pub bounds: Vec<Bound>,
}

/// A bound: `Eq`, `Ord`, or an interface, `Shape`, `m.Shape` or, for a
/// generic interface, `Source<T>` (milestone 12, v6).
#[derive(Debug, Clone, PartialEq)]
pub struct Bound {
    /// `m` in `m.Shape`.
    pub module: Option<Ident>,
    pub name: Ident,
    /// The interface's type arguments (v6); empty when none are written.
    pub args: Vec<Type>,
    /// The whole bound, module and arguments included.
    pub span: Span,
}

/// `interface Name { fun area(self): f64; … }` (v2 draft §4, milestone 12).
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    pub name: Ident,
    /// Type parameters (v6), `interface Source<T>`; empty when none.
    pub type_params: Vec<TypeParam>,
    /// Required methods and associated functions, in order.
    pub items: Vec<InterfaceItem>,
    /// From `interface` to the closing brace.
    pub span: Span,
}

/// A required method (with a receiver) or associated function: a
/// signature ending in `;`.
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceItem {
    pub name: Ident,
    pub receiver: Option<Receiver>,
    pub params: Vec<Param>,
    pub ret: Option<Type>,
    /// A default body (v5): the whole item as a method, which a type gets
    /// unless it has its own.
    pub default: Option<super::FunDecl>,
    /// From `fun` to the `;` or the end of the body.
    pub span: Span,
}

/// `private` or `public` before a declaration or a field (v2 draft decision 9).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Visibility {
    /// Which keyword.
    pub kind: VisibilityKind,
    /// Where it was written.
    pub span: Span,
}

/// The visibility keywords. No keyword means visible to the whole program.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VisibilityKind {
    /// Only its own module.
    Private,
    /// Other packages: reserved until v3 (E0317).
    Public,
}

/// A struct field: an optional visibility, a name and a type.
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    /// The field's name.
    pub name: Ident,
    /// Its declared type.
    pub ty: Type,
}
