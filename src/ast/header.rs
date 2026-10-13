//! v2 declaration headers: imports, visibility, fields and type parameters
//! (v2 draft §3, §6).

use super::{Ident, Type};
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
    /// Bound names in order, resolved by the checker (E0315).
    pub bounds: Vec<Ident>,
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
