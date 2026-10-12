//! v2 declaration headers: imports and type parameters (v2 draft §3).

use super::Ident;
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
