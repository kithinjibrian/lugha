//! Declaration headers: imports, visibility, fields and type parameters.

use super::{Ident, Param, Receiver, Type};
use crate::span::Span;

/// `import a.b.c;` — a module path, bound by its last segment
/// or by the name after `as`.
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    /// The path's segments in order; never empty.
    pub path: Vec<Ident>,
    /// `c` in `import a.b as c;`.
    pub alias: Option<Ident>,
    /// The names in `{ X, y as z }` after the path, in order.
    pub items: Vec<ImportItem>,
    /// The `*` of `public import m { * };`: every `public` item of `m`
    /// re-exported; `items` is then empty.
    pub glob: Option<Span>,
    /// The `public` before a re-export, `public import m { X };` or
    /// `public import m;`.
    pub public: Option<Span>,
    /// From `import` to the `;`.
    pub span: Span,
}

impl Import {
    /// The name the module is bound to in its file: the alias, or the
    /// path's last segment.
    pub fn bound(&self) -> &Ident {
        self.alias
            .as_ref()
            .unwrap_or_else(|| self.path.last().expect("an import has a path"))
    }
}

/// `X` or `X as Y` in an import's `{ … }`.
#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    /// The item's name in its module.
    pub name: Ident,
    /// The name it is bound to here, after `as`.
    pub alias: Option<Ident>,
}

impl ImportItem {
    /// The name the item is bound to in the importing file.
    pub fn bound(&self) -> &Ident {
        self.alias.as_ref().unwrap_or(&self.name)
    }
}

/// A type parameter and its bounds, `T` or `T: Eq + Ord`.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParam {
    /// The parameter's name.
    pub name: Ident,
    /// Bounds in order, resolved by the checker (E0315).
    pub bounds: Vec<Bound>,
}

/// A bound: `Eq`, `Ord`, or an interface, `Shape`, `m.Shape` or, for a
/// generic interface, `Source<T>`.
#[derive(Debug, Clone, PartialEq)]
pub struct Bound {
    /// `m` in `m.Shape`.
    pub module: Option<Ident>,
    pub name: Ident,
    /// The interface's type arguments; empty when none are written.
    pub args: Vec<Type>,
    /// The whole bound, module and arguments included.
    pub span: Span,
}

/// `interface Name { fun area(self): f64; … }`.
#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDecl {
    /// `private` or `public`, if written.
    pub visibility: Option<Visibility>,
    pub name: Ident,
    /// Type parameters, `interface Source<T>`; empty when none.
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
    /// A default body: the whole item as a method, which a type gets
    /// unless it has its own.
    pub default: Option<super::FunDecl>,
    /// From `fun` to the `;` or the end of the body.
    pub span: Span,
}

/// `private` or `public` before a declaration or a field.
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
    /// Other packages as well.
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
