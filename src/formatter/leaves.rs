//! Types, patterns, type parameters and parameter lists: short pieces
//! printed as one string each, names copied from the source.

use super::Printer;
use super::doc::{Doc, list, text};
use crate::ast::{
    Bound, Ident, Param, Pattern, PatternKind, Receiver, Type, TypeKind, TypeParam, Visibility,
    VisibilityKind,
};

impl Printer<'_> {
    /// A written type with one space after `,` and `:`; `Self` stays `Self`.
    pub(super) fn ty(&self, t: &Type) -> String {
        if self.text(t.span) == "Self" {
            return "Self".to_string();
        }
        let types = |ts: &[Type]| ts.iter().map(|t| self.ty(t)).collect::<Vec<_>>().join(", ");
        match &t.kind {
            // `(fun(): T)[]`: without the parentheses `[]` is the return type's.
            TypeKind::Array(inner) if matches!(inner.kind, TypeKind::Fun(..)) => {
                format!("({})[]", self.ty(inner))
            }
            TypeKind::Array(inner) => format!("{}[]", self.ty(inner)),
            TypeKind::Option(inner) => format!("Option<{}>", self.ty(inner)),
            TypeKind::Applied(applied) => {
                let mut out = match &applied.module {
                    Some(m) => format!("{}.", self.ident(m)),
                    None => String::new(),
                };
                out.push_str(&self.ident(&applied.name));
                if !applied.args.is_empty() {
                    out.push_str(&format!("<{}>", types(&applied.args)));
                }
                out
            }
            TypeKind::Tuple(elements) => format!("({})", types(elements)),
            TypeKind::Fun(params, ret) => {
                let ret = ret.as_ref().map(|r| format!(": {}", self.ty(r)));
                format!("fun({}){}", types(params), ret.unwrap_or_default())
            }
            // A keyword or a name: one token.
            _ => self.name(t.span),
        }
    }

    /// An identifier as written.
    pub(super) fn ident(&self, i: &Ident) -> String {
        self.name(i.span)
    }

    /// A pattern; an or-pattern nested in another keeps its parentheses.
    pub(super) fn pattern(&self, p: &Pattern) -> String {
        let all = |ps: &[Pattern]| ps.iter().map(|p| self.pattern(p)).collect::<Vec<_>>();
        match &p.kind {
            PatternKind::Wildcard => "_".to_string(),
            PatternKind::Variant(name, None) => self.ident(name),
            PatternKind::Variant(name, Some(slots)) => {
                format!("{}({})", self.ident(name), all(slots).join(", "))
            }
            PatternKind::Int { .. } | PatternKind::Bool(_) => self.name(p.span),
            PatternKind::Str(_) => self.text(p.span).to_string(),
            PatternKind::Type(name, ty) => {
                let name = name.as_ref().map_or("_".to_string(), |n| self.ident(n));
                format!("{name}: {}", self.ty(ty))
            }
            PatternKind::Tuple(elements) => format!("({})", all(elements).join(", ")),
            PatternKind::Or(alternatives) => alternatives
                .iter()
                .map(|a| match a.kind {
                    PatternKind::Or(_) => format!("({})", self.pattern(a)),
                    _ => self.pattern(a),
                })
                .collect::<Vec<_>>()
                .join(" | "),
            PatternKind::Mut(inner) => format!("mut {}", self.pattern(inner)),
        }
    }

    /// `<T: Eq + Ord, U>`, or nothing.
    pub(super) fn type_params(&self, params: &[TypeParam]) -> String {
        if params.is_empty() {
            return String::new();
        }
        let shown: Vec<String> = params
            .iter()
            .map(|p| match p.bounds.as_slice() {
                [] => self.ident(&p.name),
                bounds => {
                    let bounds: Vec<String> = bounds.iter().map(|b| self.bound(b)).collect();
                    format!("{}: {}", self.ident(&p.name), bounds.join(" + "))
                }
            })
            .collect();
        format!("<{}>", shown.join(", "))
    }

    fn bound(&self, b: &Bound) -> String {
        let mut out = match &b.module {
            Some(m) => format!("{}.", self.ident(m)),
            None => String::new(),
        };
        out.push_str(&self.ident(&b.name));
        if !b.args.is_empty() {
            let args: Vec<String> = b.args.iter().map(|t| self.ty(t)).collect();
            out.push_str(&format!("<{}>", args.join(", ")));
        }
        out
    }

    /// `(self, a: T, …)`, broken one per line when too long.
    pub(super) fn params(&self, receiver: Option<Receiver>, params: &[Param]) -> Doc {
        let mut items: Vec<Doc> = receiver
            .map(|r| text(if r.mutable { "mut self" } else { "self" }))
            .into_iter()
            .collect();
        items.extend(params.iter().map(|p| {
            // `out`, `ptr` or `mut` before an extern's parameter (v17).
            let mode = p
                .mode
                .map_or(String::new(), |(m, _)| format!("{} ", m.word()));
            text(format!("{mode}{}: {}", self.ident(&p.name), self.ty(&p.ty)))
        }));
        list("(", items, ")")
    }

    /// `: T`, or nothing for `void`.
    pub(super) fn ret(&self, ret: &Option<Type>) -> String {
        ret.as_ref()
            .map(|t| format!(": {}", self.ty(t)))
            .unwrap_or_default()
    }
}

/// `private `, `public ` or nothing.
pub(super) fn visibility(v: Option<Visibility>) -> &'static str {
    match v.map(|v| v.kind) {
        Some(VisibilityKind::Private) => "private ",
        Some(VisibilityKind::Public) => "public ",
        None => "",
    }
}
