//! What the checker found, even in a program with errors (v16 draft
//! decision 20): the language server's hover reads it. `check_modules` and
//! everything codegen sees are unchanged.

use std::collections::HashMap;

use super::{Type, Unit, modules};
use crate::ast::{Expr, ExprId, ExprKind, Ident};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// A name the checker resolved, by the span where it is written.
#[derive(Debug, Clone, PartialEq)]
pub enum Name {
    /// A `let`, parameter, `for` or `match` binding.
    Local {
        name: String,
        ty: Type,
        mutable: bool,
    },
    /// A function or method, at its declaration or a use: `decl` is the
    /// declaration's name, `call` the call it names, if any (for its
    /// generic instance).
    Function { decl: Span, call: Option<ExprId> },
    /// A struct field at its declaration.
    Field { name: String, ty: Type },
}

/// The checker's findings for a whole program, errors or not.
#[derive(Debug, Clone)]
pub struct Partial {
    /// Each expression's type, by `ExprId`; `None` where the checker never got.
    pub types: Vec<Option<Type>>,
    /// Each typed expression's span, by `ExprId`.
    pub spans: Vec<Option<Span>>,
    /// See `Checked::annotations`.
    pub annotations: HashMap<Span, Type>,
    /// See `Checked::instances`.
    pub instances: HashMap<ExprId, Vec<Type>>,
    /// See `Checked::conversions`.
    pub conversions: HashMap<ExprId, Type>,
    /// See `Checked::callees`.
    pub callees: HashMap<ExprId, String>,
    /// Resolved names by span.
    pub names: HashMap<Span, Name>,
    /// Every error and warning, as `check_modules` reports them.
    pub diagnostics: Vec<Diagnostic>,
}

/// Where the checker writes down names and expression spans as it goes.
#[derive(Debug, Default)]
pub(super) struct Recorder {
    pub spans: Vec<Option<Span>>,
    pub names: HashMap<Span, Name>,
}

impl Recorder {
    pub(super) fn new(count: usize) -> Recorder {
        Recorder {
            spans: vec![None; count],
            names: HashMap::new(),
        }
    }

    pub(super) fn expr(&mut self, expr: &Expr) {
        if let Some(slot) = self.spans.get_mut(expr.id.0 as usize) {
            *slot = Some(expr.span);
        }
    }

    pub(super) fn local(&mut self, name: &Ident, ty: &Type, mutable: bool) {
        let local = Name::Local {
            name: name.name.clone(),
            ty: ty.clone(),
            mutable,
        };
        self.names.insert(name.span, local);
    }

    /// A function or method named at `at`, declared at `decl`.
    pub(super) fn function(&mut self, at: Span, decl: Span, call: Option<&Expr>) {
        let call = call.map(|c| c.id);
        self.names.insert(at, Name::Function { decl, call });
    }

    pub(super) fn field(&mut self, name: &Ident, ty: &Type) {
        let field = Name::Field {
            name: name.name.clone(),
            ty: ty.clone(),
        };
        self.names.insert(name.span, field);
    }
}

/// Where a call writes its callee's name: `f` in `f(x)`, `area` in
/// `p.area()` or `m.f` in `m.f(x)`.
pub(super) fn callee_name(call: &Expr) -> Option<Span> {
    let ExprKind::Call(callee, _) = &call.kind else {
        return None;
    };
    match &callee.kind {
        ExprKind::Name(_) => Some(callee.span),
        ExprKind::Field(_, name) => Some(name.span),
        _ => None,
    }
}

/// Checks `units` as `check_modules` does, returning what was found even
/// when there are errors.
pub fn check_partial(units: &[Unit]) -> Partial {
    let checker = modules::run(units).0;
    Partial {
        types: checker.types,
        spans: checker.hover.spans,
        annotations: checker.annotations,
        instances: checker.instances,
        conversions: checker.conversions,
        callees: checker.callees,
        names: checker.hover.names,
        diagnostics: checker.diagnostics,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::test_util;

    /// The span of the `n`th (0-based) `word` in `src`.
    fn at(src: &str, word: &str, n: usize) -> Span {
        let start = src
            .match_indices(word)
            .nth(n)
            .unwrap_or_else(|| panic!("no {word}"))
            .0;
        Span::new(start, start + word.len())
    }

    /// The span of `word` inside the first `context` in `src`.
    fn within(src: &str, context: &str, word: &str) -> Span {
        let base = at(src, context, 0).start + context.find(word).expect("word in context");
        Span::new(base, base + word.len())
    }

    const SRC: &str = "struct P { x: f64 }\n\
        impl P { fun len(self): f64 = self.x; }\n\
        fun max<T: Ord>(a: T, b: T): T { if a > b { a } else { b } }\n\
        fun main() {\n\
            let p = P { x: 1.0 };\n\
            let mut n = 0;\n\
            for w of [1, 2] { n += w; }\n\
            match Some(3) { Some(k) => println(k), None => {} }\n\
            println(p.len());\n\
            println(max(1.5, 2.5));\n\
        }\n";

    #[test]
    fn locals_of_every_kind_are_named() {
        let partial = test_util::partial(SRC);
        let local = |context, word| partial.names.get(&within(SRC, context, word)).cloned();
        let ty = |name: &str, ty: Type, mutable| {
            Some(Name::Local {
                name: name.to_string(),
                ty,
                mutable,
            })
        };
        assert_eq!(
            local("let p", "p"),
            ty("p", Type::Struct("P".into(), vec![]), false)
        );
        assert_eq!(local("let mut n", "n"), ty("n", Type::I64, true));
        assert_eq!(local("for w", "w"), ty("w", Type::I64, false));
        assert_eq!(local("Some(k)", "k"), ty("k", Type::I64, false));
        assert_eq!(local("(a: T", "a"), ty("a", Type::Param("T".into()), false));
    }

    #[test]
    fn functions_and_methods_are_named_where_declared_and_used() {
        let partial = test_util::partial(SRC);
        let decl_len = at(SRC, "len", 0);
        let decl_max = at(SRC, "max", 0);
        let function = |word, n| match partial.names.get(&at(SRC, word, n)) {
            Some(Name::Function { decl, call }) => (*decl, call.is_some()),
            other => panic!("{word}: {other:?}"),
        };
        assert_eq!(function("len", 0), (decl_len, false));
        assert_eq!(function("len", 1), (decl_len, true));
        assert_eq!(function("max", 0), (decl_max, false));
        assert_eq!(function("max", 1), (decl_max, true));
    }

    #[test]
    fn fields_are_named_at_their_declaration() {
        let partial = test_util::partial(SRC);
        assert_eq!(
            partial.names.get(&within(SRC, "{ x: f64", "x")),
            Some(&Name::Field {
                name: "x".into(),
                ty: Type::F64
            })
        );
    }

    #[test]
    fn expressions_keep_their_spans_and_types() {
        let partial = test_util::partial(SRC);
        let call = at(SRC, "max(1.5, 2.5)", 0);
        let id = partial
            .spans
            .iter()
            .position(|s| *s == Some(call))
            .expect("the call is typed");
        assert_eq!(partial.types[id], Some(Type::F64));
        assert_eq!(partial.instances[&ExprId(id as u32)], [Type::F64]);
    }

    #[test]
    fn a_program_with_errors_still_has_its_other_types() {
        let src = "fun main() { let bad: i64 = \"x\"; let fine = 2.5; println(fine); }";
        let partial = test_util::partial(src);
        let codes: Vec<&str> = partial.diagnostics.iter().map(|d| d.code).collect();
        assert_eq!(
            codes,
            test_util::diagnostics(src)
                .iter()
                .map(|d| d.code)
                .collect::<Vec<_>>()
        );
        assert!(matches!(
            partial.names.get(&at(src, "fine", 0)),
            Some(Name::Local { ty: Type::F64, .. })
        ));
    }

    #[test]
    fn a_valid_program_has_the_types_check_modules_gives() {
        let partial = test_util::partial(SRC);
        let checked = test_util::ok(SRC);
        for (i, ty) in checked.types.iter().enumerate() {
            if *ty != Type::Error {
                assert_eq!(partial.types[i].as_ref(), Some(ty), "expression {i}");
            }
        }
        assert!(partial.diagnostics.is_empty());
    }
}
