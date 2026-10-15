//! Names from other modules (v2 draft §5, §6; PRP-034): `m.f(…)`, `m.T`,
//! `m.T { … }`, `m.T.f(…)`, `m.T.V`, plus the program-wide rules on externs
//! and `main`.

use crate::ast::{Expr, ExprKind, Ident, Item, Program};
use crate::check::expr::Expect;
use crate::check::{Checker, Type, errors};
use crate::span::Span;

impl Checker {
    /// The type `expr` names, when it names one: `T` (not a local), or `m.T`
    /// for an imported module `m`. Reports E0313 for another module's
    /// `private` type.
    pub(in crate::check) fn type_named_by(&mut self, expr: &Expr) -> Option<Type> {
        let key = match &expr.kind {
            ExprKind::Name(t) if self.local(t).is_none() => self.key(t),
            ExprKind::Field(base, t) => self.member_through(&Self::module_name(base)?, t)?,
            _ => return None,
        };
        let ty = self.named_type(&key)?;
        self.item_visible(&key, expr.span);
        if let Some(decl) = self.type_decl(&key) {
            match &expr.kind {
                ExprKind::Field(base, t) => {
                    self.hover.define(t.span, decl);
                    if let ExprKind::Name(m) = &base.kind {
                        self.module_named(base.span, m);
                    }
                }
                _ => self.hover.define(expr.span, decl),
            }
        }
        Some(ty)
    }

    /// The key a written type name refers to: `T` here, or `m.T` through an
    /// import; `None` with E0305 if there is none.
    pub(in crate::check) fn type_key(
        &mut self,
        module: Option<&Ident>,
        name: &Ident,
        span: Span,
    ) -> Option<String> {
        let key = match module {
            None => self.key(&name.name),
            Some(m) => match self.member_through(&m.name, name) {
                Some(key) => key,
                None => {
                    self.report(errors::unknown_type(
                        &format!("{}.{}", m.name, name.name),
                        span,
                    ));
                    return None;
                }
            },
        };
        // `Result` is the prelude's unless a module declares its own (v4).
        let key = match (module, self.prelude_type(&name.name)) {
            (None, Some(prelude)) if self.named_type(&key).is_none() => prelude,
            _ => key,
        };
        if self.named_type(&key).is_none() {
            let shown = module.map_or_else(
                || name.name.clone(),
                |m| format!("{}.{}", m.name, name.name),
            );
            let d = if self.interfaces.contains_key(&key) {
                errors::interface_as_type(&shown, span)
            } else {
                errors::unknown_type(&shown, span)
            };
            self.report(d);
            return None;
        }
        self.item_visible(&key, span);
        if let Some(decl) = self.type_decl(&key) {
            self.hover.define(name.span, decl);
        }
        if let Some(m) = module {
            self.module_named(m.span, &m.name);
        }
        Some(key)
    }

    /// Where the struct, enum or interface with key `key` is declared.
    pub(in crate::check) fn type_decl(&self, key: &str) -> Option<Span> {
        (self.structs.get(key).map(|s| s.span))
            .or_else(|| self.enums.get(key).map(|e| e.span))
            .or_else(|| self.interfaces.get(key).map(|i| i.span))
    }

    /// Records `name` written at `at` as the module it is imported as.
    pub(in crate::check) fn module_named(&mut self, at: Span, name: &str) {
        if let Some((path, _)) = self.module_path(name) {
            self.hover.module(at, &path);
        }
    }

    /// `m.f(args)`: a call of function `f` of the module imported as `m`.
    /// `None` if `callee` isn't one, so other call forms handle it.
    pub(in crate::check) fn module_call(
        &mut self,
        call: &Expr,
        callee: &Expr,
        args: &[Expr],
        expect: Option<&Expect>,
    ) -> Option<Type> {
        let ExprKind::Field(base, f) = &callee.kind else {
            return None;
        };
        let m = Self::module_name(base)?;
        self.module_path(&m)?;
        if let ExprKind::Name(name) = &base.kind {
            self.module_named(base.span, name);
        }
        // A module and a function name aren't values: `void` marks them.
        self.record(base, Type::Void);
        self.record(callee, Type::Void);
        let key = self.member_through(&m, f)?;
        if !self.functions.contains_key(&key) {
            self.report(errors::undefined(&f.name, f.span));
            return Some(self.unchecked_args(args));
        }
        self.item_visible(&key, f.span);
        Some(self.call_key(call, &key, args, expect))
    }

    /// E0306 unless every declaration of the C function `name` has the
    /// same signature: they all become one symbol (v2 draft §8).
    pub(in crate::check) fn same_extern(
        &mut self,
        name: &str,
        params: &[Type],
        ret: &Type,
        span: Span,
    ) {
        match self.externs.get(name) {
            Some((p, r, first)) if (p.as_slice(), r) != (params, ret) => {
                let first = *first;
                self.report(errors::extern_mismatch(name, span, first));
            }
            Some(_) => {}
            None => {
                let entry = (params.to_vec(), ret.clone(), span);
                self.externs.insert(name.to_string(), entry);
            }
        }
    }

    /// E0304 for a `main` outside the main file (v2 draft §6).
    pub(in crate::check) fn main_belongs_in_the_main_file(&mut self, program: &Program) {
        for item in &program.items {
            if let Item::Fun(f) = item
                && f.name.name == "main"
            {
                self.report(errors::main_in_module(f.name.span));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_util::{errors, ok};

    const GEOMETRY: &str = "struct Vec2 { x: f64, y: f64 }\n\
        impl Vec2 { fun new(): Vec2 = Vec2 { x: 0.0, y: 0.0 }; fun len(self): f64 = self.x; }\n\
        enum Shape { Circle(f64), Empty }\n\
        fun origin(): Vec2 = Vec2 { x: 0.0, y: 0.0 };\n";

    #[test]
    fn imported_items_are_used_through_the_module_name() {
        ok(&[
            (
                "main.la",
                "import geometry;\n\
                 fun len(v: geometry.Vec2): f64 = v.len();\n\
                 fun make(): geometry.Vec2 = geometry.Vec2 { x: 1.0, y: 2.0 };\n\
                 fun main() {\n\
                     let a = geometry.origin();\n\
                     let b = geometry.Vec2.new();\n\
                     let s = geometry.Shape.Circle(1.0);\n\
                     let e = geometry.Shape.Empty;\n\
                     let n = len(a) + len(b) + make().x;\n\
                     let k = match s { Circle(r) => r, Empty => 0.0 };\n\
                 }\n",
            ),
            ("geometry.la", GEOMETRY),
        ]);
    }

    #[test]
    fn each_module_has_its_own_names() {
        ok(&[
            (
                "main.la",
                "import a;\nstruct Point { x: i64 }\nfun main() { let p = a.make(); let q = Point { x: p.y }; }\n",
            ),
            (
                "a.la",
                "struct Point { y: i64 }\nfun make(): Point = Point { y: 1 };\n",
            ),
        ]);
    }

    #[test]
    fn unknown_names_and_misplaced_items_are_reported() {
        let files = |main: &str| {
            vec![
                ("main.la", main.to_string()),
                ("geometry.la", GEOMETRY.to_string()),
            ]
        };
        let run = |main: &str| {
            let f = files(main);
            let f: Vec<(&str, &str)> = f.iter().map(|(a, b)| (*a, b.as_str())).collect();
            errors(&f)
        };
        assert_eq!(
            run("import geometry;\nfun main() { let a = geometry.nope(); }\n"),
            [("E0301", "nope".to_string())]
        );
        assert_eq!(
            run("import geometry;\nfun f(v: geometry.Nope) {}\nfun main() {}\n"),
            [("E0305", "geometry.Nope".to_string())]
        );
        assert_eq!(
            run("import geometry;\nfun main() { let geometry = 1; }\n"),
            [("E0311", "geometry".to_string())]
        );
        assert_eq!(
            errors(&[
                ("main.la", "import a;\nfun main() {}\n"),
                ("a.la", "fun main() {}\n")
            ]),
            [("E0304", "main".to_string())]
        );
        assert_eq!(
            errors(&[
                (
                    "main.la",
                    "import a;\nextern fun abs(x: i32): i32;\nfun main() {}\n"
                ),
                ("a.la", "extern fun abs(x: i64): i64;\n"),
            ]),
            [("E0306", "abs".to_string())]
        );
    }

    #[test]
    fn mismatches_print_full_module_paths() {
        let d = errors(&[
            (
                "main.la",
                "import geometry;\nfun main() { let v: geometry.Vec2 = 1; }\n",
            ),
            ("geometry.la", GEOMETRY),
        ]);
        assert_eq!(d[0].0, "E0401");
        let loaded = super::super::test_util::loaded(&[
            (
                "main.la",
                "import geometry;\nfun main() { let v: geometry.Vec2 = geometry.Shape.Empty; }\n",
            ),
            ("geometry.la", GEOMETRY),
        ]);
        let d = super::super::test_util::check(&loaded).unwrap_err();
        assert_eq!(d[0].message, "expected geometry.Vec2, found geometry.Shape");
    }
}
