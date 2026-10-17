//! Global constants (v17, milestone 109): `const NAME: T = value;`.
//! Collected with the functions, checked after them, evaluated by codegen
//! once before `main` in `Checked::consts` order.

use std::collections::{HashMap, HashSet};

use super::expr::Expect;
use super::{Checker, Type, errors};
use crate::ast::{ConstDecl, Expr, ExprKind, Item, Program};
use crate::span::Span;

/// What the checker knows about one constant.
#[derive(Debug, Clone)]
pub(super) struct ConstInfo {
    /// The written type.
    pub ty: Type,
    /// The name at the declaration.
    pub span: Span,
    /// The constants its value reads, by key.
    pub deps: Vec<String>,
}

impl Checker {
    /// Pass: every constant's name and type (after the functions, so a
    /// clash with one is reported here).
    pub(super) fn collect_consts(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Const(c) = item else { continue };
            let key = self.key(&c.name.name);
            let ty = self.resolve(&c.ty);
            if let Some(first) = self
                .functions
                .get(&key)
                .map(|f| f.span)
                .or_else(|| self.type_span(&key))
                .or_else(|| self.consts.get(&key).map(|c| c.span))
            {
                self.report(errors::duplicate(&c.name.name, c.name.span, first));
                continue;
            }
            if !self.const_type(&ty, &mut HashSet::new()) {
                self.report(errors::const_type(&ty, c.ty.span));
            }
            self.register_item(&key, c.visibility);
            self.hover
                .constant(c.name.span, c.name.span, &c.name.name, &ty);
            let info = ConstInfo {
                ty,
                span: c.name.span,
                deps: Vec::new(),
            };
            self.consts.insert(key, info);
        }
    }

    /// Every constant's value in the current module: its type, then the
    /// constant-expression rule (E0445) and the constants it reads.
    pub(super) fn const_values(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Const(c) = item else { continue };
            let key = self.key(&c.name.name);
            if self.consts.get(&key).is_none_or(|i| i.span != c.name.span) {
                continue;
            }
            self.const_value(&key, c);
        }
    }

    fn const_value(&mut self, key: &str, c: &ConstDecl) {
        let ty = self.consts[key].ty.clone();
        self.ret = Type::Unit;
        self.expect_type(&c.value, &Expect::of(ty));
        let mut deps = Vec::new();
        if let Some(bad) = self.non_constant(&c.value, &mut deps) {
            self.report(errors::not_constant(bad));
        }
        self.consts.get_mut(key).expect("collected").deps = deps;
    }

    /// The first part of `e` that isn't a constant expression, if any;
    /// the constants it reads go to `deps`.
    fn non_constant(&mut self, e: &Expr, deps: &mut Vec<String>) -> Option<Span> {
        let all = |c: &mut Self, es: &[&Expr], deps: &mut Vec<String>| {
            es.iter().find_map(|x| c.non_constant(x, deps))
        };
        match &e.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Char(_)
            | ExprKind::Str(_)
            | ExprKind::Bool(_) => None,
            ExprKind::Name(name) => {
                if let Some(key) = self.const_uses.get(&e.id) {
                    deps.push(key.clone());
                    return None;
                }
                (name != "None").then_some(e.span)
            }
            ExprKind::Field(base, _) => {
                if let Some(key) = self.const_uses.get(&e.id) {
                    deps.push(key.clone());
                    return None;
                }
                if self.callees.contains_key(&e.id) || self.variant_values.contains_key(&e.id) {
                    return Some(e.span);
                }
                // A type or module name (`i16.MAX`, `Color.Red`) holds nothing to check.
                if self.type_of(base) == Some(Type::Unit) {
                    return None;
                }
                self.non_constant(base, deps)
            }
            ExprKind::Unary(_, x) | ExprKind::Cast(x, _) => self.non_constant(x, deps),
            ExprKind::Binary(_, _, a, b) | ExprKind::Index(a, _, b) | ExprKind::Repeat(a, b) => {
                all(self, &[a, b], deps)
            }
            ExprKind::Tuple(xs) | ExprKind::Array(xs) => {
                xs.iter().find_map(|x| self.non_constant(x, deps))
            }
            ExprKind::StructLit(lit) => {
                if let Some(base) = &lit.base {
                    return Some(base.span);
                }
                lit.fields
                    .iter()
                    .find_map(|(_, x)| self.non_constant(x, deps))
            }
            // `Some(x)`, `Ok(x)`, `Err(x)`, `E.V(x)`: constructors only.
            ExprKind::Call(callee, args) if self.constructor(e, callee) => {
                args.iter().find_map(|x| self.non_constant(x, deps))
            }
            _ => Some(e.span),
        }
    }

    /// True if `callee` (of `call`) builds a variant rather than calling.
    fn constructor(&self, call: &Expr, callee: &Expr) -> bool {
        if self.callees.contains_key(&call.id) || self.callees.contains_key(&callee.id) {
            return false;
        }
        match &callee.kind {
            ExprKind::Name(n) => matches!(n.as_str(), "Some" | "Ok" | "Err"),
            ExprKind::Field(base, _) => self.type_of(base) == Some(Type::Unit),
            _ => false,
        }
    }

    /// The type recorded for `e`, if any.
    fn type_of(&self, e: &Expr) -> Option<Type> {
        self.types.get(e.id.0 as usize).cloned().flatten()
    }

    /// True if a constant may have type `ty`: plain data, no functions,
    /// boxes, tasks, channels, actors, handles, maps or sets.
    fn const_type(&self, ty: &Type, seen: &mut HashSet<String>) -> bool {
        match ty {
            Type::Bool | Type::Char | Type::String | Type::Unit | Type::Error => true,
            Type::Array(inner) | Type::Option(inner) => self.const_type(inner, seen),
            Type::Struct(key, _) if key == "$prelude.Map" || key == "$prelude.Set" => false,
            Type::Struct(key, _) => {
                if !seen.insert(key.clone()) {
                    return true;
                }
                let fields = self
                    .structs
                    .get(key)
                    .map(|i| i.fields.clone())
                    .unwrap_or_default();
                let args = match ty {
                    Type::Struct(_, args) => args.clone(),
                    _ => Vec::new(),
                };
                let generics = self
                    .structs
                    .get(key)
                    .map(|i| i.generics.clone())
                    .unwrap_or_default();
                fields.iter().all(|(_, f)| {
                    let f = super::generics::subst(f, &generics, &args);
                    self.const_type(&f, seen)
                })
            }
            Type::Enum(key, args) => {
                if !seen.insert(ty.to_string()) {
                    return true;
                }
                let variants = self
                    .enums
                    .get(key)
                    .map(|i| i.variants.clone())
                    .unwrap_or_default();
                let generics = self
                    .enums
                    .get(key)
                    .map(|i| i.generics.clone())
                    .unwrap_or_default();
                variants.iter().flat_map(|(_, ps)| ps.iter()).all(|p| {
                    let p = super::generics::subst(p, &generics, args);
                    self.const_type(&p, seen)
                })
            }
            _ => ty.is_numeric(),
        }
    }

    /// `NAME` read as a value: the constant's type, recorded for codegen.
    pub(super) fn const_read(&mut self, expr: &Expr, at: Span, key: &str) -> Type {
        let info = &self.consts[key];
        let (ty, decl) = (info.ty.clone(), info.span);
        self.const_uses.insert(expr.id, key.to_string());
        let name = key.rsplit('.').next().unwrap_or(key).to_string();
        self.hover.constant(at, decl, &name, &ty);
        ty
    }

    /// After every body: the order constants are evaluated in, each after
    /// those it reads; E0446 for every cycle.
    pub(super) fn order_consts(&mut self) {
        let mut keys: Vec<String> = self.consts.keys().cloned().collect();
        keys.sort_by_key(|k| self.consts[k].span.start);
        let mut state: HashMap<String, bool> = HashMap::new();
        let mut order = Vec::new();
        for key in &keys {
            let mut path = Vec::new();
            self.visit_const(key, &mut state, &mut path, &mut order);
        }
        self.const_order = order;
    }

    fn visit_const(
        &mut self,
        key: &str,
        state: &mut HashMap<String, bool>,
        path: &mut Vec<String>,
        order: &mut Vec<String>,
    ) {
        match state.get(key) {
            Some(true) => return,
            Some(false) => {
                let start = path.iter().position(|k| k == key).unwrap_or(0);
                let mut names: Vec<&str> = path[start..]
                    .iter()
                    .map(|k| k.rsplit('.').next().unwrap_or(k))
                    .collect();
                names.push(key.rsplit('.').next().unwrap_or(key));
                let span = self.consts[key].span;
                self.report(errors::const_cycle(&names.join(" -> "), span));
                return;
            }
            None => {}
        }
        state.insert(key.to_string(), false);
        path.push(key.to_string());
        for dep in self.consts[key].deps.clone() {
            self.visit_const(&dep, state, path, order);
        }
        path.pop();
        state.insert(key.to_string(), true);
        order.push(key.to_string());
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type, ok};

    #[test]
    fn constants_are_values_of_their_type() {
        let checked = ok("const B: i64 = A * 2;\nconst A: i64 = 1;\n\
                          const S: string[] = [\"a\"; 2];\n\
                          fun main() { let x = B; let s = S; }");
        let order: Vec<&str> = checked.consts.iter().map(|(k, _)| k.as_str()).collect();
        assert_eq!(order, ["A", "B", "S"]);
        assert_eq!(
            let_type("const A: u8 = 1;\nfun main() { let x = A; }", "x"),
            Type::U8
        );
        // A local shadows a constant.
        assert_eq!(
            let_type(
                "const A: u8 = 1;\nfun main() { let A = true; let x = A; }",
                "x"
            ),
            Type::Bool
        );
    }

    #[test]
    fn only_constant_expressions_are_e0445() {
        for (value, at) in [
            ("f()", "f()"),
            ("fun(): i64 = 1", "fun(): i64 = 1"),
            ("if true { 1 } else { 2 }", "if true { 1 } else { 2 }"),
            ("{ 1 }", "{ 1 }"),
            ("f", "f"),
            ("\"{A}\".len", "\"{A}\""),
        ] {
            let src = format!(
                "fun f(): i64 = 1;\nconst A: i64 = 1;\nconst B: i64 = {value};\nfun main() {{}}"
            );
            assert_eq!(errors(&src).last(), Some(&("E0445", at)), "{value}");
        }
        assert_eq!(
            errors("const R: Ref<i64> = Ref.new(1);\nfun main() {}")[0],
            ("E0445", "Ref<i64>")
        );
        assert_eq!(
            errors("const F: fun(): i64 = f;\nfun f(): i64 = 1;\nfun main() {}")[0],
            ("E0445", "fun(): i64")
        );
    }

    #[test]
    fn cycles_are_e0446() {
        assert_eq!(
            errors("const A: i64 = B;\nconst B: i64 = A;\nfun main() {}"),
            [("E0446", "A")]
        );
        assert_eq!(
            errors("const A: i64 = B + 1;\nconst B: i64 = C;\nconst C: i64 = A;\nfun main() {}"),
            [("E0446", "A")]
        );
    }

    #[test]
    fn constants_are_not_places() {
        let src = "const A: i64 = 1;\nconst XS: i64[] = [1];\n\
                   fun main() { A = 2; XS[0] = 3; }";
        assert_eq!(errors(src), [("E0501", "A"), ("E0501", "XS[0]")]);
        assert_eq!(
            errors("const A: i64 = 1;\nfun A() {}\nfun main() {}")[0].0,
            "E0302"
        );
        assert_eq!(
            errors("const A: i64 = 1;\nconst A: i64 = 2;\nfun main() {}")[0].0,
            "E0302"
        );
        assert_eq!(errors("const A: i64 = \"s\";\nfun main() {}")[0].0, "E0403");
    }
}

#[cfg(test)]
mod module_tests {
    use crate::check::modules::test_util::{errors, ok};

    #[test]
    fn constants_follow_visibility_and_reexports() {
        let lib = (
            "geo.la",
            "public const UNIT: i64 = 1;\nprivate const HIDDEN: i64 = 2;\nconst PACKAGE: i64 = 3;\n",
        );
        ok(&[
            (
                "main.la",
                "import geo;\nfun main() { let a = geo.UNIT + geo.PACKAGE; }",
            ),
            lib,
        ]);
        let found = errors(&[
            ("main.la", "import geo;\nfun main() { let a = geo.HIDDEN; }"),
            lib,
        ]);
        assert_eq!(found[0].0, "E0313", "{found:?}");
        ok(&[
            (
                "main.la",
                "import shapes;\nfun main() { let a = shapes.UNIT; }",
            ),
            ("shapes.la", "public import geo { * };\n"),
            lib,
        ]);
    }
}
