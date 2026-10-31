//! Global constants (v17, milestone 109): `const NAME: T = value;`.
//! Collected with the functions, checked after them, evaluated by codegen
//! once before `main` in `Checked::consts` order. Since milestone 147 the
//! value is any pure expression (`purity.rs`).

use std::collections::{HashMap, HashSet};

use super::expr::Expect;
use super::purity::{Reads, const_owner, cycle_text};
use super::{Checker, Type, errors};
use crate::ast::{ConstDecl, Expr, Item, Program};
use crate::span::Span;

/// What the checker knows about one constant.
#[derive(Debug, Clone)]
pub(super) struct ConstInfo {
    /// The written type.
    pub ty: Type,
    /// The name at the declaration.
    pub span: Span,
    /// The constants its value reads, by key, with the functions on the way
    /// (v17, milestone 147).
    pub deps: Reads,
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
        // Under its own owner, so its calls, effects and reads are recorded
        // like a body's (v17, milestone 147).
        self.owner = const_owner(key);
        self.expect_type(&c.value, &Expect::of(ty));
        self.owner.clear();
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
        self.read_const(key);
        let name = key.rsplit('.').next().unwrap_or(key).to_string();
        self.hover.constant(at, decl, &name, &ty);
        ty
    }

    /// After every body: E0445 for a constant reaching outside the program,
    /// then the order constants are evaluated in, each after those it reads
    /// (through calls too, milestone 147); E0446 for every cycle.
    pub(super) fn order_consts(&mut self) {
        let mut keys: Vec<String> = self.consts.keys().cloned().collect();
        keys.sort_by_key(|k| self.consts[k].span.start);
        let found: Vec<_> = {
            let index = self.calls_by_owner();
            keys.iter().map(|k| self.reach_const(k, &index)).collect()
        };
        for (key, (impure, reads)) in keys.iter().zip(found) {
            if let Some(d) = impure {
                self.report(d);
            }
            self.consts.get_mut(key).expect("listed").deps = reads;
        }
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
                let short = |k: &str| k.rsplit('.').next().unwrap_or(k).to_string();
                let steps: Vec<(String, Vec<String>)> = path[start..]
                    .iter()
                    .enumerate()
                    .map(|(i, k)| {
                        let next = path.get(start + i + 1).map_or(key, String::as_str);
                        let via = self.consts[k.as_str()]
                            .deps
                            .iter()
                            .find(|(d, _)| d == next)
                            .map(|(_, via)| via.clone())
                            .unwrap_or_default();
                        (short(k), via)
                    })
                    .collect();
                let steps: Vec<(&str, &[String])> = steps
                    .iter()
                    .map(|(n, via)| (n.as_str(), via.as_slice()))
                    .collect();
                let text = cycle_text(&steps, &short(key));
                let span = self.consts[key].span;
                self.report(errors::const_cycle(&text, span));
                return;
            }
            None => {}
        }
        state.insert(key.to_string(), false);
        path.push(key.to_string());
        for (dep, _) in self.consts[key].deps.clone() {
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
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

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
    fn any_pure_expression_is_a_value_but_types_stay_plain() {
        ok("fun f(): i64 = 1;\nconst A: i64 = 1;\n\
            const B: i64 = f() + if A > 0 { 1 } else { 2 } + { let x = 3; x };\n\
            const S: string = \"{A}\";\nfun main() {}");
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
        // Through a function, whatever the order (milestone 147).
        let checked = ok(
            "const T: i64 = twice();\nfun twice(): i64 = N * 2;\nconst N: i64 = 4;\nfun main() {}",
        );
        let order: Vec<&str> = checked.consts.iter().map(|(k, _)| k.as_str()).collect();
        assert_eq!(order, ["N", "T"]);
        let src = "const A: i64 = f();\nfun f(): i64 = A + 1;\nfun main() {}";
        assert_eq!(errors(src), [("E0446", "A")]);
        let help = diagnostics(src)[0].help.clone().unwrap_or_default();
        assert!(help.ends_with("A calls `f`, which reads A"), "{help}");
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
