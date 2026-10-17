//! Maps and sets in the checker (v17, milestone 101, PRP-133): `#{ … }`
//! literals as calls of the prelude's `$map_of`/`$set_of`, keys without
//! floats (E0441), and equal literal keys (W0105).

use super::expr::Expect;
use super::generics::subst;
use super::{Checker, Type};
use crate::ast::{AssignOp, Expr, ExprKind, HashKind, HashLiteral, Ident};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// The prelude's map and set types.
pub(super) const MAP: &str = "$prelude.Map";
pub(super) const SET: &str = "$prelude.Set";

/// The pseudo-callee an `==` records with its operand type, so each instance
/// lists the `same` methods of the maps and sets it compares (`same_instances`).
pub(super) const EQUAL: &str = "$equal";

/// The key type of a map or set type: `K` of `Map<K, V>`, `T` of `Set<T>`.
pub(super) fn key_of(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Struct(k, args) if k == MAP || k == SET => args.first(),
        _ => None,
    }
}

/// True if `ty` holds a float anywhere a key's hash would reach.
fn has_float(checker: &Checker, ty: &Type, seen: &mut Vec<String>) -> bool {
    match ty {
        Type::F32 | Type::F64 => true,
        Type::Array(inner) | Type::Option(inner) => has_float(checker, inner, seen),
        Type::Struct(key, args) | Type::Enum(key, args) => {
            let name = ty.to_string();
            if seen.contains(&name) {
                return false;
            }
            seen.push(name);
            if args.iter().any(|a| has_float(checker, a, seen)) {
                return true;
            }
            let members: Vec<Type> = match (checker.structs.get(key), checker.enums.get(key)) {
                (Some(s), _) => s
                    .fields
                    .iter()
                    .map(|(_, t)| subst(t, &s.generics, args))
                    .collect(),
                (_, Some(e)) => e
                    .variants
                    .iter()
                    .flat_map(|(_, ts)| ts.iter().map(|t| subst(t, &e.generics, args)))
                    .collect(),
                _ => Vec::new(),
            };
            members.iter().any(|t| has_float(checker, t, seen))
        }
        _ => false,
    }
}

impl Checker {
    /// `#{ … }`: a call of `$map_of(keys, values)` or `$set_of(keys)` with
    /// this expression's id; `#{}` takes which from `expect` (E0412 without).
    pub(super) fn hashed(
        &mut self,
        expr: &Expr,
        lit: &HashLiteral,
        expect: Option<&Expect>,
    ) -> Type {
        let expected = expect.map(|e| &e.ty);
        let kind = match (lit.kind, expected) {
            (HashKind::Empty, Some(Type::Struct(k, _))) if k == MAP => HashKind::Map,
            (HashKind::Empty, Some(Type::Struct(k, _))) if k == SET => HashKind::Set,
            (HashKind::Empty, Some(Type::Error)) => return Type::Error,
            (HashKind::Empty, _) => {
                let d = Diagnostic::error("E0412", "cannot infer the type of `#{}`", expr.span)
                    .with_help(
                        "annotate it: `let m: Map<string, i64> = #{};` or `let s: Set<i64> = #{};`",
                    );
                self.report(d);
                return Type::Error;
            }
            (kind, _) => kind,
        };
        self.duplicate_keys(lit);
        let ty = match kind {
            HashKind::Set => {
                let key = super::qualify(&["$prelude".to_string()], "$set_of");
                self.call_key(expr, &key, std::slice::from_ref(&lit.keys), expect)
            }
            _ => {
                let key = super::qualify(&["$prelude".to_string()], "$map_of");
                let args = [lit.keys.clone(), lit.values.clone()];
                self.call_key(expr, &key, &args, expect)
            }
        };
        // A written type was checked where it was written; an inferred one here.
        if expect.is_none() {
            self.float_keys(&ty, expr.span);
        }
        ty
    }

    /// `m[k] = v` on a map (v17): its `insert` method, with `m` as a `mut
    /// self` receiver; true when `place` was such an entry and is checked.
    /// `m[k] += v` is E0404 (write `m[k] = m[k] + v`).
    pub(super) fn map_assign(
        &mut self,
        op: AssignOp,
        place: &Expr,
        value: &Expr,
        mark: usize,
    ) -> bool {
        let ExprKind::Index(base, _, key) = &place.kind else {
            return false;
        };
        // The base's type decides; a probe whose findings the normal path repeats.
        let reported = self.diagnostics.len();
        let ty = self.value(base, None);
        if !matches!(&ty, Type::Struct(k, _) if k == MAP) {
            self.diagnostics.truncate(reported);
            return false;
        }
        if op != AssignOp::Assign {
            let span = Span::new(place.span.start, value.span.end);
            let d = Diagnostic::error(
                "E0404",
                format!("`{}` can't change a map entry in place", op.symbol()),
                span,
            )
            .with_help("write `m[k] = m[k] + v`, or use `m.get(k)` for a missing key");
            self.report(d);
            self.expr(key, None);
            self.expr(value, None);
            return true;
        }
        let insert = Ident {
            name: "insert".to_string(),
            span: place.span,
        };
        let args = [(**key).clone(), value.clone()];
        self.on_value(place, (base, &ty), &insert, &args, (mark, None));
        true
    }

    /// Records that `at` compares values of `ty` with `==`: a map or set
    /// inside compares through its `same` (order doesn't matter, v17).
    pub(super) fn equality_used(&mut self, at: &Expr, ty: &Type) {
        if !ty.is_numeric() && !matches!(ty, Type::Bool | Type::String | Type::Char | Type::Error) {
            self.used(at, EQUAL, std::slice::from_ref(ty));
        }
    }

    /// The `same` instances comparing values of concrete type `ty` reaches:
    /// each map's and set's, found through fields, payloads and elements.
    pub(super) fn same_instances(&self, ty: &Type) -> Vec<super::instances::Instance> {
        let mut found = Vec::new();
        self.reach_same(ty, &mut Vec::new(), &mut found);
        found
    }

    fn reach_same(
        &self,
        ty: &Type,
        seen: &mut Vec<Type>,
        found: &mut Vec<super::instances::Instance>,
    ) {
        if seen.contains(ty) {
            return;
        }
        seen.push(ty.clone());
        match ty {
            Type::Array(inner) | Type::Option(inner) => self.reach_same(inner, seen, found),
            Type::Struct(key, args) if key == MAP || key == SET => {
                if let Some(m) = self.methods.get(key.as_str()).and_then(|ms| ms.get("same")) {
                    found.push(super::instances::Instance {
                        key: m.key.clone(),
                        args: args.clone(),
                    });
                }
            }
            Type::Struct(key, args) | Type::Enum(key, args) => {
                let mut parts: Vec<Type> = Vec::new();
                if let Some(info) = self.structs.get(key) {
                    parts.extend(
                        info.fields
                            .iter()
                            .map(|(_, t)| subst(t, &info.generics, args)),
                    );
                }
                if let Some(info) = self.enums.get(key) {
                    for (_, payloads) in &info.variants {
                        parts.extend(payloads.iter().map(|t| subst(t, &info.generics, args)));
                    }
                }
                for part in &parts {
                    self.reach_same(part, seen, found);
                }
            }
            _ => {}
        }
    }

    /// E0441 at `at` if `ty` is a map or set whose keys hold a float.
    pub(super) fn float_keys(&mut self, ty: &Type, at: Span) {
        let Some(key) = key_of(ty) else { return };
        if has_float(self, key, &mut Vec::new()) {
            let what = if matches!(ty, Type::Struct(k, _) if k == SET) {
                "a set's values"
            } else {
                "a map's keys"
            };
            let key = key.to_string().replace("$prelude.", "");
            let d = Diagnostic::error("E0441", format!("{what} can't hold a float: `{key}`"), at)
                .with_help(
                    "`NaN != NaN`, so a float key could never be found again; use an integer",
                );
            self.report(d);
        }
    }

    /// W0105 for a literal key equal to an earlier one in the same literal.
    fn duplicate_keys(&mut self, lit: &HashLiteral) {
        let (message, help) = match lit.kind {
            HashKind::Set => (
                "this value is already in the set",
                "a set holds it once; remove one of them",
            ),
            _ => (
                "this key is already in the map",
                "the later entry wins; remove one of them",
            ),
        };
        let ExprKind::Array(keys) = &lit.keys.kind else {
            return;
        };
        // Literal keys, compared as written: numbers, strings, chars, bools.
        let literal = |e: &Expr| match &e.kind {
            ExprKind::Int(n) => Some(format!("i{n}")),
            ExprKind::Str(t) => Some(format!("s{t}")),
            ExprKind::Char(c) => Some(format!("c{c}")),
            ExprKind::Bool(b) => Some(format!("b{b}")),
            _ => None,
        };
        let mut seen: Vec<String> = Vec::new();
        for k in keys {
            let Some(l) = literal(k) else { continue };
            if seen.contains(&l) {
                let d = Diagnostic::warning("W0105", message, k.span).with_help(help);
                self.report(d);
            } else {
                seen.push(l);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics_or_warnings, errors, let_type, ok};

    #[test]
    fn literals_take_their_types_from_entries_or_context() {
        let ty = let_type("fun main() { let m = #{\"a\": 1}; }", "m");
        assert_eq!(ty.to_string(), "$prelude.Map<string, i64>");
        let ty = let_type("fun main() { let s = #{'a', 'b'}; }", "s");
        assert_eq!(ty.to_string(), "$prelude.Set<char>");
        let ty = let_type("fun main() { let m: Map<u8, bool> = #{}; }", "m");
        assert!(matches!(ty, Type::Struct(k, _) if k == "$prelude.Map"));
        assert_eq!(errors("fun main() { let e = #{}; }"), [("E0412", "#{}")]);
        assert_eq!(
            errors("fun main() { let m = #{\"a\": 1, \"b\": 2.5}; }")[0].0,
            "E0401"
        );
    }

    #[test]
    fn keys_hold_no_floats_and_entries_change_by_assignment() {
        assert_eq!(errors("fun f(s: Set<f64>) {}\nfun main() {}")[0].0, "E0441");
        assert_eq!(errors("fun main() { let m = #{1.5: 1}; }")[0].0, "E0441");
        assert_eq!(
            errors("fun main() { let m = #{\"a\": 1}; m[\"a\"] = 2; }"),
            [("E0501", "m")]
        );
        assert_eq!(
            errors("fun main() { let mut m = #{\"a\": 1}; m[\"a\"] += 2; }")[0].0,
            "E0404"
        );
        ok("fun main() { let mut m = #{\"a\": 1}; m[\"a\"] = m[\"a\"] + 2; }");
    }

    #[test]
    fn equal_literal_keys_are_w0105() {
        let found = diagnostics_or_warnings(
            "fun main() { let m = #{\"a\": 1, \"a\": 2}; let s = #{1, 1}; }",
        );
        let codes: Vec<&str> = found.iter().map(|d| d.code).collect();
        assert_eq!(codes, ["W0105", "W0105"], "{found:?}");
        assert!(found[0].message.contains("map") && found[1].message.contains("set"));
    }

    #[test]
    fn a_modules_own_map_hides_the_preludes() {
        ok("struct Map { n: i64 }\nfun main() { let m: Map = Map { n: 1 }; let p = #{1: 2}; }");
    }
}
