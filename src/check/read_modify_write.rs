//! Writing through `[ ]`. A map's entry is a place: a write inside `m[k]`
//! finds the entry's slot (the map's `$slot`, recorded at the `[ ]`'s
//! `write_id`) and changes the value where it is. An element a type reads
//! through `index` is a copy: a write inside it (`g[i].x = v`, `g[i] += v`,
//! `g[i].push(x)`) reads the element, changes the copy and hands it back to
//! `set_index`, recorded at the `[ ]`'s `write_id` with the copy as an
//! expression at its `element_id`.

use std::collections::HashMap;

use super::indexing::by_method;
use super::maps::MAP;
use super::{Binding, Checker, Local, Type};
use crate::ast::{Expr, ExprKind, Ident};

/// The name the copy of an element goes by while `set_index` is checked;
/// no program can write it.
const ELEMENT: &str = "$element";

impl Checker {
    /// `place` changed through the element a type's `index` gives: true
    /// when the type has a fitting `set_index`, whose receiver then follows
    /// the place rules (which handle an element further in, the same way).
    pub(super) fn write_through(&mut self, place: &Expr) -> bool {
        let link = outer_link(place, &|e| self.indexes(e)).expect("a place through `index`");
        let ExprKind::Index(base, _, key) = &link.kind else {
            unreachable!("a link is a `[ ]`")
        };
        let ty = self.types[base.id.0 as usize]
            .clone()
            .unwrap_or(Type::Error);
        let element = self.types[link.id.0 as usize]
            .clone()
            .unwrap_or(Type::Error);
        if let Err(help) = self.set_index_fits(&ty) {
            let message = format!("cannot change an element of `{ty}`: it has no `set_index`");
            let d = crate::diagnostic::Diagnostic::error("E0411", message, base.span);
            self.report(d.with_help(help));
            return false;
        }
        self.through_index.insert(place.id);
        // The copy handed back is a hidden local, not the place itself, so
        // passing it doesn't count as passing `g` to a call that changes `g`.
        let copy = Expr {
            id: link.element_id(),
            span: link.span,
            kind: ExprKind::Name(ELEMENT.to_string()),
        };
        let local = Local {
            ty: element,
            binding: Binding::Let { mutable: false },
            span: link.span,
        };
        self.scopes
            .push(HashMap::from([(ELEMENT.to_string(), local)]));
        let method = Ident {
            name: "set_index".to_string(),
            span: link.span,
        };
        let mark = self.changes_mark();
        let call = link.with_id(link.write_id());
        let args = [(**key).clone(), copy];
        self.on_value(&call, (base, &ty), &method, &args, (mark, None));
        self.scopes.pop();
        self.own_changes(mark, place.span);
        true
    }

    /// Records, for every map entry on `place`'s path, the `$slot` lookup a
    /// write through it makes.
    pub(super) fn map_slots(&mut self, place: &Expr) {
        let (ExprKind::Field(base, _) | ExprKind::Index(base, _, _)) = &place.kind else {
            return;
        };
        self.map_slots(base);
        let ExprKind::Index(_, _, key) = &place.kind else {
            return;
        };
        let ty = self.types[base.id.0 as usize].clone();
        if let Some(ty @ Type::Struct(k, _)) = &ty
            && k == MAP
        {
            let method = Ident {
                name: "$slot".to_string(),
                span: key.span,
            };
            let mark = self.changes_mark();
            let call = place.with_id(place.write_id());
            let args = std::slice::from_ref(&**key);
            self.on_value(&call, (base, ty), &method, args, (mark, None));
        }
    }

    /// The expression a compound assignment to `place` keys its operator
    /// method by: `place` itself, or for a `[ ]` that already calls a method
    /// (`$entry`, `index`), its `update_id`.
    pub(super) fn update_key(&self, place: &Expr) -> Expr {
        match &place.kind {
            ExprKind::Index(base, _, _) if self.calls_for_index(base) => {
                place.with_id(place.update_id())
            }
            _ => place.clone(),
        }
    }

    /// True when `base[…]` calls `base`'s own `index`.
    fn indexes(&self, base: &Expr) -> bool {
        self.types[base.id.0 as usize]
            .as_ref()
            .is_some_and(by_method)
    }

    /// True when `base[…]` is a method call: `index`, or a map's `$entry`.
    fn calls_for_index(&self, base: &Expr) -> bool {
        self.types[base.id.0 as usize]
            .as_ref()
            .is_some_and(|ty| by_method(ty) || matches!(ty, Type::Struct(k, _) if k == MAP))
    }

    /// `Ok` when `ty` has a `set_index` of the shape, else the help.
    fn set_index_fits(&self, ty: &Type) -> Result<(), String> {
        self.index_method(ty, "set_index", (true, 2))
            .map_err(|help| {
                help.unwrap_or_else(|| super::indexing::missing(ty, super::indexing::WRITE))
            })
    }
}

/// The `[ ]` on `place`'s path nearest `place` itself whose base `indexes`
/// says reads through `index`.
pub(super) fn outer_link<'e>(place: &'e Expr, indexes: &dyn Fn(&Expr) -> bool) -> Option<&'e Expr> {
    match &place.kind {
        ExprKind::Index(base, _, _) if indexes(base) => Some(place),
        ExprKind::Field(base, _) | ExprKind::Index(base, _, _) => outer_link(base, indexes),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const ROWS: &str = "struct P { x: i64 }\nstruct Rows { rows: P[][] }\nimpl Rows {\n\
        fun index(self, i: i64): P[] = self.rows[i];\n\
        fun set_index(mut self, i: i64, v: P[]) { self.rows[i] = v; }\n}\n";

    #[test]
    fn writes_inside_an_element_go_through_set_index() {
        ok(&format!(
            "{ROWS}fun main() {{ let mut r = Rows {{ rows: [[P {{ x: 1 }}]] }};\n\
             r[0][0].x = 2; r[0].push(P {{ x: 3 }}); r[0] += [P {{ x: 4 }}]; }}"
        ));
    }

    #[test]
    fn a_type_without_set_index_is_e0411() {
        let src = "struct R { xs: i64[][] }\nimpl R { fun index(self, i: i64): i64[] = self.xs[i]; }\n\
                   fun main() { let mut r = R { xs: [[1]] }; r[0][0] = 2; r[0].push(3); r[0] += [4]; }";
        assert_eq!(
            errors(src),
            [("E0411", "r"), ("E0411", "r"), ("E0411", "r")]
        );
    }

    #[test]
    fn the_root_must_be_mutable() {
        let src = format!("{ROWS}fun main() {{ let r = Rows {{ rows: [] }}; r[0][0].x = 2; }}");
        assert_eq!(errors(&src), [("E0501", "r")]);
    }

    #[test]
    fn map_entries_are_places() {
        ok(
            "struct P { x: i64 }\nfun main() { let mut m: Map<string, P> = #{};\n\
            let mut n: Map<string, i64[]> = #{}; let mut c: Map<string, i64> = #{};\n\
            m[\"a\"].x = 5; n[\"a\"][0] = 3; n[\"a\"].push(4); c[\"a\"] += 1;\n\
            swap(n[\"a\"], n[\"b\"]); for x of mut n[\"a\"] { x = 2; } }",
        );
        assert_eq!(
            errors("fun main() { let c: Map<string, i64> = #{}; c[\"a\"] += 1; }"),
            [("E0501", "c[\"a\"]")]
        );
    }

    #[test]
    fn swap_and_loops_go_through_a_copy() {
        ok(&format!(
            "{ROWS}fun main() {{ let mut r = Rows {{ rows: [] }}; let mut q: P[] = [];\n\
             swap(r[0], q); swap(r[0], r[1]); for p of mut r[0] {{ p.x = 1; if p.x > 0 {{ break; }} }} }}"
        ));
        // The copy is handed back at the end, so the body can't change `r`.
        let src = format!(
            "{ROWS}fun main() {{ let mut r = Rows {{ rows: [] }};\n\
             for p of mut r[0] {{ r.rows = []; }} }}"
        );
        assert_eq!(errors(&src), [("E0507", "r.rows")]);
    }
}
