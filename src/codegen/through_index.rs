//! Writing through an element a type's `index` gives (`g[i].x = v`,
//! `g[i] += v`, `g[i].push(x)`): the place's indexes are evaluated once,
//! into hidden locals; the element is read with `index` into another; the
//! write happens on that copy; then `set_index` (the call the checker
//! recorded at the `[ ]`'s `write_id`) takes the copy back. A `[ ]` further
//! in is handled the same way, by `set_index`'s own `mut self` receiver.

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::scope::Local;
use super::value::Value;
use crate::ast::{Block, Expr, ExprKind, ForIter, Stmt, StmtKind};

impl<'ctx> Lowerer<'ctx> {
    /// The `[ ]` nearest `place` itself through which the checker said
    /// `place` is written, if any: the write goes through that element.
    pub(super) fn write_link(&self, place: &Expr) -> Option<Expr> {
        if !self.through_index.contains(&place.id) {
            return None;
        }
        let mut at = place;
        loop {
            match &at.kind {
                ExprKind::Index(base, _, _) if self.indexes_by_method(base) => {
                    return Some(at.clone());
                }
                ExprKind::Field(base, _) | ExprKind::Index(base, _, _) => at = base,
                _ => return None,
            }
        }
    }

    /// The expression whose id keys a compound assignment's operator method
    /// on `place`: the place, or for a `[ ]` that is a call already (a map's
    /// `$entry`, a type's `index`) its `update_id`, as the checker keyed it.
    pub(super) fn update_key(&self, place: &Expr) -> Expr {
        match &place.kind {
            ExprKind::Index(base, _, _) if self.is_map(base) || self.indexes_by_method(base) => {
                place.with_id(place.update_id())
            }
            _ => place.clone(),
        }
    }

    /// Runs `write` on `place` with its outermost `[ ]` through `index`
    /// replaced by a copy of the element, then hands the copy to
    /// `set_index`; gives what `write` gave.
    pub(super) fn through_index<T>(
        &mut self,
        place: &Expr,
        write: impl FnOnce(&mut Self, &Expr) -> Result<T, CodegenError>,
    ) -> Result<T, CodegenError> {
        let (inner, hand_back) = self.copy_out(place)?;
        let out = write(self, &inner)?;
        self.expr(&hand_back)?;
        Ok(out)
    }

    /// `for var of mut place` where `place` goes through `index`: the loop
    /// runs over a copy of the element, and handing it back is deferred to
    /// a block around the loop, so every way out of the loop does it.
    pub(super) fn for_through_index(
        &mut self,
        (label, var): (&Option<crate::ast::Ident>, &crate::ast::Ident),
        place: &Expr,
        body: &Block,
    ) -> Result<(), CodegenError> {
        let (inner, hand_back) = self.copy_out(place)?;
        let stmt = |kind| Stmt {
            kind,
            span: body.span,
        };
        let deferred = stmt(StmtKind::Expr {
            expr: hand_back,
            semicolon: true,
        });
        let around = Block {
            stmts: vec![
                stmt(StmtKind::Defer(Box::new(deferred))),
                stmt(StmtKind::For {
                    label: label.clone(),
                    var: var.clone(),
                    iter: ForIter::MutOf(inner),
                    body: body.clone(),
                }),
            ],
            tail: None,
            span: body.span,
        };
        self.block(&around)?;
        Ok(())
    }

    /// Reads the element at `place`'s outermost `[ ]` through `index` into
    /// a hidden local: `place` with that `[ ]` replaced by the copy, and the
    /// `set_index` call (keyed at the `[ ]`'s `write_id`) that hands it back.
    fn copy_out(&mut self, place: &Expr) -> Result<(Expr, Expr), CodegenError> {
        let link = self
            .write_link(place)
            .expect("checked: a write through `index`");
        let ExprKind::Index(base, open, index) = &link.kind else {
            unreachable!("a link is a `[ ]`")
        };
        // Both calls use the base and the index: evaluate them once.
        let base = self.settled(base)?;
        let index = self.hidden(index)?;
        let read = Expr {
            id: link.id,
            span: link.span,
            kind: ExprKind::Index(Box::new(base.clone()), *open, Box::new(index.clone())),
        };
        let element = match self.map_method(&read, &base, "index", std::slice::from_ref(&index))? {
            Some(Value::Val(v)) => v,
            _ => unreachable!("checked: `index` returns a value"),
        };
        let copy = self.held(&link, element);
        let handed = Expr {
            id: link.element_id(),
            ..copy.clone()
        };
        let method = crate::ast::Ident {
            name: "set_index".to_string(),
            span: link.span,
        };
        let callee = Expr {
            id: link.write_id(),
            span: link.span,
            kind: ExprKind::Field(Box::new(base), method),
        };
        let hand_back = Expr {
            id: link.write_id(),
            span: link.span,
            kind: ExprKind::Call(Box::new(callee), vec![index, handed]),
        };
        Ok((replaced(place, link.id, &copy), hand_back))
    }

    /// `place` with each of its indexes evaluated now into a hidden local,
    /// so using it twice evaluates nothing twice.
    fn settled(&mut self, place: &Expr) -> Result<Expr, CodegenError> {
        let kind = match &place.kind {
            ExprKind::Field(base, field) => {
                ExprKind::Field(Box::new(self.settled(base)?), field.clone())
            }
            ExprKind::Index(base, open, index) => {
                let base = self.settled(base)?;
                ExprKind::Index(Box::new(base), *open, Box::new(self.hidden(index)?))
            }
            kind => kind.clone(),
        };
        Ok(Expr {
            kind,
            ..place.clone()
        })
    }

    /// `e`'s value, evaluated now, as a hidden local.
    fn hidden(&mut self, e: &Expr) -> Result<Expr, CodegenError> {
        let ty = self.ty(e);
        let value = self.get(e, &ty)?;
        Ok(self.held(e, value))
    }

    /// A hidden local holding `value`, of `e`'s type, as a name with an id
    /// of its own: `e`'s id may key a call or a function, which a plain
    /// name must not pick up.
    fn held(&mut self, e: &Expr, value: inkwell::values::BasicValueEnum<'ctx>) -> Expr {
        let ty = self.ty(e);
        let id = crate::ast::ExprId(u32::try_from(self.types.len()).expect("ids fit in u32"));
        self.types.push(ty.clone());
        // `$` can't start a name a program writes.
        let name = format!("$held{}", id.0);
        let ptr = self.entry_alloca(&ty, &name);
        self.builder.build_store(ptr, value).expect(POSITIONED);
        let local = Local {
            ptr,
            ty,
            borrowed: false,
        };
        self.scopes.declare(&name, local);
        Expr {
            id,
            span: e.span,
            kind: ExprKind::Name(name),
        }
    }
}

/// `place` with the expression whose id is `id` replaced by `by`.
fn replaced(place: &Expr, id: crate::ast::ExprId, by: &Expr) -> Expr {
    if place.id == id {
        return by.clone();
    }
    let kind = match &place.kind {
        ExprKind::Field(base, field) => {
            ExprKind::Field(Box::new(replaced(base, id, by)), field.clone())
        }
        ExprKind::Index(base, open, index) => {
            ExprKind::Index(Box::new(replaced(base, id, by)), *open, index.clone())
        }
        kind => kind.clone(),
    };
    Expr {
        kind,
        ..place.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn a_write_inside_an_element_reads_it_then_hands_it_back() {
        let ir = ir("struct B { xs: i64[][] }\nimpl B {\n\
            fun index(self, i: i64): i64[] = self.xs[i];\n\
            fun set_index(mut self, i: i64, v: i64[]) { self.xs[i] = v; }\n}\n\
            fun main() { let mut b = B { xs: [[1]] }; b[0][0] += 2; b[0].push(3); }");
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        let calls = |name: &str| {
            main.lines()
                .filter(|l| l.contains(" call ") && l.contains(name))
                .count()
        };
        assert_eq!(calls("B.index"), 2, "{ir}");
        assert_eq!(calls("B.set_index"), 2, "{ir}");
    }

    #[test]
    fn a_write_inside_a_map_entry_finds_its_slot() {
        let ir =
            ir("fun main() { let mut m = #{\"a\": [1]}; m[\"a\"].push(2); m[\"a\"][0] += 1; }");
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        let main = &main[..main.find("\n}\n").unwrap()];
        let calls = |name: &str| {
            main.lines()
                .filter(|l| l.contains(" call ") && l.contains(name))
                .count()
        };
        assert_eq!(calls("$slot"), 2, "{main}");
        assert_eq!(calls("insert"), 0, "{main}");
    }
}
