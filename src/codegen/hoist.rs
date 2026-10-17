//! Owning arrays once per loop (v15, milestone 56).
//!
//! Copy-on-write (`cow.rs`) tests the shared flag of every array on a
//! write's path. Before a loop, `hoist_loop` makes unique the arrays the
//! loop writes through, when nothing in the loop can share them again
//! (draft decisions 25–27); writes inside the loop then skip the test
//! (`places.rs::own_place`).
//!
//! "Can't share" is syntactic and per root variable. The root may appear
//! only as the root of a written place whose new value holds no array, or
//! in a read whose value holds no array. Every array under such a root is
//! then marked by nothing in the loop and cloned by nothing (a clone needs
//! a shared array), so once unique before the loop, it stays unique.

use inkwell::IntPredicate;
use inkwell::values::PointerValue;

use super::CodegenError;
use super::heap::element_of;
use super::hoist_scan::{Scan, chain_root};
use super::lower::{Lowerer, POSITIONED};
use super::value::llvm_type;
use crate::ast::{Block, Expr, ExprKind, Pattern};

/// What a loop does on every iteration besides running its body.
pub(super) enum Head<'a> {
    /// Evaluates an expression: `while`'s condition, `while let`'s scrutinee.
    Read(&'a Expr),
    /// Binds a name: the loop variable.
    Binds(&'a str),
    /// Binds a pattern's names (`while let`).
    Pattern(&'a Pattern),
    /// `for var of mut place` over an array: `var` names its elements.
    Alias(&'a str, &'a Expr),
    /// `for var of mut place` over an iterator: `place.next()`.
    Steps(&'a Expr),
}

/// Whether two place chains name the same place, given that the names in
/// them mean the same bindings: same fields, and literal or named indexes.
fn same_shape(a: &Expr, b: &Expr) -> bool {
    match (&a.kind, &b.kind) {
        (ExprKind::Name(x), ExprKind::Name(y)) => x == y,
        (ExprKind::Field(x, f), ExprKind::Field(y, g)) => f.name == g.name && same_shape(x, y),
        (ExprKind::Index(x, _, i), ExprKind::Index(y, _, j)) => {
            let index = match (&i.kind, &j.kind) {
                (ExprKind::Int(m), ExprKind::Int(n)) => m == n,
                (ExprKind::Name(m), ExprKind::Name(n)) => m == n,
                _ => false,
            };
            index && same_shape(x, y)
        }
        _ => false,
    }
}

/// How many indexes a path has: shorter paths are hoisted first.
fn depth(path: &Expr) -> usize {
    match &path.kind {
        ExprKind::Index(base, _, _) => 1 + depth(base),
        ExprKind::Field(base, _) => depth(base),
        _ => 0,
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// Makes unique, just before a loop, each array the loop writes through
    /// and can't share again; returns how many paths were hoisted, for
    /// `end_hoists` once the loop is lowered.
    pub(super) fn hoist_loop(
        &mut self,
        heads: &[Head<'_>],
        body: &Block,
    ) -> Result<usize, CodegenError> {
        let mut scan = self.scan_loop(heads, body);
        let mut paths: Vec<Expr> = Vec::new();
        for path in std::mem::take(&mut scan.paths) {
            if self.fixed(&scan, &path) && !paths.iter().any(|p| same_shape(p, &path)) {
                paths.push(path);
            }
        }
        paths.sort_by_key(depth);
        for path in &paths {
            self.hoist_path(path)?;
        }
        Ok(paths.len())
    }

    /// Ends the hoists of the loop just lowered.
    pub(super) fn end_hoists(&mut self, count: usize) {
        let kept = self.owned.len() - count;
        self.owned.truncate(kept);
    }

    /// Whether the array at `path` was made unique before an enclosing loop.
    pub(super) fn hoisted(&self, path: &Expr) -> bool {
        if self.owned.is_empty() {
            return false;
        }
        let Some(local) = chain_root(path).and_then(|root| self.scopes.find(root)) else {
            return false;
        };
        self.owned
            .iter()
            .any(|(slot, owned)| *slot == local.ptr && same_shape(owned, path))
    }

    /// Whether `path` can be hoisted: its root is a variable from outside
    /// the loop that the loop can't share, and each index is a literal or
    /// such a variable that the loop never changes.
    fn fixed(&self, scan: &Scan, path: &Expr) -> bool {
        let Some(root) = chain_root(path) else {
            return false;
        };
        if scan.bound.contains(root)
            || self.scopes.find(root).is_none()
            || scan.sharing.contains(&self.root_of(scan, root))
        {
            return false;
        }
        let mut at = path;
        loop {
            match &at.kind {
                ExprKind::Index(base, _, index) => {
                    let steady = match &index.kind {
                        ExprKind::Int(_) => true,
                        // An outer `for … of mut` variable changes when its
                        // array does, so it is never fixed.
                        ExprKind::Name(n) => {
                            !scan.changed.contains(n)
                                && self
                                    .scopes
                                    .find(n)
                                    .is_some_and(|l| !self.aliases.contains_key(&l.ptr))
                        }
                        _ => false,
                    };
                    if !steady {
                        return false;
                    }
                    at = base;
                }
                ExprKind::Field(base, _) => at = base,
                _ => return true,
            }
        }
    }

    /// Makes the array at `path` unique and records it as hoisted. Each
    /// index is tested first: out of bounds, the hoist is skipped (the
    /// write's own bounds check reports it, if the loop gets there).
    fn hoist_path(&mut self, path: &Expr) -> Result<(), CodegenError> {
        let skip = self.append("hoist.skip");
        let slot = self.guarded_slot(path, skip)?;
        self.own_labelled(slot, &self.ty(path), "hoist");
        self.branch(skip);
        self.builder.position_at_end(skip);
        let root = chain_root(path).expect("checked: a hoisted path is a place");
        let local = self.scopes.lookup(root);
        self.owned.push((local.ptr, path.clone()));
        Ok(())
    }

    /// The address of `path`, owning the arrays on the way, or a jump to
    /// `skip` when an index is out of bounds.
    fn guarded_slot(
        &mut self,
        path: &Expr,
        skip: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<PointerValue<'ctx>, CodegenError> {
        match &path.kind {
            ExprKind::Name(name) => {
                let local = self.scopes.lookup(name);
                Ok(self.local_address(local.ptr, true))
            }
            ExprKind::Field(base, field) => {
                let address = self.guarded_slot(base, skip)?;
                let base_ty = self.ty(base);
                if self.is_union(&base_ty) {
                    return Ok(address);
                }
                let (index, _) = self.field_index(&base_ty, &field.name);
                let llvm = llvm_type(self.context, &base_ty).into_struct_type();
                Ok(self
                    .builder
                    .build_struct_gep(llvm, address, index, &field.name)
                    .expect("checked: the field exists"))
            }
            ExprKind::Index(base, _, index) => {
                let slot = self.guarded_slot(base, skip)?;
                let base_ty = self.ty(base);
                let array = self.own_place(slot, base, &base_ty);
                let index_ty = self.ty(index);
                let i = self.get(index, &index_ty)?.into_int_value();
                let i = self.widen_index(i, &index_ty);
                let len = self.length(array);
                // Unsigned, so a negative index is out of bounds too.
                let inside = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, i, len, "hoist.in")
                    .expect(POSITIONED);
                let next = self.append("hoist.index");
                self.builder
                    .build_conditional_branch(inside, next, skip)
                    .expect(POSITIONED);
                self.builder.position_at_end(next);
                let element = element_of(&base_ty);
                Ok(self.element_address(array, i, self.element_size(&element)))
            }
            _ => unreachable!("checked: a hoisted path is a place"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    /// `main`'s body.
    fn main(src: &str) -> String {
        let ir = ir(src);
        let main = &ir[ir.find("define void @lugha_fn_main").expect("main")..];
        main[..main.find("\n}").expect("main ends")].to_owned()
    }

    /// Hoisted: owned before the loop, no test inside it.
    fn hoists(body: &str) {
        let main = main(body);
        assert!(
            main.contains("hoist.clone"),
            "owned before the loop: {main}"
        );
        assert!(!main.contains("cow.clone"), "no test in the loop: {main}");
    }

    /// Not hoisted: every write tests the flag, as in milestone 55.
    fn keeps(body: &str) {
        let main = main(body);
        assert!(main.contains("cow.clone"), "the test stays: {main}");
    }

    #[test]
    fn a_counting_loop_owns_its_array_once() {
        hoists(
            "fun main() { let mut counts = [0; 4]; let keep = counts; let t = [1, 2, 1];
             for i in 0..t.len { counts[t[i]] += 1; } println(counts[1] + keep[1]); }",
        );
    }

    #[test]
    fn nested_loops_own_each_fixed_path() {
        let src = "fun main() { let n = 2; let mut out = [[0; 2]; 2]; let keep = out;
             for i in 0..n { for j in 0..n { out[i][j] += i * j; } } println(out[1][1] + keep[0][0]); }";
        hoists(src);
        // The `i` loop owns `out`; the `j` loop owns `out` and `out[i]`.
        let labels = main(src);
        let hoisted = labels
            .lines()
            .filter(|l| l.starts_with("hoist.clone"))
            .count();
        assert_eq!(hoisted, 3, "{labels}");
    }

    #[test]
    fn every_kind_of_loop_hoists() {
        hoists(
            "fun main() { let mut xs = [0; 3]; let mut k = 0; while k < 3 { xs[0] += k; k += 1; } }",
        );
        hoists("fun main() { let mut xs = [0; 3]; for x of [1, 2] { xs[1] = x; } }");
        hoists("fun main() { let mut xs = [0; 3]; for x of mut xs { x += 1; } println(xs[0]); }");
        hoists(
            "fun main() { let mut xs = [0; 3]; let mut o = Some(1);
             while let Some(n) = o { xs[2] = n; o = None; } }",
        );
    }

    #[test]
    fn reads_without_arrays_dont_block() {
        hoists(
            "struct P { x: i64 }
             fun main() { let mut xs = [3, 1, 2]; let p = P { x: 1 }; let mut m = 0;
             for i in 0..xs.len { if xs[i] > m { m = xs[i]; } xs[i] = m + p.x + xs.len; } }",
        );
    }

    #[test]
    fn copies_and_calls_keep_the_test() {
        let pre = "struct S { v: i64[] }
             impl S { fun first(self): i64 = self.v[0]; }
             fun id(xs: i64[]): i64[] = xs;
             fun main() { let mut xs = [1, 2]; let mut ys = [3, 4]; let mut g = [[1], [2]];
             let mut s = S { v: [1] }; let mut t = 0;";
        for body in [
            "let c = xs; xs[0] = 1;",
            "let c = id(xs); xs[0] = 1;",
            "t += s.first(); s.v[0] = 1;",
            "let f = fun(): i64 = xs[0]; xs[0] = f();",
            "swap(xs, ys); xs[0] = 1;",
            "xs = ys; xs[0] = 1;",
            "g[0] = xs; g[0][0] = 1;",
            "let row = g[1]; g[0][0] = row[0];",
            "for y of xs { t += y; } xs[0] = 1;",
            "for r of mut g { let c = r; r[0] = 1; }",
        ] {
            keeps(&format!("{pre} for i in 0..2 {{ {body} }} }}"));
        }
    }

    #[test]
    fn changing_indexes_are_not_fixed() {
        let pre = "fun two(): i64 = 1;
             fun main() { let mut g = [[1, 2], [3, 4]]; let mut k = 0;";
        for body in [
            "g[k][0] = 1; k = 1;",
            "let k = i; g[k][0] = 1;",
            "g[two()][0] = 1;",
        ] {
            // `g` itself is hoisted; `g[k]` keeps its test.
            let main = main(&format!("{pre} for i in 0..2 {{ {body} }} }}"));
            assert!(
                main.contains("hoist.clone") && main.contains("cow.clone"),
                "{main}"
            );
        }
    }
}
