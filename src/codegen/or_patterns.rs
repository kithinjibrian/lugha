//! Or-patterns and `mut` bindings in codegen (v9 draft §9, milestone 31):
//! alternatives are tried in turn, each binding into the arm's one local per
//! name, and a `mut` binding is the arm's own deep copy.

use inkwell::basic_block::BasicBlock;

use super::lower::Lowerer;
use super::patterns::Subject;
use crate::ast::{Pattern, PatternKind};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `p | q | …`: each alternative that fails tries the next; the last
    /// one's failure jumps to `next`.
    pub(super) fn test_or(
        &mut self,
        alternatives: &[Pattern],
        subject: Subject<'ctx>,
        ty: &Type,
        next: BasicBlock<'ctx>,
        top: bool,
    ) {
        let matched = self.append("or.ok");
        for (i, alternative) in alternatives.iter().enumerate() {
            let last = i + 1 == alternatives.len();
            let fail = if last { next } else { self.append("or.next") };
            self.test(alternative, subject, ty, fail, top);
            self.branch(matched);
            if !last {
                self.builder.position_at_end(fail);
            }
        }
        self.builder.position_at_end(matched);
    }

    /// `mut name` or `mut name: T`: the inner pattern's test, then its
    /// binding replaced by an owned, deep-copied local.
    pub(super) fn test_mut(
        &mut self,
        inner: &Pattern,
        subject: Subject<'ctx>,
        ty: &Type,
        next: BasicBlock<'ctx>,
        top: bool,
    ) {
        if let PatternKind::Variant(name, None) = &inner.kind {
            let value = self.load_subject(subject, ty);
            let value = if ty.is_deep(&self.structs) {
                self.deep_copy(value, ty)
            } else {
                value
            };
            self.bind_as(name, value, ty, false);
            return;
        }
        // `mut c: Circle`: bind through the type test, then own the copy.
        self.test(inner, subject, ty, next, top);
        if let PatternKind::Type(Some(name), _) = &inner.kind {
            let local = self.scopes.lookup(&name.name);
            let value = self.load(&local, &name.name);
            let value = if local.ty.is_deep(&self.structs) {
                self.deep_copy(value, &local.ty)
            } else {
                value
            };
            self.builder
                .build_store(local.ptr, value)
                .expect(super::lower::POSITIONED);
            let owned = super::scope::Local {
                borrowed: false,
                ..local
            };
            self.scopes.declare(&name.name, owned);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    #[test]
    fn alternatives_share_one_binding_slot_in_the_arm_chain() {
        let ir = ir("enum S { A(i64), B(i64), C }\n\
            fun f(s: S): i64 = match s { A(x) | B(x) => x, C => 0 };\nfun main() {}");
        let start = ir.find("define i64 @lugha_fn_f(").expect("f is defined");
        let f = &ir[start..start + ir[start..].find("\n}").expect("f ends")];
        assert!(!f.contains("switch"), "{f}");
        assert!(f.contains("or.next") && f.contains("or.ok"), "{f}");
        assert_eq!(f.matches("%x = alloca").count(), 1, "one slot for x: {f}");
    }
}
