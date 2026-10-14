//! Bounds on `impl` blocks (v15 draft decision 12, milestone 52):
//! `impl NdArray<T: Num> { … }` gives every method of the block those
//! bounds on top of the type's own, checked at each use like a function's.

use super::Checker;
use super::generics::{Generic, renamed};
use crate::ast::{Ident, TypeParam};

impl Checker {
    /// The generics an `impl` block's methods see: the type's declared
    /// parameters under the block's names, with the block's own bounds
    /// added (E0315 for an unknown one).
    pub(super) fn impl_generics(
        &mut self,
        declared: &[Generic],
        params: &[TypeParam],
    ) -> Vec<Generic> {
        let names: Vec<Ident> = params.iter().map(|p| p.name.clone()).collect();
        let mut outer = renamed(declared, &names);
        if params.iter().all(|p| p.bounds.is_empty()) {
            return outer;
        }
        // A bound may name the block's other parameters (`T: Source<U>`).
        let saved = std::mem::replace(&mut self.type_params, outer.clone());
        for (g, p) in outer.iter_mut().zip(params) {
            self.check_bounds(p);
            let extra = self.bounds_of(p);
            g.bounds.eq |= extra.eq;
            g.bounds.ord |= extra.ord;
            for applied in extra.interfaces {
                if !g.bounds.interfaces.contains(&applied) {
                    g.bounds.interfaces.push(applied);
                }
            }
        }
        self.type_params = saved;
        outer
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const BOX: &str = "struct Box<T> { v: T }\n\
        impl Box<T: Ord> { fun bigger(self, o: T): bool = self.v > o; }\n\
        impl Box<T> { fun get(self): T = self.v; }\n";

    #[test]
    fn an_impl_s_bounds_reach_its_methods() {
        ok(&format!(
            "{BOX}fun main() {{ let b = Box {{ v: 2 }}; let x = b.bigger(1); \
             let s = Box {{ v: [1] }}; let y = s.get(); }}"
        ));
    }

    #[test]
    fn a_bounded_method_needs_its_bound_at_the_use() {
        let src = format!("{BOX}fun main() {{ let s = Box {{ v: [1] }}; let y = s.bigger([2]); }}");
        let found = errors(&src);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0419"), "{found:?}");
        let unknown = errors(
            "struct Box<T> { v: T }\nimpl Box<T: Nope> { fun get(self): T = self.v; }\nfun main() {}",
        );
        assert_eq!(
            unknown.first().map(|(c, _)| *c),
            Some("E0315"),
            "{unknown:?}"
        );
    }
}
