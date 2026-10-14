//! Whole-module re-exports and three-part names (v14 draft §6, milestone
//! 49): `public import a.b [as v];` makes module `a.b` a member `v` of the
//! importing module, so a file that imports that module as `m` writes
//! `m.v.X`. One re-export is followed, never a chain, and only `public`
//! items are reached that way (E0313).

use super::{Checker, errors, qualify};
use crate::ast::{Expr, ExprKind, Ident};

impl Checker {
    /// The module `name` (`m`, or `m.v`) refers to here, and whether a
    /// re-export was followed; `None` if a local shadows `m` or there is no
    /// such module.
    pub(super) fn module_path(&self, name: &str) -> Option<(Vec<String>, bool)> {
        let (first, member) = match name.split_once('.') {
            Some((m, v)) => (m, Some(v)),
            None => (name, None),
        };
        if self.local(first).is_some() {
            return None;
        }
        let path = self.imports.get(first)?;
        match member {
            None => Some((path.clone(), false)),
            Some(v) => self
                .reexports
                .get(&qualify(path, v))
                .map(|p| (p.clone(), true)),
        }
    }

    /// The module name an expression spells, `m` or `m.v`, if it has that
    /// shape; whether it is a module is `module_path`'s answer.
    pub(super) fn module_name(expr: &Expr) -> Option<String> {
        match &expr.kind {
            ExprKind::Name(m) => Some(m.clone()),
            ExprKind::Field(base, v) => match &base.kind {
                ExprKind::Name(m) => Some(format!("{m}.{}", v.name)),
                _ => None,
            },
            _ => None,
        }
    }

    /// The key `name` of module `module` (`m` or `m.v`) refers to, `None`
    /// when `module` isn't one. Through a re-export, an item the ordinary
    /// rules let this module see but that isn't `public` is E0313.
    pub(super) fn member_through(&mut self, module: &str, name: &Ident) -> Option<String> {
        let (path, reexported) = self.module_path(module)?;
        let key = self.member(&path, name);
        if reexported
            && let Some(owner) = self.privacy.items.get(&key).cloned()
            && owner.level != super::levels::Level::Public
            && self
                .denied(&name.name, &owner, owner.level, name.span, None)
                .is_none()
        {
            self.report(errors::not_reexported(&name.name, module, name.span));
        }
        Some(key)
    }
}

#[cfg(test)]
mod tests {
    use crate::check::modules::test_util::{errors, ok};

    const VEC: &str = "public struct Vec2 { public x: i64, public y: i64 }\n\
        impl Vec2 { public fun zero(): Vec2 = Vec2 { x: 0, y: 0 }; }\n\
        public fun add(a: Vec2, b: Vec2): Vec2 = Vec2 { x: a.x + b.x, y: a.y + b.y };\n\
        public enum Dir { Up, Down }\n\
        public interface Sized { fun size(self): i64; }\n\
        fun hidden(): i64 = 1;\n";

    fn files(main: &str) -> Vec<(&str, &str)> {
        vec![
            ("main.la", main),
            (
                "shapes.la",
                "public import shapes.vec;\npublic import shapes.circle as round;\n",
            ),
            ("shapes/vec.la", VEC),
            (
                "shapes/circle.la",
                "public fun area(r: i64): i64 = r * r * 3;\n",
            ),
        ]
    }

    #[test]
    fn re_exported_modules_are_reached_by_three_part_names() {
        ok(&files(
            "import shapes;\n\
             import shapes.circle as c;\n\
             import shapes.vec { Vec2 as V, add };\n\
             struct Box { s: i64 }\n\
             impl Box { fun size(self): i64 = self.s; }\n\
             fun g<T: shapes.vec.Sized>(t: T): i64 = t.size();\n\
             fun main() {\n\
                 let a = shapes.vec.Vec2 { x: 1, y: 2 };\n\
                 let b: shapes.vec.Vec2 = shapes.vec.add(a, V { x: 1, y: 1 });\n\
                 let d = shapes.vec.Dir.Up;\n\
                 let z = shapes.vec.Vec2.zero();\n\
                 let r = c.area(2) + shapes.round.area(1) + g(Box { s: 1 });\n\
                 let f = shapes.vec.add;\n\
                 let s = add(z, b);\n\
             }\n",
        ));
    }

    #[test]
    fn re_exports_offer_only_public_items_and_aliases_replace_names() {
        let codes = |main: &str| errors(&files(main));
        assert_eq!(
            codes("import shapes;\nfun main() { let h = shapes.vec.hidden(); }\n"),
            [("E0313", "hidden".to_string())]
        );
        assert_eq!(
            codes("import shapes;\nfun main() { let h = shapes.vec.nope(); }\n")[0].0,
            "E0301"
        );
        assert_eq!(
            codes(
                "import shapes.vec { Vec2 as V };\nfun main() { let v = Vec2 { x: 1, y: 1 }; }\n"
            )[0]
            .0,
            "E0305"
        );
        assert_eq!(
            codes("import shapes.circle as c;\nfun main() { let a = circle.area(1); }\n")[0].0,
            "E0301"
        );
    }
}
