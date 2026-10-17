//! Named imports and re-exports (v11 draft §6, milestone 38):
//! `import a.b { X, y };` makes `X` and `y` names of the importing file, and
//! `public import a.b { X, y };` also offers them to every module that
//! imports this one. Each is an alias to the item itself, so keys, types
//! and symbols stay the declaring module's.

use std::collections::HashMap;

use super::levels::Level;
use super::{Checker, Unit, errors, qualify};
use crate::ast::{Ident, Item, Visibility, VisibilityKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// How many aliases a name may pass through: more is a cycle, never valid.
const MAX_STEPS: usize = 64;

/// A name a file brings in: the item's key, where it is listed, and
/// whether it is re-exported.
#[derive(Debug, Clone)]
pub(super) struct Alias {
    pub target: String,
    pub at: Span,
    pub public: bool,
}

/// Aliases by the key the name has in its file's module.
pub(super) type Aliases = HashMap<String, Alias>;

/// The name and span of what `item` declares.
pub(super) fn declared(item: &Item) -> Option<&Ident> {
    Some(match item {
        Item::Fun(f) => &f.name,
        Item::Extern(e) => &e.name,
        Item::Struct(s) => &s.name,
        Item::Enum(e) => &e.name,
        Item::Interface(i) => &i.name,
        Item::ExternType(t) => &t.name,
        Item::Impl(_) => return None,
    })
}

impl Checker {
    /// Registers `unit`'s named imports, before every declaration pass:
    /// E0302 for a name the module declares, E0314 for one listed twice or
    /// named like an imported module.
    pub(super) fn named_imports(&mut self, unit: &Unit) {
        let own: Vec<&Ident> = unit.program.items.iter().filter_map(declared).collect();
        let mut listed: Vec<&str> = Vec::new();
        for (n, import) in unit.program.imports.iter().enumerate() {
            let written = || import.path.iter().map(|s| s.name.clone()).collect();
            let path: Vec<String> = unit.imports.get(n).cloned().unwrap_or_else(written);
            // `public import a.b [as v];` (v14): the module is a member of this one.
            if import.public.is_some() && import.items.is_empty() {
                let key = qualify(&self.module, &import.bound().name);
                self.reexports.insert(key, path.clone());
            }
            for item in &import.items {
                let bound = item.bound();
                let name = bound.name.as_str();
                if let Some(first) = own.iter().find(|d| d.name == name) {
                    self.report(errors::duplicate(name, bound.span, first.span));
                    continue;
                }
                if listed.contains(&name) || self.imported(name).is_some() {
                    self.report(clash(name, bound.span));
                    continue;
                }
                listed.push(name);
                let key = qualify(&self.module, name);
                let public = import.public.is_some();
                let alias = Alias {
                    target: qualify(&path, &item.name.name),
                    at: bound.span,
                    public,
                };
                self.aliases.insert(key.clone(), alias);
                // File-only names are `private` to the module; re-exports `public`.
                let kind = if public {
                    VisibilityKind::Public
                } else {
                    VisibilityKind::Private
                };
                let visibility = Visibility {
                    kind,
                    span: bound.span,
                };
                self.register_item(&key, Some(visibility));
            }
        }
    }

    /// `key`, or the item it is an alias of, through re-exports of re-exports.
    pub(super) fn follow(&self, mut key: String) -> String {
        for _ in 0..MAX_STEPS {
            match self.aliases.get(&key) {
                Some(alias) => key = alias.target.clone(),
                None => break,
            }
        }
        key
    }

    /// The key `name` of the module at `path` refers to (`m.X`): E0313 for a
    /// name that module imported for itself only, then the item behind it.
    pub(super) fn member(&mut self, path: &[String], name: &Ident) -> String {
        let key = qualify(path, &name.name);
        if self.aliases.contains_key(&key) {
            self.item_visible(&key, name.span);
        }
        self.follow(key)
    }

    /// After the declaration passes: each listed name must be an item of
    /// its module (E0301) visible here (E0313), and a re-exported one
    /// `public` (E0318).
    pub(super) fn check_named_imports(&mut self, unit: &Unit) {
        let items = unit.program.imports.iter().flat_map(|i| &i.items);
        for item in items {
            let bound = item.bound();
            let key = qualify(&self.module, &bound.name);
            let Some(alias) = self
                .aliases
                .get(&key)
                .filter(|a| a.at == bound.span)
                .cloned()
            else {
                continue; // a clash, reported when registered
            };
            // A name the other module only imports for itself (no `public`) is
            // its own, private to it: E0313, as `m.X` is (session 247).
            if self.aliases.contains_key(&alias.target) {
                let reported = self.diagnostics.len();
                self.item_visible(&alias.target, item.name.span);
                if self.diagnostics.len() > reported {
                    continue;
                }
            }
            let target = self.follow(key);
            let exists = self.functions.contains_key(&target)
                || self.structs.contains_key(&target)
                || self.enums.contains_key(&target)
                || self.interfaces.contains_key(&target)
                // An `extern type` (v16) is an item like a struct.
                || self.handles.contains_key(&target);
            if !exists {
                self.report(errors::undefined(&item.name.name, item.name.span));
                continue;
            }
            self.item_visible(&target, item.name.span);
            // The editor's hover, definition, references and rename reach
            // the name, and its alias, as they reach any use.
            let spans = [Some(item.name.span), item.alias.as_ref().map(|a| a.span)];
            for at in spans.into_iter().flatten() {
                match self.functions.get(&target) {
                    Some(f) => self.hover.function(at, f.span, None),
                    None => {
                        if let Some(decl) = self.type_decl(&target) {
                            self.hover.define(at, decl);
                        }
                    }
                }
            }
            let level = self.privacy.items.get(&target).map(|o| o.level);
            if alias.public && level.is_some_and(|l| l != Level::Public) {
                self.report(not_public(&item.name.name, item.name.span));
            }
        }
    }
}

/// E0318 at `at`: `name` is re-exported, but isn't `public` where declared.
fn not_public(name: &str, at: Span) -> Diagnostic {
    Diagnostic::error(
        "E0318",
        format!("`{name}` is not `public`, so it can't be re-exported"),
        at,
    )
    .with_help("make it `public` where it is declared, or drop `public` from this import")
}

/// E0314 at `at`: `name` is listed twice, or is also an imported module's name.
fn clash(name: &str, at: Span) -> Diagnostic {
    Diagnostic::error(
        "E0314",
        format!("`{name}` is imported twice in this file"),
        at,
    )
    .with_help("each name may come from one import: drop or rename one of them")
}

#[cfg(test)]
mod tests {
    use crate::check::modules::test_util::{errors, ok};

    const VEC: &str = "public struct Vec2 { public x: i64, public y: i64 }\n\
        public fun add(a: Vec2, b: Vec2): Vec2 = Vec2 { x: a.x + b.x, y: a.y + b.y };\n\
        public enum Dir { Up, Down }\n\
        public interface Sized { fun size(self): i64; }\n\
        fun hidden(): i64 = 1;\n\
        private fun secret(): i64 = 2;\n";

    #[test]
    fn named_items_work_unqualified() {
        ok(&[
            (
                "main.la",
                "import vec { Vec2, add, Dir, Sized };\n\
                 struct Box { n: i64 }\nimpl Box { fun size(self): i64 = self.n; }\n\
                 fun big<T: Sized>(t: T): bool = t.size() > 1;\n\
                 fun main() {\n\
                 let v: Vec2 = add(Vec2 { x: 1, y: 2 }, vec.Vec2 { x: 3, y: 4 });\n\
                 let d = match Dir.Up { Up => 1, Down => 2 };\n\
                 let b = big(Box { n: 2 });\n}",
            ),
            ("vec.la", VEC),
        ]);
    }

    #[test]
    fn re_exports_pass_on_the_same_items() {
        ok(&[
            (
                "main.la",
                "import shapes;\nimport vec;\nimport outer;\n\
                 fun main() {\n\
                 let v: vec.Vec2 = shapes.add(shapes.Vec2 { x: 1, y: 2 }, vec.Vec2 { x: 3, y: 4 });\n\
                 let w: shapes.Vec2 = outer.twice(v);\n}",
            ),
            ("shapes.la", "public import vec { Vec2, add };\n"),
            (
                "outer.la",
                "public import shapes { Vec2 };\n\
                 public fun twice(v: Vec2): Vec2 = shapes.add(v, v);\n",
            ),
            ("vec.la", VEC),
        ]);
    }

    fn found(main: &str, shapes: &str) -> Vec<(&'static str, String)> {
        errors(&[("main.la", main), ("shapes.la", shapes), ("vec.la", VEC)])
    }

    #[test]
    fn a_modules_private_imports_stay_its_own() {
        // `shapes` imports `Vec2` for itself (no `public`): naming it through
        // `shapes` is E0313, as `shapes.Vec2` already was.
        let shapes = "import vec { Vec2 };\npublic fun origin(): Vec2 = Vec2 { x: 0, y: 0 };\n";
        let found = errors(&[
            ("main.la", "import shapes { Vec2 };\nfun main() {}"),
            ("shapes.la", shapes),
            ("vec.la", VEC),
        ]);
        assert_eq!(found, [("E0313", "Vec2".into())]);
        // Re-exported with `public`, it is importable.
        ok(&[
            (
                "main.la",
                "import shapes { Vec2 };\nfun main() { let v = Vec2 { x: 1, y: 2 }; }",
            ),
            ("shapes.la", "public import vec { Vec2 };\n"),
            ("vec.la", VEC),
        ]);
    }

    #[test]
    fn misuse_is_reported() {
        let main = "import shapes;\nfun main() {}";
        assert_eq!(
            found(main, "import vec { Nope };\n"),
            [("E0301", "Nope".into())]
        );
        assert_eq!(
            found(main, "import vec { add };\nfun add() {}\n"),
            [("E0302", "add".into())]
        );
        assert_eq!(
            found(main, "import vec { add, add };\n"),
            [("E0314", "add".into())]
        );
        assert_eq!(
            found(main, "import vec { secret };\n"),
            [("E0313", "secret".into())]
        );
        assert_eq!(
            found(main, "public import vec { hidden };\n"),
            [("E0318", "hidden".into())]
        );
        let uses = "import shapes;\nfun main() { let v = shapes.add(shapes.Vec2 { x: 1, y: 1 }, shapes.Vec2 { x: 1, y: 1 }); }";
        let codes: Vec<&str> = found(uses, "import vec { Vec2, add };\n")
            .into_iter()
            .map(|(c, _)| c)
            .collect();
        assert!(
            !codes.is_empty() && codes.iter().all(|c| *c == "E0313"),
            "{codes:?}"
        );
    }
}
