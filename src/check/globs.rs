//! Glob re-exports (v17, milestone 100, PRP-132): `public import raw { * };`
//! makes every `public` item of `raw` an item of this module, as if each
//! were listed by name.

use super::modules::Unit;
use super::named_imports::Alias;
use super::{Checker, qualify};
use crate::ast::{Item, Visibility, VisibilityKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// A name a glob brought that an earlier import of the module already
/// binds: fine if both reach one item, E0314 otherwise (decided once
/// every alias exists).
#[derive(Debug, Clone)]
pub(super) struct Pending {
    module: Vec<String>,
    name: String,
    first: String,
    second: String,
    at: Span,
}

/// The names of the `public` items of the module at `path`: its own
/// `public` declarations, the names it re-exports by name, and those its
/// own globs bring (draft decision 61). `seen` breaks glob cycles.
fn public_names(units: &[Unit], path: &[String], seen: &mut Vec<Vec<String>>) -> Vec<String> {
    if seen.iter().any(|p| p == path) {
        return Vec::new();
    }
    seen.push(path.to_vec());
    let Some(unit) = units.iter().find(|u| u.path == path) else {
        return Vec::new();
    };
    let public = |v: &Option<Visibility>| v.is_some_and(|v| v.kind == VisibilityKind::Public);
    let mut names: Vec<String> = unit
        .program
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Fun(f) if public(&f.visibility) => Some(&f.name),
            Item::Extern(e) if public(&e.visibility) => Some(&e.name),
            Item::Struct(s) if public(&s.visibility) => Some(&s.name),
            Item::Enum(e) if public(&e.visibility) => Some(&e.name),
            Item::Interface(i) if public(&i.visibility) => Some(&i.name),
            Item::ExternType(t) if public(&t.visibility) => Some(&t.name),
            _ => None,
        })
        .map(|n| n.name.clone())
        .collect();
    for (n, import) in unit.program.imports.iter().enumerate() {
        if import.public.is_none() {
            continue;
        }
        names.extend(import.items.iter().map(|i| i.bound().name.clone()));
        if import.glob.is_some() {
            let written = || import.path.iter().map(|s| s.name.clone()).collect();
            let target: Vec<String> = unit.imports.get(n).cloned().unwrap_or_else(written);
            names.extend(public_names(units, &target, seen));
        }
    }
    names.sort();
    names.dedup();
    names
}

impl Checker {
    /// Registers `unit`'s glob re-exports after its named imports: one
    /// `public` alias per item, E0314 for a name the module declares or
    /// binds to a module, and a [`Pending`] check for one another import
    /// already binds.
    pub(super) fn glob_imports(&mut self, unit: &Unit, units: &[Unit]) {
        for (n, import) in unit.program.imports.iter().enumerate() {
            let Some(star) = import.glob else { continue };
            if import.public.is_none() {
                continue; // E0201 from the parser
            }
            let written = || import.path.iter().map(|s| s.name.clone()).collect();
            let path: Vec<String> = unit.imports.get(n).cloned().unwrap_or_else(written);
            let mut seen = vec![self.module.clone()];
            for name in public_names(units, &path, &mut seen) {
                let key = qualify(&self.module, &name);
                let target = qualify(&path, &name);
                if self.module_declares(unit, &name) {
                    self.report(clash(
                        &name,
                        &format!("this module's own `{name}`"),
                        &target,
                        star,
                    ));
                } else if self.imported(&name).is_some() {
                    self.report(clash(&name, &format!("the module `{name}`"), &target, star));
                } else if let Some(first) = self.aliases.get(&key) {
                    self.glob_clashes.push(Pending {
                        module: self.module.clone(),
                        name,
                        first: first.target.clone(),
                        second: target,
                        at: star,
                    });
                } else {
                    let alias = Alias {
                        target,
                        at: star,
                        public: true,
                    };
                    self.aliases.insert(key.clone(), alias);
                    let visibility = Visibility {
                        kind: VisibilityKind::Public,
                        span: star,
                    };
                    self.register_item(&key, Some(visibility));
                }
            }
        }
    }

    /// True if `unit` declares an item named `name`.
    fn module_declares(&self, unit: &Unit, name: &str) -> bool {
        unit.program
            .items
            .iter()
            .any(|item| super::named_imports::declared(item).is_some_and(|d| d.name == name))
    }

    /// The current module's [`Pending`] names: E0314 unless both reach one item.
    pub(super) fn settle_globs(&mut self) {
        let mine: Vec<Pending> = self
            .glob_clashes
            .iter()
            .filter(|p| p.module == self.module)
            .cloned()
            .collect();
        for p in mine {
            if self.follow(p.first.clone()) != self.follow(p.second.clone()) {
                let first = format!("`{}`", p.first);
                self.report(clash(&p.name, &first, &p.second, p.at));
            }
        }
    }
}

/// E0314 at a glob's `*`: `name` is both `first` and the glob's `second`.
fn clash(name: &str, first: &str, second: &str, at: Span) -> Diagnostic {
    Diagnostic::error(
        "E0314",
        format!("`{name}` is both {first} and `{second}`"),
        at,
    )
    .with_help("list the items you want instead of `*`: `public import m { … };`")
}

#[cfg(test)]
mod tests {
    use crate::check::modules::test_util::{check, errors, loaded, ok};

    const RAW: &str = "public extern type Widget;\n\
        public extern fun make(): Widget;\n\
        public struct Point { public x: i64 }\n\
        public enum Dir { Up, Down }\n\
        public interface Sized { fun size(self): i64; }\n\
        public fun twice(n: i64): i64 = n * 2;\n\
        fun hidden(): i64 = 1;\n\
        private fun secret(): i64 = 2;\n";

    #[test]
    fn every_public_item_is_re_exported_as_the_same_item() {
        ok(&[
            (
                "main.la",
                "import gtk { Widget, make, Point, Dir, Sized, twice };\n\
                 import raw;\n\
                 fun keep(w: Widget): raw.Widget = w;\n\
                 fun main() {\n\
                     let p: raw.Point = Point { x: twice(1) };\n\
                     let d = match Dir.Up { Up => 1, Down => 2 };\n\
                     let q = gtk.Point { x: gtk.twice(p.x) };\n\
                 }",
            ),
            ("gtk.la", "public import raw { * };\n"),
            ("raw.la", RAW),
        ]);
    }

    #[test]
    fn package_level_and_private_items_stay_behind() {
        let found = errors(&[
            (
                "main.la",
                "import gtk;\nfun main() { println(gtk.hidden() + gtk.secret()); }",
            ),
            ("gtk.la", "public import raw { * };\n"),
            ("raw.la", RAW),
        ]);
        let codes: Vec<&str> = found.iter().map(|(c, _)| *c).collect();
        assert!(
            !codes.is_empty() && codes.iter().all(|c| *c == "E0301"),
            "{found:?}"
        );
    }

    #[test]
    fn the_modules_own_re_exports_come_along_and_cycles_end() {
        ok(&[
            (
                "main.la",
                "import top { twice, Point, extra };\n\
                 fun main() { let p = Point { x: twice(extra()) }; }",
            ),
            ("top.la", "public import mid { * };\n"),
            (
                "mid.la",
                "public import raw { * };\npublic import more { extra };\npublic import loop { * };\n",
            ),
            (
                "loop.la",
                "public import mid { * };\npublic fun looped(): i64 = 0;\n",
            ),
            ("more.la", "public fun extra(): i64 = 3;\n"),
            ("raw.la", RAW),
        ]);
    }

    #[test]
    fn whole_module_re_exports_stay_members() {
        let found = errors(&[
            (
                "main.la",
                "import gtk;\nfun main() { println(gtk.extra()); }",
            ),
            ("gtk.la", "public import mid { * };\n"),
            ("mid.la", "public import more;\n"),
            ("more.la", "public fun extra(): i64 = 3;\n"),
        ]);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0301"), "{found:?}");
    }

    /// The E0314 messages for `gtk.la`.
    fn clashes(gtk: &str) -> Vec<String> {
        let files = [
            ("main.la", "import gtk;\nfun main() {}"),
            ("gtk.la", gtk),
            ("raw.la", RAW),
            ("other.la", "public fun twice(n: i64): i64 = n;\n"),
            ("again.la", "public import raw { twice };\n"),
        ];
        match check(&loaded(&files)) {
            Ok(_) => Vec::new(),
            Err(found) => found
                .iter()
                .filter(|d| d.code == "E0314")
                .map(|d| d.message.clone())
                .collect(),
        }
    }

    #[test]
    fn clashes_name_both_sources_unless_the_same_item() {
        let own = clashes("public import raw { * };\npublic fun twice(n: i64): i64 = n;\n");
        assert_eq!(own.len(), 1, "{own:?}");
        assert!(
            own[0].contains("twice") && own[0].contains("raw.twice"),
            "{own:?}"
        );
        let named = clashes("public import raw { * };\npublic import other { twice };\n");
        assert_eq!(named.len(), 1, "{named:?}");
        assert!(
            named[0].contains("raw.twice") && named[0].contains("other.twice"),
            "{named:?}"
        );
        let globs = clashes("public import raw { * };\npublic import other { * };\n");
        assert_eq!(globs.len(), 1, "{globs:?}");
        // Two ways to one item are fine.
        assert_eq!(
            clashes("public import raw { * };\npublic import again { * };\n"),
            Vec::<String>::new()
        );
    }
}
