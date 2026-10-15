//! Visibility levels (v8 draft §6, PRP-059): `private` is its own module,
//! no keyword is its own package, `public` is every package. Who may use an
//! item, and the first type a declaration mentions that it may not expose.

use super::{Checker, Type, errors};
use crate::ast::{Visibility, VisibilityKind};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// How far an item, method or field is visible.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum Level {
    /// Its own module only.
    Private,
    /// Every module of its own package (no keyword).
    Package,
    /// Every package (v8).
    Public,
}

/// The level a written visibility gives.
pub(super) fn level(v: Option<Visibility>) -> Level {
    match v.map(|v| v.kind) {
        Some(VisibilityKind::Private) => Level::Private,
        Some(VisibilityKind::Public) => Level::Public,
        None => Level::Package,
    }
}

/// Where an item was declared, and how far it is visible.
#[derive(Debug, Clone)]
pub(super) struct Owner {
    pub module: Vec<String>,
    pub package: Option<String>,
    pub level: Level,
}

impl Checker {
    /// The owner record for an item declared here with `v`; the prelude's
    /// items are visible to every package.
    pub(super) fn owner_here(&self, v: Option<Visibility>) -> Owner {
        let level = if self.in_prelude() {
            Level::Public
        } else {
            level(v)
        };
        Owner {
            module: self.module.clone(),
            package: self.package.clone(),
            level,
        }
    }

    /// E0313 for `name`, declared in `owner`'s module at `level`, if this
    /// module may not use it; `help` replaces the default help.
    pub(super) fn denied(
        &self,
        name: &str,
        owner: &Owner,
        level: Level,
        span: Span,
        help: Option<String>,
    ) -> Option<Diagnostic> {
        match level {
            Level::Public => None,
            Level::Private if owner.module != self.module => {
                Some(errors::private_item(name, &owner.module, span, help))
            }
            Level::Package if owner.package != self.package => Some(errors::package_item(
                name,
                owner.package.as_deref(),
                span,
                help,
            )),
            _ => None,
        }
    }

    /// The first struct, enum or interface in `ty` less visible than `at`:
    /// a declaration visible at `at` can't expose it (E0318). For a `public`
    /// declaration the search reaches into type arguments too.
    pub(super) fn hidden_inside(&self, ty: &Type, at: Level) -> Option<String> {
        let below = |key: &String| {
            self.privacy
                .items
                .get(key)
                .is_some_and(|owner| owner.level < at)
        };
        // v7's rule (shallow, structs and enums) is unchanged for the
        // levels it had; `public` looks deeper, so no v7 program breaks.
        let deep = at == Level::Public;
        match ty {
            Type::Struct(k, args) | Type::Enum(k, args) => {
                if below(k) {
                    return Some(k.clone());
                }
                args.iter()
                    .filter(|_| deep)
                    .find_map(|a| self.hidden_inside(a, at))
            }
            Type::Interface(k, args) if deep => {
                if below(k) {
                    return Some(k.clone());
                }
                args.iter().find_map(|a| self.hidden_inside(a, at))
            }
            Type::Array(inner) | Type::Option(inner) | Type::Ref(inner) => {
                self.hidden_inside(inner, at)
            }
            Type::Fun(params, ret) => params
                .iter()
                .chain([&**ret])
                .find_map(|t| self.hidden_inside(t, at)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::modules::test_util::{errors, ok};

    const MANIFEST: &str = "[packages]\ngeo = \"libs/geo\"\n";

    const GEO: &str = "public struct Circle { r: f64, public tag: i64 }\n\
        impl Circle {\n\
            public fun area(self): f64 = 3.0 * self.r * self.r;\n\
            fun diameter(self): f64 = 2.0 * self.r;\n\
        }\n\
        public fun circle(r: f64): Circle = Circle { r: r, tag: 0 };\n\
        fun helper(): i64 = 1;\n\
        struct Hidden { n: i64 }\n\
        public interface Shape { fun area(self): f64; }\n";

    fn run(body: &str) -> Vec<(&'static str, String)> {
        let main = format!("import geo.shapes;\nfun main() {{ {body} }}\n");
        errors(&[
            ("main.la", main.as_str()),
            ("lugha.toml", MANIFEST),
            ("libs/geo/shapes.la", GEO),
        ])
    }

    #[test]
    fn public_things_cross_packages() {
        let main = "import geo.shapes;\n\
            fun total<T: shapes.Shape>(x: T): f64 = x.area();\n\
            fun main() { let c = shapes.circle(1.0); let a = c.area() + total(c); let t = c.tag; }\n";
        ok(&[
            ("main.la", main),
            ("lugha.toml", MANIFEST),
            ("libs/geo/shapes.la", GEO),
        ]);
    }

    #[test]
    fn package_things_are_e0313_from_another_package() {
        let one = run;
        assert_eq!(
            one("let n = shapes.helper();"),
            [("E0313", "helper".to_string())]
        );
        assert_eq!(
            one("let c = shapes.circle(1.0); let d = c.diameter();"),
            [("E0313", "diameter".to_string())]
        );
        assert_eq!(
            one("let c = shapes.circle(1.0); let r = c.r;"),
            [("E0313", "r".to_string())]
        );
        assert_eq!(
            one("let c = shapes.Circle { r: 1.0, tag: 0 };"),
            [("E0313", "shapes.Circle { r: 1.0, tag: 0 }".to_string())]
        );
        assert_eq!(
            one("let h = shapes.Hidden { n: 1 };"),
            [("E0313", "shapes.Hidden { n: 1 }".to_string())]
        );
    }

    #[test]
    fn a_method_another_package_cant_see_doesnt_satisfy_its_bounds() {
        let main = "import geo.shapes;\n\
            interface Wide { fun diameter(self): f64; }\n\
            fun w<T: Wide>(x: T): f64 = x.diameter();\n\
            fun main() { let d = w(shapes.circle(1.0)); }\n";
        let found = errors(&[
            ("main.la", main),
            ("lugha.toml", MANIFEST),
            ("libs/geo/shapes.la", GEO),
        ]);
        assert_eq!(found.iter().map(|(c, _)| *c).collect::<Vec<_>>(), ["E0422"]);
    }

    #[test]
    fn public_declarations_expose_only_public_types() {
        let src = "struct Inner { n: i64 }\n\
            public struct Box2<T> { v: T }\n\
            interface Quiet { fun q(self); }\n\
            public fun a(): Inner = Inner { n: 1 };\n\
            public fun b(x: Box2<Inner>) {}\n\
            public fun c<T: Quiet>(x: T) {}\n\
            fun fine(): Inner = Inner { n: 1 };\n\
            fun main() {}\n";
        let found = errors(&[("main.la", src)]);
        assert_eq!(
            found,
            [
                ("E0318", "Inner".to_string()),
                ("E0318", "Box2<Inner>".to_string()),
                ("E0318", "Quiet".to_string())
            ]
        );
    }
}
