//! Interfaces (v2 draft §4, milestone 12): declarations, interface bounds
//! (`T: Shape`, `T: m.Shape + Eq`), E0315 and E0423 on bound lists.
//! Satisfaction and calls through bounds are in `satisfy.rs`.

use super::enums::BUILTIN_NAMES;
use super::env::is_intrinsic;
use super::generics::Bounds;
use super::{Checker, Type, errors, qualify};
use crate::ast::{Bound, Item, Program, TypeParam};
use crate::span::Span;

/// A required method (`receiver` is `Some(mutable)`) or associated function.
#[derive(Debug, Clone)]
pub(super) struct Requirement {
    pub name: String,
    pub receiver: Option<bool>,
    pub params: Vec<Type>,
    pub ret: Type,
    /// The item's name in its interface.
    pub span: Span,
}

impl Requirement {
    /// True if `other` asks for the same thing: name, receiver kind and types.
    pub(super) fn same(&self, other: &Requirement) -> bool {
        (&self.name, self.receiver, &self.params, &self.ret)
            == (&other.name, other.receiver, &other.params, &other.ret)
    }
}

/// A declared interface.
pub(super) struct InterfaceInfo {
    /// Its items in order; empty until pass 2 resolves them.
    pub items: Vec<Requirement>,
    /// The interface's name in its declaration.
    pub span: Span,
    /// Its first associated function, if any: such an interface can't be a
    /// type (E0425, v5).
    pub not_value: Option<String>,
}

impl Checker {
    /// Pass 1, before structs and enums: interface names, in the module's
    /// item namespace (E0302).
    pub(super) fn collect_interfaces(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Interface(i) = item else { continue };
            let (name, span) = (&i.name.name, i.name.span);
            let key = self.key(name);
            if is_intrinsic(name) {
                self.report(errors::reserved(name, span));
            } else if BUILTIN_NAMES.contains(&name.as_str()) {
                self.report(errors::builtin_name(name, span));
            } else if let Some(first) = self.type_span(&key) {
                self.report(errors::duplicate(name, span, first));
            } else {
                self.register_item(&key, i.visibility);
                let not_value = i.items.iter().find(|it| it.receiver.is_none());
                let info = InterfaceInfo {
                    items: Vec::new(),
                    span,
                    not_value: not_value.map(|it| it.name.name.clone()),
                };
                self.interfaces.insert(key, info);
            }
        }
    }

    /// Pass 2: every item's signature, in concrete types only (no type
    /// parameter is in scope), and E0310 for a name used twice.
    pub(super) fn resolve_interfaces(&mut self, program: &Program) {
        self.type_params.clear();
        for item in &program.items {
            let Item::Interface(i) = item else { continue };
            let key = self.key(&i.name.name);
            if self
                .interfaces
                .get(&key)
                .is_none_or(|info| info.span != i.name.span)
            {
                continue; // a duplicate, already reported
            }
            let mut items: Vec<Requirement> = Vec::new();
            for it in &i.items {
                let params = it.params.iter().map(|p| self.resolve(&p.ty)).collect();
                let ret = it.ret.as_ref().map_or(Type::Void, |t| self.resolve(t));
                let name = &it.name.name;
                if let Some(first) = items.iter().find(|r| r.name == *name) {
                    let d = errors::duplicate_method(name, &key, it.name.span, first.span);
                    self.report(d);
                    continue;
                }
                items.push(Requirement {
                    name: name.clone(),
                    receiver: it.receiver.map(|r| r.mutable),
                    params,
                    ret,
                    span: it.name.span,
                });
            }
            let info = self.interfaces.get_mut(&key).expect("collected in pass 1");
            info.items = items;
        }
    }

    /// The bounds of `p` that name `Eq`, `Ord` or a known interface.
    pub(super) fn bounds_of(&self, p: &TypeParam) -> Bounds {
        let mut bounds = Bounds::default();
        for b in &p.bounds {
            match (b.module.is_some(), b.name.name.as_str()) {
                (false, "Eq") => bounds.eq = true,
                (false, "Ord") => bounds.ord = true,
                _ => {
                    if let Some(key) = self.interface_key(b)
                        && !bounds.interfaces.contains(&key)
                    {
                        bounds.interfaces.push(key);
                    }
                }
            }
        }
        bounds
    }

    /// The key of the interface `b` names, here or through an import.
    fn interface_key(&self, b: &Bound) -> Option<String> {
        let key = match &b.module {
            None => self.key(&b.name.name),
            Some(m) => qualify(&self.imported(&m.name)?, &b.name.name),
        };
        self.interfaces.contains_key(&key).then_some(key)
    }

    /// E0315 for a bound that is neither `Eq`, `Ord` nor an interface, E0313
    /// for another module's `private` interface, and E0423 for two
    /// interfaces declaring one name differently.
    pub(super) fn check_bounds(&mut self, p: &TypeParam) {
        for b in &p.bounds {
            if b.module.is_none() && matches!(b.name.name.as_str(), "Eq" | "Ord") {
                continue;
            }
            match self.interface_key(b) {
                Some(key) => self.item_visible(&key, b.span),
                None => {
                    let is_type = b.module.is_none() && self.is_type_name(&b.name.name);
                    self.report(errors::unknown_bound(&b.name.name, b.span, is_type));
                }
            }
        }
        let (Some(first), Some(last)) = (p.bounds.first(), p.bounds.last()) else {
            return;
        };
        let span = Span::new(first.span.start, last.span.end);
        let mut seen: Vec<(Requirement, String)> = Vec::new();
        for key in self.bounds_of(p).interfaces {
            for req in &self.interfaces[&key].items {
                match seen.iter().find(|(r, _)| r.name == req.name) {
                    Some((r, _)) if r.same(req) => {}
                    Some((_, other)) => {
                        let d = errors::clashing_bounds(other, &key, &req.name, span);
                        self.report(d);
                        return;
                    }
                    None => seen.push((req.clone(), key.clone())),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

    const SHAPE: &str = "interface Shape { fun area(self): f64; fun name(self): string; }\n";

    #[test]
    fn the_drafts_interfaces_program_checks() {
        let draft = include_str!("../../docs/specs/Language v2 Draft.md");
        let start = draft
            .find("**Interfaces** (milestone 12). Prints")
            .expect("the draft has the interfaces program");
        let code = draft[start..]
            .split("```\n")
            .nth(1)
            .expect("a fenced block follows");
        ok(code);
    }

    #[test]
    fn declarations_share_the_item_namespace() {
        let main = "fun main() {}";
        assert_eq!(
            errors(&format!("{SHAPE}{SHAPE}{main}")),
            [("E0302", "Shape")]
        );
        assert_eq!(
            errors(&format!("{SHAPE}struct Shape {{}}\n{main}")),
            [("E0302", "Shape")]
        );
        assert_eq!(
            errors(&format!(
                "interface I {{ fun f(self); fun f(self): i64; }}\n{main}"
            )),
            [("E0310", "f")]
        );
        assert_eq!(
            errors(&format!("interface I {{ fun f(self): Nope; }}\n{main}")),
            [("E0305", "Nope")]
        );
    }

    #[test]
    fn bounds_name_interfaces() {
        ok(&format!(
            "{SHAPE}fun f<T: Shape + Eq>(x: T) {{}}\nfun main() {{}}"
        ));
        let src = "struct Circle {}\nfun f<T: Circle>(x: T) {}\nfun main() {}";
        assert_eq!(errors(src), [("E0315", "Circle")]);
        let help = diagnostics(src)[0].help.clone().expect("a help");
        assert!(help.contains("only interfaces"), "{help}");
    }

    #[test]
    fn clashing_bounds_are_e0423() {
        let ab = "interface A { fun size(self): i64; }\ninterface B { fun size(self): f64; }\n\
            interface C { fun size(self): i64; }\n";
        assert_eq!(
            errors(&format!("{ab}fun f<T: A + B>(x: T) {{}}\nfun main() {{}}")),
            [("E0423", "A + B")]
        );
        ok(&format!("{ab}fun f<T: A + C>(x: T) {{}}\nfun main() {{}}"));
    }

    #[test]
    fn an_interface_with_associated_functions_is_not_a_type() {
        ok(&format!("{SHAPE}fun f(s: Shape) {{}}\nfun main() {{}}"));
        let factory = "interface Factory { fun make(): i64; fun size(self): i64; }\n";
        let src = format!("{factory}fun f(x: Factory) {{}}\nfun main() {{}}");
        assert_eq!(errors(&src), [("E0425", "Factory")]);
        let help = diagnostics(&src)[0].help.clone().expect("a help");
        assert!(
            help.contains("`make`") && help.contains("<T: Factory>"),
            "{help}"
        );
        ok(&format!(
            "{factory}fun g<T: Factory>(x: T): i64 = T.make();\nfun main() {{}}"
        ));
    }
}
