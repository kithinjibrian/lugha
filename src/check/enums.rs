//! Enums (v1 §4, §6): declarations, their variants (construction is in
//! `construct.rs`) and the names a program can't define.

use super::env::is_intrinsic;
use super::generics::Generic;
use super::{Checker, Type, errors};
use crate::ast::{Item, Program};
use crate::span::Span;

/// Built-in names a program can't define (v1 §2).
pub(super) const BUILTIN_NAMES: [&str; 8] =
    ["Option", "Some", "None", "Eq", "Ord", "Result", "Ok", "Err"];

/// A declared enum.
pub(super) struct EnumInfo {
    /// Variants and their payload types, in declaration order.
    pub variants: Vec<(String, Vec<Type>)>,
    /// The enum's name in its declaration.
    pub span: Span,
    /// Type parameters, `enum Tree<T>` (v2 draft §4).
    pub generics: Vec<Generic>,
}

impl Checker {
    /// Pass 1: enum names, after struct names (v1 §6).
    pub(super) fn collect_enums(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Enum(e) = item else { continue };
            let name = &e.name.name;
            let key = self.key(name);
            if is_intrinsic(name) {
                self.report(errors::reserved(name, e.name.span));
            } else if BUILTIN_NAMES.contains(&name.as_str()) && !self.in_prelude() {
                self.report(errors::builtin_name(name, e.name.span));
            } else if let Some(first) = self.type_span(&key) {
                self.report(errors::duplicate(name, e.name.span, first));
            } else {
                self.register_item(&key, e.visibility);
                self.hover.define(e.name.span, e.name.span);
                let info = EnumInfo {
                    variants: Vec::new(),
                    span: e.name.span,
                    generics: self.generics(&e.type_params),
                };
                self.enums.insert(key, info);
            }
        }
    }

    /// Pass 2: payload types (E0305) and duplicate variants (E0309).
    pub(super) fn resolve_enums(&mut self, program: &Program) {
        for item in &program.items {
            let Item::Enum(e) = item else { continue };
            let key = self.key(&e.name.name);
            if self
                .enums
                .get(&key)
                .is_none_or(|info| info.span != e.name.span)
            {
                continue; // a duplicate, already reported
            }
            // Again now every type is known: bounds' arguments may name them (v6).
            let generics = self.generics(&e.type_params);
            self.enums.get_mut(&key).expect("collected").generics = generics.clone();
            self.type_params = generics;
            self.check_params(&e.type_params, &[]);
            let mut variants: Vec<(String, Vec<Type>)> = Vec::new();
            for (i, v) in e.variants.iter().enumerate() {
                let payloads = v.payloads.iter().map(|t| self.resolve(t)).collect();
                if let Some(first) = e.variants[..i].iter().find(|w| w.name.name == v.name.name) {
                    let d = errors::duplicate_variant(
                        &v.name.name,
                        &e.name.name,
                        v.name.span,
                        first.name.span,
                    );
                    self.report(d);
                    continue;
                }
                self.hover.variant(&key, &v.name);
                variants.push((v.name.name.clone(), payloads));
            }
            self.enums
                .get_mut(&key)
                .expect("collected in pass 1")
                .variants = variants;
        }
        self.type_params.clear();
    }

    /// The variants of an enum or `Option` type.
    pub(super) fn variants_of(&self, ty: &Type) -> Option<Vec<(String, Vec<Type>)>> {
        match ty {
            Type::Enum(name, args) => self.enum_variants(name, args),
            Type::Option(inner) => Some(vec![
                ("None".into(), Vec::new()),
                ("Some".into(), vec![(**inner).clone()]),
            ]),
            // A tuple has one constructor, its elements the payloads (v14).
            Type::Struct(key, args) if key.starts_with(super::tuples::TUPLE) => {
                Some(vec![("()".into(), args.clone())])
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, let_type, ok};

    const SHAPE: &str = "enum Shape { Circle(f64), Rect(f64, f64), Empty }\n";

    #[test]
    fn enums_declare_in_any_order_and_construct() {
        let src =
            format!("fun main() {{ let s = Shape.Circle(2.0); let e = Shape.Empty; }}\n{SHAPE}");
        assert_eq!(let_type(&src, "s"), Type::Enum("Shape".into(), Vec::new()));
        assert_eq!(let_type(&src, "e"), Type::Enum("Shape".into(), Vec::new()));
        ok(&format!(
            "{SHAPE}struct Car {{ s: Shape }}\nfun main() {{ let c = Car {{ s: Shape.Rect(1.0, 2.0) }}; }}"
        ));
    }

    #[test]
    fn declarations_are_checked() {
        assert_eq!(
            errors("enum E { A, B, A }\nfun main() {}"),
            [("E0309", "A")]
        );
        assert_eq!(
            errors("enum P { A }\nstruct P { x: i64 }\nfun main() {}"),
            [("E0302", "P")]
        );
        assert_eq!(
            errors("enum Option { A }\nfun main() {}"),
            [("E0302", "Option")]
        );
        assert_eq!(errors("enum E { A(Q) }\nfun main() {}"), [("E0305", "Q")]);
        let boxed = diagnostics("enum L { N, C(Box<L>) }\nfun main() {}");
        assert_eq!(boxed[0].code, "E0305");
        assert!(boxed[0].help.as_deref().unwrap().contains("need no `Box`"));
    }
}
