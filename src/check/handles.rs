//! Handle types (v16, milestone 88, PRP-118): `extern type Name;` declares
//! an opaque C pointer type. Handles compare by address and do nothing
//! else; only C makes them, as an extern's result or a callback's argument.

use super::Checker;
use super::env::is_intrinsic;
use super::errors;
use crate::ast::{Item, Program};

impl Checker {
    /// Pass 0: every `extern type`, before structs, so fields may hold handles.
    pub(super) fn collect_handles(&mut self, program: &Program) {
        for item in &program.items {
            let Item::ExternType(t) = item else { continue };
            let name = &t.name.name;
            let key = self.key(name);
            if is_intrinsic(name) {
                self.report(errors::reserved(name, t.name.span));
            } else if super::enums::BUILTIN_NAMES.contains(&name.as_str()) {
                self.report(errors::builtin_name(name, t.name.span));
            } else if let Some(first) = self.type_span(&key) {
                self.report(errors::duplicate(name, t.name.span, first));
            } else {
                self.register_item(&key, t.visibility);
                self.hover.define(t.name.span, t.name.span);
                self.handles.insert(key, t.name.span);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const W: &str = "extern type W;\nextern fun make(): W;\nextern fun take(w: W);\n";

    fn with(body: &str) -> String {
        format!("{W}{body}")
    }

    #[test]
    fn handles_go_where_types_go() {
        ok(&with(
            "struct Win { w: W, title: string }\n\
             fun pick(ws: W[], i: i64): W = ws[i];\n\
             fun main() {\n\
                 let w = make();\n\
                 take(w);\n\
                 let s = Win { w: w, title: \"t\" };\n\
                 let ws = [w, s.w];\n\
                 take(pick(ws, 1));\n\
                 let o: Option<W> = Some(w);\n\
                 println(w == s.w);\n\
                 println(w != make());\n\
                 println(w);\n\
                 println(\"{w}\");\n\
             }",
        ));
    }

    #[test]
    fn handles_are_eq_and_work_in_generics() {
        ok(&with(
            "fun same<T: Eq>(a: T, b: T): bool = a == b;\n\
             fun main() { println(same(make(), make())); let ws: W[] = [make()]; println(ws == ws); }",
        ));
    }

    #[test]
    fn handle_types_are_distinct() {
        assert_eq!(
            errors(&with(
                "extern type V;\nextern fun v(): V;\nfun main() { take(v()); }"
            )),
            [("E0403", "v()")]
        );
    }

    #[test]
    fn nothing_but_equality_applies() {
        for (body, code, at) in [
            ("let a = make() + make();", "E0404", "make() + make()"),
            ("let a = make() < make();", "E0404", "make() < make()"),
            ("let a = make() as i64;", "E0408", "make() as i64"),
            ("let a = make().x;", "E0410", "x"),
            ("let a = make().show();", "E0415", "show"),
        ] {
            let src = with(&format!("fun main() {{ {body} }}"));
            let found = errors(&src);
            assert_eq!(
                found.first().map(|(c, _)| *c),
                Some(code),
                "{body}: {found:?}"
            );
            assert!(found[0].1.contains(at), "{body}: {found:?}");
        }
    }

    #[test]
    fn lugha_cant_make_a_handle() {
        let src = with("fun main() { let w = W {}; }");
        let found = errors(&src);
        assert_eq!(found.first().map(|(c, _)| *c), Some("E0305"), "{found:?}");
    }

    #[test]
    fn option_of_a_handle_crosses_the_boundary_and_nothing_else_new_does() {
        ok(&with(
            "extern fun find(name: string): Option<W>;\nextern fun close(w: Option<W>);\n\
             fun main() { close(find(\"x\")); close(None); }",
        ));
        assert_eq!(
            errors(&with("extern fun bad(): Option<i64>;\nfun main() {}")),
            [("E0409", "Option<i64>")]
        );
    }

    #[test]
    fn names_follow_the_rules_of_types() {
        assert_eq!(
            errors("extern type W;\nstruct W { x: i64 }\nfun main() {}"),
            [("E0302", "W")]
        );
        assert_eq!(
            errors("extern type W;\nextern type W;\nfun main() {}"),
            [("E0302", "W")]
        );
        assert_eq!(errors(&with("fun main() { let W = 1; }")), [("E0311", "W")]);
        assert_eq!(
            errors("extern type Option;\nfun main() {}"),
            [("E0302", "Option")]
        );
    }

    #[test]
    fn methods_may_be_added_to_a_handle() {
        ok(&with(
            "impl W { fun show_it(self) { take(self); } }\nfun main() { make().show_it(); }",
        ));
    }
}
