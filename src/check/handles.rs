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

#[cfg(test)]
mod extern_struct_tests {
    use crate::check::test_util::{errors, ok};

    #[test]
    fn extern_structs_are_ordinary_structs_that_cross_to_c_v16() {
        ok("extern type W;\n\
            extern struct Color { r: u8, g: u8, b: u8, a: u8 }\n\
            extern struct V2 { x: f32, y: f32 }\n\
            extern struct Cam { at: V2, zoom: f32, ok: bool, c: char, w: W }\n\
            extern fun draw(at: V2, c: Color): Cam;\n\
            extern fun each(f: fun(V2, data): Color, u: data);\n\
            impl Color { fun dim(self): Color = Color { r: self.r / 2, g: self.g / 2, b: self.b / 2, a: self.a }; }\n\
            fun main() {\n\
                let red = Color { r: 230, g: 41, b: 55, a: 255 };\n\
                let cam = draw(V2 { x: 1.0, y: 2.0 }, red.dim());\n\
                println(cam.at.x);\n\
                println(red == red);\n\
                each(fun(v: V2): Color = red);\n\
            }");
    }

    #[test]
    fn fields_c_cant_hold_are_e0437() {
        for (decl, at) in [
            ("extern struct S { s: string }", "string"),
            ("extern struct S { xs: i64[] }", "i64[]"),
            ("struct P { x: i64 }\nextern struct S { p: P }", "P"),
            ("extern struct S { o: Option<i64> }", "Option<i64>"),
            ("extern struct S { f: fun(i64) }", "fun(i64)"),
            ("extern struct S<T> { x: T }", "S"),
            ("extern struct S {}", "S"),
        ] {
            let src = format!("{decl}\nfun main() {{}}");
            let found = errors(&src);
            assert_eq!(found.first(), Some(&("E0437", at)), "{decl}: {found:?}");
        }
    }

    #[test]
    fn option_handle_fields_are_allowed_v17() {
        ok("extern type W;\n\
            extern struct S { n: u64, w: Option<W>, p: Option<CPtr> }\n\
            extern fun f(out s: S);\n\
            fun main() {\n\
                let mut s = S { n: 0, w: None, p: None };\n\
                f(out s);\n\
                if let Some(w) = s.w { println(1); }\n\
                s.w = None;\n\
            }");
    }

    #[test]
    fn unions_hold_plain_data_v17() {
        ok("extern struct P { x: f64, ok: bool }\n\
            extern union In { a: i32, c: char }\n\
            extern union U { a: i64, p: P, i: In }\n\
            fun main() {}");
        for (decl, at) in [
            ("extern type W;\nextern union U { n: u64, w: W }", "W"),
            ("extern union U { n: u64, p: CPtr }", "CPtr"),
            (
                "extern type W;\nextern union U { n: u64, w: Option<W> }",
                "Option<W>",
            ),
            (
                "extern struct H { p: CPtr }\nextern union U { n: u64, h: H }",
                "H",
            ),
            (
                "extern type W;\nextern struct D { w: Option<W> }\nextern struct H { d: D }\nextern union U { n: u64, h: H }",
                "H",
            ),
            (
                "extern union In { p: CPtr }\nextern union U { n: u64, i: In }",
                "In",
            ),
        ] {
            let src = format!("{decl}\nfun main() {{}}");
            let found = errors(&src);
            assert_eq!(found.last(), Some(&("E0437", at)), "{decl}: {found:?}");
            let d = crate::check::test_util::diagnostics(&src);
            assert!(
                d.iter()
                    .any(|d| d.message.contains("union can't hold a pointer")),
                "{d:?}"
            );
        }
    }

    #[test]
    fn a_plain_struct_still_cant_cross_to_c() {
        let src = "struct P { x: i64 }\nextern fun f(p: P);\nfun main() {}";
        assert_eq!(errors(src), [("E0409", "P")]);
        let d = crate::check::test_util::diagnostics(src);
        assert!(
            d[0].help.as_deref().unwrap_or("").contains("extern struct"),
            "{d:?}"
        );
    }
}

#[cfg(test)]
mod module_tests {
    use crate::check::Type;
    use crate::check::modules::test_util::ok;

    #[test]
    fn handles_named_through_their_module_keep_their_type() {
        let lib = (
            "m.la",
            "public extern type H;\npublic extern fun make(): H;\n\
             public fun take(f: fun(H)) {}\npublic struct Holder { h: H }\n",
        );
        let checked = ok(&[
            (
                "main.la",
                "import m;\n\
                 struct Mine { h: m.H }\n\
                 fun keep(h: m.H): m.H = h;\n\
                 fun main() {\n\
                     m.take(fun(h: m.H) {});\n\
                     let mine = Mine { h: keep(m.make()) };\n\
                     let held = m.Holder { h: mine.h };\n\
                 }\n",
            ),
            lib,
        ]);
        let fields = &checked.structs["Mine"];
        assert_eq!(fields[0].1, Type::Handle("m.H".into()));
    }
}
