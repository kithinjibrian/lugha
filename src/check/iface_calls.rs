//! Calls on interface values (v5 draft §4, milestone 20): `s.area()` is
//! checked against the interface's signature and dispatched at run time.

use super::methods::Method;
use super::{Checker, Type, errors};
use crate::ast::{Expr, Ident};

impl Checker {
    /// Method `name` of a value of interface type `key`: the interface's
    /// item, recorded as the callee `<key>.<name>` so codegen dispatches it.
    /// `None` after E0415 when the interface has no such method.
    pub(super) fn through_interface(
        &mut self,
        call: &Expr,
        key: &str,
        name: &Ident,
    ) -> Option<Method> {
        let items = &self.interfaces[key].items;
        let Some(req) = items.iter().find(|r| r.name == name.name).cloned() else {
            let names: Vec<String> = items.iter().map(|r| format!("`{}`", r.name)).collect();
            let help = match names.is_empty() {
                true => format!("`{key}` declares no methods"),
                false => format!("`{key}` declares {}", names.join(", ")),
            };
            let ty = Type::Interface(key.to_string());
            self.report(errors::no_method(&name.name, &ty, name.span, Some(help)));
            return None;
        };
        self.callees.insert(call.id, format!("{key}.{}", name.name));
        Some(Method {
            receiver: req.receiver,
            params: req.params,
            ret: req.ret,
            span: req.span,
            generics: Vec::new(),
            outer: 0,
            key: String::new(),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{SHAPES, diagnostics, errors, ok};
    use crate::check::{Structs, Type};

    fn e(body: &str) -> Vec<(&'static str, String)> {
        let src = format!("{SHAPES}{body}\nfun main() {{}}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    fn help(body: &str) -> String {
        let src = format!("{SHAPES}{body}\nfun main() {{}}");
        diagnostics(&src)[0].help.clone().expect("a help")
    }

    #[test]
    fn methods_are_called_through_the_interface() {
        let checked = ok(&format!(
            "{SHAPES}fun f(s: Shape): f64 = s.area();\n\
             fun g(s: Shape) {{ let mut t = s; t.grow(2.0); }}\n\
             fun main() {{}}"
        ));
        let callees: Vec<&String> = checked.callees.values().collect();
        assert!(callees.contains(&&"Shape.area".to_string()), "{callees:?}");
        assert!(callees.contains(&&"Shape.grow".to_string()), "{callees:?}");
        assert_eq!(
            e("fun f(s: Shape): i64 = s.area();"),
            [("E0403", "s.area()".into())]
        );
        assert_eq!(
            e("fun f(s: Shape) { let n = s.size(); }"),
            [("E0415", "size".into())]
        );
        assert_eq!(
            e("fun f(s: Shape) { let n = s.area(1); }"),
            [("E0405", "s.area(1)".into())]
        );
        assert_eq!(
            e("fun f(s: Shape) { let mut t = s; t.grow(\"x\"); }"),
            [("E0403", "\"x\"".into())]
        );
    }

    #[test]
    fn mut_self_calls_need_a_mutable_place() {
        assert_eq!(e("fun f(s: Shape) { s.grow(1.0); }")[0].0, "E0501");
        assert_eq!(
            e("fun mk(): Shape = Circle { r: 1.0 };\nfun f() { mk().grow(1.0); }"),
            [("E0502", "mk()".into())]
        );
        ok(&format!(
            "{SHAPES}fun main() {{ let mut xs: Shape[] = [Circle {{ r: 1.0 }}]; xs[0].grow(2.0); }}"
        ));
    }

    #[test]
    fn fields_are_out_of_reach() {
        assert_eq!(e("fun f(s: Shape): f64 = s.r;"), [("E0410", "r".into())]);
        assert!(help("fun f(s: Shape): f64 = s.r;").contains("type pattern"));
    }

    #[test]
    fn interface_values_have_no_equality() {
        assert_eq!(
            e("fun f(a: Shape, b: Shape): bool = a == b;"),
            [("E0404", "a == b".into())]
        );
        assert!(help("fun f(a: Shape, b: Shape): bool = a == b;").contains("match"));
        assert_eq!(
            e("fun g<T: Eq>(x: T) {}\nfun f(s: Shape) { g(s); }")[0].0,
            "E0419"
        );
        assert_eq!(
            e("struct H { s: Shape }\nfun f(a: H, b: H): bool = a == b;")[0].0,
            "E0404"
        );
    }

    #[test]
    fn interface_values_dont_print() {
        assert_eq!(e("fun f(s: Shape) { println(s); }")[0].0, "E0403");
        assert_eq!(e("fun f(s: Shape) { let t = \"{s}\"; }")[0].0, "E0403");
    }

    #[test]
    fn interface_values_dont_cross_the_c_boundary() {
        assert_eq!(
            e("extern fun c_area(s: Shape): f64;"),
            [("E0409", "Shape".into())]
        );
    }

    #[test]
    fn interface_values_are_deep() {
        let shape = Type::Interface("Shape".into());
        let mut structs = Structs::new();
        assert!(shape.is_deep(&structs));
        structs.insert("H".into(), vec![("s".into(), shape)]);
        assert!(Type::Struct("H".into(), vec![]).is_deep(&structs));
        assert!(!Type::F64.is_deep(&structs));
    }
}
