//! Interface values (v5 draft §4, milestone 20): interfaces as types,
//! E0425, and conversion where an interface is the expected type.

use super::applied::Applied;
use super::expr::Expect;
use super::{Checker, Type, errors, qualify};
use crate::ast::{AppliedType, Expr, Ident};
use crate::span::Span;

impl Checker {
    /// What an interface's name means in a type position: `Interface(key)`,
    /// or `Error` after E0425 when the interface has an associated function.
    /// `None` if `name` isn't an interface, so other types resolve as before.
    pub(super) fn interface_type(
        &mut self,
        module: Option<&Ident>,
        name: &Ident,
        span: Span,
    ) -> Option<Type> {
        let key = match module {
            None => self.key(&name.name),
            Some(m) => qualify(&self.imported(&m.name)?, &name.name),
        };
        let info = self.interfaces.get(&key)?;
        if let Some(item) = info.not_value.clone() {
            let shown = module.map_or_else(
                || name.name.clone(),
                |m| format!("{}.{}", m.name, name.name),
            );
            self.report(errors::interface_not_value(&shown, &item, span));
            return Some(Type::Error);
        }
        self.item_visible(&key, span);
        Some(Type::Interface(key, Vec::new()))
    }

    /// `ty`, an interface, with the type arguments written after it (v6):
    /// E0418 unless their count is the interface's, E0419 for one that
    /// misses its parameter's bound.
    pub(super) fn no_type_args(&mut self, ty: Type, applied: &AppliedType, span: Span) -> Type {
        let Type::Interface(key, _) = &ty else {
            return ty;
        };
        let generics = self.interface_generics(key);
        let name = &applied.name.name;
        if applied.args.len() != generics.len() {
            let d = errors::type_args(name, generics.len(), applied.args.len(), span);
            self.report(d);
            return Type::Error;
        }
        let args: Vec<Type> = applied.args.iter().map(|a| self.resolve(a)).collect();
        if args.contains(&Type::Error) || !self.meets_bounds(&generics, &args, span) {
            return Type::Error;
        }
        Type::Interface(key.clone(), args)
    }

    /// The type `expr` has for its context, given `ty`, the type it was
    /// found to have. Where the expected type is an interface, a value of a
    /// type that satisfies it converts (E0422 otherwise): `Checked.conversions`
    /// records it and the interface type is returned, while `expr` keeps its
    /// own type for codegen. Every other case is `ty` unchanged.
    pub(super) fn convert(&mut self, expr: &Expr, ty: Type, expect: Option<&Expect>) -> Type {
        let Some(Expect { ty: wanted, reason }) = expect else {
            return ty;
        };
        let Some(interface) = Applied::of(wanted) else {
            return ty;
        };
        match &ty {
            // Already reported, never produced, already converted, or E0407 to come.
            Type::Error | Type::Never | Type::Void => ty,
            Type::Interface(..) if ty.fits(wanted) => ty,
            Type::Interface(..) => {
                let d =
                    errors::interface_to_interface(wanted.clone(), ty, expr.span, reason.as_ref());
                self.report(d);
                Type::Error
            }
            _ if self.satisfies(&ty, &interface, expr.span) => {
                self.conversions.insert(expr.id, wanted.clone());
                self.conversion_uses(expr, &ty, &interface);
                wanted.clone()
            }
            _ => Type::Error,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{SHAPES, diagnostics, errors, let_type, ok};

    fn e(body: &str) -> Vec<(&'static str, String)> {
        let src = format!("{SHAPES}{body}\nfun main() {{}}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    #[test]
    fn interfaces_are_types() {
        ok(&format!(
            "{SHAPES}struct Scene {{ main: Shape, all: Shape[] }}\n\
             enum Slot {{ Full(Shape), Empty }}\n\
             fun apply(f: fun(Shape): f64, s: Shape): f64 = f(s);\n\
             fun pick(o: Option<Shape>): f64 = match o {{ Some(s) => s.area(), None => 0.0 }};\n\
             fun main() {{}}"
        ));
        let shape = Type::Interface("Shape".into(), Vec::new());
        let src =
            format!("{SHAPES}fun main() {{ let s: Shape = Circle {{ r: 1.0 }}; let t = s; }}");
        assert_eq!(let_type(&src, "t"), shape);
        assert_eq!(shape.to_string(), "Shape");
        assert_eq!(
            e("fun f(s: Shape<i64>) {}"),
            [("E0418", "Shape<i64>".into())]
        );
    }

    #[test]
    fn another_modules_interface_is_a_type() {
        use crate::check::modules::test_util::{check, loaded};
        let files = [
            (
                "main.la",
                "import geo;\nstruct Dot {}\nimpl Dot { fun size(self): i64 = 1; }\n\
                 fun f(s: geo.Sized): i64 = s.size();\n\
                 fun main() { let n = f(Dot {}); }\n",
            ),
            ("geo.la", "interface Sized { fun size(self): i64; }\n"),
        ];
        check(&loaded(&files)).unwrap_or_else(|e| panic!("{e:?}"));
    }

    #[test]
    fn values_convert_where_an_interface_is_expected() {
        ok(&format!(
            "{SHAPES}fun show(s: Shape): f64 = s.area();\n\
             fun total(xs: Shape[]): f64 {{ let mut t = 0.0; for s of xs {{ t += s.area(); }} t }}\n\
             fun id<T>(x: T): T = x;\n\
             struct Holder {{ s: Shape }}\n\
             impl Holder {{ fun put(mut self, s: Shape) {{ self.s = s; }} }}\n\
             fun make(big: bool): Shape {{\n\
                 if big {{ return Square {{ side: 9.0 }}; }}\n\
                 if big {{ Circle {{ r: 1.0 }} }} else {{ Square {{ side: 1.0 }} }}\n\
             }}\n\
             fun main() {{\n\
                 let c = Circle {{ r: 1.0 }};\n\
                 let mut s: Shape = c;\n\
                 s = Square {{ side: 2.0 }};\n\
                 let a = show(Circle {{ r: 2.0 }});\n\
                 let xs: Shape[] = [c, Square {{ side: 2.0 }}];\n\
                 let mut h = Holder {{ s: c }};\n\
                 h.put(Square {{ side: 3.0 }});\n\
                 let o: Option<Shape> = Some(c);\n\
                 let r: Result<Shape, string> = Ok(c);\n\
                 let g: Shape = id(c);\n\
                 let m: Shape = match 1 {{ 1 => c, _ => Square {{ side: 1.0 }} }};\n\
                 let n = total([c, Square {{ side: 1.0 }}]);\n\
             }}"
        ));
    }

    #[test]
    fn a_conversion_is_recorded_and_the_value_keeps_its_type() {
        let checked = ok(&format!(
            "{SHAPES}fun main() {{ let c = Circle {{ r: 1.0 }}; let s: Shape = c; }}"
        ));
        let converted: Vec<_> = checked.conversions.iter().collect();
        assert_eq!(converted.len(), 1);
        let (id, to) = converted[0];
        assert_eq!(*to, Type::Interface("Shape".into(), Vec::new()));
        assert_eq!(
            checked.types[id.0 as usize],
            Type::Struct("Circle".into(), vec![])
        );
        let plain = ok(&format!(
            "{SHAPES}fun main() {{ let c = Circle {{ r: 1.0 }}; }}"
        ));
        assert!(plain.conversions.is_empty());
    }

    #[test]
    fn nothing_converts_without_an_interface_expected() {
        let src = format!(
            "{SHAPES}fun main() {{ let c = Circle {{ r: 1.0 }}; let s = c; let t: Circle = s; }}"
        );
        ok(&src);
        assert_eq!(
            e("fun f() { let xs = [Circle { r: 1.0 }, Square { side: 1.0 }]; }"),
            [("E0403", "Square { side: 1.0 }".into())]
        );
        assert_eq!(
            e("fun f() { let cs = [Circle { r: 1.0 }]; let xs: Shape[] = cs; }"),
            [("E0403", "cs".into())]
        );
        let named = "interface Named { fun area(self): f64; fun grow(mut self, k: f64); }\n";
        assert_eq!(
            e(&format!("{named}fun f(n: Named) {{ let s: Shape = n; }}")),
            [("E0403", "n".into())]
        );
        let src =
            format!("{SHAPES}{named}fun f(n: Named) {{ let s: Shape = n; }}\nfun main() {{}}");
        let help = diagnostics(&src)[0].help.clone().expect("a help");
        assert!(help.contains("another interface"), "{help}");
    }

    #[test]
    fn a_type_that_doesnt_satisfy_the_interface_is_e0422() {
        assert_eq!(
            e("fun f() { let n = 1; let s: Shape = n; }"),
            [("E0422", "n".into())]
        );
        let dot = "struct Dot {}\nimpl Dot { fun area(self): f64 = 0.0; }\n";
        assert_eq!(
            e(&format!("{dot}fun f() {{ let s: Shape = Dot {{}}; }}")),
            [("E0422", "Dot {}".into())]
        );
        let src = format!("{SHAPES}{dot}fun f() {{ let s: Shape = Dot {{}}; }}\nfun main() {{}}");
        let help = diagnostics(&src)[0].help.clone().expect("a help");
        assert!(help.contains("no method `grow`"), "{help}");
    }

    #[test]
    fn another_modules_private_method_doesnt_satisfy() {
        use crate::check::modules::test_util::errors;
        let found = errors(&[
            (
                "main.la",
                "import geo;\ninterface Sized { fun size(self): i64; }\n\
                 fun main() { let s: Sized = geo.Dot {}; }\n",
            ),
            (
                "geo.la",
                "struct Dot {}\nimpl Dot { private fun size(self): i64 = 1; }\n",
            ),
        ]);
        assert_eq!(found.iter().map(|(c, _)| *c).collect::<Vec<_>>(), ["E0422"]);
    }

    #[test]
    fn literals_take_their_default_type_and_convert() {
        ok("interface Any {}\nfun main() { let x: Any = 1; let xs: Any[] = [1, 4.5, -2]; }");
        assert_eq!(e("fun f() { let s: Shape = 1; }"), [("E0422", "1".into())]);
    }

    #[test]
    fn type_parameters_convert_through_their_bounds() {
        ok(&format!(
            "{SHAPES}fun conv<T: Shape>(x: T): Shape = x;\n\
             fun total<T: Shape>(xs: T[]): f64 {{ let mut t = 0.0; for x of xs {{ t += x.area(); }} t }}\n\
             fun all(xs: Shape[]): f64 = total(xs);\n\
             fun main() {{}}"
        ));
        assert_eq!(e("fun conv<T>(x: T): Shape = x;"), [("E0422", "x".into())]);
    }
}
