//! Indexing your own types: `a[i]` on a struct, enum, interface value or
//! bounded type parameter calls `a.index(i)`, and `a[i] = v` calls
//! `a.set_index(i, v)`. A method qualifies by its shape, as operator
//! methods do: `fun index(self, i: I): T` (a plain `self`, one parameter, a
//! result) and `fun set_index(mut self, i: I, v: T)` (two parameters, no
//! result). Several indexes are one tuple: the parser reads `a[x, y]` as
//! `a[(x, y)]`. The call is checked as a method call whose id is the index
//! expression's (the place's, for a write), so instances, bounds and
//! interface tables work unchanged.

use super::operators::overloaded;
use super::{Checker, Type, errors};
use crate::ast::{AssignOp, Expr, Ident};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// The shape a read or a write needs, for the help of an E0411.
const READ: &str = "fun index(self, i: …): …";
const WRITE: &str = "fun set_index(mut self, i: …, v: …)";

/// True for the types whose `[ ]` calls a method: those whose operators
/// do, except the built-in map, which indexes by itself.
pub(super) fn by_method(ty: &Type) -> bool {
    overloaded(ty) && !matches!(ty, Type::Struct(k, _) if k == super::maps::MAP)
}

impl Checker {
    /// `base[index]`, where `base` has type `ty` (`by_method`): the call
    /// `base.index(index)`, or E0411 when `ty` has no `index` of the shape.
    pub(super) fn index_call(
        &mut self,
        expr: &Expr,
        (base, ty): (&Expr, &Type),
        index: &Expr,
        mark: usize,
    ) -> Type {
        if let Err(help) = self.index_method(ty, "index", (false, 1)) {
            let help = help.unwrap_or_else(|| missing(ty, READ));
            self.report(errors::not_indexable(ty.clone(), base.span).with_help(help));
            return self.unchecked(std::slice::from_ref(index));
        }
        let method = Ident {
            name: "index".to_string(),
            span: index.span,
        };
        self.on_value(
            expr,
            (base, ty),
            &method,
            std::slice::from_ref(index),
            (mark, None),
        )
    }

    /// `base[key] op value;` on a `by_method` type `ty`: the call
    /// `base.set_index(key, value)`. A compound operator is E0404, as for a
    /// map entry; a type without `set_index` of the shape is E0411.
    pub(super) fn index_assign(
        &mut self,
        op: AssignOp,
        (place, base, ty): (&Expr, &Expr, &Type),
        (key, value): (&Expr, &Expr),
        mark: usize,
    ) {
        let args = [key.clone(), value.clone()];
        if op != AssignOp::Assign {
            let span = Span::new(place.span.start, value.span.end);
            let message = format!(
                "`{}` can't change an element of `{ty}` in place",
                op.symbol()
            );
            let d = Diagnostic::error("E0404", message, span)
                .with_help("write `a[i] = a[i] + v`, which calls `index` and then `set_index`");
            self.report(d);
            self.unchecked(&args);
            return;
        }
        if let Err(help) = self.index_method(ty, "set_index", (true, 2)) {
            let help = help.unwrap_or_else(|| missing(ty, WRITE));
            let message = format!("cannot assign by index into a value of type `{ty}`");
            self.report(Diagnostic::error("E0411", message, base.span).with_help(help));
            self.unchecked(&args);
            return;
        }
        let method = Ident {
            name: "set_index".to_string(),
            span: place.span,
        };
        self.on_value(place, (base, ty), &method, &args, (mark, None));
    }

    /// Whether `ty` has method `name` with receiver `mutable` and `arity`
    /// parameters, and a result exactly when it is `index`: `Err` with help
    /// when a method of that name has another shape, `Err(None)` when there
    /// is none.
    fn index_method(
        &self,
        ty: &Type,
        name: &str,
        (mutable, arity): (bool, usize),
    ) -> Result<(), Option<String>> {
        let wants_result = name == "index";
        let fits = |receiver: Option<bool>, params: usize, ret: &Type| {
            receiver == Some(mutable) && params == arity && (*ret != Type::Unit) == wants_result
        };
        let shape = || {
            let want = if wants_result { READ } else { WRITE };
            format!("to be used by `[ ]`, `{name}` must be `{want}`")
        };
        let found = match ty {
            Type::Param(p) => match self.bound_entry(p, name) {
                Some((r, _)) => fits(r.receiver, r.params.len(), &r.ret),
                None => {
                    let help = format!("add a bound whose interface declares `{name}`");
                    return Err(Some(help));
                }
            },
            Type::Interface(..) => {
                let applied = super::applied::Applied::of(ty).expect("an interface type");
                match self
                    .items_for(&applied, ty)
                    .into_iter()
                    .find(|r| r.name == name)
                {
                    Some(r) => fits(r.receiver, r.params.len(), &r.ret),
                    None => return Err(None),
                }
            }
            _ => match self.method(ty, name) {
                // A method with type parameters of its own never matches.
                Some(m) => m.generics.len() == m.outer && fits(m.receiver, m.params.len(), &m.ret),
                None => return Err(None),
            },
        };
        if found { Ok(()) } else { Err(Some(shape())) }
    }
}

/// The help of an E0411 for a type without the method: the shape to
/// write, for the program's own types.
fn missing(ty: &Type, want: &str) -> String {
    match ty {
        Type::Struct(k, _) | Type::Enum(k, _) if k.starts_with("$prelude.") => {
            format!("`{ty}` has no `[ ]`")
        }
        _ => format!("give `{ty}` a method `{want}`"),
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    const GRID: &str = "struct Grid { w: i64, cells: f64[] }\nimpl Grid {\n\
        fun index(self, at: (i64, i64)): f64 { let (r, c) = at; self.cells[r * self.w + c] }\n\
        fun set_index(mut self, at: (i64, i64), v: f64) { let (r, c) = at; self.cells[r * self.w + c] = v; }\n}\n";

    #[test]
    fn index_and_set_index_make_brackets() {
        ok(&format!(
            "{GRID}fun main() {{ let mut g = Grid {{ w: 2, cells: [0.0; 4] }}; \
             g[1, 0] = 2.5; let x: f64 = g[1, 0]; g[(0, 1)] = x; }}"
        ));
    }

    #[test]
    fn a_literal_index_takes_the_parameter_type() {
        ok(
            "struct B { xs: u8[] }\nimpl B { fun index(self, i: u8): u8 = self.xs[i]; }\n\
            fun main() { let b = B { xs: [1, 2] }; let x: u8 = b[1]; }",
        );
    }

    #[test]
    fn a_type_without_index_is_not_indexable() {
        assert_eq!(
            errors("struct P { x: i64 }\nfun main() { let p = P { x: 1 }; let y = p[0]; }"),
            [("E0411", "p")]
        );
    }

    #[test]
    fn other_shapes_do_not_count() {
        // A `mut self` read, a read without a result, and a method with type
        // parameters of its own.
        for method in [
            "fun index(mut self, i: i64): i64 = i;",
            "fun index(self, i: i64) {}",
            "fun index<U>(self, i: i64): i64 = i;",
        ] {
            let src = format!(
                "struct P {{ x: i64 }}\nimpl P {{ {method} }}\n\
                 fun main() {{ let mut p = P {{ x: 1 }}; let y = p[0]; }}"
            );
            assert_eq!(errors(&src), [("E0411", "p")], "{method}");
        }
    }

    #[test]
    fn reading_alone_does_not_allow_writing() {
        assert_eq!(
            errors(
                "struct P { x: i64 }\nimpl P { fun index(self, i: i64): i64 = i; }\n\
                 fun main() { let mut p = P { x: 1 }; p[0] = 2; }"
            ),
            [("E0411", "p")]
        );
    }

    #[test]
    fn writes_need_a_mutable_place_and_no_compound_operator() {
        let main = |body: &str| {
            format!("{GRID}fun main() {{ let g = Grid {{ w: 1, cells: [0.0] }}; {body} }}")
        };
        assert_eq!(errors(&main("g[0, 0] = 1.0;"))[0].0, "E0501");
        let compound = main("let mut h = g; h[0, 0] += 1.0;");
        assert_eq!(errors(&compound), [("E0404", "h[0, 0] += 1.0")]);
    }

    #[test]
    fn writing_inside_an_element_is_not_a_place() {
        let src = "struct Rows { rows: i64[][] }\nimpl Rows {\n\
            fun index(self, i: i64): i64[] = self.rows[i];\n\
            fun set_index(mut self, i: i64, v: i64[]) { self.rows[i] = v; }\n}\n\
            fun main() { let mut r = Rows { rows: [[1]] }; r[0][0] = 2; }";
        assert_eq!(errors(src), [("E0502", "r[0][0]")]);
    }

    #[test]
    fn a_bound_or_interface_declaring_index_allows_it() {
        ok("interface Table { fun index(self, k: string): i64; }\n\
            fun first<T: Table>(t: T): i64 = t[\"a\"];\n\
            fun get(t: Table): i64 = t[\"b\"];\n\
            fun main() {}");
        assert_eq!(
            errors("fun first<T>(t: T): i64 = t[0];\nfun main() {}"),
            [("E0411", "t")]
        );
    }

    #[test]
    fn arrays_strings_and_maps_are_unchanged() {
        ok(
            "fun main() { let xs = [1, 2]; let s = \"ab\"; let m = #{\"a\": 1}; \
            let a = xs[0] + m[\"a\"]; let b = s[1]; }",
        );
        assert_eq!(
            errors("fun main() { let xs = [1, 2]; let a = xs[0, 1]; }"),
            [("E0403", "0, 1")]
        );
    }
}
