//! Indexing your own types: `a[i]` on a struct, enum, interface value or
//! bounded type parameter calls `a.index(i)`, and `a[i] = v` calls
//! `a.set_index(i, v)`. A method qualifies by its shape, as operator
//! methods do: `fun index(self, i: I): T` (a plain `self`, one parameter, a
//! result) and `fun set_index(mut self, i: I, v: T)` (two parameters, no
//! result). Several indexes are one tuple: the parser reads `a[x, y]` as
//! `a[(x, y)]`. The call is checked as a method call whose id is the index
//! expression's (the place's, for a write), so instances, bounds and
//! interface tables work unchanged.

use super::assign::PlaceUse;
use super::maps::MAP;
use super::operators::overloaded;
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprId, ExprKind, Ident};
use crate::diagnostic::Diagnostic;

/// The shape a read or a write needs, for the help of an E0411.
const READ: &str = "fun index(self, i: …): …";
pub(super) const WRITE: &str = "fun set_index(mut self, i: …, v: …)";

/// True for the types whose `[ ]` calls a method: those whose operators
/// do, except the built-in map, which indexes by itself.
pub(super) fn by_method(ty: &Type) -> bool {
    overloaded(ty) && !matches!(ty, Type::Struct(k, _) if k == MAP)
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

    /// `base[key] = value;` on a `by_method` type `ty`: the call
    /// `base.set_index(key, value)`, or E0411 when `ty` has no `set_index`
    /// of the shape. (A compound operator reads and writes the element, as
    /// any write inside it does.)
    pub(super) fn index_assign(
        &mut self,
        (place, base, ty): (&Expr, &Expr, &Type),
        (key, value): (&Expr, &Expr),
        mark: usize,
    ) {
        let args = [key.clone(), value.clone()];
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
    pub(super) fn index_method(
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

impl Checker {
    /// The element of a place chain, nearest its root, that `[ ]` reads
    /// as a copy through a type's `index` method, with what gave it.
    pub(super) fn read_by_index<'e>(&self, place: &'e Expr) -> Option<(&'e Expr, &'static str)> {
        match &place.kind {
            ExprKind::Field(base, _) => self.read_by_index(base),
            ExprKind::Index(base, _, _) => self.read_by_index(base).or_else(|| {
                let ty = self.types[base.id.0 as usize].as_ref()?;
                by_method(ty).then_some((place, "`index` returned"))
            }),
            _ => None,
        }
    }
}

/// The help of an E0502 for a write inside `element`, which `source` gave:
/// copy it out, change the copy, write it back, in the program's own words
/// (`let mut e = s[0]; e[0] = …; s[0] = e;`).
pub(super) fn copy_help(
    place: &Expr,
    (element, source): (&Expr, &str),
    place_use: PlaceUse,
) -> String {
    // No expression has the id `u32::MAX`, so the element is spelt in full.
    let elem = written(element, ExprId(u32::MAX)).unwrap_or_else(|| "a[i]".to_string());
    let changed = written(place, element.id).unwrap_or_else(|| "e…".to_string());
    let step = match place_use {
        PlaceUse::Assign => format!("{changed} = …;"),
        PlaceUse::Receiver(method) => format!("{changed}.{method}(…);"),
        PlaceUse::Swap => format!("swap({changed}, …);"),
    };
    write_back(&elem, source, &step)
}

/// The help of an E0502 for `for … of mut element`, which `source` gave.
pub(super) fn loop_copy_help((element, source): (&Expr, &str)) -> String {
    // No expression has the id `u32::MAX`, so the element is spelt in full.
    let elem = written(element, ExprId(u32::MAX)).unwrap_or_else(|| "a[i]".to_string());
    write_back(&elem, source, "for x of mut e { … }")
}

fn write_back(elem: &str, source: &str, step: &str) -> String {
    format!(
        "`{elem}` is a copy that {source}: change a copy, then write it back: \
         `let mut e = {elem}; {step} {elem} = e;`"
    )
}

/// How `e` is written, with the element whose id is `copy` written `e`
/// when `e` is inside it; `None` for what a help can't spell simply.
fn written(e: &Expr, copy: ExprId) -> Option<String> {
    if e.id == copy {
        return Some("e".to_string());
    }
    Some(match &e.kind {
        ExprKind::Name(name) => name.clone(),
        ExprKind::Int(n) => n.to_string(),
        ExprKind::Bool(b) => b.to_string(),
        ExprKind::Str(s) if !s.contains(['"', '\\', '{']) => format!("\"{s}\""),
        ExprKind::Field(base, field) => format!("{}.{}", written(base, copy)?, field.name),
        ExprKind::Index(base, _, index) => {
            let index = match &index.kind {
                ExprKind::Tuple(items) if items.len() >= 2 => items
                    .iter()
                    .map(|i| written(i, copy))
                    .collect::<Option<Vec<_>>>()?
                    .join(", "),
                _ => written(index, copy)?,
            };
            format!("{}[{index}]", written(base, copy)?)
        }
        ExprKind::Array(items) => {
            let items = items.iter().map(|i| written(i, copy));
            format!("[{}]", items.collect::<Option<Vec<_>>>()?.join(", "))
        }
        _ => return None,
    })
}

/// The help of an E0411 for a type without the method: the shape to
/// write, for the program's own types.
pub(super) fn missing(ty: &Type, want: &str) -> String {
    match ty {
        Type::Struct(k, _) | Type::Enum(k, _) if k.starts_with("$prelude.") => {
            format!("`{ty}` has no `[ ]`")
        }
        _ => format!("give `{ty}` a method `{want}`"),
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok};

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
    fn writes_need_a_mutable_place() {
        let main = |body: &str| {
            format!("{GRID}fun main() {{ let g = Grid {{ w: 1, cells: [0.0] }}; {body} }}")
        };
        assert_eq!(errors(&main("g[0, 0] = 1.0;"))[0].0, "E0501");
        assert_eq!(errors(&main("g[0, 0] += 1.0;"))[0].0, "E0501");
        ok(&main("let mut h = g; h[0, 0] += 1.0;"));
    }

    #[test]
    fn swapping_an_element_read_through_index_is_e0502() {
        let src = "struct Rows { rows: i64[][] }\nimpl Rows {\n\
            fun index(self, i: i64): i64[] = self.rows[i];\n\
            fun set_index(mut self, i: i64, v: i64[]) { self.rows[i] = v; }\n}\n\
            fun main() { let mut r = Rows { rows: [[1]] }; let mut q = [2]; swap(r[0], q); }";
        assert_eq!(errors(src), [("E0502", "r[0]")]);
    }

    #[test]
    fn the_copy_help_uses_the_program_s_own_place() {
        let help = |body: &str| {
            let src = format!(
                "struct P {{ x: i64 }}\nstruct Ps {{ ps: P[] }}\nimpl Ps {{\n\
                 fun index(self, k: (string, i64)): P = self.ps[k.1];\n}}\n\
                 fun main() {{ let mut s = Ps {{ ps: [P {{ x: 1 }}] }}; let mut q = P {{ x: 2 }}; {body} }}"
            );
            let d = diagnostics(&src);
            assert_eq!(d.len(), 1, "{d:?}");
            d[0].help.clone().unwrap_or_default()
        };
        let want = "`let mut e = s[\"a\", 0]; swap(e, …); s[\"a\", 0] = e;`";
        assert!(help("swap(s[\"a\", 0], q);").ends_with(want));
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
