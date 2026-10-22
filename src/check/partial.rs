//! What the checker found, even in a program with errors (v16 draft
//! decisions 20 and 26): the language server's hover and go-to-definition
//! read it. `check_modules` and
//! everything codegen sees are unchanged.

use std::collections::HashMap;

use super::{Type, Unit, modules};
use crate::ast::{Expr, ExprId, ExprKind, Ident};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// A name the checker resolved, by the span where it is written.
#[derive(Debug, Clone, PartialEq)]
pub enum Name {
    /// A `let`, parameter, `for` or `match` binding.
    Local {
        name: String,
        ty: Type,
        mutable: bool,
    },
    /// A function or method, at its declaration or a use: `decl` is the
    /// declaration's name, `call` the call it names, if any (for its
    /// generic instance).
    Function { decl: Span, call: Option<ExprId> },
    /// A struct field at its declaration.
    Field { name: String, ty: Type },
    /// A constant at its declaration or a use (v17, milestone 109).
    Const { name: String, ty: Type, decl: Span },
    /// The path of an `include_bytes`/`include_str` call: the file and its
    /// size in bytes (v17, milestone 128); `shown` is its path in its package.
    Included {
        file: std::path::PathBuf,
        shown: String,
        size: usize,
    },
}

/// Where a written name is defined (v16 draft decision 23).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Target {
    /// A declaration's name, in the program's shared offset space.
    Decl(Span),
    /// The module with this path, as in `import geo;` or `geo.f`.
    Module(Vec<String>),
    /// A file an `include_bytes`/`include_str` path names (v17, milestone 128).
    File(std::path::PathBuf),
}

/// The checker's findings for a whole program, errors or not.
#[derive(Debug, Clone)]
pub struct Partial {
    /// Each expression's type, by `ExprId`; `None` where the checker never got.
    pub types: Vec<Option<Type>>,
    /// Each typed expression's span, by `ExprId`.
    pub spans: Vec<Option<Span>>,
    /// See `Checked::annotations`.
    pub annotations: HashMap<Span, Type>,
    /// See `Checked::instances`.
    pub instances: HashMap<ExprId, Vec<Type>>,
    /// See `Checked::conversions`.
    pub conversions: HashMap<ExprId, Type>,
    /// See `Checked::callees`.
    pub callees: HashMap<ExprId, String>,
    /// Resolved names by span.
    pub names: HashMap<Span, Name>,
    /// Where each written use (and declaration) is defined, by its span.
    pub definitions: HashMap<Span, Target>,
    /// Each struct's, enum's and interface's members, by key, for completion.
    pub members: HashMap<String, super::TypeMembers>,
    /// Every prelude function's declaration name, by its name.
    pub prelude: HashMap<String, Span>,
    /// Each function's and method's signature, by its declaration's name.
    pub signatures: HashMap<Span, super::Sig>,
    /// Every missing pattern of each non-exhaustive `match`, in full
    /// (`Rect(_, _)`), by its E0417 span; empty when only `_` applies.
    pub missing_arms: HashMap<Span, Vec<String>>,
    /// The variant names of the enum each unused `match` binding (W0103)
    /// matches, by the binding's span, for the misspelled-variant fix.
    pub variants: HashMap<Span, Vec<String>>,
    /// Every error and warning, as `check_modules` reports them.
    pub diagnostics: Vec<Diagnostic>,
}

/// Where the checker writes down names and expression spans as it goes.
#[derive(Debug, Default)]
pub(super) struct Recorder {
    pub spans: Vec<Option<Span>>,
    pub names: HashMap<Span, Name>,
    pub definitions: HashMap<Span, Target>,
    pub signatures: HashMap<Span, super::Sig>,
    pub missing_arms: HashMap<Span, Vec<String>>,
    pub unused_variants: HashMap<Span, Vec<String>>,
    /// Each field's declaration by struct key and field name.
    pub(super) fields: HashMap<(String, String), Span>,
    /// Each variant's declaration by enum key and variant name.
    pub(super) variants: HashMap<(String, String), Span>,
}

impl Recorder {
    pub(super) fn new(count: usize) -> Recorder {
        Recorder {
            spans: vec![None; count],
            ..Recorder::default()
        }
    }

    pub(super) fn expr(&mut self, expr: &Expr) {
        if let Some(slot) = self.spans.get_mut(expr.id.0 as usize) {
            *slot = Some(expr.span);
        }
    }

    /// A written name at `at` defined at the declaration `decl`.
    pub(super) fn define(&mut self, at: Span, decl: Span) {
        self.definitions.insert(at, Target::Decl(decl));
    }

    /// A written module name at `at` (a path segment, or `m` in `m.f`).
    pub(super) fn module(&mut self, at: Span, path: &[String]) {
        self.definitions.insert(at, Target::Module(path.to_vec()));
    }

    pub(super) fn local(&mut self, name: &Ident, ty: &Type, mutable: bool) {
        self.define(name.span, name.span);
        let local = Name::Local {
            name: name.name.clone(),
            ty: ty.clone(),
            mutable,
        };
        self.names.insert(name.span, local);
    }

    /// A function or method named at `at`, declared at `decl`.
    pub(super) fn function(&mut self, at: Span, decl: Span, call: Option<&Expr>) {
        let call = call.map(|c| c.id);
        self.names.insert(at, Name::Function { decl, call });
        self.define(at, decl);
    }

    /// Constant `name` written at `at`, declared at `decl` (v17).
    pub(super) fn constant(&mut self, at: Span, decl: Span, name: &str, ty: &Type) {
        let name = name.to_string();
        let ty = ty.clone();
        self.names.insert(at, Name::Const { name, ty, decl });
        self.define(at, decl);
    }

    /// Field `name` of struct `key`, at its declaration.
    pub(super) fn field(&mut self, key: &str, name: &Ident, ty: &Type) {
        self.fields
            .insert((key.to_string(), name.name.clone()), name.span);
        self.define(name.span, name.span);
        let field = Name::Field {
            name: name.name.clone(),
            ty: ty.clone(),
        };
        self.names.insert(name.span, field);
    }
}

impl Recorder {
    /// A written use of field `name` of struct `key`, at `at`.
    pub(super) fn field_use(&mut self, key: &str, name: &Ident) {
        if let Some(decl) = self.fields.get(&(key.to_string(), name.name.clone())) {
            self.define(name.span, *decl);
        }
    }

    /// Variant `name` of enum `key`, at its declaration.
    pub(super) fn variant(&mut self, key: &str, name: &Ident) {
        self.variants
            .insert((key.to_string(), name.name.clone()), name.span);
        self.define(name.span, name.span);
    }

    /// A written use of variant `name` of enum `key`, at `at`.
    pub(super) fn variant_use(&mut self, key: &str, name: &Ident) {
        if let Some(decl) = self.variants.get(&(key.to_string(), name.name.clone())) {
            self.define(name.span, *decl);
        }
    }
}

/// Where a call writes its callee's name: `f` in `f(x)`, `area` in
/// `p.area()` or `m.f` in `m.f(x)`.
pub(super) fn callee_name(call: &Expr) -> Option<Span> {
    let ExprKind::Call(callee, _) = &call.kind else {
        return None;
    };
    match &callee.kind {
        ExprKind::Name(_) => Some(callee.span),
        ExprKind::Field(_, name) => Some(name.span),
        _ => None,
    }
}

/// Checks `units` as `check_modules` does, returning what was found even
/// when there are errors.
pub fn check_partial(units: &[Unit]) -> Partial {
    let checker = modules::run(units).0;
    let members = checker.type_members();
    let prelude = checker.prelude_functions();
    Partial {
        members,
        prelude,
        types: checker.types,
        spans: checker.hover.spans,
        annotations: checker.annotations,
        instances: checker.instances,
        conversions: checker.conversions,
        callees: checker.callees,
        names: checker.hover.names,
        definitions: checker.hover.definitions,
        signatures: checker.hover.signatures,
        missing_arms: checker.hover.missing_arms,
        variants: checker.hover.unused_variants,
        diagnostics: checker.diagnostics,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::test_util;

    /// The span of the `n`th (0-based) `word` in `src`.
    fn at(src: &str, word: &str, n: usize) -> Span {
        let start = src
            .match_indices(word)
            .nth(n)
            .unwrap_or_else(|| panic!("no {word}"))
            .0;
        Span::new(start, start + word.len())
    }

    /// The span of `word` inside the first `context` in `src`.
    fn within(src: &str, context: &str, word: &str) -> Span {
        let base = at(src, context, 0).start + context.find(word).expect("word in context");
        Span::new(base, base + word.len())
    }

    const SRC: &str = "struct P { x: f64 }\n\
        impl P { fun len(self): f64 = self.x; }\n\
        fun max<T: Ord>(a: T, b: T): T { if a > b { a } else { b } }\n\
        fun main() {\n\
            let p = P { x: 1.0 };\n\
            let mut n = 0;\n\
            for w of [1, 2] { n += w; }\n\
            match Some(3) { Some(k) => println(k), None => {} }\n\
            println(p.len());\n\
            println(max(1.5, 2.5));\n\
        }\n";

    #[test]
    fn locals_of_every_kind_are_named() {
        let partial = test_util::partial(SRC);
        let local = |context, word| partial.names.get(&within(SRC, context, word)).cloned();
        let ty = |name: &str, ty: Type, mutable| {
            Some(Name::Local {
                name: name.to_string(),
                ty,
                mutable,
            })
        };
        assert_eq!(
            local("let p", "p"),
            ty("p", Type::Struct("P".into(), vec![]), false)
        );
        assert_eq!(local("let mut n", "n"), ty("n", Type::I64, true));
        assert_eq!(local("for w", "w"), ty("w", Type::I64, false));
        assert_eq!(local("Some(k)", "k"), ty("k", Type::I64, false));
        assert_eq!(local("(a: T", "a"), ty("a", Type::Param("T".into()), false));
    }

    #[test]
    fn functions_and_methods_are_named_where_declared_and_used() {
        let partial = test_util::partial(SRC);
        let decl_len = at(SRC, "len", 0);
        let decl_max = at(SRC, "max", 0);
        let function = |word, n| match partial.names.get(&at(SRC, word, n)) {
            Some(Name::Function { decl, call }) => (*decl, call.is_some()),
            other => panic!("{word}: {other:?}"),
        };
        assert_eq!(function("len", 0), (decl_len, false));
        assert_eq!(function("len", 1), (decl_len, true));
        assert_eq!(function("max", 0), (decl_max, false));
        assert_eq!(function("max", 1), (decl_max, true));
    }

    #[test]
    fn fields_are_named_at_their_declaration() {
        let partial = test_util::partial(SRC);
        assert_eq!(
            partial.names.get(&within(SRC, "{ x: f64", "x")),
            Some(&Name::Field {
                name: "x".into(),
                ty: Type::F64
            })
        );
    }

    #[test]
    fn expressions_keep_their_spans_and_types() {
        let partial = test_util::partial(SRC);
        let call = at(SRC, "max(1.5, 2.5)", 0);
        let id = partial
            .spans
            .iter()
            .position(|s| *s == Some(call))
            .expect("the call is typed");
        assert_eq!(partial.types[id], Some(Type::F64));
        assert_eq!(partial.instances[&ExprId(id as u32)], [Type::F64]);
    }

    #[test]
    fn a_program_with_errors_still_has_its_other_types() {
        let src = "fun main() { let bad: i64 = \"x\"; let fine = 2.5; println(fine); }";
        let partial = test_util::partial(src);
        let codes: Vec<&str> = partial.diagnostics.iter().map(|d| d.code).collect();
        assert_eq!(
            codes,
            test_util::diagnostics(src)
                .iter()
                .map(|d| d.code)
                .collect::<Vec<_>>()
        );
        assert!(matches!(
            partial.names.get(&at(src, "fine", 0)),
            Some(Name::Local { ty: Type::F64, .. })
        ));
    }

    #[test]
    fn a_valid_program_has_the_types_check_modules_gives() {
        let partial = test_util::partial(SRC);
        let checked = test_util::ok(SRC);
        for (i, ty) in checked.types.iter().enumerate() {
            if *ty != Type::Error {
                assert_eq!(partial.types[i].as_ref(), Some(ty), "expression {i}");
            }
        }
        assert!(partial.diagnostics.is_empty());
    }
    const DEFS: &str = "struct P { x: f64 }
enum Shape { Circle(f64), Dot }
interface Area { fun area(self): f64; }
impl P { fun len(self): f64 = self.x; }
fun twice(n: i64): i64 = n * 2;
fun measure(a: Area): f64 = a.area();
fun main() {
    let p = P { x: 1.0 };
    let mut total = 0;
    total += twice(3);
    let f = twice;
    println(p.x + p.len());
    let s = Shape.Circle(2.0);
    match s { Circle(r) => println(r), Dot => {} }
    println(total);
    println(f(1));
}
";

    /// Where `definitions` sends the `word` inside the first `context`.
    fn target(partial: &Partial, context: &str, word: &str) -> Option<Target> {
        partial
            .definitions
            .get(&within(DEFS, context, word))
            .cloned()
    }

    fn decl(context: &str, word: &str) -> Option<Target> {
        Some(Target::Decl(within(DEFS, context, word)))
    }

    #[test]
    fn locals_go_to_their_bindings() {
        let partial = test_util::partial(DEFS);
        assert_eq!(target(&partial, "p.x +", "p"), decl("let p", "p"));
        assert_eq!(
            target(&partial, "total += ", "total"),
            decl("let mut total", "total")
        );
        assert_eq!(
            target(&partial, "println(total", "total"),
            decl("let mut total", "total")
        );
        assert_eq!(target(&partial, "n * 2", "n"), decl("(n: i64)", "n"));
        assert_eq!(target(&partial, "f(1)", "f"), decl("let f", "f"));
        assert_eq!(target(&partial, "(r), Dot", "r"), decl("(r)", "r"));
    }

    #[test]
    fn functions_and_methods_go_to_their_declarations() {
        let partial = test_util::partial(DEFS);
        assert_eq!(
            target(&partial, "+= twice", "twice"),
            decl("fun twice", "twice")
        );
        assert_eq!(
            target(&partial, "= twice;", "twice"),
            decl("fun twice", "twice")
        );
        assert_eq!(target(&partial, "p.len()", "len"), decl("fun len", "len"));
        assert_eq!(
            target(&partial, "a.area()", "area"),
            decl("fun area(self): f64;", "area")
        );
        // A declaration is its own definition.
        assert_eq!(
            target(&partial, "fun twice", "twice"),
            decl("fun twice", "twice")
        );
    }

    #[test]
    fn types_fields_and_variants_go_to_their_declarations() {
        let partial = test_util::partial(DEFS);
        assert_eq!(target(&partial, "= P {", "P"), decl("struct P", "P"));
        assert_eq!(target(&partial, "impl P", "P"), decl("struct P", "P"));
        assert_eq!(
            target(&partial, "(a: Area)", "Area"),
            decl("interface Area", "Area")
        );
        assert_eq!(
            target(&partial, "= Shape.Circle", "Shape"),
            decl("enum Shape", "Shape")
        );
        assert_eq!(
            target(&partial, "= Shape.Circle", "Circle"),
            decl("{ Circle(f64)", "Circle")
        );
        assert_eq!(target(&partial, "Dot =>", "Dot"), decl("Dot }", "Dot"));
        assert_eq!(target(&partial, "p.x +", "x"), decl("{ x: f64", "x"));
        assert_eq!(target(&partial, "P { x: 1.0", "x"), decl("{ x: f64", "x"));
    }
    #[test]
    fn an_unused_bindings_variants_are_recorded() {
        let src = "enum Shape { Circle(f64), Dot, Empty }
fun main() {
    let o: Option<Shape> = None;
    match o { Some(Emtpy) => {}, None => {} }
    match 3 { n => {} }
}
";
        let partial = test_util::partial(src);
        let at = |needle: &str| {
            let start = src.find(needle).expect("a binding");
            partial
                .variants
                .get(&Span::new(start, start + 1))
                .cloned()
                .or_else(|| partial.variants.get(&Span::new(start, start + 5)).cloned())
        };
        assert_eq!(at("Emtpy").expect("recorded"), ["Circle", "Dot", "Empty"]);
        // `n` matches an integer: no variants to offer.
        assert_eq!(at("n =>"), None);
    }

    #[test]
    fn missing_arms_are_recorded_in_full() {
        let src = "enum Shape { Circle(f64), Rect(f64, f64), Dot, Tri(f64), Hex }
fun main() {
    let s = Shape.Dot;
    match s { Circle(r) => println(r) }
    let o: Option<Shape> = None;
    match o { Some(Circle(_)) => {}, None => {} }
    let n = 3;
    match n { 1 => {} }
}
";
        let partial = test_util::partial(src);
        let at = |needle: &str| {
            let start = src.find(needle).expect("a match");
            partial
                .missing_arms
                .iter()
                .find(|(s, _)| s.start == start)
                .map(|(_, m)| m.clone())
        };
        // Every one, not only the three the message lists, payloads as `_`.
        assert_eq!(
            at("match s").expect("recorded"),
            ["Rect(_, _)", "Dot", "Tri(_)", "Hex"]
        );
        assert_eq!(
            at("match o").expect("recorded"),
            ["Some(Rect(_, _))", "Some(Dot)", "Some(Tri(_))", "Some(Hex)"]
        );
        assert_eq!(at("match n").expect("recorded"), Vec::<String>::new());
    }
}
