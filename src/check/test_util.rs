//! Test helpers for the checker's unit tests: check a source and look at
//! its types, errors or warnings.

use super::{Checked, Type, Unit, check_modules};
use crate::ast::{Item, Program, StmtKind};
use crate::diagnostic::Diagnostic;
use crate::modules::{Module, with_prelude};
use crate::{lexer, parser};

/// Checks `program` as the main file, with the prelude after it as in
/// every real program (PRP-041).
fn check(program: &Program) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
    let main = Module {
        path: Vec::new(),
        file: "test.la".into(),
        base: 0,
        // Only the prelude's base depends on it: past every test source.
        text: " ".repeat(1 << 16),
        program: Some(program.clone()),
    };
    let mut modules = vec![main];
    let mut prelude_errors = Vec::new();
    with_prelude(&mut modules, &mut prelude_errors);
    assert!(
        prelude_errors.is_empty(),
        "the prelude parses: {prelude_errors:?}"
    );
    let units: Vec<Unit> = modules
        .iter()
        .map(|m| Unit {
            path: &m.path,
            program: m.program.as_ref().expect("both parsed"),
        })
        .collect();
    check_modules(&units)
}

fn program(src: &str) -> crate::ast::Program {
    let (tokens, _) = lexer::lex(src).expect("test source lexes");
    parser::parse(&tokens).expect("test source parses").0
}

/// Checks `src`, which must have no errors.
pub fn ok(src: &str) -> Checked {
    check(&program(src))
        .unwrap_or_else(|e| panic!("{src:?}: {e:?}"))
        .0
}

/// Every diagnostic for `src`, errors or warnings, in order.
pub fn diagnostics_or_warnings(src: &str) -> Vec<crate::diagnostic::Diagnostic> {
    match check(&program(src)) {
        Ok((_, warnings)) => warnings,
        Err(diags) => diags,
    }
}

/// `(code, spanned source)` of every diagnostic for `src`.
pub fn errors(src: &str) -> Vec<(&'static str, &str)> {
    match check(&program(src)) {
        Err(diags) => diags
            .iter()
            .map(|d| (d.code, &src[d.span.start..d.span.end]))
            .collect(),
        other => panic!("{src:?}: expected diagnostics, got {other:?}"),
    }
}

/// Every diagnostic for `src`, which must have at least one error.
pub fn diagnostics(src: &str) -> Vec<crate::diagnostic::Diagnostic> {
    match check(&program(src)) {
        Err(diags) => diags,
        other => panic!("{src:?}: expected diagnostics, got {other:?}"),
    }
}

/// `(code, spanned source)` of every warning for `src`, which must have no errors.
pub fn warnings(src: &str) -> Vec<(&'static str, &str)> {
    let (_, warnings) = check(&program(src)).unwrap_or_else(|e| panic!("{src:?}: {e:?}"));
    warnings
        .iter()
        .map(|d| (d.code, &src[d.span.start..d.span.end]))
        .collect()
}

/// The type of the initialiser of the first `let name` in `fun main`.
pub fn let_type(src: &str, name: &str) -> Type {
    let p = program(src);
    let types = check(&p)
        .unwrap_or_else(|e| panic!("{src:?}: {e:?}"))
        .0
        .types;
    let mut found = None;
    for item in &p.items {
        if let Item::Fun(f) = item {
            visit(&f.body, name, &mut found);
        }
    }
    let id = found.unwrap_or_else(|| panic!("no `let {name}` in {src:?}"));
    types[id as usize].clone()
}

fn visit(block: &crate::ast::Block, name: &str, found: &mut Option<u32>) {
    for stmt in &block.stmts {
        match &stmt.kind {
            StmtKind::Let { name: n, init, .. } if n.name == name && found.is_none() => {
                *found = Some(init.id.0);
            }
            StmtKind::While { body, .. } | StmtKind::For { body, .. } => visit(body, name, found),
            _ => {}
        }
    }
}

/// An interface whose items are all methods, and two types satisfying it
/// (v5 draft §10), for the interface-value tests.
pub const SHAPES: &str = "interface Shape { fun area(self): f64; fun grow(mut self, k: f64); }\n\
    struct Circle { r: f64 }\n\
    impl Circle { fun area(self): f64 = 3.0 * self.r * self.r; fun grow(mut self, k: f64) { self.r *= k; } }\n\
    struct Square { side: f64 }\n\
    impl Square { fun area(self): f64 = self.side * self.side; fun grow(mut self, k: f64) { self.side *= k; } }\n";
