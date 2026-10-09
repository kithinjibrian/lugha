//! Type checker — names, types and inference for the milestone 3 subset
//! (spec §4–§6).
//!
//! Gives every expression a type, recorded in a table keyed by `ExprId` for
//! codegen (spec §9). Reports E03xx/E04xx and keeps going: an expression that
//! already failed gets `Type::Error`, which fits anything and is never
//! reported again. Constructs from later milestones stop the checker with
//! `CheckError::Unsupported` (rule 9).
//!
//! Depends on: ast, diagnostic, span.

mod access;
mod array;
mod assign;
mod call;
mod env;
mod errors;
mod expr;
mod flow;
mod literal;
mod ops;
mod stmt;
mod types;

use std::collections::HashMap;

pub use types::Type;

use crate::ast::Program;
use crate::diagnostic::{Diagnostic, Severity};
use crate::span::Span;

/// The checker's output.
#[derive(Debug, Clone, PartialEq)]
pub struct Checked {
    /// The type of every expression, indexed by `ExprId`.
    pub types: Vec<Type>,
}

/// Why checking failed.
#[derive(Debug, Clone, PartialEq)]
pub enum CheckError {
    /// Errors in the program (E03xx/E04xx) — exit 1.
    Program(Vec<Diagnostic>),
    /// A construct a later milestone adds — exit 2.
    Unsupported {
        what: &'static str,
        milestone: u8,
        span: Span,
    },
}

/// Type-checks `program`.
///
/// # Errors
///
/// [`CheckError::Program`] with every diagnostic found, or
/// [`CheckError::Unsupported`] at the first milestone 4/5 construct.
pub fn check(program: &Program) -> Result<(Checked, Vec<Diagnostic>), CheckError> {
    let mut checker = Checker {
        types: vec![None; program.expr_count as usize],
        diagnostics: Vec::new(),
        functions: HashMap::new(),
        scopes: Vec::new(),
        ret: Type::Void,
        loops: 0,
        dead: false,
        iterating: Vec::new(),
    };
    match checker.program(program) {
        Err(Stop {
            what,
            milestone,
            span,
        }) => Err(CheckError::Unsupported {
            what,
            milestone,
            span,
        }),
        Ok(())
            if checker
                .diagnostics
                .iter()
                .any(|d| d.severity == Severity::Error) =>
        {
            Err(CheckError::Program(checker.diagnostics))
        }
        Ok(()) => {
            // Every expression of a valid program has been visited; `Error` is a safe filler.
            let types = checker
                .types
                .into_iter()
                .map(|t| t.unwrap_or(Type::Error))
                .collect();
            Ok((Checked { types }, checker.diagnostics))
        }
    }
}

/// A construct beyond milestone 3: checking stops here.
pub(super) struct Stop {
    what: &'static str,
    milestone: u8,
    span: Span,
}

pub(super) fn stop(what: &'static str, milestone: u8, span: Span) -> Stop {
    Stop {
        what,
        milestone,
        span,
    }
}

/// The result of a checking step that may hit an unsupported construct.
pub(super) type Checking<T> = Result<T, Stop>;

/// A declared function.
pub(super) struct Signature {
    pub params: Vec<Type>,
    pub ret: Type,
    pub span: Span,
}

/// A local variable or parameter.
#[derive(Clone)]
pub(super) struct Local {
    pub ty: Type,
    pub binding: Binding,
    /// Where the name was declared, for "declared here" labels.
    pub span: Span,
}

/// How a local was bound, which decides whether it may be assigned (spec §4).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Binding {
    Let { mutable: bool },
    Param,
    LoopVar,
}

/// Checking state for one program.
pub(super) struct Checker {
    types: Vec<Option<Type>>,
    diagnostics: Vec<Diagnostic>,
    functions: HashMap<String, Signature>,
    scopes: Vec<HashMap<String, Local>>,
    /// The return type of the function being checked.
    ret: Type,
    /// How many loops enclose the current statement, for `break`/`continue`.
    loops: u32,
    /// True while checking code after a divergence, so nested blocks don't
    /// repeat W0101.
    dead: bool,
    /// Places being iterated by enclosing `for … of` loops, with the loop's span (E0507).
    iterating: Vec<(array::Path, Span)>,
}

impl Checker {
    fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    fn record(&mut self, expr: &crate::ast::Expr, ty: Type) {
        self.types[expr.id.0 as usize] = Some(ty);
    }
}

#[cfg(test)]
pub(crate) mod test_util {
    use super::{CheckError, Checked, Type, check};
    use crate::ast::{Item, StmtKind};
    use crate::{lexer, parser};

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

    /// `(code, spanned source)` of every diagnostic for `src`.
    pub fn errors(src: &str) -> Vec<(&'static str, &str)> {
        match check(&program(src)) {
            Err(CheckError::Program(diags)) => diags
                .iter()
                .map(|d| (d.code, &src[d.span.start..d.span.end]))
                .collect(),
            other => panic!("{src:?}: expected diagnostics, got {other:?}"),
        }
    }

    /// Every diagnostic for `src`, which must have at least one error.
    pub fn diagnostics(src: &str) -> Vec<crate::diagnostic::Diagnostic> {
        match check(&program(src)) {
            Err(CheckError::Program(diags)) => diags,
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

    /// `(what, milestone, spanned source)` of the unsupported construct in `src`.
    pub fn stopped(src: &str) -> (&'static str, u8, &str) {
        match check(&program(src)) {
            Err(CheckError::Unsupported {
                what,
                milestone,
                span,
            }) => (what, milestone, &src[span.start..span.end]),
            other => panic!("{src:?}: expected Unsupported, got {other:?}"),
        }
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
                StmtKind::While { body, .. } | StmtKind::For { body, .. } => {
                    visit(body, name, found)
                }
                _ => {}
            }
        }
    }
}
