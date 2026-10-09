//! Helpers shared by the parser's unit tests.

use super::{parse, sexp};
use crate::ast::{Item, Program};
use crate::lexer::lex;

/// Lexes and parses `src`. Panics if lexing fails.
pub fn parse_src(src: &str) -> Result<Program, Vec<crate::diagnostic::Diagnostic>> {
    let (tokens, _) = lex(src).unwrap_or_else(|d| panic!("{src:?} does not lex: {d:?}"));
    parse(&tokens).map(|(program, _)| program)
}

/// The S-expression of a whole program. Panics on any error.
pub fn program(src: &str) -> String {
    sexp::program(&parse_src(src).unwrap_or_else(|d| panic!("{src:?}: {d:?}")))
}

/// The S-expression of the body block of `fun t() { <body> }`.
pub fn body(body: &str) -> String {
    let p =
        parse_src(&format!("fun t() {{ {body} }}")).unwrap_or_else(|d| panic!("{body:?}: {d:?}"));
    let Item::Fun(f) = &p.items[0] else {
        unreachable!("parsed a fun")
    };
    sexp::block(&f.body)
}

/// The S-expression of `expr` parsed as the tail of a function body.
pub fn expr(expr: &str) -> String {
    let p =
        parse_src(&format!("fun t() {{ {expr} }}")).unwrap_or_else(|d| panic!("{expr:?}: {d:?}"));
    let Item::Fun(f) = &p.items[0] else {
        unreachable!("parsed a fun")
    };
    sexp::expr(
        f.body
            .tail
            .as_ref()
            .unwrap_or_else(|| panic!("{expr:?} has no tail")),
    )
}

/// `(code, spanned source text)` for every diagnostic. Panics if parsing succeeds.
pub fn errors(src: &str) -> Vec<(&'static str, &str)> {
    let diags = parse_src(src).expect_err("expected syntax errors");
    diags
        .iter()
        .map(|d| (d.code, &src[d.span.start..d.span.end]))
        .collect()
}
