//! Public-API tests for the parser (PRP-003): every spec program parses.

use lugha::lexer::{Token, TokenKind, lex};
use lugha::parser::{parse, sexp};
use lugha::span::Span;

#[path = "common/spec_programs.rs"]
mod spec_programs;

fn tokens(src: &str) -> Vec<Token> {
    lex(src)
        .unwrap_or_else(|d| panic!("{src:?} does not lex: {d:?}"))
        .0
}

#[test]
fn every_spec_program_parses_without_diagnostics() {
    for (name, src) in spec_programs::all() {
        let (_, warnings) = parse(&tokens(&src)).unwrap_or_else(|d| panic!("{name}: {d:?}"));
        assert!(warnings.is_empty(), "{name}: {warnings:?}");
    }
}

#[test]
fn milestone_1_program_has_the_expected_tree() {
    let (program, _) = parse(&tokens("fun main(): i32 { 2 + 3 * 4 }")).unwrap();
    assert_eq!(
        sexp::program(&program),
        "(fun main () i32 (block (+ 2 (* 3 4))))"
    );
}

#[test]
fn parsing_never_panics_on_any_token_prefix() {
    let all: String = spec_programs::all()
        .into_iter()
        .map(|(_, src)| src)
        .collect::<Vec<_>>()
        .join("\n");
    let full = tokens(&all);
    for len in 0..full.len() {
        let mut prefix = full[..len].to_vec();
        let end = prefix.last().map_or(0, |t| t.span.end);
        prefix.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(end, end),
        });
        let _ = parse(&prefix);
    }
}
