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

#[test]
fn every_v2_draft_program_parses() {
    // PRP-030: the draft isn't embedded in lughac, so read it from disk. Each
    // §10 code block parses on its own, the two module files included.
    let path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/docs/specs/Language v2 Draft.md"
    );
    let draft = std::fs::read_to_string(path).expect("the v2 draft exists");
    let examples = &draft[draft.find("## §10").expect("§10")..draft.find("## §11").expect("§11")];
    let blocks: Vec<&str> = examples.split("```").skip(1).step_by(2).collect();
    assert_eq!(
        blocks.len(),
        5,
        "patterns, geometry.la, main.la, generics, interfaces"
    );
    // Milestone 12's `interface` syntax is parsed once that milestone lands.
    let (pending, blocks): (Vec<&str>, Vec<&str>) = blocks
        .into_iter()
        .partition(|src| src.contains("\ninterface "));
    assert_eq!(
        pending.len(),
        1,
        "only the interfaces program waits for M12"
    );
    for src in blocks {
        let (_, warnings) = parse(&tokens(src)).unwrap_or_else(|d| panic!("{src}\n{d:?}"));
        assert!(warnings.is_empty(), "{warnings:?}");
    }
}
