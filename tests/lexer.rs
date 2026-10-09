//! Public-API tests for the lexer (PRP-002): every spec §10/§11 program lexes cleanly.

use lugha::lexer::{TokenKind, lex};

#[path = "common/spec_programs.rs"]
mod spec_programs;

#[test]
fn every_spec_program_lexes_without_diagnostics() {
    for (name, src) in spec_programs::all() {
        let (tokens, warnings) = lex(&src).unwrap_or_else(|d| panic!("{name}: {d:?}"));
        assert!(warnings.is_empty(), "{name}: {warnings:?}");
        assert_eq!(
            tokens.last().map(|t| &t.kind),
            Some(&TokenKind::Eof),
            "{name}"
        );
    }
}

#[test]
fn milestone_1_program_has_the_expected_tokens() {
    use TokenKind::*;
    let (tokens, _) = lex("fun main(): i32 { 2 + 3 * 4 }").unwrap();
    let kinds: Vec<_> = tokens.into_iter().map(|t| t.kind).collect();
    let main = Ident("main".into());
    let expected = [
        Fun,
        main,
        LParen,
        RParen,
        Colon,
        TyI32,
        LBrace,
        Int(2),
        Plus,
        Int(3),
        Star,
        Int(4),
        RBrace,
        Eof,
    ];
    assert_eq!(kinds, expected);
}

#[test]
fn lexing_never_panics_on_any_prefix() {
    // Cutting a valid program at every char boundary produces every kind of
    // half-finished token: open strings, `0x`, `2.0e`, lone `&`, multi-byte chars.
    let centroid = spec_programs::source("Structs, for-of and casts");
    let sample = format!("{centroid}\nlet s = \"h\\é👋\"; let n = 0xFF + 2.0e-3 && x || y; // c");
    for (i, _) in sample.char_indices() {
        let _ = lex(&sample[..i]);
    }
}
