//! lugha — the compiler library behind `lughac`.
//!
//! Compiles one `.la` source file to a native executable through the stages
//! defined in spec §9: lex, parse, check, lower to LLVM IR, emit, link.
//! The language itself is defined in `docs/specs/Language v7 Specification.md`
//! (v6 plus the v7 features implemented so far; `Language v7 Draft.md` is
//! the plan, and v6's file is frozen).

pub mod ast;
pub mod check;
pub mod codegen;
pub mod diagnostic;
pub mod driver;
pub mod lexer;
pub mod link;
pub mod modules;
pub mod parser;
pub mod span;

/// The prelude: the array helpers, written in Lugha and loaded with every
/// program (v3 draft §4, PRP-041).
pub const PRELUDE: &str = include_str!("prelude.la");

/// The language specification this compiler implements, embedded verbatim;
/// `lughac spec` prints it (spec §9, "LLM-ready spec").
pub const SPEC: &str = include_str!("../docs/specs/Language v7 Specification.md");
