//! lugha — the compiler library behind `lughac`.
//!
//! Compiles one `.la` source file to a native executable through the stages
//! defined in spec §9: lex, parse, check, lower to LLVM IR, emit, link.
//! The language itself is defined in `docs/specs/Language v3 Specification.md`
//! (v2 plus the v3 features implemented so far; `Language v3 Draft.md` is
//! the plan, and v2's file is frozen).

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

/// The language specification this compiler implements, embedded verbatim;
/// `lughac spec` prints it (spec §9, "LLM-ready spec").
pub const SPEC: &str = include_str!("../docs/specs/Language v3 Specification.md");
