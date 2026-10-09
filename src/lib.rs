//! lugha — the compiler library behind `lughac`.
//!
//! Compiles one `.la` source file to a native executable through the stages
//! defined in spec §9: lex, parse, check, lower to LLVM IR, emit, link.
//! The language itself is defined in `docs/specs/Language v0 Specification.md`.

pub mod ast;
pub mod check;
pub mod codegen;
pub mod diagnostic;
pub mod driver;
pub mod lexer;
pub mod link;
pub mod parser;
pub mod span;
