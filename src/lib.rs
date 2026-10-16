//! lugha — the compiler library behind `lughac`.
//!
//! Compiles one `.la` source file to a native executable through the stages
//! defined in spec §9: lex, parse, check, lower to LLVM IR, emit, link.
//! The language itself is defined in `docs/specs/Language v16 Specification.md`
//! (v15 plus the v16 features implemented so far; `Language v16 Draft.md`
//! is the plan, and v15's file is frozen).

pub mod ast;
pub mod check;
pub mod codegen;
pub mod diagnostic;
pub mod driver;
pub mod explain;
pub mod format;
pub mod formatter;
pub mod lexer;
pub mod link;
pub mod modules;
pub mod parser;
pub mod span;
pub mod template;

/// The standard library (v15): each `std` module's name and source, loaded
/// only when a program imports `std.<name>`.
pub const STD: &[(&str, &str)] = &[
    ("math", include_str!("std/math.la")),
    ("random", include_str!("std/random.la")),
    ("env", include_str!("std/env.la")),
    ("fs", include_str!("std/fs.la")),
    ("process", include_str!("std/process.la")),
];

/// The prelude: the array helpers, written in Lugha and loaded with every
/// program (v3 draft §4, PRP-041), then the iterator helpers (v10, v13),
/// one module from five files (Unicode text, v14; operator bounds, v15).
pub const PRELUDE: &str = concat!(
    include_str!("prelude/core.la"),
    include_str!("prelude/iter.la"),
    include_str!("prelude/more.la"),
    include_str!("prelude/text.la"),
    include_str!("prelude/ops.la")
);

/// The language specification this compiler implements, embedded verbatim;
/// `lughac spec` prints it (spec §9, "LLM-ready spec").
pub const SPEC: &str = include_str!("../docs/specs/Language v16 Specification.md");
