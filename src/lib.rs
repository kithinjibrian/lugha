//! lugha — the compiler library behind `lughac`.
//!
//! Compiles one `.la` source file to a native executable through the stages
//! defined in spec §9: lex, parse, check, lower to LLVM IR, emit, link.
//! The language itself is defined in `docs/specs/Language v17 Specification.md`
//! (v16 plus the v17 features implemented so far; `Language v17 Draft.md`
//! is the plan, and v16's file is frozen).

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
    ("task", include_str!("std/task.la")),
    ("time", include_str!("std/time.la")),
    ("net", include_str!("std/net.la")),
    ("tls", include_str!("std/tls.la")),
    ("crypto", include_str!("std/crypto.la")),
    ("http", include_str!("std/http.la")),
    ("toml", include_str!("std/toml.la")),
    // Internal to `std.http`: a `$` name no program can write.
    ("$http_wire", include_str!("std/http_wire.la")),
    // Internal to `std.toml`.
    ("$toml_value", include_str!("std/toml_value.la")),
    ("$toml_strings", include_str!("std/toml_strings.la")),
    ("$toml_scan", include_str!("std/toml_scan.la")),
    ("$toml_parse", include_str!("std/toml_parse.la")),
    ("json", include_str!("std/json.la")),
    // Internal to `std.json`.
    ("$json_value", include_str!("std/json_value.la")),
    ("$json_parse", include_str!("std/json_parse.la")),
    ("$json_write", include_str!("std/json_write.la")),
];

/// The prelude: the array helpers, written in Lugha and loaded with every
/// program (v3 draft §4, PRP-041), then the iterator helpers (v10, v13),
/// one module from six files (Unicode text, v14; operator bounds, v15; maps
/// and sets, v17).
pub const PRELUDE: &str = concat!(
    include_str!("prelude/core.la"),
    include_str!("prelude/iter.la"),
    include_str!("prelude/more.la"),
    include_str!("prelude/text.la"),
    include_str!("prelude/ops.la"),
    include_str!("prelude/maps.la")
);

/// The language specification this compiler implements, embedded verbatim;
/// `lughac spec` prints it (spec §9, "LLM-ready spec").
pub const SPEC: &str = include_str!("../docs/specs/Language v17 Specification.md");
