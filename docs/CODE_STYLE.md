# Code Style — lugha

Documentation rules for all code in this project.
Read this before writing any function, type, or module.

Formatting is whatever `cargo fmt` produces. Lints are whatever `cargo clippy -- -D warnings` accepts.

---

## Layer 1 — Doc comments (what this is)

Every `pub` function, struct, enum, trait, and type alias gets a `///` doc comment.
Private items get one only if their purpose is not obvious.

```rust
/// Parses a decimal integer literal.
///
/// Leading and trailing whitespace is ignored.
///
/// # Errors
///
/// Returns [`ParseError::InvalidNumber`] if `src` is not a valid `i64`.
///
/// # Examples
///
/// ```
/// assert_eq!(lugha::parse_number(" 42 ").unwrap(), 42);
/// ```
pub fn parse_number(src: &str) -> Result<i64, ParseError> { ... }
```

Rules:
- The first line is one sentence starting with a verb: "Parses", "Returns", "Creates". Never "This function...".
- Every function returning `Result` has an `# Errors` section listing each variant and when it occurs.
- Every function that can panic has a `# Panics` section stating the invariant.
- Every `unsafe fn` has a `# Safety` section; every `unsafe` block has a `// SAFETY:` comment.
- Every public API function has an `# Examples` section — it runs as a doctest, so it must compile.

## Layer 2 — Inline comments (why this decision was made)

Inline `//` comments explain decisions, not code.

- Good: `// Peek two tokens ahead — `a.b` and `a..b` share a prefix`
- Bad: `// increment the index`

Rules:
- Comment above the line it explains, not at the end.
- Comment when: a magic number appears, a crate is used non-obviously, a performance trade-off was made, a guard prevents a non-obvious bug, a workaround exists.
- If code needs a comment to say *what* it does, rewrite the code.
- `// TODO` always carries an issue reference or a date: `// TODO(#12): ...` or `// TODO(2026-10-09): ...`.

## Module-Level Documentation

Every module file starts with a `//!` block:

```rust
//! Lexer — turns source text into tokens.
//!
//! Does not build syntax trees; that is the parser's job.
```

## Anti-Patterns

1. **Describing the code.** Comments explain what the code cannot.
2. **Stale comments.** Change code and its comment in the same edit.
3. **Untracked TODOs.** Every TODO gets an issue or a date.
4. **Over-documenting trivial private helpers.**
5. **Missing `# Errors`.** The error contract is the most important part of the doc for callers.
