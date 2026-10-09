# CHANGELOG — lugha

Follows [Keep a Changelog](https://keepachangelog.com) format.
Updated at the end of every session when something is completed and merged.
Never deleted. Older entries are never modified.

---

## [Unreleased]

### Added
- AI context system (CLAUDE.md, MEMORY.md, CONTEXT.md, DECISIONS.md, PRPs, code style guide)
- Lugha v0 language specification (`docs/specs/`)
- `lughac` crate skeleton (builds against LLVM 21)
- Parser for the full Lugha v0 grammar, reporting several syntax errors per run with codes E0201–E0206
- Lexer for the full Lugha v0 token set, reporting every lexical error with codes E0101–E0109
- Acceptance-test runner: drop a `.la` program and its expected output into `tests/programs/` and `cargo test -- --ignored` checks it
