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
- Checker rules for casts, mutability, assignment targets, missing returns (with "remove this semicolon"), `break`/`continue` outside loops and discarded `if` values (E0408, E0501–E0505), plus the unreachable-code warning W0101
- Type checker: names, primitive types, literal inference and operator typing, with error codes E0301–E0305 and E0401–E0407; `lughac check` now type-checks
- Functions with parameters, calls (including forward and mutual recursion), `return` and void functions — **milestone 2 complete**
- Variables, assignment, blocks with tail values, `if` expressions, `while` and `for` loops, `break`/`continue`, comparisons and short-circuit `&&`/`||` inside `main`
- `lughac` command line: `build`, `run`, `check`, `--emit=tokens|ast|ir`, `-O0`/`-O2`, human and JSON diagnostics — **milestone 1 complete**
- Code generation for milestone 1 (integer arithmetic in `main`) to native object files via LLVM 21, and linking with `cc`
- Parser for the full Lugha v0 grammar, reporting several syntax errors per run with codes E0201–E0206
- Lexer for the full Lugha v0 token set, reporting every lexical error with codes E0101–E0109
- Acceptance-test runner: drop a `.la` program and its expected output into `tests/programs/` and `cargo test -- --ignored` checks it
