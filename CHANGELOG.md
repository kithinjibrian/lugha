# CHANGELOG — lugha

Follows [Keep a Changelog](https://keepachangelog.com) format.
Updated at the end of every session when something is completed and merged.
Never deleted. Older entries are never modified.

---

## [Unreleased]

### Added
- Structs: declarations in any order, literals, field reads and writes, structs in arrays and arrays in structs, deep copies of structs holding arrays, and trees through `kids: Node[]` (E0307, E0308, E0413, E0414) — the §10 centroid program prints `centroid: 2.0, 1.0`
- Arrays: `T[]` types, list and repeat literals, `.len`, bounds-checked element reads and writes, `for x of xs`, and value semantics with deep copies at the spec §4 copy sites (E0409 array case, E0411 iteration, E0412, E0507) — the §10 primes program prints 25
- AI context system (CLAUDE.md, MEMORY.md, CONTEXT.md, DECISIONS.md, PRPs, code style guide)
- Lugha v0 language specification (`docs/specs/`)
- `lughac` crate skeleton (builds against LLVM 21)
- String concatenation, comparison by content, `.len` and bounds-checked byte indexing, with an `index out of bounds: the length is N but the index is I` panic
- `extern fun` for calling C, and integer overflow and division panics at the operator (`panic: integer overflow at file:line:col`, exit 101) — **milestone 4 complete**
- C runtime and the `print`, `println`, `panic` and `to_string` intrinsics, with spec float formatting; string literals as values — programs can print
- Real `i32`, `i64`, `u8`, `f64` and `bool` in generated code, with spec §4 casts (saturating float-to-int) — **milestone 3 complete**
- Checker rules for casts, mutability, assignment targets, missing returns (with "remove this semicolon"), `break`/`continue` outside loops and discarded `if` values (E0408, E0501–E0505), plus the unreachable-code warning W0101
- Type checker: names, primitive types, literal inference and operator typing, with error codes E0301–E0305 and E0401–E0407; `lughac check` now type-checks
- Functions with parameters, calls (including forward and mutual recursion), `return` and void functions — **milestone 2 complete**
- Variables, assignment, blocks with tail values, `if` expressions, `while` and `for` loops, `break`/`continue`, comparisons and short-circuit `&&`/`||` inside `main`
- `lughac` command line: `build`, `run`, `check`, `--emit=tokens|ast|ir`, `-O0`/`-O2`, human and JSON diagnostics — **milestone 1 complete**
- Code generation for milestone 1 (integer arithmetic in `main`) to native object files via LLVM 21, and linking with `cc`
- Parser for the full Lugha v0 grammar, reporting several syntax errors per run with codes E0201–E0206
- Lexer for the full Lugha v0 token set, reporting every lexical error with codes E0101–E0109
- Acceptance-test runner: drop a `.la` program and its expected output into `tests/programs/` and `cargo test -- --ignored` checks it
