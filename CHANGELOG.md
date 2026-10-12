# CHANGELOG — lugha

Follows [Keep a Changelog](https://keepachangelog.com) format.
Updated at the end of every session when something is completed and merged.
Never deleted. Older entries are never modified.

---

## [Unreleased]

### Added
- Too-deep recursion panics with `panic: stack overflow` (exit 101) instead of crashing with a signal; other segfaults (from C code) are unchanged
- A crash inside lughac is now reported as `error: internal compiler error: …` (or one JSON line) and exits with 2 instead of 101
- Growable arrays: `xs.push(v)`, `xs.pop()` (an `Option`), `xs.clear()`, with a capacity header and doubling growth; the v1 specification is complete and is what `lughac spec` prints — **milestone 8 complete: Lugha v1 is complete**
- Methods: `impl` blocks with `self` and `mut self` receivers, associated functions like `Vec2.new(…)`, method calls (E0310, E0415, E0508 for a `mut self` call whose argument aliases the receiver); every v1 construct now compiles — **milestone 7 complete**
- Recursive enums and structs: `enum List { Nil, Cons(i64, List) }`, trees, `struct Item { next: Option<Item> }`, with no `Box`; only the slots that lead back live on the heap — **milestone 6 complete**
- Enums, `match` and `Option<T>`: declarations, `Shape.Circle(2.0)`, `Some`/`None`, exhaustive flat `match` on enums, integers, `bool`s and strings (E0309, E0311, E0415–E0417, W0102); `lughac spec` now prints the v1 specification (in progress)
- v1 syntax: `enum`, `match`, `impl`, `self`, `Option<T>` and `=>` lex and parse (`--emit=ast` shows them); `Shape::Circle` is E0207 with help; v1 constructs stop with "not implemented yet" until their milestone
- Lugha v1 spec draft approved: enums and `match`, `Option<T>`, recursive enums, methods, growable arrays (milestones 6–8)
- Move on last use: storing a value from a local that is never used again moves it instead of copying (the particles benchmark runs 2× faster); spec §4 notes it
- DECISION-004 (design system) closed as not applicable — Lugha has no UI
- DECISION-010 resolved: array copies stay eager (no copy-on-write), backed by measurements in `docs/decisions/decision-010.md`; spec §12 updated
- README with prerequisites, usage and tests; CI (fmt, clippy, tests in `ubuntu:26.04` with LLVM 21 and libgc) on every push and PR; Claude Code deny rules for the protected files; `setup.md` moved to `docs/`
- `lughac spec` prints the bundled language specification; the §10 programs are tested straight from it, and every one passes — **milestone 5 complete: Lugha v0 is complete**
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
