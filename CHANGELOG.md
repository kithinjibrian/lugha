# CHANGELOG — lugha

Follows [Keep a Changelog](https://keepachangelog.com) format.
Updated at the end of every session when something is completed and merged.
Never deleted. Older entries are never modified.

---

## [Unreleased]

### Changed
- Checker housekeeping: function bodies moved to `check/body.rs` and the array built-in methods to `check/builtins.rs`, keeping `stmt.rs` and `method_calls.rs` well under 300 lines
- The C runtime's source is split into three files under `runtime/` (core, stack overflow handler, printing) and joined into the same single `lugha_rt.c` at build time; compiled programs are unchanged

### Added
- Lugha v4 spec draft approved (`docs/specs/Language v4 Draft.md`): input and output — `Result<T, E>` and the `?` operator, string and `Option`/`Result` helpers, and `read_line`, `read_file`, `write_file`, `append_file`, `eprint`/`eprintln`, `exit` (milestones 17–19)
- Array helpers: `xs.map(f)`, `xs.filter(f)`, `xs.fold(init, f)`, `xs.sort_by(less)` (a stable merge sort), `xs.any(f)` and `xs.all(f)`, each returning a new value and chaining (`people.filter(…).map(…)`); closure arguments need no written types. They are written in Lugha in a prelude bundled with the compiler and compiled only for the types a program uses — **milestone 16 complete: Lugha v3 is complete**
- Closures: `fun(x) = x + k` and `fun(x: i64): i64 { … }` literals capture the variables they mention by copy when created, so later changes on either side never leak through (assigning a capture is E0511); parameter types come from the expected function type, the return type from the body, and `return` leaves the closure. Closures work in arrays, fields, return values and generic code — **milestone 15 complete**
- Functions are values: `fun(i64, i64): bool` types (`(fun(i64): i64)[]` for an array of them), named, associated and other modules' functions stored, passed and returned (`let f = double;`, `Point.at`, `m.f`; a generic one with an expected type), and any function value called — locals, elements, results (`make()(3)`) and fields (`b.on_click(2)`). Functions have no `==`, so `T: Eq` now restricts — **milestone 14 complete**
- Programs read their command line and interpolate strings: `args()` (with `lughac run prog.la -- 21`), `parse_i64`/`parse_f64` returning `Option`, and `"{name} is {age}"` in every string literal with `{{`/`}}` for literal braces (E0111). `lughac spec` now prints `Language v3 Specification.md` (in progress); v2's file is frozen. Test programs take arguments from a `<name>.args` file — **milestone 13 complete**
- Lugha v3 spec draft approved (`docs/specs/Language v3 Draft.md`): closures with copy capture and `fun(A): R` function types, functions as values, `args()`, `parse_i64`/`parse_f64`, string interpolation, and `map`/`filter`/`fold`/`sort_by`/`any`/`all` on arrays (milestones 13–16)
- Interfaces: `interface Shape { fun area(self): f64; }` used as a bound (`fun total<T: Shape>(xs: T[])`, `T: shapes.Shape + Eq`); any type with matching visible methods satisfies one, generic types included, and each instance calls the concrete method. New errors: a call no bound declares (E0421), a type that doesn't satisfy an interface (E0422, listing what's missing), two bounds declaring one item differently (E0423); an interface written as a type is E0305 and a struct as a bound E0315, each with help. `interface` is now a keyword — **milestone 12 complete: Lugha v2 is complete**
- Generic programs compile and run: each generic function and method is compiled once per set of type arguments (`lugha_fn_max<i64>`, `lugha_fn_Stack<string>.push`), generic structs and enums get one layout per instance, and instantiation nested more than 64 deep is E0420. The driver gate is gone, and the generics rules are in `Language v2 Specification.md` with the §10 generics program — **milestone 11 complete**
- Generics are type-checked: `lughac check` accepts type parameters with `Eq`/`Ord` bounds on functions, structs, enums and `impl` blocks, and infers type arguments from arguments and the expected type (`let s: Stack<string> = Stack.new();`). New errors: unknown bound (E0315), bad parameter name (E0316), wrong number of type arguments (E0418), a type argument missing its bound (E0419), an undecided type argument (E0412); `==` or `<` on a parameter without the bound is E0404 with a "declare `T: Eq`" hint. `build` and `run` still stop at generics until milestone 11's second half
- v2 draft amended (interfaces): a new milestone 12 adds structural interfaces as bounds (`fun total<T: Shape>(xs: T[])`), after generics (milestone 11); `==` stays deep and built-in, with no operator overloading; rationale in `docs/decisions/generics.md`
- Modules work end to end: `geometry.origin()`, `geometry.Vec2 { … }`, `geometry.Vec2.new()`, `shapes.Shape.Round(c)` across files, each module with its own names; `private` items, methods and fields stay in their module (E0313), `public` is reserved for packages (E0317), and private types can't leak into visible signatures (E0318); module items compile to `lugha_fn_<module/path>/<name>` — **milestone 10 complete**
- Modules front end: `lughac` loads the main file and every module it imports (cycles allowed), reporting a missing module (E0312), clashing imports (E0314) and importing the main file (E0319); diagnostics name the file they are in (human and JSON), and `--emit=tokens|ast` prints every module. `private` and `public` replace `pub` as keywords; programs that import or use them still stop at the milestone 10 gate
- v2 draft amended (visibility): everything is visible to the whole program by default, `private` limits an item, method or field to its module (struct literals need every field visible), `public` is reserved for v3 packages; rationale in `docs/decisions/visibility.md`
- Nested `match` patterns (`Some(Circle(r))`, `Pair(1, Some("a"))`) and `if` guards; E0417 now lists the missing cases as patterns (up to three, then "and N more"), W0102 also catches arms covered by several earlier arms, and new warning W0103 flags unused bindings (such as a misspelled variant) — **milestone 9 complete**
- `==` and `!=` on every type, compared deeply (structs, enums, `Option`, arrays; `NaN` stays unequal), and bytewise `<`/`<=`/`>`/`>=` on strings; either operand can type the other (`None == o`). `lughac spec` now prints `Language v2 Specification.md` (in progress: milestone 9); v1.2's file is frozen
- v2 syntax: `import`, `pub`, type parameters with `Eq`/`Ord` bounds, `Stack<T>` and `geometry.Vec2` types, `geometry.Vec2 { … }` literals, nested patterns and `if` guards all parse and show in `--emit=ast` (E0208 for a late `import`); checking stops them with "not implemented yet (milestone N)", exit 2, until their milestone. `import` and `pub` are now keywords
- Lugha v2 spec draft approved (`docs/specs/Language v2 Draft.md`): nested patterns and guards, deep `==` and string `<` (milestone 9), modules with `import`/`pub` (milestone 10), generics with `Eq`/`Ord` bounds and inference (milestone 11)
- **Lugha v1.2:** E0510 rejects a statement that changes what contains the place it writes (`xs[0] = { xs.push(2); 9 };`), which used to lose the write silently
- **Lugha v1.1:** the `swap(a, b)` intrinsic exchanges two places of the same type (variables, fields, elements) without copying, so double buffering is free (the DECISION-010 merge sort: 95 copies → 5, 154 → 109 ms); E0509 rejects swapping a value with a part of itself
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
