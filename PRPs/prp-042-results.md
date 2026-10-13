## FEATURE: Results and `?` — the prelude's `Result<T, E>` and early returns; the v4 spec begins.

**Status:** approved 2026-10-13 — session 53
**Milestone:** 17 (one PRP; first v4 milestone)
**Spec:** `Language v4 Draft.md`:
- decisions 2–3;
- §2 (`?` token);
- §3 (`postfix … "?"`);
- §4 "`Result<T, E>`" and "The `?` operator";
- §9 (`?` lowering, E0424);
- §10 "Results and `?`";
- §11 (milestone 17);
- §12 (error conversion).

**Decisions:** MEMORY 49 (v4 direction), MEMORY 48 (the prelude), MEMORY 45 (a new spec file starts at a version's first milestone).

## OBJECTIVE
Programs return errors as values and pass them up with `?`. The §10 program prints `3` and `3 is odd`.
- **`Result<T, E>`:**
  - `Ok(x)` and `Err(e)` are written without a prefix;
  - their type arguments come from the expected type;
  - `match` and exhaustiveness work as for any enum.
- **`e?`** on a `Result` gives the `Ok` value or returns the `Err` at once. On an `Option` it gives the `Some` value or returns `None`. In a closure it returns from the closure. Misuse is E0424.
- **`Language v4 Specification.md`** starts here and becomes what `lughac spec` prints; v3's file is frozen.

## CONTEXT

- **Starting state:**
  - v3 is complete.
  - The prelude holds the array helpers.
  - `Option` is built into the compiler.
  - `?` is an unexpected character (E0101).
- **Ending state:**
  - The prelude also declares `enum Result<T, E> { Ok(T), Err(E) }`. The compiler makes it nameable from every module and resolves bare `Ok`/`Err`.
  - `?` is a postfix operator lowered to a branch and an early return.
- **Related existing code:**
  - `src/prelude.la`;
  - `src/lexer/punct.rs`, `src/parser/expr.rs` (postfix), `src/ast/expr.rs`, `sexp/`;
  - `src/check/` (`construct.rs`, `enums.rs`, `modules/lookup.rs`, `generics.rs`, `env.rs`, `types.rs`, `closures.rs`, `liveness/walk.rs`, `stmt.rs`);
  - `src/codegen/` (`expr.rs`, `construct.rs`, `enums.rs`, `matching.rs`);
  - `src/lib.rs`, `tests/cli.rs`, `tests/spec.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 53)
1. **`Result` is a prelude enum:** `enum Result<T, E> { Ok(T), Err(E) }` in `src/prelude.la`, an ordinary generic enum. The compiler adds only three things:
   - the name `Result` resolves to it from every module;
   - bare `Ok(x)`/`Err(e)` construct it;
   - it prints as `Result<…>`.

   Patterns, exhaustiveness, `==`, layout, copies and instances are the existing generic-enum machinery.

### Design choices for review
- **Names:**
  - `Result` resolves to `$prelude.Result` when no type of that name is in scope.
  - `Result`, `Ok` and `Err` join the built-in names, so a program declaring one is E0302 (the prelude itself is exempt) and a local named `Result` is E0311.
  - A local named `Ok` or `Err`, like one named `Some`, shadows the constructor.
- **Construction:**
  - `Ok(x)`/`Err(e)` build the prelude's `Result` through the same path as `E.V(x)`, so the missing type argument comes from the expected type.
  - `Ok(1)` with no expected type is E0412, as for `Tree.Leaf`.
  - `Err` without arguments is E0416.
- **Printing:** a prelude type prints without its module, so messages show `Result<i64, string>`, never `$prelude.Result<…>`. Its LLVM type and helper names use the same printed form.
- **`?` rules:**
  - on `Result<T, E>` in a function returning `Result<U, E>` (with `E` equal): type `T`;
  - on `Option<T>` in a function returning `Option<U>`: type `T`;
  - **E0424** "`?` can't return here" otherwise, with help:
    - in `main`: "handle it with `match`";
    - an `Option` in a `Result` function: "convert it with `.ok_or(e)?` (milestone 18)", or simply `match` until then;
    - different error types: "the error types must match: `E1` vs `E2`";
    - not a `Result` or `Option`: "`?` works on `Result` and `Option`";
  - in a closure whose return type isn't known yet: E0412, as for `return`.
- **Lowering `?`:**
  - store the operand in a slot and branch on its tag;
  - the failing branch builds `Err(e)` of the function's own return type from the operand's payload (or `None`) and returns it;
  - the passing branch loads the `Ok`/`Some` payload;
  - the operand is evaluated once.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Syntax**
- **Lexer:** `?` is the token `Question`.
- **Parser:** `postfix` accepts `?` like a call, index or member, giving `ExprKind::Try(Box<Expr>)`, printed `(? e)`.

**Prelude**
- `enum Result<T, E> { Ok(T), Err(E) }` is added, with a comment.

**Checker**
- **Names:**
  - `Result` resolves to the prelude's (type positions, plain and applied);
  - `BUILTIN_NAMES` gains `Result`, `Ok` and `Err` (not enforced in the prelude);
  - `is_type_name` covers `Result`.
- **Construction:** bare `Ok`/`Err` construct the prelude's `Result`.
- **Display:** a `$prelude.` key prints without that prefix.
- **`?`:** `ExprKind::Try` follows the rules above (E0424, E0412 in closures). It uses the current function's or closure's return type, so a closure's `?` returns from the closure.
- **Liveness:** the walk enters `?` operands.

**Codegen**
- `ExprKind::Try`, lowered as above, inside functions, methods, instances and closures.

**Spec** — `Language v4 Specification.md`, created from the v3 spec
- **Header:** title "(in progress: milestone 17)", a status paragraph, a "Changes since v3" list, and the compatibility sentence (`Result`, `Ok`, `Err`).
- **Folded in:**
  - §2: `?`;
  - §3: `postfix`;
  - §4: `Result` and `?`;
  - §9: lowering, and E0424 in the code table;
  - §10: the results program;
  - §11: a v4 milestone table starting at 17.
- **The draft's §12** error-conversion question is resolved (the same `E`, as drafted).
- **`src/lib.rs` `SPEC`** points at the new file. v3's status line says it is frozen. `tests/cli.rs` checks the new title.

**Docs**
- CLAUDE.md: STACK (the v4 spec is in progress, v3 frozen), the tree, and anti-pattern 5.
- README, TODO, CHANGELOG, MEMORY decision 50, CONTEXT.

### Must NOT Do
- **No helpers** (`ok_or`, `unwrap_or`, …; milestone 18), and no I/O (milestone 19).
- **No error conversion in `?`,** no `?` in `main`, and no `unwrap()`.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Records:** E0424 and the reused codes (E0302, E0311, E0412, E0416, E0417) are `Diagnostic` records; checking continues.
- **Codegen invariants:** a `?` whose function return type isn't a matching `Result`/`Option` is a compiler bug (an `expect` stating the invariant, exit 2).

## SECURITY CONSIDERATIONS
- No `unsafe`, no new inputs or dependencies.

## TESTS TO WRITE

Unit tests — lexer and parser:
- [ ] `?` lexes as `Question`.
- [ ] `f(x)?`, `a?.b` and `xs[0]?` parse as postfix: `(? (call f x))`, `(. (? a) b)`.
- [ ] `-x?` is `(- (? x))`.

Unit tests — checker (a new `check/results.rs`):
- [ ] **Construction:**
  - `let r: Result<i64, string> = Ok(1);` and `Err("x")` work;
  - `Ok(1)` alone is E0412;
  - `Err()` is E0416.
- [ ] **Patterns:** `match r { Ok(n) => n, Err(_) => 0 }` checks, and a missing `Err` arm is E0417.
- [ ] **Names:**
  - `struct Result {}` and `fun Ok() {}` are E0302;
  - `let Result = 1;` is E0311;
  - `Result<i64>` is E0418;
  - a message prints `Result<i64, string>`.
- [ ] **`?` types:** `?` on `Result` in a `Result` function gives `T`, and on `Option` in an `Option` function gives `T`.
- [ ] **E0424 for:**
  - `?` on an `i64`;
  - `?` on an `Option` in a `Result` function;
  - different error types;
  - `?` in `main`;
  - `?` in a `void` function.
- [ ] **`?` in closures:** with a written or expected return type it works; with an inferred one it is E0412.
- [ ] **Equality:** `Result<i64, string>` values compare with `==`.

Acceptance — `tests/programs/m17/` (run at `-O0` and `-O2`):
- [ ] `results.la`: `Ok`/`Err` construction, `match`, `==`, results in arrays and structs, generic functions returning `Result<T, string>`.
- [ ] `try.la`:
  - `?` on `Result` and `Option`, several in a row;
  - early return skips the rest (a `println` after `?` doesn't run on `Err`);
  - `?` in a method, in a generic function and in a closure;
  - `?` on a call result (`half(n)?`).
- [ ] `e0424.la`: reject golden.
- [ ] The §10 results program runs from the spec.

Regression:
- [ ] Every earlier test passes. Programs that use neither `Result` nor `?` give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-042-results`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 results program and the `m17/` programs pass.
- [ ] E0424 and the reused codes are reported as specified.
- [ ] `lughac spec` prints `Language v4 Specification.md` (in progress: milestone 17); v3 is frozen.
- [ ] CLAUDE.md, README, TODO, CHANGELOG and MEMORY are updated; tag `m17`.
- [ ] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m17/try.la
```
