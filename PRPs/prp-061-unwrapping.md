## FEATURE: Unwrapping — `unwrap()` and `expect(msg)` on `Option` and `Result`, and `%=`; milestone 32.

**Status:** approved 2026-10-14 — session 78. This is part of the user's "in one go" (milestones 31–33), merged once CI is green.
**Milestone:** 32 (one PRP; tag `m32` at merge)
**Spec:** `Language v9 Draft.md`:
- decision 4, and the choice "`unwrap` and `expect` are built in";
- §2 (`%=`);
- §3 (`assign_op`);
- §4 "`unwrap` and `expect`";
- §5 (`%=`);
- §9;
- §10 "Unwrapping";
- §11 (milestone 32).

**Decisions:** MEMORY 72–73 (v9), MEMORY 50 (`Result` in the prelude).

## OBJECTIVE
The §10 unwrapping program prints `9` and `5`, then panics with `not a number` (exit 101):

```
let n = parse_i64("42").unwrap();
let m = parse_i64("7").expect("need a number");
x %= 10;
println(parse_i64("x").expect("not a number"));   // panic: not a number at <file>:<line>:<col>
```

## CONTEXT

- **Starting state:**
  - Built-in methods are `push`/`pop`/`clear`, `slice`, and the v7 integer methods (`check/builtins.rs`).
  - `unwrap` and `expect` are E0415.
  - `%=` is a lexing error.
- **Ending state:** both methods are built in, and `%=` exists.
- **Related existing code:**
  - `src/check/builtins.rs`, `src/check/assign.rs`;
  - `src/codegen/methods.rs`, `codegen/runtime.rs` (`call_panic`), `codegen/stmt.rs` (`compound_op`), `codegen/enums.rs` (`tag`, `variant`, `load_payload`);
  - `src/lexer/punct.rs`, `src/ast/mod.rs` (`AssignOp`), `src/parser/operators.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **Checker:** `builtin` gains, with a `self` receiver:
  - `unwrap()` on `Option<T>`, and on the prelude's `Result<T, E>`, giving `T`;
  - `expect(msg: string)` on the same, giving `T`.
- **Codegen** (new `codegen/unwrap.rs`, called from `methods.rs`):
  - store the value, compare its tag with `Some` or `Ok`, and on a mismatch call `lugha_rt_panic` at the method name's offset;
  - the message is a string literal (`unwrap on None` or `unwrap on Err`), or `expect`'s argument;
  - otherwise load the payload.
- **`%=`:**
  - the lexer's `PercentEq`;
  - `AssignOp::Rem`;
  - the parser's operator table;
  - the checker: a numeric target, like `/=`;
  - codegen: `compound_op` maps it to `BinOp::Rem`, which checks the divisor.
- **Programs without these** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Checker:** the `builtins.rs` entries, plus `%=` in `assign.rs`.
- **Codegen:** `codegen/unwrap.rs` and its hook; `call_panic` becomes `pub(super)`; `%=`.
- **Lexer, AST and parser:** `%=`.
- **Spec:**
  - §2;
  - §3;
  - §4 (`unwrap` and `expect`);
  - §5 (`%=`);
  - the panics list;
  - §9;
  - the §10 program;
  - §11.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 74, CONTEXT. The tag `m32` is added at the merge.

### Must NOT Do
- **No printing of `Err` values in the message:** that is a v9 §12 question.
- **No `unwrap_or_else`, and no `if let`:** `if let` is milestone 33.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0415:** `unwrap` and `expect` on other types.
- **E0403:** a non-`string` message (`expect(1)`).
- **E0405:** the wrong argument count.
- **E0404:** `%=` on a non-number.
- **At run time:** the panics `unwrap on None`, `unwrap on Err` and the given message, at the method name (exit 101). `%=` by zero panics with `division by zero`.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/builtins.rs`):
- [ ] `unwrap` and `expect` types on `Option<i64>` and `Result<string, i64>`.
- [ ] **Errors:**
  - E0415 on `i64` and `string`;
  - E0403 for `expect(1)`;
  - E0405 for `unwrap(1)`.
- [ ] `%=` on `i64` and `f64`; E0404 on a `string`.

Unit tests — codegen (`codegen/unwrap.rs`): `unwrap` compares the tag and calls `lugha_rt_panic` with `unwrap on None`.

Unit tests — lexer: `%=` lexes.

Acceptance — `tests/programs/m32/`:
- [ ] `unwrap.la`: the §10 program, exit 101 with its stderr.
- [ ] `more.la`:
  - `unwrap` on `Ok`;
  - `expect` on `Some`;
  - nested `Option<Option<i64>>`;
  - payloads that are structs and arrays;
  - `%=` on `i64`, `u8` and `f64`;
  - `%=` with a place (`xs[i] %= 3`).
- [ ] `err.la`: `unwrap` on `Err` panics with `unwrap on Err`.
- [ ] The §10 program runs from the spec (exit 101).

Regression:
- [ ] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-061-unwrapping`, merged into `main` after CI passes; tag `m32`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 program and the `m32/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [ ] The spec is at milestone 32; docs updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m32/more.la
```
