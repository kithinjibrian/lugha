## FEATURE: Move function bodies out of `check/stmt.rs` and array built-ins out of `check/method_calls.rs`, before either passes 300 lines.

**Status:** merged into `main` 2026-10-12 — session 35
**Milestone:** none; housekeeping (CLAUDE.md file size rule: `stmt.rs` is 294 lines, `method_calls.rs` 285)
**Spec:** none. No language or diagnostic change.
**Decisions:** MEMORY 27 (methods), MEMORY 28 (growable arrays).

## OBJECTIVE
Both files get room for the next feature that touches them, through pure moves: no code inside a moved function changes, and every test moves with the code it tests.

## CONTEXT

- **Starting state:**
  - `check/stmt.rs` (294 lines) holds:
    - function bodies (`function`, `body`);
    - blocks and statements;
    - `let`, `for … in`, `return`.
  - `check/method_calls.rs` (285 lines) holds:
    - method and associated calls;
    - receivers, E0415, E0508;
    - the array built-ins' signatures (`builtin`).
- **Ending state:**
  - New `check/body.rs`: `function` and `body`.
  - New `check/builtins.rs`: `builtin`.
  - The two old files are at about 235 and 225 lines.
- **Related existing code:** `src/check/{mod,stmt,method_calls,env,methods}.rs` (callers of `function`/`body`).
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 35)
- **Sizes:** `stmt.rs` 236, `body.rs` 72, `method_calls.rs` 230, `builtins.rs` 66.
- **Check:** a script confirmed the moved functions and tests appear verbatim in the new files, with `builtin` differing only by `pub(super)`. Two now-unused imports were dropped (`FunDecl`, and the `ok` test helper in `stmt.rs`).

### Discovery answers (session 35)
1. **Two new files:** `body.rs` and `builtins.rs`. Not moves into `flow.rs` and `array.rs`, which would push those to the limit.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **`check/body.rs`.**
  - `//!` doc: "Function and method bodies: parameters and `self` bound, the body checked against the return type (spec §6, v1 §4)."
  - It takes `Checker::function` and `Checker::body` verbatim, and the test `every_expression_of_a_valid_program_gets_a_type`.
  - `stmt.rs` keeps blocks, statements, `let`, `for … in` and `return`. Its `//!` drops "and function bodies".
- **`check/builtins.rs`.**
  - `//!` doc: "The array built-in methods `push`, `pop` and `clear` (v1 §4, milestone 8)."
  - It takes `builtin` (now `pub(super)`) verbatim, and the test `arrays_grow_through_push_pop_and_clear`.
  - `method_calls.rs` imports it.
- **Imports:** adjust only what the moves need.
- **CLAUDE.md:** the tree lists both files.
- **Records:** CHANGELOG ("Changed"), TODO, CONTEXT.

### Must NOT Do
- No edits inside moved functions; no renames beyond `builtin`'s visibility; no behaviour or diagnostic change.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- Unchanged; nothing new can fail.

## SECURITY CONSIDERATIONS

- None. This is an internal refactor with no new inputs or `unsafe`.

## TESTS TO WRITE

No new behaviour, so no new tests: the moved tests and the whole suite are the check.
- [x] The two moved tests pass in their new files.
- [x] The test count is unchanged (252), and every test passes.
- [x] The moved code is identical, checked with a diff of the removed and added function bodies.

## ROLLBACK PLAN

- Branch `prp-029-checker_split`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] `stmt.rs` and `method_calls.rs` are each at most about 245 lines; no file over 300.
- [x] CLAUDE.md tree, CHANGELOG.md, TODO.md updated.
- [x] No new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `wc -l src/check/*.rs`
