## FEATURE: Printing anything — interface and function values, `unwrap on Err: <error>`; milestone 40, v12 complete.

**Status:** approved 2026-10-14 — session 89. This is the last part of the user's "in one go" (milestones 39–40), merged once CI is green.
**Milestone:** 40 (one PRP; completes v12; tags `m40` and `v12` at merge)
**Spec:** `Language v12 Draft.md`:
- decision 4, and the choices on interface values and `unwrap`;
- §5 "How values print";
- §9;
- §10 "Printing anything";
- §11 (milestone 40);
- §12.

**Decisions:** MEMORY 83 (v12), MEMORY 84 (milestone 39), MEMORY 74 (`unwrap`).

## OBJECTIVE
The §10 printing-anything program prints `Circle { r: 2.0 }`, `square 3` and `<fun>`, then panics with `unwrap on Err: Bad { code: 7 }` (exit 101).

## CONTEXT

- **Starting state (milestone 39):**
  - codegen prints every type, including interface values (by identity, `codegen/show_iface.rs`) and function values (`<fun>`);
  - the checker still rejects printing interface and function values directly (E0403);
  - `unwrap` on `Err` panics with the fixed `unwrap on Err` (`codegen/unwrap.rs`).
- **Ending state:** both print directly; `unwrap` shows the error; v12 is complete.
- **Related existing code:**
  - `src/check/{printing,method_calls}.rs`, `src/codegen/unwrap.rs`;
  - `tests/programs/m32/err.*`, `tests/programs/m39/`.
- **Open decisions that must be resolved first:** none.

### Design
- **`printing::printable`** is every type but `void`.
- **`unwrap()` on a `Result<T, E>`:**
  - **Checker:** it records `printed(call, E)` (in `on_value`, beside the built-ins), so `E`'s `show` instances are listed.
  - **Codegen:** in the failing branch, it loads the `Err` payload and panics with `"unwrap on Err: "` joined with that payload's `text`. `expect(msg)` and `unwrap` on `Option` are unchanged.
- **IR:** programs that print only numbers, `bool` and strings, and never call `unwrap()` on a `Result`, give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `printable`, the `unwrap` use, and the `unwrap` message.
- **Spec:**
  - the changes summary;
  - §5 (interface and function rows, `unwrap`);
  - §6 intrinsics table;
  - interpolation;
  - §4 interface and function values print;
  - the E0403 row;
  - §9;
  - the panics list;
  - the §10 program;
  - §11 row 40 done;
  - the title and status say v12 is complete;
  - §12 (printing errors decided).
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 85, CONTEXT. The tags `m40` and `v12` are added at the merge.

### Must NOT Do
- **No format specifiers, debug form or parsing.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0407:** printing `void` (unchanged).
- **At run time:** `unwrap on Err: <error>` (exit 101).

## SECURITY CONSIDERATIONS
- No new input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/printing.rs`):
- [ ] An interface value and a function value print and interpolate.
- [ ] `unwrap` on `Result<i64, Stack<i64>>` lists `Stack.show<i64>`.

Unit tests — codegen (`codegen/unwrap.rs`): `unwrap` on a `Result` builds its message with the error's text.

Acceptance — `tests/programs/m40/`:
- [ ] `anything.la`: the §10 program (exit 101, stderr golden).
- [ ] `more.la`:
  - an interface array printed directly and interpolated;
  - a generic interface value;
  - a struct holding an interface value and a function value;
  - `to_string` of an interface value.
- [ ] `m32/err` golden updated to the new message.
- [ ] The §10 program runs from the spec.

Regression:
- [ ] Every earlier test passes, and earlier programs that don't `unwrap` a `Result` give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-069-printing_anything`, merged into `main` after CI passes; tags `m40` and `v12`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 program and the `m40/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [ ] The v12 spec is complete; docs updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m40/anything.la
```
