## FEATURE: Nested `mut self` calls — an argument of a `mut self` call may call methods on the receiver (`g.tanh(g.add(a, b))`); milestone 57.

**Status:** approved 2026-10-15 — session 122 ("approved. build in one go"); proposed in session 120.
**Milestone:** 57 (one PRP; tag `m57` at merge)
**Spec:** `Language v15 Draft.md` (decisions 29–32, the milestone 57 choices); spec §4 (`mut self` methods, "No aliasing"), §5 (method-call order, "Changing what a statement writes"), §9 (E0508, E0510 rows), §10 (a program), §11 (row 57).

**Decisions:**
- narrows E0508 (v1, MEMORY on `mut self` receivers) and E0510 (PRP-027) for `mut self` receivers;
- builds on M55's two-phase places (MEMORY 107);
- no open entries in DECISIONS.md apply.

## OBJECTIVE
Tape-style model code reads like PyTorch: `g.matmul(g.tanh(g.add(a, b)), w)` and `self.g.matmul(self.norm(x, gain, bias), w)` compile and run with the same results as the one-`let`-per-op form. Real aliasing (`v.add(v)`) is still rejected.

## CONTEXT

- **Starting state:**
  - **Checker:** `check/method_calls.rs::no_alias` reports E0508 for any argument mentioning the receiver's root. `check/conflicts.rs::no_change_inside` reports E0510 for any change, made by the arguments, to a strict container of the receiver.
  - **Codegen:** `codegen/methods.rs::receiver` takes the receiver's address (`place_address`) before the arguments are evaluated.
- **Ending state:** decisions 29–31.
- **Related existing code:**
  - **Checker (`src/check/`):** `method_calls.rs` (E0508), `conflicts.rs` (E0510), `iface_calls.rs` / `satisfy.rs` (`mut self` calls through interfaces and bounds, which use the same rules), `liveness/` (mentions).
  - **Codegen (`src/codegen/`):** `methods.rs` (receivers), `dispatch.rs` (interface `mut self` calls, `place_address`), `defaults.rs` (default methods), `places.rs` (`place_plan`, `place_commit`).
- **Open decisions that must be resolved first:** none.

### Design
1. **E0508 (decision 29):** for each argument, find the expressions it can evaluate to: itself, or through `if`/`else`, block tails and `match` arms, their tails. For each one that is a place chain rooted at the receiver's root, whose type is passed by pointer (`Struct`, `Enum`, `Option`, `Result`, `Array`, tuple, `Interface`), report E0508 at that mention. Mentions anywhere else (inside calls, operators, struct or array literals, interpolation, closures) are allowed.
2. **E0510 (decision 31):** in `no_change_inside`, for a `PlaceUse::Receiver` target only, a change that strictly contains the receiver is reported only if the receiver's path has an `Index` step deeper than the change's depth. Assignment and `swap` targets are unchanged.
3. **Codegen order (decision 30):** `receiver` for `mut self` becomes `place_plan(base)` before the arguments, then `place_commit(base, &plan)` after them, then the call. The same applies to dynamic `mut self` calls (`dispatch.rs`) and default methods. Array built-ins (`push`/`pop`/`clear`) already do this (M55).
4. **Liveness (move on last use):** an argument that mentions the receiver must not let the receiver's root move away before the outer call. A mention that is a fresh read can't move it, since the root is used again as the receiver. Verify with a test.

### Risk
- **Use-after-move or a stale address** if the argument order or the plan/commit split is wrong somewhere: dynamic calls, default methods, receivers through bounds. Every `mut self` call path gets a test that checks its output against the one-`let`-per-op form.
- **A missed alias** if the "evaluates to" walk misses a form (a `match` arm, a block tail, `if let`). Each form gets an E0508 test.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec v15:**
  - §4 "No aliasing" rewritten (decision 29, with examples);
  - §5 method-call order (decision 30) and "Changing what a statement writes" (decision 31);
  - §9 E0508/E0510 examples;
  - §10 program "Nested calls";
  - §11 row 57;
  - the changes summary.
- **Docs:** CLAUDE.md (STACK line), README, TODO (mark the item done), CHANGELOG, MEMORY decisions 110 (plan) and 111 (built), CONTEXT. Tag `m57` at the merge.
- **firefly:** rewrite `layer` (and `forward` where it helps) in `example/gpt.la` in the nested style. The losses must match the current ones: `./gpt` (small data) prints the same output. Commit, don't push.

### Must NOT Do
- **No change to what valid programs print.** Every currently valid program stays valid with the same output, since the order only changes when arguments mention the receiver, which used to be E0508.
- **Assignment and `swap` E0510 rules stay unchanged.**
- **No new syntax, no new error codes.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0508** keeps its message and code. It now points at the aliasing mention inside the argument (the `v` in `if c { v } else { w }`).
- **E0510** keeps its message and code.

## SECURITY CONSIDERATIONS
- **No `unsafe`, no dependencies, no external input.** A wrong relaxation would break value semantics (an alias inside a call), not memory safety; the tests cover each still-rejected form.

## TESTS TO WRITE

Checker unit tests (`method_calls.rs`, `conflicts.rs`):
- [ ] **Allowed:**
  - `g.tanh(g.add(a, b))`;
  - `g.f(g.h(x), g.k(y))`;
  - `g.scale(x, g.lr)` (f64);
  - `g.label(g.name)` (string);
  - `v.add(v.scaled(2.0))`;
  - `v.add(V { x: v.x, y: 0.0 })`;
  - a closure capturing the receiver.
- [ ] **Still E0508:**
  - `v.add(v)`;
  - `v.add(v.inner)`;
  - `g.set(g.values[0])` (array);
  - `v.add(if c { v } else { w })`;
  - `v.add({ v })`;
  - `v.add(match k { 0 => v, _ => w })`;
  - a tuple or `Option` field of the receiver.
- [ ] **E0510 now allowed:** `self.g.f(self.norm(x))`, `a.b.f(a.reset())`.
- [ ] **E0510 still reported:** `gs[i].f(gs.clear())`, `self.layers[i].f(self.reset())`, `gs[{ gs.clear(); 0 }].f()`; assignment's and `swap`'s cases unchanged.

Codegen unit test:
- [ ] A `mut self` call evaluates its arguments before reaching the receiver: in the IR, the argument's inner call comes before the receiver's `own` check.

Acceptance (`tests/programs/m57/`):
- [ ] **`nested.la`:** a small tape (struct with `values: f64[]`, `push`-style ops returning ids). Nested calls on a local, on `self.g` inside a `mut self` method, through an interface value, through a default method, and on `gs[i]` with inner calls on `gs[i]`. Prints the same as `flat.la`, the same computations one `let` per op. Both goldens come from the flat form under the M56 compiler.
- [ ] **`e0508.la`, `e0510.la`:** rejected programs (`.stderr`).
- [ ] The §10 program.

Regression:
- [ ] Every earlier program passes unchanged; `v1_2/e0510*` keep their output.

## ROLLBACK PLAN
- Branch `prp-086-nested_mut_self`, merged into `main` after CI passes; tag `m57`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`, and the `m57/` programs at `-O2`.
- [ ] firefly's nested `gpt.la` prints the same output as before on `./gpt` (small data).
- [ ] The spec is at milestone 57; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m57/nested.la
```
