## FEATURE: Lazy std modules — an imported std module's functions and methods are compiled only when the program reaches them, as the prelude's are; milestone 60.

**Status:** approved 2026-10-15 — session 126 ("yes build"); proposed in session 125.
**Milestone:** 60 (one PRP; tag `m60` at merge)
**Spec:** `Language v15 Draft.md` (decisions 41–43); spec §6 (std modules), §9 (lowering), §11 (row 60).

**Decisions:**
- extends PRP-043's compile-when-reached prelude (`$prelude.` keys) and the v10 prelude-methods rule to std;
- M53's std design (MEMORY 102–103) said std costs nothing unless imported; this makes it cost nothing unless reached;
- no open entries in DECISIONS.md apply.

## OBJECTIVE
A program that imports `std.random` and uses `seeded` and `below` compiles `seeded`, `below` and `next_u64`, and nothing else from `std.random` or `std.math`. Every program's behaviour, output and diagnostics stay the same.

## CONTEXT

- **Starting state:**
  - **Prelude:**
    - `check/instances.rs` lists prelude bodies as instances even when not generic (`used` keeps `$prelude.` keys; `bound_target` lists prelude methods);
    - codegen skips declaring and defining unlisted prelude functions and methods (`codegen/declare.rs::is_prelude`, `methods.rs`, `lower.rs`) and resolves prelude calls through `monos` (`prelude_target`).
  - **Std:** compiled eagerly, like any module.
- **Ending state:** decisions 41–42.
- **Related existing code:**
  - **Checker (`src/check/`):** `instances.rs` (`used`, `monomorphize` roots, `bound_target`), `fun_values.rs` (functions used as values), `iface_tables.rs` / `iface_values.rs` (interface tables), `defaults.rs`.
  - **Codegen (`src/codegen/`):** `declare.rs`, `lower.rs`, `methods.rs`, `function.rs` (`prelude_target`), `fun_values.rs` (`lugha_ref_` wrappers), `interfaces.rs` (tables).
  - **Modules:** `modules/` (`STD`, std unit paths).
- **Open decisions that must be resolved first:** none.

### Design
1. **One predicate,** "lazy key" / "lazy unit": a key starting `$prelude.` or `std.`, a unit whose path starts with `std`. It replaces the prelude-only tests in `instances.rs` and codegen (`is_prelude` → `is_lazy`, where the meaning is "compiled when reached").
2. **Reached** (decision 41):
   - **calls**, direct and through bounds, already recorded by `used` and `bound_used`; with the predicate, std keys are kept like prelude keys;
   - **function values** (`let f = math.pi;`): the checker records a use of the function, so it is listed;
   - **interface tables**: a std type converted to an interface value lists the methods its table holds;
   - **default methods and generic instances**: unchanged, already listed.
3. **Roots:** std bodies are never roots of `monomorphize`, as prelude bodies aren't.
4. **Checking:** unchanged. Every std body is checked on import.

### Risk
A reached std function that isn't listed fails at codegen with an internal error (exit 2), never a wrong program. Mitigations:
- one test per way of reaching (call, method, bound, interface value, function value, a std function calling another, `std.random` → `std.math`);
- every program that imports std is in the existing suite (`m53`, `m54`, the §10 programs, glowworm-style examples).

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec v15:** §6 (std modules compiled when reached), §9 (lowering note), §11 (row 60), the changes summary.
- **Docs:** CLAUDE.md (STACK), README, TODO (mark "Lazy std modules" done), CHANGELOG (with the IR sizes), MEMORY 116 (plan) and 117 (built), CONTEXT; tag `m60`.

### Must NOT Do
- **Program modules and packages stay eager** (decision 42).
- **No change to checking or diagnostics,** and no change to any program's output.
- **No language change.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **No new diagnostics.** A missing listing is a compiler bug: exit 2, as any internal error.

## SECURITY CONSIDERATIONS
- **No `unsafe`, no dependencies, no external input.**
- **Fewer emitted calls:** unused std functions that call the runtime (`from_os` → `lugha_rt_os_seed`) are no longer emitted.

## TESTS TO WRITE

IR integration tests (`tests/lazy_std.rs`, through `lughac build --emit=ir`):
- [ ] `seeded` and `below` only: `seeded`, `Rng.below` and `Rng.next_u64` are defined; `from_os`, `Rng.uniform`, `Rng.between`, `Rng.chance`, `math/pi` and `math/e` are not.
- [ ] `math.sqrt` only: no other `std/math` function defined.
- [ ] Reached through a function value (`let f = math.pi;`), through a generic function bounded by an interface `Rng` satisfies, through an interface value holding an `Rng`, and via a std function calling another: each is defined.

Regression:
- [ ] Every earlier program passes unchanged (`m53`, `m54` and the §10 std programs included).

## ROLLBACK PLAN
- Branch `prp-089-lazy_std`, merged into `main` after CI passes; tag `m60`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`; the std programs at `-O2` too.
- [ ] IR sizes recorded.
- [ ] The spec is at milestone 60; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo test --test lazy_std
```
