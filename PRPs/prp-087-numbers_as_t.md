## FEATURE: Numbers as `T` — `as` casts with numeric type parameters, and any literal as a `T: Num`/`T: Float`; milestone 58.

**Status:** approved 2026-10-15 — session 122 ("approved. build in one go"); proposed in session 121.
**Milestone:** 58 (one PRP; tag `m58` at merge)
**Spec:** `Language v15 Draft.md` (decisions 33–36); spec §4 (casts, inference rules 3–5, operator bounds), §9 (E0401, E0402, E0408 rows), §10 (a program), §11 (row 58).

**Decisions:**
- extends M52's "`0`/`1` as `T: Num`" (MEMORY 100–101) and M53's `Float` bound (MEMORY 102–103);
- no open entries in DECISIONS.md apply.

## OBJECTIVE
Generic numeric code writes numbers directly:
- `s / xs.len as T` and `out.push(i as T)` convert runtime values;
- `x * 2` and `x * 0.5` use literals as `T`;
- `x as f64` converts back.

Every instance computes what the same code at its concrete type would.

## CONTEXT

- **Starting state:**
  - **Casts:** `check/ops.rs::cast` accepts numeric ↔ numeric and `char` ↔ integer; a type parameter is E0408.
  - **Literals:** `check/literal.rs` lets `0` and `1` take a `T: Num`; codegen makes them each instance's type (`codegen/expr.rs`).
  - **Bounds:** `Num`/`Float` are prelude interfaces satisfied only by the numbers (`check/bounds.rs`).
- **Ending state:** decisions 33–35.
- **Related existing code:**
  - **Checker (`src/check/`):** `ops.rs` (casts), `literal.rs` (literal types, range checks, E0401/E0402), `bounds.rs`, `operators.rs`, `instances.rs` (`Checked.instances` / `monos`, the instance list), `generics.rs` (substitution), `errors/`.
  - **Codegen (`src/codegen/`):** `cast.rs` (§4 casts at concrete types), `expr.rs` (literals at the substituted type), `instances.rs` (`concrete`).
- **Open decisions that must be resolved first:** none.

### Design
1. **Casts (decision 33):** in `cast`, a type parameter whose bounds include `Num` or `Float` counts as numeric on either side. Codegen already casts at `self.concrete(ty)`, so each instance lowers an ordinary numeric cast (including `T → T`, a no-op).
2. **Literals (decision 34):** extend the `0`/`1` rule to every integer literal for `T: Num`/`Float`, and to float literals for `T: Float`. A float literal for a `T` that is only `Num` is E0401 ("a float literal can't be an integer `T`"). Record each literal typed as a type parameter: its span, value, sign, and the generic function or method it is in.
3. **Per-instance range (decision 35):** after the instance list is built, check each recorded literal against each instance's concrete type with the existing range check. Out of range is E0402 at the literal, with a note naming the instance as it prints (`in big<u8>`). It is reported once per (literal, type).
4. **Unchanged:** literals with a concrete expected type, and casts between concrete types.

### Risk
- **A literal lowered at the wrong width:** codegen must take the literal's type from the substituted side table, as it does for `0`/`1`. Tests run the same generic code at `i32`, `u8`, `i64`, `f32` and `f64`.
- **Missed instances** (a generic method of a generic struct, default methods, closures inside generics): each gets an out-of-range test.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec v15:**
  - §4: casts with numeric type parameters, inference rule 3 (literals as `T`), the operator-bounds paragraph;
  - §9: E0401/E0402/E0408 examples;
  - §10: program "Numbers as T";
  - §11: row 58;
  - the changes summary.
- **Docs:** CLAUDE.md (STACK), README, TODO (mark "Any number as a `T: Num`" done), CHANGELOG, MEMORY 112 (plan) and 113 (built), CONTEXT; tag `m58`.
- **Libraries:**
  - glowworm: `arange`, `linspace` and `mean` in `src/array.la` use `i as T` / `n as T`;
  - firefly: `mean` in `src/graph.la` uses the same;
  - both repos' examples print the same as before; commit each, don't push.

### Must NOT Do
- **No implicit conversions:** a literal still never turns from integer into float; `T` values never convert without `as`.
- **No casts for unbounded `T`** (E0408 as today), and none to or from non-numeric types.
- **No change to concrete code's typing or output.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0402:** per instance, with the instance in a note.
- **E0401:** a float literal as an integer-only `T`.
- **E0408:** a cast with an unbounded `T`, with help to add `: Num`.

## SECURITY CONSIDERATIONS
- **No `unsafe`, no dependencies, no external input.**

## TESTS TO WRITE

Checker unit tests:
- [ ] **Allowed casts:** `n as T` (`T: Num`), `x as f64` (`x: T`, `T: Num`), `x as T` (`x: U`, both `Num`), `x as T` with `T: Float`.
- [ ] **E0408:** `n as T` with unbounded `T`; `T: Eq` only; `b as T` where `b: bool`.
- [ ] **Literals:** `x * 2` and `x - 10` (`T: Num`); `x * 0.5` (`T: Float`); `x * 0.5` with `T: Num` only is E0401.
- [ ] **E0402 per instance:**
  - `x + 300` at `u8`;
  - `-1` at `u32`;
  - a generic method of a generic struct;
  - a closure inside a generic function;
  - one report per (literal, type).

Acceptance (`tests/programs/m58/`):
- [ ] **`numbers.la`:** generic `mean`, `arange`, `scale`, `half` at `i32`, `u8`, `i64`, `f32`, `f64`. The golden is computed from the same functions written concretely per type (built with the M57 compiler).
- [ ] **`e0402_instance.la`:** a rejected program (`.stderr`).
- [ ] The §10 program.

Regression:
- [ ] Every earlier program passes unchanged.

## ROLLBACK PLAN
- Branch `prp-087-numbers_as_t`, merged into `main` after CI passes; tag `m58`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`, and the `m58/` programs at `-O2`.
- [ ] glowworm's and firefly's examples print the same after the cleanup.
- [ ] The spec is at milestone 58; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m58/numbers.la
```
