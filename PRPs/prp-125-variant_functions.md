## FEATURE: Variants as function values — a variant with payloads used without a call is a function of its payloads (`Msg.Named`, `Shape.Rect`, `Some`, `Ok`, `Err`); milestone 94 (v17).

**Status:** approved 2026-10-17 ("approved", session 216); proposed in session 215, with PRP-124.
**Milestone:** 94 (tag `m94` at merge).
**Spec:** `Language v17 Draft.md` decisions 30–34; v17 spec §4 ("Functions as values"), §9 (symbols), §11 (row 94).

**Why:** the second prerequisite of kivuli (KICKOFF decision 11): `input(m.name, Msg.Named)` instead of `fun(s) = Msg.Named(s)`, Elm's most common idiom, and `xs.map(Some)`.

**Decisions (discovery, session 215; the user's choice: every payload variant):**
- A variant with payloads, written without a call, is a value of type `fun(P1, …): E`: `Msg.Named` is `fun(string): Msg`, `Shape.Rect` is `fun(f64, f64): Shape`; the predeclared `Some`, and the prelude's `Ok` and `Err`, too.
- A generic enum's variant needs an expected function type to fix its type arguments, as a generic function does (E0412 otherwise): `let s: fun(i64): Option<i64> = Some;`, `xs.map(Some)` where the element type is known.
- A variant without payloads stays a value of its enum (`Msg.Add`), as today and as in Elm.
- Decided by me, for review: through modules and re-exports as variants already resolve (`m.E.V`, `m.v.E.V`, named imports of the enum); visibility as for the enum; the value's code is a wrapper `lugha_ref_<Enum>.<Variant>` (`lugha_ref_Shape.Circle`, `lugha_ref_Option<i64>.Some`), emitted once, building the variant with the indirect-slot rules — it can't collide with a method's wrapper, since a method may not share a variant's name (E0310).
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
enum Msg { Add, Named(string), Moved(f64, f64) }

let f: fun(string): Msg = Msg.Named;
let shapes = [1.0, 2.0].iter().map(Shape.Circle).collect();
let maybes: Option<i64>[] = [1, 2].iter().map(Some).collect();
input(m.name, Msg.Named);          // kivuli
let a = Msg.Add;                   // a Msg, unchanged
let s = Some;                      // E0412: needs an expected type
```

## CONTEXT
- **Starting state:** today `Msg.Named` without arguments is E0416 ("1 payload, but 0 were given") and `Some` alone E0301; spec v16/v17 §4 "Functions as values" lists named and associated functions as values, generic ones with an expected type, and what isn't (methods, externs, intrinsics: E0406). Variants are resolved in `check/construct.rs` (`E.V`, `Some`, `None`) and `check/results.rs` (`Ok`/`Err`); function values in `check/fun_values.rs` and `codegen/fun_values.rs` (`{ wrapper, null }`, `lugha_ref_` wrappers built by `wrapper`); generic instances through `Checked::instances`.
- **Ending state:**
  - **Checker:** in value position (not called), `E.V`, `m.E.V`, `Some`, `Ok`, `Err` with payloads give `fun(payloads): E` (substituted for a generic enum from the expected type; E0412 without one); recorded so codegen knows the variant and type arguments (a `Checked` side table, e.g. `variant_values: ExprId → (enum type, variant)`).
  - **Codegen:** the value is `{ lugha_ref_<Enum>.<Variant>, null }`; the wrapper takes the environment first, then the payloads, and builds the variant as `construct_values` does (indirect slots included).
  - **Language server:** hover on such a value shows its function type; completion unchanged.
  - **Spec v17:** §4 "Functions as values" (variants), §9 (the wrapper symbol, CLAUDE.md rule 7), §11 row 94, changes list. **Draft:** decisions 30–34, row 94.
- **Related existing code:** `src/check/{construct,results,fun_values,infer}.rs`, `src/codegen/{fun_values,construct}.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first; byte-identical IR for programs not using a variant as a value.
### Must NOT Do
- No change to payload-less variants, calls of variants, or patterns.
### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- **E0412**: a generic variant (`Some`, `Ok`, a generic user enum's) used as a value with no expected type to fix its arguments.
- **E0403**: a variant value where another function type is expected (`let f: fun(i64): Msg = Msg.Named;`).
- Existing E0415 (unknown variant), E0313 (visibility) unchanged. No new code.

## SECURITY CONSIDERATIONS
- None beyond the compiler's usual.

## TESTS TO WRITE
- [ ] Checker: user variants as values (one, two payloads), `Some`/`Ok`/`Err` with expected types, E0412 without, E0403 for a mismatch, through `m.E.V`, payload-less variants unchanged.
- [ ] `tests/programs/`: `map(Shape.Circle)`, `map(Some)`, a variant value stored in a struct and an array and called later, a recursive enum's variant as a value (indirect slot), `Ok`/`Err` with `map_err`, a generic user enum's variant with an expected type; -O0 and -O2 (the programs runner).
- [ ] Codegen unit test: one `lugha_ref_Shape.Circle` per program however many uses.
- [ ] IR unchanged for the compiling `tests/programs` that don't use the feature.

## ROLLBACK PLAN
- Branch `prp-125-variant_functions` in its own worktree.

## ACCEPTANCE CRITERIA
- [ ] The objective compiles and runs; E0412 and E0403 as specified.
- [ ] All tests pass; fmt, clippy clean; every `pub` item documented; CHANGELOG updated.

## VALIDATION
- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`
