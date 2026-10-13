## FEATURE: Integer methods — `wrapping_add`/`sub`/`mul`, `count_ones`, `leading_zeros`, `trailing_zeros`; milestone 29, v7 complete.

**Status:** approved 2026-10-13 — session 72. This is the second half of the user's "do all of them in one go" (milestones 28–29), merged once CI is green.
**Milestone:** 29 (one PRP; completes v7; tags `m29` and `v7` at merge)
**Spec:** `Language v7 Draft.md`:
- decisions 2 and 4, and the choice "The counting methods return `i64`";
- §4 "Methods on integers";
- §9;
- §10 "Hashing";
- §11 (milestone 29);
- §12 (the two v7 open questions are folded into the spec).

**Decisions:** MEMORY 65–67 (v7).

## OBJECTIVE
The §10 hashing program, an FNV-1a hash, prints `12638187200555641996`, `14695981039346656037`, `3`, `3`, `25`, `0`:

```
h = (h ^ (s[i] as u64)).wrapping_mul(1099511628211);
x.count_ones()  x.trailing_zeros()  x.leading_zeros()
let m: u8 = 255;  m.wrapping_add(1)   // 0
```

## CONTEXT

- **Starting state:**
  - Built-in methods are `push`/`pop`/`clear` on arrays and `slice` on strings (`check/builtins.rs::builtin`).
  - Any method on an integer is E0415.
- **Ending state:**
  - Every integer type has the six methods.
  - v7 is complete.
- **Related existing code:** `src/check/builtins.rs`, `src/check/method_calls.rs`, `src/codegen/methods.rs` (`method_call`, where `slice` is special-cased).
- **Open decisions that must be resolved first:** none.

### Design
- **Checker:** `builtin` gains, for any integer receiver, with a `self` receiver:
  - `wrapping_add(b)`, `wrapping_sub(b)` and `wrapping_mul(b)`, with `b` of the receiver's type, which also types a literal argument, and a result of that type;
  - `count_ones()`, `leading_zeros()` and `trailing_zeros()`, giving `i64`.

  Any other name on an integer stays E0415. A literal receiver with no expected type is `i64`, so `(5).count_ones()` works.
- **Codegen** (new file `codegen/int_methods.rs`, called from `methods.rs` like `slice`):
  - **The wrapping methods** are plain `add`, `sub` and `mul` at the type's width, with no overflow check.
  - **The counting methods** are `llvm.ctpop.iN`, `llvm.ctlz.iN` and `llvm.cttz.iN` (`is_zero_poison` false, so 0 gives the width), widened to `i64`.
- **Programs without these methods** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Checker:** the six entries in `builtins.rs`, and their tests.
- **Codegen:** `codegen/int_methods.rs`, plus the hook in `methods.rs`.
- **Spec** (`Language v7 Specification.md`):
  - §4 methods on integers;
  - §9 the lowering;
  - the §10 hashing program;
  - §11 milestone 29 done;
  - the title and status say v7 is complete;
  - §12 gains the draft's two v7 questions.
- **Docs:**
  - CLAUDE.md: STACK says v7 is complete; anti-pattern 5; the tree;
  - README, TODO, CHANGELOG, MEMORY decision 68, CONTEXT.

  The tags `m29` and `v7` are added at the merge.

### Must NOT Do
- **No rotations, and no checked or saturating methods:** out of scope for v7.
- **No methods on floats or `bool`:** those names are E0415 there.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0415:** an unknown method on an integer (`x.rotate_left(1)`), or a counting method on a float.
- **E0405:** the wrong number of arguments.
- **E0401 and E0403:** a wrong argument type (`a.wrapping_add(1.5)`; `u32` with `u64`).
- **The wrapping methods never panic.**

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/builtins.rs`):
- [x] **Types:**
  - `a.wrapping_add(1)` has `a`'s type at every width;
  - `count_ones` and the other counting methods are `i64`;
  - `(5).count_ones()` checks.
- [x] **Errors:**
  - E0415 for `x.rotate_left(1)`, for `f.count_ones()` on an `f64`, and for `b.wrapping_add(…)` on a `bool`;
  - E0405 for `a.wrapping_add()`;
  - E0403 for `u32.wrapping_add(u64)`.

Unit tests — codegen (`codegen/int_methods.rs`):
- [x] `wrapping_mul` is a plain `mul i64` with no overflow intrinsic.
- [x] The counting methods call `llvm.ctpop.i32`, `llvm.ctlz.i32` and `llvm.cttz.i32` and widen to `i64`.

Acceptance — `tests/programs/m29/`:
- [x] `hashing.la`: the §10 program.
- [x] `methods.la`:
  - each method at `u8`, `i32`, `u32`, `i64` and `u64`;
  - wrapping across the limit in both directions (`i64` minimum minus one);
  - the counts of 0 and of all-ones;
  - a negative signed value's counts.
- [x] `e0415.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-057-integer_methods`, merged into `main` after CI passes; tags `m29` and `v7`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m29/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The v7 spec is complete; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m29/hashing.la
```

## AMENDMENTS (during build, session 72)

- **No deviations from the design.** `-O2` was checked by hand for `m29/hashing` and `m29/methods`, and every earlier program gives byte-identical IR.
- **No file splits.**
