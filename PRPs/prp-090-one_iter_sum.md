## FEATURE: One `iter_sum` — `it.sum()` through a single generic prelude function instead of ten per-type copies; milestone 61.

**Status:** proposed 2026-10-15 — session 128 (discovery in session 127).
**Milestone:** 61 (one PRP; tag `m61` at merge)
**Spec:** `Language v15 Draft.md` decision 44; spec §4 (iterator helpers), §9 (lowering), §11 (row 61).

**Decisions:**
- the session-112 TODO item;
- builds on M52's `Num` bound and M58's literals as `T` (MEMORY 101, 113);
- no open entries in DECISIONS.md apply.

## OBJECTIVE
`it.sum()` works exactly as today for every numeric element type, through one prelude `fun iter_sum<T: Num>(it: Iter<T>): T`.

## CONTEXT
- **Starting state:**
  - `src/prelude/iter.la` has `iter_sum_i8` … `iter_sum_f64`, ten copies of one loop;
  - `check/iter_helpers.rs` maps `sum` to `iter_sum_<element>` for numeric elements and returns `None` otherwise (E0415).
- **Ending state:**
  - one generic `iter_sum<T: Num>`;
  - the checker maps `sum` to it for numeric elements;
  - a non-numeric element is still E0415 (not E0419), with the same message.
- **Related existing code:** `src/prelude/iter.la`, `src/check/iter_helpers.rs`, `check/helpers.rs` (instances), `check/instances.rs`.
- **Open decisions that must be resolved first:** none.

### Design
1. **Prelude:** `fun iter_sum<T: Num>(it: Iter<T>): T { let mut s: T = 0; for x of it { s += x; } s }`, at the place of the first old copy. The copies are deleted, so later prelude line numbers shift; update any panic golden that names a prelude line, keeping each panic itself unchanged.
2. **Checker:** `"sum" if element.is_numeric()` maps to `iter_sum` with the element as the type argument, recorded as an instance (`used`), as the other generic iterator helpers are.
3. **Overflow:** `+=` panics on integer overflow at the prelude's line, as today. The golden for `m36/overflow` changes only if the line moves.

### Risk
- **Panic locations in the prelude move:** goldens that name `<prelude>:N` are updated in the same commit, and the panic text is otherwise identical.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec v15:** §9 lowering note (`iter_sum<T>` instances); §11 row 61; the changes summary ("no visible change").
- **Docs:** README (none needed), TODO (mark the item done), CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m61`.

### Must NOT Do
- **No change to which element types `sum` accepts, or to its error** (E0415 on `["a"].iter().sum()`).
- **No change to results or panic messages.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0415** for non-numeric elements, unchanged.

## SECURITY CONSIDERATIONS
- **No `unsafe`, dependencies or external input.**

## TESTS TO WRITE
- [ ] **Checker:** `sum` on `i64`, `u8`, `f32` and `f64` iterators types as the element; `["a"].iter().sum()` stays E0415.
- [ ] **Codegen IR:** a program summing `i64` and `f64` iterators defines `iter_sum<i64>` and `iter_sum<f64>` and no `iter_sum_i64`.
- [ ] **Program `m61/sums.la`:** sums at all ten numeric types, including an overflow-free `u8` and `i8`; golden from the M60 compiler.
- [ ] **Regression:** every earlier program unchanged (prelude-line goldens updated if lines moved).

## ROLLBACK PLAN
- Branch `prp-090-one_iter_sum`; tag `m61`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`; `m61/` at `-O2`.
- [ ] Spec at milestone 61; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
```
