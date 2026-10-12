## FEATURE: Deep `==`/`!=` on every type and `<` on strings — and the start of the v2 specification.

**Status:** merged into `main` 2026-10-13 — session 38
**Milestone:** 9 (second of three PRPs: 030 syntax, **031 equality and string order**, 032 nested patterns and guards)
**Spec:** `Language v2 Draft.md` §4 (equality on every type, string order), §5 (evaluation of `==`), §9 (`lugha_eq_<type>`, `lugha_rt_str_cmp`). It folds into a new `Language v2 Specification.md`.
**Decisions:**
- MEMORY 33 (v2 direction: `Eq` on every type, `Ord` = numbers and strings);
- MEMORY 34 (gate);
- MEMORY 25 (enum layout), MEMORY 26 (indirect slots);
- architecture rules 1 (the printed spec matches the compiler) and 7 (symbol prefixes).

## OBJECTIVE
`==` and `!=` work on any two values of the same type, deeply:
- structs field by field;
- enums and `Option` by variant, then payloads;
- arrays by length, then elements.

`<`, `<=`, `>` and `>=` also work on strings, compared bytewise. The spec that `lughac spec` prints says so.

## CONTEXT

- **Starting state:**
  - `==`/`!=` accept primitives and strings; anything else is E0404.
  - `<` accepts numbers only.
  - Only numeric types flow between operands (`from_operand`), so `o == None` is E0412.
  - `lughac spec` prints v1.2.
- **Ending state:**
  - **Checker:** the new operator rules and two-way operand typing.
  - **Codegen:** `lugha_eq_<type>` helpers for structs, enums, `Option` and arrays.
  - **Runtime:** `lugha_rt_str_cmp`.
  - **Spec:** a new `Language v2 Specification.md` is what `lughac spec` prints; v1.2 freezes.
- **Related existing code:**
  - `src/check/ops.rs`;
  - `src/codegen/{arith,heap,copy,enums,matching}.rs` (comparisons, `string_binary`, the copy-helper pattern and `mangle`, enum tags and payloads);
  - `runtime/lugha_rt.c`, `src/codegen/runtime.rs` (`RUNTIME_SYMBOLS`);
  - `src/lib.rs` (`SPEC`), `tests/{spec,cli}.rs`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 38)
- **One E0412 for `None == None`:** when the first operand failed, the second gets an `Error` expectation. `None` and `[]` then stay quiet, so one mistake gives one diagnostic.
- **Existing tests:** four checker tests that asserted E0404 for `==` on arrays, enums and structs, and for string `<`, now assert v2 behaviour. One test was renamed to `arrays_have_len_elements_and_equality`.
- **Codegen detail:** `compare` in `arith.rs` became `pub(super)`. `string_binary` (`heap.rs`) handles the orderings.
- **Spec placement:** the operand-typing rule went into §4 next to the equality text. §11 got a milestone 9 row.

### Discovery answers (session 38)
1. **Spec file:** start the v2 spec now, as v1 did at PRP-020.
   - Copy v1.2 to `Language v2 Specification.md`, fold in only what has landed, and title it "Lugha v2 Specification (in progress: milestone 9)".
   - It becomes `lugha::SPEC`, and v1.2's file is frozen.
   - Constructs still gated stay only in the draft.
2. **Operand typing:** either side informs the other.
   - The operand that can be typed alone is checked first, and its type (any type, for the equality and comparison operators) becomes the other side's expected type.
   - So `o == None`, `None == o`, `xs == []` and `o == Some(3)` work.
   - `None == None` is still E0412.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Checker** — `check/ops.rs`
- **Allowed operands:**
  - `==` and `!=`: two operands of the same type, any type.
  - `<`, `<=`, `>`, `>=`: two numbers of the same type, or two strings.
  - Anything else stays E0404, for example `"a" < 1`, `true < false`, or a struct `<`.
- **Operand order:** an operand *defers* if it can't be typed alone:
  - a numeric literal (as today);
  - `None`, `[]`, or `[e; n]` / `[e, …]` whose first element defers;
  - `Some(e)` where `e` defers.

  If the left side defers and the right doesn't, the right side is checked first. Otherwise the left side is checked first.
- **Expected types:** for `==`, `!=` and the comparisons, the first operand's type, whatever it is, becomes the second operand's expected type. The one exception is a numeric literal receiving a non-numeric type: that keeps E0404 (`"a" == 1`), not a literal error. Arithmetic and range bounds keep today's numeric-only flow.
- **Unchanged:** liveness. `==` operands are reads, not store sites.

**Codegen** — a new `codegen/equality.rs`; `arith.rs` dispatches to it
- **`equal(a, b, ty) -> i1`:**
  - numbers and `bool`: inline (`f64` uses ordered `==`, so `NaN != NaN`);
  - strings: `lugha_rt_str_eq`;
  - structs, enums, `Option` and arrays: a call to `lugha_eq_<mangled>(T, T) -> i1`.

  `!=` is the negation of `==`.
- **Helpers.** Each is generated once per type, `internal` linkage, declared before its body (so recursive types call themselves), with the builder restored after, like `lugha_copy_<type>`.
  - **Struct:** compare field by field; the first unequal field returns false.
  - **Array:** compare `.len`, then loop over the elements and return false at the first difference. Spare capacity is never read.
  - **Enum and `Option`:** both values go into entry-block slots. Compare the tags, then switch on the tag. For each variant with payloads, compare the payloads in order; an indirect slot is loaded through its pointer first. Payload-less variants are equal.
- **Mangling:** `mangle` (shared with `copy.rs`) gains `Enum(name)` → `{len}{name}` (like structs) and `Option(T)` → `{T}_opt`. Existing copy-helper names are unchanged.
- **String order:** `<`, `<=`, `>` and `>=` on strings call `lugha_rt_str_cmp(a, b)` and compare its `i32` result with 0.

**Runtime** — `runtime/lugha_rt.c` (the core part)
- `int32_t lugha_rt_str_cmp(const LughaString *a, const LughaString *b)`: `memcmp` over the shorter length, then the shorter string is less. It returns −1, 0 or 1.
- The declaration goes with the others, and the function joins `RUNTIME_SYMBOLS`.

**Spec**
- **New file:** `docs/specs/Language v2 Specification.md`, a copy of v1.2 with:
  - the title "Lugha v2 Specification (in progress: milestone 9)" and a status note: v2 is being built milestone by milestone, the plan is `Language v2 Draft.md`, and this file describes exactly what `lughac` implements now;
  - the "changes" notes replaced by a "Changes since v1.2" line: deep `==` on every type, string order;
  - **§4:**
    - the operator table rows become "`==` `!=` — two values of the same type" and "`<` … — two numbers of the same type, or two strings";
    - the "no `==`" sentences for structs, arrays and enums are replaced by the draft §4 equality text;
    - the string paragraph mentions bytewise order;
    - the two-way operand rule is added;
  - **§5:** "Evaluation of `==`" (draft §5);
  - **§9:** a `lugha_rt_str_cmp` runtime row, and `lugha_eq_<type>` in the symbol rules;
  - **§11:** the milestone 9 row, in progress.
- **Embedding:** `lugha::SPEC` includes the v2 file. `Language v1 Specification.md` is frozen, and keeps v1.2's text.
- **CLAUDE.md:** the STACK note, architecture rule 1 (it points at the v2 spec), and rule 7 (`lugha_eq_<type>`).

**Docs**
- CLAUDE.md tree (`codegen/equality.rs`, specs).
- README (v2 in progress).
- TODO, CHANGELOG, MEMORY decision 35, CONTEXT.

### Must NOT Do
- No change to the gate, patterns, modules or generics. No pointer-equality shortcut, which would be wrong for `NaN`.
- No ordering on anything but numbers and strings.
- No `unsafe` in Rust, and no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- **Checker:** E0404 and E0412 are reported and checking continues, as today.
- **Generated code:** `==` never panics. The helpers recurse only as deep as the values do, the same as copying.

## SECURITY CONSIDERATIONS

- **Bounds:** array equality reads only indexes below `.len` of each array.
- **Strings:** `lugha_rt_str_cmp` reads `min(len)` bytes, through `memcmp` with explicit lengths. It never relies on the NUL.
- **Recursion:** deep recursive values recurse in the helper, just as their copies and drops do. Too deep a value is the existing stack-overflow panic (PRP-025).

## TESTS TO WRITE

Unit tests — `check/ops.rs`:
- [x] **Equality accepted:** `==`/`!=` on a struct, an enum, an `Option`, `i64[][]`, and a struct holding arrays and strings.
- [x] **Operand typing:**
  - `o == None` and `None == o`;
  - `o == Some(3)`, where `3` takes `u8`;
  - `xs == []` and `[] == xs`;
  - `[1] == bytes`, where the literal elements take `u8`.
- [x] **Order:** `"a" < "b"` gives `bool`.
- [x] **E0404 cases:**
  - `"a" < 1`, `true < false`, a struct `<`;
  - `"a" == 1` (E0404, not a literal error);
  - different struct types compared.
- [x] **E0412:** `None == None` and `[] == []`.

Codegen IR — `codegen/equality.rs`:
- [x] **Structs:** a struct `==` calls `@lugha_eq_…` defined once, and two comparisons share it.
- [x] **Arrays:** an array `==` compares lengths before the loop.
- [x] **Recursion and strings:** a recursive enum's helper calls itself, and a string `<` calls `@lugha_rt_str_cmp`.
- [x] **`!=`:** it negates the helper result.

Runtime:
- [x] The existing `-Werror` compile covers `lugha_rt_str_cmp`, and the `RUNTIME_SYMBOLS` test includes it.

Acceptance — `tests/programs/m9/` (also cross-checked at `-O2`; the expected output is written by hand):
- [x] **`equality.la`:**
  - structs with arrays and strings;
  - nested arrays (equal, different lengths, different elements);
  - an enum with payloads;
  - a recursive list enum, equal and unequal;
  - `Option` (`None` vs `Some`, both orders);
  - `!=`;
  - `NaN` in an array (unequal to itself);
  - string order including the prefix case (`"ab" < "abc"`), and bytes ≥ 128 compared unsigned.
- [x] **`e0404_struct_order.la`:** a struct `<` is rejected. The golden is captured and reviewed.
- [x] **Spec:** `tests/spec.rs` passes against the new file. `lughac spec` prints a title starting "# Lugha v2 Specification".
- [x] **Regression:** every earlier test passes.

## ROLLBACK PLAN

- Branch `prp-031-deep_equality`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] Deep `==`/`!=` and string order work as above, at `-O0` and `-O2`.
- [x] `lughac spec` prints the v2 spec (in progress), and v1.2's file is unchanged.
- [x] CLAUDE.md, README, TODO, CHANGELOG, MEMORY updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac run tests/programs/m9/equality.la`
