## FEATURE: Arrays — `T[]` types, list and repeat literals, `.len`, bounds-checked element reads and writes, `for x of xs`, and value-semantics deep copies.

**Status:** merged into `main` 2026-10-09 — session 17
**Milestone:** 5, second of four PRPs. Done-when: the spec §10 primes program prints `25`.
**Spec:** §4 (arrays, array copies table, deep copies, mutability of places), §5 (`for … of`, evaluation order, panics), §7 (heap layout, natural alignment, `bool` stored as `i8`), §8 (no arrays across C), §9 (bounds checks, `lugha_copy_*`, `llvm.memcpy`)
**Decisions:** PRP-013 (integer address arithmetic in `codegen/heap.rs`, no `unsafe`; bounds panic message); MEMORY 14 (codegen reads the checker's types)

## OBJECTIVE
Lugha programs can use arrays as values:
- `let mut sieve = [false; limit + 1];`, `xs[i] = v`, `grid[i][j] += 1`, `for p of pts { … }`, `xs.len`
- reading or writing out of range panics with the PRP-013 message
- a negative repeat count panics with `negative array length`

Arrays behave as values (§4): `let ys = xs; ys[0] = 9;` never changes `xs`, and returning a parameter, or a `for … of` variable, gives the caller its own copy. The §10 primes program prints `25`.

## CONTEXT

- Starting state:
  - Array types and literals stop the checker and codegen with milestone 5.
  - `check::Type` is a `Copy` enum.
  - `codegen/heap.rs` handles string length, indexing and bounds checks.
- Ending state:
  - `check::Type::Array(Box<Type>)`, so `Type` is `Clone`.
  - New `check/array.rs` (literal and `for … of` typing, the E0507 iteration guard) and an extended `check/assign.rs` (element places, root mutability).
  - New `codegen/array.rs` (literals, repeat, element load and store, `for … of`) and `codegen/copy.rs` (deep-copy functions, copy-site decisions); `heap.rs` is generalised to arrays.
  - New `m5/` programs; spec §4 refinement and §9 codes.
- Related existing code: `src/check/{types,env,expr,access,assign,stmt,errors}.rs`, `src/codegen/{value,heap,expr,stmt,control,function,runtime}.rs`.
- Open decisions that must be resolved first: none.

### Amendments during implementation (session 17)
- **`Type` refactor** landed as its own commit (`7ba1c8d`) before the feature, so the mechanical `Copy` → `Clone` change is reviewable apart from the array rules.
- **Layout helpers:** `element_size`, `element_of` and the `bool`-as-`i8` element load/store live in `heap.rs` next to `element_address`, keeping `array.rs` under 300 lines.
- **Function tails:** `control::block_inner(block, returns)` lowers a function body's tail while its locals are still in scope, so the move-vs-copy decision can see whether the root is an owned local.
- **E0507 label** points at the iterated expression (`xs` in `for x of xs`), not the whole loop, which was noisy.
- **Updated test:** the CLI internal-error test used an array literal as its unsupported construct; it now uses a struct.
- **Goldens:** the run-program goldens were written by hand and matched on the first run; the three reject goldens were captured and reviewed.

### Discovery answers (session 17)
1. Codes:
   - **E0412**: empty array literal without an expected type.
   - **E0507**: assigning to the array being iterated.
   - Reused: E0403 (mixed element types), E0401/E0403 (count and index types), E0404 (`==` on arrays), E0411 (iterating a string), E0409 (arrays in `extern` signatures), E0501 (element assignment needs a `let mut` root).
2. **`for … of` variables count as borrowed**, like parameters: returning one, ending a function with one, or storing one copies it. Spec §4's table gains this.
3. **`Type::Array(Box<Type>)`**: a recursive enum. `Type` becomes `Clone` (not `Copy`) across the checker and codegen.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Types**
- `check::Type::Array(Box<Type>)`, displayed as `T[]` (`i64[][]`). `Type` derives `Clone, PartialEq, Eq, Debug`.
- Every former `Copy` use becomes a clone or a borrow. `codegen::value::llvm_type` takes `&Type`, and an array lowers to `ptr`.
- Annotations resolve `T[]` recursively. `extern` parameters or returns of array type → **E0409** "arrays can't cross the C boundary" (§8).

**Checker** — `check/array.rs`, `check/access.rs`, `check/assign.rs`
- **List literal `[a, b, c]`:**
  - With an expected `T[]`, each element expects `T`.
  - Otherwise the first element's type is the element type, and later elements expect it: a mismatch is E0403, with a label "the first element is T".
  - `[]` with an expected `T[]` is that type; with no expected type it's **E0412** "cannot infer the element type of `[]`", with help "annotate it: `let xs: i64[] = [];`".
  - Element type `void` → E0407.
- **Repeat literal `[v; n]`:** `v` is checked first (expecting `T` when `T[]` is expected); `n` expects `i64`. The type is `T[]`.
- **Element access:**
  - `xs[i]` on `T[]` → `T`, with an `i64` index (as for strings).
  - `xs.len` → `i64`. Other fields → E0410.
  - `==`/`!=` and every other operator on arrays → E0404.
- **Assignment to element places** (`xs[i]`, `grid[i][j]`, chains through `.len` excepted):
  - The place's root variable must be `let mut` → E0501, as for plain names.
  - A root of type `string` (or a chain through one) → E0506.
  - The value expects the place's type. Compound operators need a numeric element type.
- **`for x of xs`:**
  - `xs` must be `T[]`; a string or anything else is **E0411** "cannot iterate over `string`".
  - `x : T` is an immutable loop variable (`Binding::LoopVar`).
  - While the body is checked, the iterated place (when `xs` is a place) is kept on a stack. An assignment whose target overlaps it is **E0507** "cannot assign to `xs` while iterating over it", with a label on the `for`.
  - Overlap is conservative: same root, and a matching path where two index steps count as possibly equal and field names must match.

**Codegen layout** (spec §7)
- An array is a pointer to `{ i64 len, T elems[len] }` on the GC heap (`lugha_rt_alloc`).
- Element storage size by element type:
  - `i32` 4, `i64` 8, `u8` 1, `f64` 8
  - `bool` 1, stored as `i8`: zero-extended on store, truncated on load
  - `string` and arrays 8 (pointers)
- Natural alignment holds with an 8-byte header. Sizes assume a 64-bit target, stated in the code.
- Element addresses use `heap::element_address` (integer arithmetic, no `unsafe`).

**Codegen operations** — `codegen/array.rs`, `heap.rs`
- **List literal:**
  - allocate `8 + n*size`, store `len`, then evaluate and store each element in order (§5);
  - elements that come from a place are deep-copied (§4 table).
- **Repeat literal:**
  - evaluate `v`, then `n` (§5);
  - `n < 0` panics with `negative array length` at the `[`;
  - allocate, store `len`, then fill in a loop;
  - every element is an independent copy of `v` (§4): plain types are stored as-is; types containing arrays get a deep copy per element.
- **Reads:** `xs[i]` bounds-checks (PRP-013 message, at the `[`) and loads; `xs.len` loads the header.
- **Writes:** element assignment evaluates the place once — base pointers down the chain, the index, the bounds check — then the value, then stores. Compound element assignment loads, applies the checked operator at `op_span`, and stores once (§5).
- **`for x of xs`:**
  - `xs` is evaluated once and not copied; its length is read once;
  - an internal counter runs `0..len`; `x` is loaded fresh each iteration;
  - `continue` steps the counter;
  - the borrowed-variable rule applies to `x`.

**Copies** — `codegen/copy.rs` (spec §4, §9)
- `deep_copy(value, &Type)`:
  - for `T[]` with plain `T`: `lugha_rt_alloc` plus `llvm.memcpy` of `8 + len*size` bytes;
  - for `T[]` where `T` contains arrays: an internal function `lugha_copy_<mangled>` (e.g. `lugha_copy_i64_arr_arr`), generated once per type, that allocates, stores `len` and copies each element recursively;
  - strings are shared (immutable).
- **Copy sites:** a value whose type contains arrays is copied when it comes from a place and is stored into another place:
  - `let`, assignment, list-literal elements, repeat fill;
  - `return`, and a function's tail, when the expression is rooted in a parameter or a `for … of` variable.

  "From a place" looks through `if`/block tails. A plain place rooted in an owned `let` local moves on `return` and tail (§4).
- Never copied: call arguments, fresh values (literals, calls, `+`).

**Spec and docs**
- §4 copy table: `return p;` row reads "where `p` is a parameter, a `for … of` variable, or part of one".
- §9 code table: E0412, E0507, and E0409's new array case.
- CLAUDE.md file tree: `check/array.rs`, `codegen/array.rs`, `codegen/copy.rs`.

**Acceptance** — `tests/programs/m5/`, also cross-checked at `-O2`:
- `primes.la`: the spec §10 program → `25`.
- `array_basics.la`: literals of every element type including `bool` and `string`; `.len`; reads; `xs[i] = v`; `xs[i] += 1`; nested `grid[i][j]`; `for … of` summing and printing.
- `value_semantics.la`: `let ys = xs; ys[0] = 9;`, an assigned copy, a nested deep copy, a function returning its parameter and then mutated, a `for … of` variable returned, and `[[0; 2]; 2]` with independent rows. Each prints the original unchanged; expected stdout is written by hand.
- `array_oob.la`: an out-of-range write → `panic: index out of bounds: the length is 3 but the index is 3 at …`, exit 101.
- `negative_length.la`: `[0; n]` with `n = -1` → `panic: negative array length at …`, exit 101.
- Reject mode: `e0412.la`, `e0507.la`, and `e0501_element.la` (element assignment with a non-`mut` root). Goldens captured and reviewed.

### Must NOT Do
- No structs (PRP-015), no `lughac spec` (PRP-016).
- No array `==`, slicing, growth or `push` (the growable list is §11 v1).
- No copy-on-write (DECISION-010 stays deferred until measurements exist).
- No `unsafe`, no new dependencies.

## ERROR HANDLING REQUIREMENTS

- Checker rules report and keep going; `Type::Error` suppresses cascades.
- Every element access is bounds-checked before its address is formed. The IR verifies on every test.
- Allocation failure panics with "out of memory" (runtime). `8 + len*size` can't overflow for any `len` a 64-bit allocator could satisfy; a huge `n` simply fails allocation.

## SECURITY CONSIDERATIONS

- No element access happens without a prior unsigned bounds check against the header length.
- Deep copies read exactly `len` elements from the source header and write into a fresh allocation of that size.
- No `unsafe`.

## TESTS TO WRITE

Unit tests:
- [x] Checker:
  - literal typing (expected, first-element, mismatch with label); E0412; repeat typing; `T[]` annotations;
  - `xs[i]` and `.len`; E0404 on `==`; E0411 for `for c of "abc"`;
  - E0501 and E0506 on element places;
  - E0507 for `xs`, `xs[0]` and `grid[0][1]` inside `for x of grid[0]`; no E0507 for unrelated arrays;
  - E0409 for an array extern parameter.
- [x] Codegen IR:
  - a literal allocates `8 + n*size` and stores `len`;
  - `bool` elements are `i8` (`zext`/`trunc`);
  - a negative-length check calls `lugha_rt_panic`;
  - `let ys = xs` calls `llvm.memcpy` (plain) or `lugha_copy_i64_arr_arr` (nested);
  - `return param` copies, `return local` doesn't;
  - call arguments aren't copied;
  - `for … of` reads the length once.

Acceptance:
- [x] The five run programs and three reject programs pass; all earlier tests pass after the `Type: Clone` refactor.

## ROLLBACK PLAN

- Branch `prp-014-arrays`, merged into `main` on acceptance.
- To abandon: delete the branch. The `Type` refactor lives in the branch.

## ACCEPTANCE CRITERIA
- [x] `lughac run tests/programs/m5/primes.la` prints `25`.
- [x] Every test above exists and passes.
- [x] Spec §4 and §9, CLAUDE.md, CHANGELOG.md, TODO.md and MEMORY.md updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run tests/programs/m5/primes.la` → `25`
