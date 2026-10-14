## FEATURE: More integers — `i8`, `i16`, `u16`, checked and saturating methods, rotations, `MIN`/`MAX`, `parse_u64`; milestone 35.

**Status:** approved 2026-10-14 — session 82. This is part of the user's "in one go" (milestones 34–36), merged once CI is green.
**Milestone:** 35 (one PRP; tag `m35` at merge)
**Spec:** `Language v10 Draft.md`:
- decision 6, and the choices on rotations, `checked_div`, `parse_u64` and `MIN`/`MAX`;
- §2 and §3;
- §4 "More integer types", "Integer methods and constants" and `parse_u64`;
- §9;
- §10 "More integers";
- §11 (milestone 35).

**Decisions:** MEMORY 76 (v10), MEMORY 66 and 68 (v7 numbers and integer methods).

## OBJECTIVE
The §10 more-integers program prints `32767`, `-1`, `32767`, `255`, `3`, `true`, `18446744073709551615`:

```
let a: i16 = i16.MAX;
println(a.checked_add(1).unwrap_or(-1));
println(c.rotate_left(1));
println(parse_u64("18446744073709551615").unwrap());
```

## CONTEXT

- **Starting state:**
  - The integer types are `i32`, `i64`, `u8`, `u32` and `u64`, which are type keywords.
  - The v7 methods `wrapping_*` and the counts exist (`check/builtins.rs`, `codegen/int_methods.rs`).
  - `parse_i64` and `parse_f64` are intrinsics (`check/input.rs`, `codegen/input.rs`, `runtime/input.c`).
- **Ending state:**
  - three more integer types;
  - nine more methods;
  - `I.MIN` and `I.MAX`;
  - `parse_u64`.
- **Related existing code:**
  - `src/lexer/{token,mod}.rs`;
  - `src/ast/types.rs`;
  - `src/parser/{item,describe,primary}.rs`, `src/parser/sexp/item.rs`;
  - `src/check/{types,numbers,builtins,access,input}.rs`;
  - `src/codegen/{value,structs,runtime,input,declare,int_methods,expr}.rs`;
  - `runtime/input.c`, `src/link.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **The new types follow `u32`'s path:**
  - **Lexer:** the keywords `TyI8`, `TyI16` and `TyU16`.
  - **AST and parser:** `TypeKind::{I8, I16, U16}`, and `Type::{I8, I16, U16}` in the checker.
  - **Checker:** `is_integer` includes them, and so does `is_unsigned` (`U16`). Literal ranges are `i8` −128…127, `i16` −32768…32767 and `u16` 0…65535.
  - **Codegen:** LLVM `i8`/`i16`; layouts (1, 1) and (2, 2); `is_signed` includes `I8`/`I16`.
  - **Printing and text:** through the `i64` and `u64` runtime functions, sign- or zero-extended in `abi_value`, so the runtime gets no new printers.
  - **Externs:** `signext` on `i8`/`i16` and `zeroext` on `u16`, as C passes them.
- **`I.MIN` / `I.MAX`:**
  - **Parser:** an integer type keyword directly followed by `.` is read as a name, `Name("i16")`, so `i16.MAX` is an ordinary `Field`. A variable can't be named `i16`, so nothing is ambiguous.
  - **Checker:** in `access.rs::field`, a base naming an integer keyword gives `MIN`/`MAX` of that type, and any other name is E0415. Floats (`f64.MAX`) and `bool`/`string` stay parse errors, since only integer keywords are read this way.
  - **Codegen:** an arm in `expr.rs` before the field arms emits the constant (helpers in a new `codegen/int_checked.rs`).
- **Methods** (`check/builtins.rs` entries; codegen in `codegen/int_checked.rs`, called from `int_methods.rs`):
  - **`checked_add/sub/mul`:** `llvm.{s,u}{add,sub,mul}.with.overflow`; the overflow flag selects `None` or `Some(value)`.
  - **`checked_div`:** "bad" is `b == 0`, or, for a signed type, `a == MIN && b == -1`. The division uses `select(bad, 1, b)` as its divisor, so it never divides by zero, and `bad` selects `None`.
  - **`saturating_add/sub`:** `llvm.{s,u}{add,sub}.sat`.
  - **`saturating_mul`:** `mul.with.overflow`. On overflow it gives `MAX`, or for a signed type `MIN` when the signs differ.
  - **`rotate_left/right(n: i64)`:** `llvm.fshl/fshr(a, a, n')`, where `n'` is `n` truncated to the width (the intrinsic takes it modulo the width).
- **`parse_u64(s)`:**
  - a new intrinsic, next to `parse_i64` in `check/input.rs`'s `INPUT`;
  - the runtime's `lugha_rt_parse_u64(s, *out)`: digits only, `None` past `u64.MAX`;
  - `RUNTIME_SYMBOLS` grows to 34.
- **Programs without these** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Lexer, AST, parser and sexp:** the three keywords and types; integer keywords before `.` in expressions.
- **Checker:** the types, literal ranges, methods, `MIN`/`MAX` (E0415 otherwise), and `parse_u64`.
- **Codegen:** the types, printing, externs, methods, constants, and `parse_u64`.
- **Runtime:** `lugha_rt_parse_u64`.
- **Spec:** the v10 changes summary, including that `i8`/`i16`/`u16` became keywords and `parse_u64` an intrinsic name; §2; §3; §4 (types table, methods, constants, `parse_u64`); §8 (externs); §9; the §10 program; §11.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 78, CONTEXT. The tag `m35` is added at the merge.

### Must NOT Do
- **No unsigned indexes, no `MIN`/`MAX` on floats, and no `checked_rem`/`saturating_div`** (not in the draft).
- **No new print functions in the runtime.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0415:**
  - `i64.TOP`;
  - the new methods on a float or `bool`.
- **E0401:** a literal out of range for the new types, as for the others.
- **E0403:** a method argument of another type (`a.checked_add(b)` with `b` of another integer type); a non-`i64` rotation count.
- **E0405:** the wrong argument count.
- **E0302:** a user function named `parse_u64` (it is now an intrinsic).
- **At run time:** nothing new. The checked and saturating methods never panic, and the new types' ordinary operators panic on overflow, as the others do.

## SECURITY CONSIDERATIONS
- **`parse_u64` reads a string** that may come from program input. It validates every byte and checks for overflow before each step, as `parse_i64` does, and never reads past `len`.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — lexer: `i8 i16 u16` lex as keywords.

Unit tests — parser:
- [x] `let a: i8 = 1;` and casts to the new types print.
- [x] `i16.MAX` parses as a field of the name `i16`.

Unit tests — checker (`check/numbers.rs`, `check/builtins.rs`):
- [x] **Literal ranges:** `let a: i8 = -128;`, `let b: u16 = 65535;`; E0401 for `let c: i8 = 128;` and `let d: u16 = -1;`.
- [x] **Method types:** `checked_add` gives `Option<i16>`, `saturating_mul` gives `u8`, `rotate_left` gives `u16`; E0403 for a `u8` argument on an `i16`; E0415 on `f64`.
- [x] **Constants:** `i8.MIN` is an `i8`; `i64.TOP` is E0415.
- [x] **`parse_u64`:** gives `Option<u64>`; a user `fun parse_u64` is E0302.

Unit tests — codegen (`codegen/int_checked.rs`):
- [x] `checked_add` uses `sadd.with.overflow`; `saturating_add` on `u8` uses `uadd.sat.i8`; `rotate_left` uses `fshl.i16`; `i16.MAX` is `i16 32767`.

Acceptance — `tests/programs/m35/`:
- [x] `integers.la`: the §10 program.
- [x] `more.la`:
  - arithmetic, comparison, casts and printing for `i8`, `i16` and `u16`;
  - interpolation;
  - every `checked_*` at its edges, including `checked_div` by 0 and `MIN / -1`;
  - every `saturating_*` at both ends;
  - rotations with negative and large counts;
  - `MIN`/`MAX` of all eight integer types;
  - `parse_u64` on `""`, `"-1"`, `"18446744073709551616"` and `"007"`;
  - struct fields and arrays of the new types.
- [x] `overflow.la`: `i8` addition overflow panics (exit 101).
- [x] `e0402.la`: reject golden for out-of-range `i8` and `u16` literals.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-064-more_integers`, merged into `main` after CI passes; tag `m35`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m35/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The spec is at milestone 35; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m35/integers.la
```

## AMENDMENTS (during build, session 82)

- **Out-of-range literals are E0402** ("integer literal out of range"), not E0401. The golden is `m35/e0402.la`, and the unit tests say so.
- **`codegen/function.rs`** lists the input intrinsics by name: `parse_u64` was added there too.
- **`parser/describe.rs::symbol`** became `pub(super)`, to name an integer keyword read as a name.
- **`rotate_left` was v7's example of a missing method:**
  - in `check/builtins.rs::integers_have_wrapping_and_counting_methods`;
  - in the `m29/e0415.la` golden.

  Both now use `swap_bytes`, and the golden's comment was updated.
- **IR:** the 135 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for `m35/integers`, `m35/more` and `m35/overflow`.
- **No file splits.**
