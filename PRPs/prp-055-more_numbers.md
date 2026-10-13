## FEATURE: More numbers — `u32`, `u64` and `f32`; milestone 27.

**Status:** approved 2026-10-13 — session 70
**Milestone:** 27 (one PRP; tag `m27` at merge; `Language v7 Specification.md` begins)
**Spec:** `Language v7 Draft.md`:
- decision 1, and the choices "`f32` prints as the shortest decimal…" and "`parse_i64` and `parse_f64` stay";
- §2 (type keywords);
- §4 "New numeric types";
- §7;
- §8;
- §9 (runtime printing);
- §10 "More numbers";
- §11 (milestone 27).

**Decisions:** MEMORY 65 (v7 direction); v0 casts and overflow rules (spec §4, §5).

## OBJECTIVE
The §10 more-numbers program prints `18446744073709551615`, `4000000001`, `0.1`, `true`, `4294967295`:

```
let big: u64 = 18446744073709551615;
let n: u32 = 4000000000;  println(n + 1);
let x: f32 = 0.1;         println(x);            // 0.1, the f32 shortest form
println(x as f64 > 0.1);                         // true
println(-1 as u32);                              // 4294967295
```

## CONTEXT

- **Starting state:**
  - The numeric types are `i32`, `i64`, `u8` and `f64`.
  - Codegen chooses signed or unsigned operations by `value::is_signed`.
  - Float code assumes `f64`.
  - The runtime prints `i32`, `i64`, `u8` and `f64`.
- **Ending state:**
  - `u32`, `u64` and `f32` everywhere a numeric type goes.
  - `Language v7 Specification.md` begins, and v6 is frozen.
- **Related existing code:**
  - lexer: `src/lexer/token.rs`, `lexer/mod.rs`;
  - parser and AST: `src/parser/item.rs` (`ty`), `parser/describe.rs`, `parser/sexp/item.rs`, `src/ast/mod.rs` (`TypeKind`);
  - checker: `src/check/types.rs` (`Type`, `is_integer`, `literal_range`, `Display`), `check/env.rs` (type resolution, extern checks), `check/literal.rs`, `check/expr.rs` (negation), `check/ops.rs` (casts, operators);
  - codegen: `src/codegen/value.rs`, `structs.rs` (layout), `arith.rs`, `cast.rs`, `expr.rs` (float literals), `runtime.rs` and `input.rs` (printing, `to_string`, interpolation), `declare.rs` (C ABI);
  - runtime and spec: `runtime/print.c`, `src/link.rs` (`RUNTIME_SYMBOLS`), `src/lib.rs`, `tests/cli.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery
- **No interview:** the approved v7 draft settles this milestone. The design below only maps it onto the compiler.

### Design
- **Types:** `Type::U32`, `Type::U64` and `Type::F32`, plus the matching `TypeKind`s and lexer keywords.
  - `is_integer` includes `U32` and `U64`.
  - A new `is_float` covers `F32` and `F64`, and replaces the `== Type::F64` tests in the checker and codegen.
  - `literal_range` covers the new types, so `18446744073709551615` fits a `u64` and E0402 applies above it.
  - E0104 stays the lexer's limit, `u64::MAX`.
- **Literals:** a float literal takes `f32` where `f32` is expected. It is emitted as the `f32` nearest the literal's `f64` value; that double rounding is the same as `0.1 as f32`.
- **Operators:** the existing integer code handles any width, and `is_signed` is false for `u32` and `u64`. So `+ - *` use the `u` overflow intrinsics, `/ %` use `udiv`/`urem` (zero check only), and comparisons are unsigned. Negating an unsigned type is E0404, as for `u8`. The float operations take the operand's float type.
- **Casts:** every pair of numeric types:
  - float to integer: `fpto[su]i.sat` at the source width;
  - integer to float: `[su]itofp` to the target;
  - `f64` to `f32`: `fptrunc`;
  - `f32` to `f64`: `fpext`;
  - integer to integer, as today.
- **Layout:** `u32` and `f32` are 4 bytes aligned 4; `u64` is 8 bytes aligned 8.
- **Printing, `to_string` and interpolation:**
  - **Runtime:** it gains `lugha_rt_print_u64`, `lugha_rt_to_string_u64`, `lugha_rt_print_f32` and `lugha_rt_to_string_f32`. `u32` is zero-extended to the `u64` functions.
  - **The float formatter** is shared: it stops at the fewest digits that read back as the same value, using `strtof` for `f32` and `strtod` for `f64`.
  - **Calling convention:** `f32` crosses to C as a `float`.
- **Externs:** `u32`, `u64` and `f32` are allowed in extern signatures, as `uint32_t`, `uint64_t` and `float`.
- **Unchanged:** indexes, lengths, `parse_i64` and `parse_f64`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Lexer, parser and AST:** the three type keywords.
- **Checker:** the types, `is_float`, literal typing and ranges, negation, casts, and externs.
- **Codegen:** LLVM types, layout, float arithmetic and comparisons at either width, casts, literals, printing and `to_string`, interpolation, and the extern ABI.
- **Runtime:**
  - the four new functions and the shared float formatter (`print.c`);
  - `RUNTIME_SYMBOLS`, which also reserves the names (E0306).
- **Spec:** create `Language v7 Specification.md` from v6, titled "Lugha v7 Specification (in progress: milestone 27)". v6 is then frozen. Fold in:
  - the keywords (§2);
  - the types, literals, casts and printing (§4, §5);
  - §7, §8, §9;
  - the §10 program;
  - §11 milestone 27 done, with milestones 28–29 planned and v7's out-of-scope list;
  - "Changes since v6".

  `lughac spec` prints it (`lib.rs`, `tests/cli.rs`).
- **Docs:** CLAUDE.md (STACK, tree), README, TODO, CHANGELOG, MEMORY decision 66, CONTEXT. The tag `m27` is added at the merge.

### Must NOT Do
- **No bit operators, binary literals or integer methods:** those are milestones 28–29.
- **No `i8`, `i16` or `u16`, and no `parse_u64`:** out of scope for v7.
- **No implicit conversions between numeric types.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **No new codes:**
  - **E0402:** a literal out of the type's range (`let n: u32 = 4294967296;`);
  - **E0401:** a float literal where an integer is expected;
  - **E0404:** negating an unsigned value;
  - **E0403:** mixing types (`n + big` with `u32` and `u64`).
- **At run time:** overflow panics as `integer overflow` at every width, and division by zero panics.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — lexer and parser:
- [x] `u32`, `u64` and `f32` lex as type keywords, and parse and print in types.

Unit tests — checker (`check/types.rs`, `check/literal.rs`, `check/ops.rs`):
- [x] **Literals:**
  - `let b: u64 = 18446744073709551615;` checks, and `let c: u32 = 4294967296;` is E0402;
  - `let x: f32 = 0.5;` checks, and `let y: f32 = 1;` is E0401.
- [x] **Negation:** `-n` on a `u32` and on a `u64` is E0404; on an `f32` it's fine.
- [x] **Casts:** every numeric pair casts; `true as u32` is E0408.
- [x] **Mixing:** `u32 + u64` is E0403.
- [x] **Externs:** `extern fun f(a: u32, b: u64, c: f32): f32;` is allowed.

Unit tests — codegen (`codegen/arith.rs`, `codegen/cast.rs`):
- [x] `u64` addition uses `llvm.uadd.with.overflow.i64`, and `u32` division `udiv` with no MIN check.
- [x] `f32` arithmetic is `fadd float`, `f32` to `f64` is `fpext`, and `f64` to `f32` is `fptrunc`.
- [x] `u64` comparison is `icmp ult`.

Unit tests — runtime (`link.rs`):
- [x] Every new symbol is in the runtime source (the existing symbol test).

Acceptance — `tests/programs/m27/`:
- [x] `numbers.la`: the §10 program.
- [x] `widths.la`:
  - `u32` and `u64` arithmetic near their limits, and `u64` division and remainder;
  - every cast pair, including saturation (`1.0e20 as u32`) and NaN;
  - `f32` arithmetic and printing (`1.0 / 3.0`, `1.0e30`, `-0.0`);
  - interpolation and `to_string`;
  - arrays and struct fields of each type;
  - `u64` comparison above `i64::MAX`.
- [x] `overflow.la`: `u64` addition past the limit panics (`integer overflow`, exit 101).
- [x] `e0402.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-055-more_numbers`, merged into `main` after CI passes; tag `m27`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m27/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] `Language v7 Specification.md` begins (milestone 27); docs updated.
- [x] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m27/numbers.la
```

## AMENDMENTS (during build, session 70)

- **Keywords, at the user's direction.** The build first recognised `u32`, `u64` and `f32` only in type positions, to keep v6 programs that use them as names valid. The user asked for lexer keywords instead, like the v0 types. That breaks only such programs; no repo program, prelude code or spec example uses those names. The draft's compatibility sentence and the spec's v7 note say so (v4's `Result` precedent).
- **Mixing types is E0404, not E0403** (`u32 + u64`), as `i32 + i64` always was.
- **Approved splits** (verbatim):
  - `check/types.rs` (305 lines): `slot_names_variant`, `indirect_slot` and their test moved to `check/slots.rs`;
  - `ast/mod.rs` (302 lines): `Type`, `TypeKind` and `AppliedType` moved to `ast/types.rs`.
- **`check/numbers.rs`** holds the new helpers (`is_float`, `is_unsigned`, `Type::primitive`), which keeps `check/env.rs` and `check/types.rs` under 300 lines.
- **`-O2`** was checked by hand for `m27/numbers` and `m27/widths`.
