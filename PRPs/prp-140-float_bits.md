## FEATURE: A float's raw bits — `x.to_bits()` and `f64.from_bits(b)` / `f32.from_bits(b)`; milestone 108 (v17).

**Status:** approved 2026-10-17 (session 259: "build"); proposed in session 258, from the user's report: "lughac has no way to turn a float into its raw bits or back (to_bits/from_bits for f64/u64 and f32/u32)".

## OBJECTIVE
```lugha
let x = 1.5;
let b = x.to_bits();              // u64: 0x3FF8000000000000
let y = f64.from_bits(b);         // 1.5
let h = (0.1 as f32).to_bits();   // u32
let n = f32.from_bits(0x7FC00000); // NaN
```

## CONTEXT
- Today: `as` converts values (`1.5 as u64` is 1), never reinterprets; nothing exposes a float's bits. Integer methods (`count_ones`, `wrapping_add`) live in `check/builtins.rs` and `codegen/int_methods.rs`; `I.MIN`/`I.MAX` in `check/numbers.rs` and `int_checked.rs`; `primary` reads an integer type keyword before `.` as a name (v10).
- Related: spec v17 §4 (numbers), §5 (methods on numbers).

## DECISIONS (draft decisions 124–128)
124. **`x.to_bits()`**: `f64` → `u64`, `f32` → `u32`; the IEEE 754 bits exactly (NaN payloads and `-0.0` kept). A method, like `count_ones`.
125. **`f64.from_bits(b: u64): f64`, `f32.from_bits(b: u32): f32`**: the reverse, called on the type keyword (as `i16.MAX` names a constant). Any bits are valid: signalling NaNs come back as LLVM gives them. The argument must be exactly `u64`/`u32` (no coercion; an integer literal takes the type).
126. **Lowering:** LLVM `bitcast` both ways; no runtime call, works at -O0 and -O2.
127. **Not:** `to_bits` on integers, other widths (`f16`), byte-order helpers (`to_le_bytes`) — later if wanted.
128. **Success:** milestone 108, tag `m108`; spec §4/§5 and the changes list; completion offers `to_bits` on floats.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Parser: `f32`/`f64` keywords before `.` read as names (as integer keywords already do).
- Checker: `to_bits` on floats; `f64.from_bits`/`f32.from_bits` calls with an exact-width argument; wrong type E0403, wrong arity E0405, unknown member E0415.
- Codegen: bitcasts. Members list for completion.
### Must NOT Do
- Decision 127.
### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- Existing codes only: E0403 (argument type), E0405 (arity), E0415 (`to_bits` on a non-float, other members on `f64.`).

## SECURITY CONSIDERATIONS
- None: pure value reinterpretation, no memory access.

## TESTS TO WRITE
- [ ] Parser: `f64.from_bits(b)` parses as a call on the name `f64`.
- [ ] Checker: types of `to_bits` and `from_bits` for both widths; E0403 for `f64.from_bits(1u32)`/an `i64`; E0415 for `(1).to_bits()` and `f64.nope(1)`.
- [ ] `tests/programs/m108/float_bits.la`: round trips, `1.5`, `-0.0`, `NaN`, infinity, `f32` values, printed as hex (`{b:x}`), at -O0 and -O2.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE program runs; narrow tests, fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --lib -- float_bits`, `lughac run tests/programs/m108/float_bits.la` at -O0/-O2, fmt, clippy
