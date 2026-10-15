## FEATURE: C → Lugha — `parse_i64`, `parse_u64` and UTF-8 decoding written in the prelude, over one shared decoder; milestone 62.

**Status:** approved 2026-10-15 — session 129 ("approved"); proposed in session 128 (discovery in session 127).
**Milestone:** 62 (one PRP; tag `m62` at merge)
**Spec:** `Language v15 Draft.md` decisions 45–46; spec §4 (strings, `char`, `chars()`), §5 (`parse_i64`, `parse_u64`, `string_from_utf8`), §9 (runtime table), §11 (row 62).

**Decisions:**
- the session-112 TODO item;
- follows M53's bundled-only `$` built-ins (MEMORY 103);
- no open entries in DECISIONS.md apply.

## OBJECTIVE
These keep exactly their current results and messages, but are written in Lugha in the prelude:
- `parse_i64(s)` and `parse_u64(s)`;
- `string_from_utf8(bytes)` and `s.chars()`, which share one decoder.

The three C functions are removed. `parse_f64` stays in C (`strtod`).

## CONTEXT
- **Starting state:**
  - **Parsers:** `runtime/input.c` has `lugha_rt_parse_i64` and `lugha_rt_parse_u64`.
  - **UTF-8:** `runtime/bytes.c` has `utf8_step` and `lugha_rt_utf8_check`; `prelude/text.la::Chars.next` has its own decoder (U+FFFD on invalid).
  - **Codegen:** `codegen/input.rs` and `codegen/io.rs` call the C functions.
- **Ending state:** decisions 45–46.
- **Related existing code:**
  - `src/prelude/text.la` and `core.la`;
  - `runtime/input.c`, `bytes.c`;
  - `src/link.rs` (`RUNTIME_SYMBOLS`);
  - `src/codegen/input.rs`, `io.rs`; `src/check/input.rs`, `float_ops.rs` (bundled built-ins);
  - `tests/programs/` (`m14`/`m45` parsing and UTF-8 programs).
- **Open decisions that must be resolved first:** none.

### Design
1. **The decoder:** `fun utf8_step(at: fun(i64): i64, i: i64, len: i64): i64` gives the number of bytes (1–4) of the scalar starting at `i`, or 0 if invalid (overlong, surrogate, above U+10FFFF, truncated, bad continuation). It reads bytes through `at`, so both a `string` and a `u8[]` can use it without copying, and it gives the same answers as `bytes.c::utf8_step`.
2. **`Chars.next`:** uses `utf8_step` and builds the scalar; a 0 step yields U+FFFD and advances one byte, as today.
3. **`string_from_utf8`:** scans with `utf8_step`. On 0 it returns `Err("invalid UTF-8 at byte N")`, the same text as today. Otherwise it returns `Ok($string.from_bytes(bytes))`, a new bundled-only built-in that copies the bytes into a string with its NUL terminator, through `lugha_rt_alloc`.
4. **`parse_i64(s)` and `parse_u64(s)`** keep the same grammar:
   - an optional `-` (`i64` only), then one or more ASCII digits and nothing else;
   - checked accumulation, with `None` on overflow; `-9223372036854775808` parses;
   - empty input, a lone `-`, `+`, spaces or other characters give `None`.
5. **Removed:** `lugha_rt_parse_i64`, `lugha_rt_parse_u64` and `lugha_rt_utf8_check` go from the runtime fragments and `RUNTIME_SYMBOLS`; their callers become prelude calls (lazy, M60).

### Risk
- **Behaviour drift** in edge cases (overlong forms, surrogates, `-0`, leading zeros, `u64::MAX`). Mitigations:
  - an exhaustive differential test (below) against the M61 compiler;
  - the existing parsing and UTF-8 programs.
- **Differential test:** a program feeding a table of tricky inputs (and, for UTF-8, every byte pair and a set of 3- and 4-byte sequences) through all three functions prints the results. Its golden comes from the M61 compiler (the C implementations).
- **Speed:** parsing in Lugha is slower than C per call, but nothing hot depends on it; noted only.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec v15:**
  - §9: the runtime-function table loses the three rows, and `$string.from_bytes` joins the bundled built-ins;
  - §11: row 62; the changes summary ("no visible change").
- **Docs:** CLAUDE.md (runtime fragments' contents), TODO (mark done), CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m62`.

### Must NOT Do
- **No change to any result, `None`/`Err` case or message.**
- **`parse_f64` stays in C.**
- **Programs can't name `$string.from_bytes`** (`$` is E0101 outside bundled sources).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Same as today:** `None` from the parsers; `Err("invalid UTF-8 at byte N")` from `string_from_utf8`.

## SECURITY CONSIDERATIONS
- **Less C:** three C functions handling untrusted input (stdin text, file bytes) are replaced by bounds-checked Lugha. No `unsafe`, no dependencies.
- **The new built-in:** `$string.from_bytes` is reachable only from bundled sources, and only after validation.

## TESTS TO WRITE
- [ ] **Differential program `m62/decode.la`:** parser edge cases; `string_from_utf8` on every 2-byte pattern and on chosen 3- and 4-byte sequences (valid, overlong, surrogate, too high, truncated); `chars()` on the same bytes with U+FFFD. Golden from M61.
- [ ] **Checker:** `$string.from_bytes` in a program is E0101.
- [ ] **Link:** `RUNTIME_SYMBOLS` no longer lists the three; a test that the runtime source doesn't define them.
- [ ] **Regression:** every earlier program unchanged.

## ROLLBACK PLAN
- Branch `prp-091-c_to_lugha`; tag `m62`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`; `m62/` at `-O2`.
- [ ] Spec at milestone 62; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
```
