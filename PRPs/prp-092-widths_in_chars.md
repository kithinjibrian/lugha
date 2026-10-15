## FEATURE: Format widths in characters — `{name:<8}` pads by Unicode scalar values, not bytes, and a fill may be any `char`; milestone 63.

**Status:** approved 2026-10-15 — session 129 ("approved"); proposed in session 128 (discovery in session 127).
**Milestone:** 63 (one PRP; tag `m63` at merge)
**Spec:** `Language v15 Draft.md` decision 47; spec §4/§5 (format specifiers), §9 (`lugha_rt_pad`), §11 (row 63).

**Decisions:**
- the v14 TODO's Unicode follow-up;
- the user's ruling (session 127): char classification and case stay Latin-1, and §11's "full Unicode tables" exclusion stands;
- no open entries in DECISIONS.md apply.

## OBJECTIVE
`"{n:<6}|"` with `n = "Zoë"` prints `Zoë   |` (3 characters plus 3 spaces), not `Zoë  |`. Width and fill count characters everywhere a format specifier pads: interpolation and `format`, for strings, chars and any printed value.

## CONTEXT
- **Starting state:**
  - `runtime/format.c::pad_bytes` counts bytes;
  - a fill is passed as an `i32` code point, so the lexer may already accept a non-ASCII fill.
- **Ending state:** decision 47.
- **Related existing code:**
  - `runtime/format.c` (`lugha_rt_pad`, `pad_bytes`, `pad_number`);
  - `src/format.rs` (specifier parsing), `src/codegen/format.rs`, `src/lexer/format_spec.rs`;
  - `tests/programs/m44` (format specifiers) and `m48` (`format`).
- **Open decisions that must be resolved first:** none.

### Design
1. **Width:** the width compares with the number of UTF-8 scalar values in the text: bytes that aren't continuation bytes (`(b & 0xC0) != 0x80`). Strings are valid UTF-8, so this is exact.
2. **Fill:** written as its UTF-8 encoding, 1–4 bytes per padding position. The output buffer is sized for the padding × the fill's encoded length.
3. **Numbers:** `pad_number` is ASCII already; zero padding is unchanged.
4. **Lexer and parser:** a non-ASCII fill (`{x:é^9}`) is accepted if it isn't already. Any `char` other than `{` and `}` may fill.

### Risk
- **Output changes** for programs that pad non-ASCII text with a width: the intended fix. Existing goldens that do so are updated, and each is listed in the amendments.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec v15:**
  - §4/§5: the format-specifier paragraph says widths count characters, and the fill is any `char`;
  - §11: row 63; the changes summary (a visible change for non-ASCII padding).
- **Docs:** README, TODO (mark the widths part done; record the tables ruling), CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m63`.

### Must NOT Do
- **No case or classification tables beyond Latin-1** (§11).
- **No change for ASCII text.**
- **No East Asian width or grapheme clustering:** a scalar value counts as one, and the spec says so.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Same as today:** a malformed specifier is still E0431 (interpolation) or E0432 (`format`).

## SECURITY CONSIDERATIONS
- **The C buffer arithmetic changes:** each size computation keeps an overflow check (huge widths still `die` with out of memory, as now). No `unsafe`, no dependencies.

## TESTS TO WRITE
- [ ] **Program `m63/widths.la`:**
  - `<`, `>` and `^` with `é`, `ñ`, `日`, an emoji, mixed text, a non-ASCII fill, and widths smaller than the text;
  - `format(…)`, and a char and a struct printed with a width.

  Expected output written by hand (character counts).
- [ ] **Unit (`src/format.rs`):** a non-ASCII fill parses.
- [ ] **Regression:** every earlier program; any golden that changes is listed.

## ROLLBACK PLAN
- Branch `prp-092-widths_in_chars`; tag `m63`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`; `m63/` at `-O2`.
- [ ] Spec at milestone 63; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
```
