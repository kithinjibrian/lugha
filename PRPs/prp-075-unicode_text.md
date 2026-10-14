## FEATURE: Unicode text — `char`, `'a'` literals, `s.chars()`, `char_count()`, casts, `char_from`, classification and case (E0112); milestone 46.

**Status:** approved 2026-10-14 — session 97. This is part of the user's "in one go" (milestones 45–49), merged once CI is green.
**Milestone:** 46 (one PRP; tag `m46` at merge)
**Spec:** `Language v14 Draft.md`:
- decision 6, and the choices on `char`;
- §2 (the `char` keyword and literals);
- §3;
- §9 (`i32`, runtime);
- E0112;
- §11 (milestone 46).

**Decisions:** MEMORY 91 (v14), MEMORY 84 (printing).

## OBJECTIVE
The §10 program counts characters in `"naïve café"` (10 characters, 12 bytes), upper-cases a Latin-1 string, prints code points and quoted `char`s inside an array, and rejects an invalid code point with `char_from`.

## CONTEXT

- **Starting state:**
  - strings are UTF-8 bytes, and `for b of s` yields `u8`;
  - `'` is an unexpected character (E0101).
- **Ending state:** a `char` type and its operations.
- **Related existing code:**
  - `src/lexer/{mod,token}.rs`, `src/parser/{primary,item,describe}.rs`, `src/parser/sexp`;
  - `src/ast/{expr,types}.rs`;
  - `src/check/{types,numbers,literal,expr,ops,cast,helpers,bounds,printing,call}.rs`;
  - `src/codegen/{value,structs,expr,cast,runtime,input,show}.rs`;
  - `runtime/`, `src/link.rs`, the prelude.
- **Open decisions that must be resolved first:** none.

### Design
- **Lexer** (new `lexer/char_lit.rs`):
  - `'…'` lexes to `TokenKind::Char(u32)`, with escapes `\n \t \r \0 \\ \' \"` and `\u{H…}` (1–6 hex digits);
  - empty, more than one character, a bad escape or a non-scalar code is E0112;
  - the `char` keyword is `TyChar`.
- **AST and parser:** `ExprKind::Char(u32)`, `TypeKind::Char`, sexp `'a'` (as Rust's `{:?}`).
- **Checker:**
  - `Type::Char`, printed `char`;
  - a `char` literal is `char`;
  - `==`/`!=` and `<`… compare code points, and `char` is `Ord` (an `ops.rs` line and a `bounds.rs` line);
  - no arithmetic;
  - **casts:** `char as` any integer, and any integer `as char`;
  - **prints:** `char` prints as a plain value.
- **Helpers:**
  - `helper_function` maps a `char` receiver's `is_alphabetic`, `is_digit`, `is_whitespace`, `is_upper`, `is_lower`, `to_upper` and `to_lower` to prelude `char_<name>`, and a string's `chars` and `char_count` to `str_chars` and `str_char_count`;
  - `char_from` is a prelude function visible like `range` (`call.rs`'s list of bare prelude names).
- **Prelude** (new fourth file `src/prelude_text.la`):
  - `struct Chars { s, at }` with a strict UTF-8 decoding `next` (U+FFFD and one byte forward on any invalid sequence);
  - `str_chars`, `str_char_count`, `char_from(n: u32): Option<char>`;
  - the classification and case functions over ASCII and Latin-1 (U+00C0–U+00DE ↔ U+00E0–U+00FE, except U+00D7 and U+00F7; `ÿ` (U+00FF) has no upper case here).
- **Codegen:**
  - `char` is LLVM `i32` (layout 4, 4), compared unsigned;
  - `char → int` casts like `u32`;
  - `int → char` widens to `i64` by signedness and selects the value, or U+FFFD unless it is in 0–0xD7FF or 0xE000–0x10FFFF;
  - **printing:** `print`/`to_string`/interpolation of a `char` go through `lugha_rt_print_char` and `lugha_rt_to_string_char` (UTF-8 encoding);
  - **inside built-in forms:** `lugha_rt_char_quote` gives `'a'`, `'\n'`, `'\''`;
  - format specifiers pad its text.
- **Runtime:** a new fragment `runtime/chars.c` with the three functions; `RUNTIME_SYMBOLS` grows to 46.
- **Programs without `char`** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - §2 keywords and char literals;
  - §3;
  - §4 (the types table, `char`, casts, order);
  - §5 (print table, string and char methods);
  - §9 and the runtime table;
  - the E0112 row;
  - the §10 program;
  - §11;
  - the changes summary and compatibility (`char` keyword).
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 93, CONTEXT. The tag `m46` is added at the merge.

### Must NOT Do
- **No full Unicode tables, grapheme clusters, `char` arithmetic, char-width formatting, or `char` literal patterns.** Literal patterns go to §12.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0112:** `''`, `'ab'`, `'\q'`, `'\u{D800}'`, `'\u{110000}'`, `'\u{}'`, and an unclosed `'a`.
- **E0404:** arithmetic on `char`.
- **E0408:** casting `char` to a float or `bool`.
- **At run time:** decoding never fails (U+FFFD), and `as char` never fails (U+FFFD).

## SECURITY CONSIDERATIONS
- **The UTF-8 decoder** is strict and never reads past `len`.
- **Encoding** writes at most 4 bytes into a sized buffer.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — lexer: every escape, multi-byte characters, and each E0112.

Unit tests — checker:
- [x] Literal type, comparisons, casts, and `Ord`.
- [x] E0404 for `'a' + 1`; E0408 for `'a' as f64`.
- [x] The helper types (`chars()` yields `char`, `to_upper()` gives `char`, `char_from` gives `Option<char>`).

Unit tests — codegen: `as char` selects U+FFFD; `print` of a `char` calls `lugha_rt_print_char`.

Acceptance — `tests/programs/m46/`:
- [x] `text.la`: the §10 program.
- [x] `more.la`:
  - invalid bytes from `read_line` decoded as U+FFFD (via `.stdin`);
  - surrogates and the maximum through `as char`;
  - `char_from` edges;
  - classification of `'ß'`, `'ÿ'`, digits and whitespace;
  - format padding of a `char`;
  - `char` in a struct and an array, printed;
  - `match` on `char` through `if`/`==`.
- [x] `e0112.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-075-unicode_text`, merged into `main` after CI passes; tag `m46`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m46/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The spec is at milestone 46; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m46/text.la
```

## AMENDMENTS (during build, session 97)

- **`char_from` is a bare prelude name like `range`,** shadowable by a program's own function, rather than a reserved intrinsic as the draft's compatibility list said. That is less breaking; the spec says so.
- **`char` compares unsigned** (it isn't in `is_signed`); casts from `char` behave like `u32`.
- **Format widths for `char`** count bytes, as for strings (`{'é':^5}` pads three bytes).
- **IR:** the 161 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for both `m46` run programs.
- **No file splits:** the new code is in new files (`lexer/char_lit.rs`, `src/prelude_text.la`, `runtime/chars.c`).
