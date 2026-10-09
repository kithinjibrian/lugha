## FEATURE: A lexer that turns `.la` source text into tokens with byte spans, reporting every lexical error with a stable code.

**Status:** approved 2026-10-09 — session 5 (E0109 confirmed)
**Milestone:** 1 (first pipeline stage)
**Spec:** §2 (lexical structure), §9 (diagnostics, error codes)
**Decisions:** DECISION-002 (stage return type), DECISION-001 (library layout)

## OBJECTIVE
`lugha::lexer::lex(source)` returns every token of a valid `.la` file, each with its byte span, ending in `Eof`. For invalid input it returns every lexical error it can find, each a `Diagnostic` with a code (E0101–E0109), message, span and, where useful, help. Nothing is printed — rendering belongs to the driver.

## CONTEXT

- Starting state: `src/lib.rs` (doc only), `src/main.rs` (empty `main`). Test runner from PRP-001 exists.
- Ending state: `src/span.rs`, `src/diagnostic.rs`, `src/lexer/{mod,token,number,string}.rs`, `tests/lexer.rs` created; `src/lib.rs` declares the modules; spec §2 and CLAUDE.md rule 9 amended.
- Related existing code: none in `src/` yet.
- Related source files: spec §2, §9.
- Open decisions that must be resolved first: none.

### Discovery answers (session 5)
1. Scope: the full §2 lexer now. CLAUDE.md rule 9 amended: lexer and parser cover the full §2–§3 syntax; checker and codegen follow the milestone subset.
2. Diagnostics: full record now (severity, code, message, span, labels, help), no rendering.
3. Numbers: strict, spec letter (details below).
4. Strings: no raw newlines; the lexer keeps going after errors.
5. Error codes: eight specific codes, plus E0109 added in the draft (float out of range) — confirm on approval.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Span** — `src/span.rs`
- `Span { start: usize, end: usize }`, byte offsets, `end` exclusive. `Copy`, `Eq`, `Debug`.

**Diagnostic** — `src/diagnostic.rs`
- `Severity { Error, Warning }`.
- `Label { span: Span, message: String }`.
- `Diagnostic { severity, code: &'static str, message: String, span: Span, labels: Vec<Label>, help: Option<String> }` — the fields of the spec §9 JSON.
- Constructors `Diagnostic::error(code, message, span)`, `Diagnostic::warning(...)`, and `with_label(span, message)` / `with_help(text)` builders.
- No printing, no line/column computation (the renderer derives those from spans).

**Tokens** — `src/lexer/token.rs`
- `Token { kind: TokenKind, span: Span }`.
- `TokenKind`:
  - Literals: `Int(u64)`, `Float(f64)`, `Str(String)` (escapes decoded), `Ident(String)`.
  - One variant per keyword in §2, including `True`, `False` and the type names `I32`, `I64`, `U8`, `F64`, `Bool`, `String`.
  - One variant per operator and punctuation token in §2.
  - `Eof`, always the last token, with an empty span at the end of the source.
- `TokenKind` derives `PartialEq` (not `Eq`, because of `f64`).

**Lexing** — `src/lexer/mod.rs`
- `pub fn lex(source: &str) -> Result<(Vec<Token>, Vec<Diagnostic>), Vec<Diagnostic>>`. `Ok` = tokens + warnings (none today), `Err` = every diagnostic found. Never panics on any input.
- Whitespace: space, tab, `\r`, `\n` skipped.
- `//` comments run to the end of the line and may contain any UTF-8.
- Identifiers: `[A-Za-z_][A-Za-z0-9_]*`; keywords are recognised by exact, case-sensitive match.
- Operators by longest match: `<=` `>=` `==` `!=` `&&` `||` `+=` `-=` `*=` `/=` `..` before their one-character prefixes.
- A lone `&` or `|` is E0101.

**Numbers** — `src/lexer/number.rs`
- Decimal integer: digits with `_` only between two digits. Leading zeros allowed (`007` is 7).
- Hex integer: lowercase `0x`, at least one hex digit (either case), `_` only between two hex digits.
- Integer values are stored as `u64`. Range checks per type belong to the checker (§4 rules 5–6), except that over `u64::MAX` is E0104.
- Float: digits `.` digits, optional exponent `e`/`E`, optional sign, then digits. No `_` anywhere in a float.
  - A `.` not followed by a digit ends the integer: `1..10` → `1` `..` `10`; `1.x` → `1` `.` `x`.
  - Parsed with Rust's `str::parse::<f64>` (correctly rounded).
  - A finite literal that rounds to infinity is E0109.
- Errors:
  - E0105 misplaced `_`: `1__0`, `1_`, `0x_FF`, `1_0.5`.
  - E0106 missing digits: `0x`, `2.0e`, `2.0e+`.
  - E0107 float needs a fractional part: an integer immediately followed by an exponent (`2e5`, `2E-3`). Help: `write 2.0e5`.
  - E0108 invalid character in number: an identifier character directly after a number (`123abc`, `0XFF`, `0xFG`, `1.5x`).

**Strings** — `src/lexer/string.rs`
- Double-quoted. Escapes `\n` `\t` `\r` `\\` `\"` `\0`.
- Any other UTF-8 is allowed inside.
- A raw newline, or end of input, before the closing `"` is E0102 "unterminated string". Its span runs from the opening `"` to the end of that line. Lexing resumes on the next line.
- `\` followed by any other character is E0103, with a span covering the two characters. Lexing continues inside the string.

**Other errors**
- E0101 unexpected character: any character that can't start a token, including non-ASCII outside strings and comments. The span covers the whole UTF-8 character; lexing resumes after it.

**Error recovery**
- After every error the lexer skips the offending text and keeps going, so one run reports every lexical error.
- Each number error consumes the whole number-like run (`[0-9A-Za-z_]` plus `.digit` parts), so `123abc` produces one error, not two.

**Spec and rule updates**
- Spec §2: write in the number, string and non-ASCII rules above, and a table of lexical error codes E0101–E0109.
- CLAUDE.md architecture rule 9: the lexer and parser cover the full §2–§3 syntax; the checker and codegen follow the milestone subset.

**Structure** (each file under 300 lines, unit tests at the bottom of each)
- `src/span.rs`, `src/diagnostic.rs`
- `src/lexer/mod.rs` — `lex`, the main loop, identifiers, punctuation, comments.
- `src/lexer/token.rs` — `Token`, `TokenKind`, the keyword table.
- `src/lexer/number.rs`, `src/lexer/string.rs`
- `tests/lexer.rs` — public-API tests, including lexing every spec §10 program without errors.
- `src/lib.rs` — `pub mod span; pub mod diagnostic; pub mod lexer;`

### Must NOT Do
- Do not print or render diagnostics. That belongs to the driver (architecture rule 7).
- Do not check whether an integer fits `i32`, `i64` or `u8`, and do not fold `-` into literals. Both belong to the checker (§4).
- Do not add `--emit=tokens`. That belongs to the driver PRP.
- Do not add dependencies.
- Do not change the parser-facing design beyond `Token` and `TokenKind`. No interning or arenas yet.
- Do not touch `tests/support/` or `tests/programs/`.

## ERROR HANDLING REQUIREMENTS

- `lex` returns `Err(diagnostics)` if there is at least one error. All diagnostics are `Severity::Error` with codes E0101–E0109.
- `lex` never panics. No `unwrap` or indexing on byte positions that could split a UTF-8 character; iterate with `char_indices` or check `is_char_boundary`.
- Internal invariants (e.g. "a digit was just matched") may use `expect("…invariant…")`.

## SECURITY CONSIDERATIONS

- Input is untrusted source text of any size.
  - No recursion, so deep nesting can't overflow the stack.
  - Work is linear in the input length.
  - No panics on malformed UTF-8 boundaries. The input is `&str`, so it is already valid UTF-8, and the driver handles invalid files.
- No `unsafe`.

## TESTS TO WRITE

Unit tests (module bottoms):
- [ ] span/diagnostic: builders set label and help; `error` sets `Severity::Error`.
- [ ] Every keyword maps to its variant; `Fun` vs `fun_x` (identifier); case-sensitive (`Fun` is an identifier).
- [ ] Longest match for each two-character operator; `a..b` → ident `..` ident; `1..10`.
- [ ] Comments: `// x` to end of line skipped; `//` at EOF; UTF-8 inside comments.
- [ ] Integers: `0`, `42`, `007`, `1_000_000`, `0xFF`, `0xff`, `0xDEAD_BEEF`, `18446744073709551615`.
- [ ] Floats: `3.14`, `2.0e-3`, `2.0E5`, `1.5e+2`, `0.0`.
- [ ] E0104: `18446744073709551616`. E0109: `1.0e999`.
- [ ] E0105: `1__0`, `1_`, `0x_FF`, `1_0.5`, `0xF__F`.
- [ ] E0106: `0x`, `2.0e`, `2.0e+`.
- [ ] E0107: `2e5` with help `write 2.0e5`; `2E-3`.
- [ ] E0108: `123abc`, `0XFF`, `0xFG`, `1.5x` — one diagnostic each.
- [ ] Strings: `""`, `"hello\n"` decodes, every escape, `"héllo 👋"`.
- [ ] E0102: `"abc` at EOF; `"abc⏎let x` — span ends at end of line 1, `let x` still lexed.
- [ ] E0103: `"\q"` — span is the two characters `\q`; lexing continues.
- [ ] E0101: `@`, `#`, `é` outside a string (span covers 2 bytes), lone `&`, lone `|`.
- [ ] Recovery: a source with three separate errors reports exactly three diagnostics, in source order.
- [ ] Spans: every token's span slices back to its source text; `Eof` span is `len..len`.

Integration (`tests/lexer.rs`):
- [ ] Each spec §10 program and the §11 milestone 2 program lexes with no diagnostics, ending in `Eof`.
- [ ] `fun main(): i32 { 2 + 3 * 4 }` gives exactly the expected token kinds.
- [ ] The §10 rejected program lexes cleanly (its error belongs to the checker).

## ROLLBACK PLAN

- Branch `prp-002-lexer`, merged into `main` on acceptance.
- To abandon: delete the branch. No migrations, no runtime state.

## ACCEPTANCE CRITERIA
- [ ] Every test above exists and passes.
- [ ] `lex` never panics. Covered by a test that lexes every prefix of a mixed sample source.
- [ ] Spec §2 updated with the clarified rules and the E0101–E0109 table.
- [ ] CLAUDE.md rule 9 amended; FILE ORGANIZATION lists the new modules.
- [ ] No file over 300 lines; no new dependencies.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.
- [ ] Every `pub` item has a doc comment; functions returning `Result` document `# Errors`.
- [ ] CHANGELOG.md updated.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo test -- --ignored` still shows only `m1/arith.la: exit code` (lexer alone doesn't change lughac's behaviour).
