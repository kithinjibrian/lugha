## FEATURE: Octal integer literals `0o600`, and a warning for a leading zero that looks octal; milestone 117 (v17).

**Status:** approved 2026-10-18 (session 282); proposed in session 281, from the user's request ("Real 0o600 literals: today 0600 silently means six hundred"), after `fs.set_mode` (milestone 115).

## OBJECTIVE
```lugha
fs.set_mode("key.pem", 0o600)?;     // 384: owner reads and writes
let x = 0o7_55;                     // underscores between digits, as 0x and 0b allow
let y = 0600;                       // warning W0107: a leading zero doesn't make octal — 0o600 or 600
```

## CONTEXT
- The lexer reads decimal, `0x…` and `0b…` (`src/lexer/number.rs`, milestone 27); a leading zero is decimal (`007` is 7, `0600` is 600). Integer literals take their type from context; out-of-range literals are already errors (E0102, by the type's range).
- The formatter prints literals as written, so `0o600` keeps its spelling.
- Related: `src/lexer/number.rs`, its tests, spec §2 (literals), the VS Code grammar (numbers).

## DECISIONS (draft decisions 231–236)
231. **`0o` literals**: `0o` then octal digits (`0`–`7`), with `_` only between digits, as `0x`/`0b` take them; a value like any integer literal (its type from context; past `u64` an error as for hex). `0o` with no digits, or a digit 8/9, is E0102's existing malformed-number error (or a new message under it).
232. **Only lower-case `0o`**, as only lower-case `0x`/`0b` are read (no `0O`).
233. **A leading zero** (`0600`, `007`, `00`; not `0`, not `0.5`, not `0x…`): **warning W0107** (new): "a leading zero doesn't make an octal number: this is 600" with help "write `0o600` for octal, or `600`". A warning, not an error, so no existing program breaks. (The alternative — making it an error, like Python 3 and Rust's lint-free stance — would break programs such as the lexer's own test of `007`; a warning catches the mistake without that.)
234. **Floats** are unaffected (`00.5` is a float as today, no warning; `0o` is never a float).
235. **Editor**: the VS Code grammar highlights `0o…` as a number (version bump).
236. **Success**: milestone 117, tag `m117`; spec §2 and the error table (W0107), its page; CHANGELOG.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Lexer: the `0o` radix; the leading-zero warning returned with the lexer's warnings (`Diagnostic::warning`, its span the literal).
- W0107's page (`docs/errors/W0107.md`, registered in `src/explain.rs`); spec §2 and the table; VS Code grammar and test; `fs.set_mode`'s docs and the spec's `std.fs` row switch to `0o600`.
### Must NOT Do
- Making a leading zero an error; `0O`; octal floats or escapes.
### New dependencies
- none.

## BUILT (session 282)
- As planned; malformed `0o` literals reuse the lexer's existing hex/binary errors (E0106 for no digits, the invalid-character and underscore errors).
- Tests: lexer units (`0o600`, underscores, `0o0`, `u64::MAX`, five malformed forms; W0107 for `0600`, `007`, `00` with their help, none for `0`, `0.5`, `00.5`, `0x10`, `0b10`, `0o10`, `10`), `fs_time` (now `0o600`), VS Code `octal.la`, `explain` (W0107's page); `programs` and `spec` unchanged (no existing program warns).

## ERROR HANDLING REQUIREMENTS
- E0102 for a malformed `0o` literal; W0107 for a leading zero.

## SECURITY CONSIDERATIONS
- None.

## TESTS TO WRITE
- [ ] Lexer units: `0o600` is 384, `0o7_55`, `0o0`, `0o17777777777777777777777` (u64::MAX region) and one past it; `0o`, `0o8`, `0o_7`, `0o7_` rejected; `0600`, `007`, `00` warn W0107 and keep their decimal values; `0`, `0.5`, `00.5`, `0x10`, `0b10` don't.
- [ ] A program using `0o600` with `fs.set_mode`; `lughac check` shows W0107 for `0600`; the formatter keeps `0o600`; VS Code grammar test; `explain` (the page).

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] `0o600` works everywhere an integer literal does; narrow tests (lexer units, `lexer`, `explain`, `fmt_corpus`, `programs`, `npm test`), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --lib -- lexer`, `cargo test --test lexer --test explain --test fmt_corpus`, `npm test`, fmt, clippy
