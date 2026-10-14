## FEATURE: Format specifiers — `"{x:[[fill]align][#][0][width][.precision][kind]}"` (E0431); milestone 44, v13 complete.

**Status:** approved 2026-10-14 — session 94. This is the last part of the user's "in one go" (milestones 41–44), merged once CI is green.
**Milestone:** 44 (one PRP; completes v13; tags `m44` and `v13` at merge)
**Spec:** `Language v13 Draft.md`:
- decision 2, and the choices on format specifiers;
- §3 (`interpolated`, `FORMAT`);
- §5;
- §9;
- E0431;
- §10 "Format specifiers";
- §11 (milestone 44);
- §12.

**Decisions:** MEMORY 86 (v13), MEMORY 84 (printing text).

## OBJECTIVE
The §10 program prints `3.14`, `|    42|`, `|42    |`, `-0042`, `0xff`, `***ann***`, `11111111`.

## CONTEXT

- **Starting state:**
  - an interpolation `{expr}` is lexed into `StrStart`/`StrMid`/`StrEnd` around the expression's tokens (`lexer/interp.rs`), with only brace depth tracked;
  - it is parsed into `Part::Expr(Expr)` (`parser/primary.rs`);
  - it is printed by `text` (`codegen/input.rs`, `codegen/show.rs`).
- **Ending state:** an optional specifier after a top-level `:`; v13 is complete.
- **Related existing code:**
  - `src/lexer/{interp,token,mod}.rs`, `src/ast/expr.rs`;
  - `src/parser/{primary,describe}.rs`, `src/parser/sexp/mod.rs`;
  - `src/check/input.rs`, `src/codegen/input.rs`;
  - `runtime/`, `src/link.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **Lexer:**
  - `Interp` also counts open `(` and `[`;
  - in an interpolation at depth 0, a `:` starts the specifier: its raw text up to the next `}` (or the line's end) becomes one token, `FmtSpec(String)`, and the `}` then closes the interpolation as before;
  - an empty specifier is still a token: E0431 comes from the checker.
- **AST and parser:**
  - `Part::Expr(Expr, Option<FormatText>)`, where `FormatText { text, span }`;
  - the parser takes a `FmtSpec` after the expression;
  - the sexp is `(fmt e "spec")`.
- **`src/format.rs`** (new, shared by the checker and codegen, like `span.rs`):
  - `FormatSpec { fill: u8, align: Option<Align>, alternate, zero, width: u32, precision: Option<u32>, kind: Option<Kind> }`;
  - `parse(text) -> Result<FormatSpec, String>`, which rejects an empty spec, a non-ASCII or brace fill, a width or precision over 1024, and trailing text;
  - `check(&spec, Value::{Int, Float, Other}) -> Result<(), String>`, which rejects precision on non-floats, a kind on non-integers, `#` without a kind, and `0` on non-numbers.
- **Checker** (`check/input.rs`): each part's specifier is parsed and checked against the part's type, and every failure is E0431 at the specifier's span (constructor in `errors/v13.rs`).
- **Codegen** (`codegen/input.rs` → new `codegen/format.rs`):
  - an integer calls `lugha_rt_fmt_int(value as i64 (sign- or zero-extended), bits, signed, kind, alternate, fill, align, width, zero)`;
  - a float calls `lugha_rt_fmt_float(value as f64, is_f32, precision or -1, fill, align, width, zero)`;
  - anything else calls `lugha_rt_pad(text(value), fill, align, width)`;
  - the alignment is resolved in codegen: 0 left, 1 right, 2 centre, defaulting to right for numbers.
- **Runtime:** a new fragment `runtime/format.c` with the three functions:
  - integers: two's complement at `bits` for kinds; `0x`/`0b`/`0o` with `#`;
  - floats: `%.*f`, sized by a first `snprintf`;
  - zero padding: after the sign and prefix.

  `RUNTIME_PARTS` gains the fragment, and `RUNTIME_SYMBOLS` grows to 38.
- **Programs without specifiers** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - §3 grammar;
  - §5 "Format specifiers";
  - §9 and the runtime table;
  - the E0431 row;
  - the §10 program;
  - §11 row 44 done;
  - the title and status say v13 is complete;
  - §12.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 90, CONTEXT. The tags `m44` and `v13` are added at the merge.

### Must NOT Do
- **No `format()` function, sign flag (`+`) or named or positional arguments.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0431:**
  - an unknown kind;
  - precision on an integer or a non-number;
  - a kind on a float or a non-number;
  - `#` without a kind;
  - `0` on a non-number;
  - a width or precision over 1024;
  - a non-ASCII or brace fill;
  - an empty spec;
  - trailing text.

## SECURITY CONSIDERATIONS
- **Bounded output:** widths and precisions are bounded (1024) at compile time.
- **Buffers:** the runtime sizes every buffer from `snprintf`'s count or the known width, with no fixed buffer for `%.*f`. Fill is one ASCII byte.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — `src/format.rs`:
- [x] Parsing: every part, defaults, and each error.
- [x] `check`: precision, kind, `#` and `0` against the three value classes.

Unit tests — lexer: `"{x:>5}"` lexes `StrStart`, `x`, `FmtSpec(">5")`, `StrEnd`; `"{f(a: 1)}"`-style colons inside parentheses don't start a spec.

Unit tests — parser: the sexp `(fmt x ">5")`.

Unit tests — checker: E0431 for `{n:.2}` (int), `{f:x}`, `{s:05}`, `{n:#}`, `{n:q}`, `{n:2000}`; a struct with `{p:>20}` checks.

Acceptance — `tests/programs/m44/`:
- [x] `format.la`: the §10 program.
- [x] `more.la`:
  - every alignment with fill;
  - zero-padding of negatives and `#x`;
  - `X`, `o` and `b` on several widths;
  - `u64.MAX` in hex;
  - `f32` precision;
  - a struct padded;
  - a value longer than its width;
  - a specifier on a string literal;
  - nested interpolations.
- [x] `e0431.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-073-format_specifiers`, merged into `main` after CI passes; tags `m44` and `v13`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m44/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The v13 spec is complete; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m44/format.la
```

## AMENDMENTS (during build, session 94)

- **The lexer's specifier code** is in a new file, `src/lexer/format_spec.rs`: adding it to `lexer/interp.rs` took that file to 326 lines. `interp.rs` keeps only the `parens` count and the `:` arm.
- **`RUNTIME_SOURCE`** (a separate `concat!` in `link.rs`) also needed `format.c`, besides `RUNTIME_PARTS`; the first link failed without it.
- **`0` is ignored when an alignment is given,** as the draft says: codegen passes `zero` only without one. `m44/more` checks `{7:>05}`.
- **The checker's specifier tests** live in `check/input.rs`, and the parser's in `parser/conditional.rs`. No codegen unit test; the programs cover it.
- **IR:** the 155 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for both `m44` programs.
- **No file splits.**
