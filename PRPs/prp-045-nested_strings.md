## FEATURE: String literals inside interpolations — Lugha v4.1.

**Status:** approved 2026-10-13 — session 56
**Milestone:** none (a point release, like v1.1 `swap` and v1.2 E0510); tag `v4.1`
**Spec:** `Language v4 Specification.md` §2 "String interpolation" and the E0111 row (amended below).
**Decisions:** MEMORY 45 (interpolation lexing), MEMORY 52 (v4 complete). The friction was found in sessions 48, 54 and 55: test programs hit E0111 three times.

## OBJECTIVE
An interpolation may contain any expression on its line, including string literals and nested interpolated strings:

```
println("<{parts.join("-")}>");
println("{if ok { "yes" } else { "no" }}");
println("{name}: {"{a}/{b}"}");
```

E0111 keeps only real mistakes: a lone `}`, a `{` that is never closed, and an empty `{}`.

## CONTEXT

- **Starting state:**
  - The lexer tracks one interpolation at a time (`Lexer.interp: Option<Interp>`).
  - A `"` inside `{…}` is E0111: "a string literal can't appear inside `{…}`", found by a heuristic that looks for another `"` on the line.
- **Ending state:**
  - Interpolations nest: the lexer keeps a stack, and a `"` inside `{…}` starts an ordinary string, which may itself interpolate.
  - The heuristic and that E0111 case are gone.
- **Related existing code:** `src/lexer/interp.rs`, `src/lexer/mod.rs`; the spec; `tests/programs/m13/`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 56)
1. **Scope:** a v4.1 point release that allows string literals and nested interpolations inside `{…}`, rather than starting v5.

### Design choices for review
- **The lexer keeps a stack of open interpolations.**
  - Inside an interpolation, `"` starts an ordinary string literal, and its own `{` pushes another interpolation.
  - `{` and `}` count braces for the innermost interpolation only. Its closing `}` pops it and resumes the enclosing string's text.
- **Strings stay on one line:** a line break inside an interpolation still ends things. If the innermost open string started inside an interpolation, it is E0111 "`{` in a string is never closed", at that interpolation's `{`, since the missing `}` is the likely mistake. Otherwise it is E0102 as today.
- **`"{x"`** (an unclosed `{`) is still E0111 at the `{`. The `"` after `x` starts a nested string that runs to the end of the line, and that unclosed nested string is reported as the outer `{` never closed.
- **Remaining E0111 cases:** a lone `}`, an unclosed `{`, and an empty `{}` (unchanged).
- **The token stream:** a nested string appears as its own `Str` or `StrStart…StrEnd` tokens among the interpolation's tokens, so the parser needs no change.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Lexer:**
  - `Lexer.interp` becomes a stack;
  - a `"` inside an interpolation lexes a nested string;
  - remove the heuristic (`quote_in_interpolation`) and its E0111 case;
  - unclosed nesting gets the reporting above.
- **Spec** (`Language v4 Specification.md`):
  - §2 "String interpolation": the string-literal ban is removed, and nesting is described;
  - the E0111 row lists the remaining cases;
  - the status paragraph gains "v4.1 allows string literals inside interpolations".
- **Docs:** CHANGELOG ("Lugha v4.1"), MEMORY decision 53, CONTEXT, TODO; tag `v4.1`.

### Must NOT Do
- **No multi-line strings or interpolations,** and no format specifiers.
- **No change to how interpolated values are formatted or typed.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Every mistake is a lexer `Diagnostic`** (E0111, E0102), and lexing continues after it.

## SECURITY CONSIDERATIONS
- **Nesting depth** is bounded by the line's length: each level needs at least `"{`. The parser's existing nesting limit (E0206) still guards its recursion.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — lexer (`interp.rs`):
- [x] **A string literal inside:** `"a{f("s")}b"` gives `StrStart("a")`, `Ident(f)`, `LParen`, `Str("s")`, `RParen`, `StrEnd("b")`.
- [x] **A nested interpolation:** `"{"{x}"}"` gives `StrStart("")`, `StrStart("")`, `Ident(x)`, `StrEnd("")`, `StrEnd("")`.
- [x] **Braces inside a nested string's expression:** `"{if c { "y" } else { "n" }}"` lexes completely.
- [x] **E0111** still for `"a } b"`, `"{x"` (at the `{`) and `"{}"`; `"{ "a" "` (unclosed, at the `{`).
- [x] **A line break inside a nested string** is E0111 at the outer `{`.

Unit tests — parser:
- [x] `"<{xs.join("-")}>"` parses to `(interp "<" (call (. xs join) "-") ">")`.

Acceptance — `tests/programs/v4_1/`:
- [x] `nested.la`: the three examples above, plus a nested interpolation two levels deep. Run at `-O0` and `-O2`.
- [x] `e0111.la`: reject golden (a `{` never closed).

Regression:
- [x] Every earlier test passes. The `m13/e0111` golden (a lone `}`) is unchanged.

## ROLLBACK PLAN

- Branch `prp-045-nested_strings`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The examples above compile and run.
- [x] Only the three real E0111 cases remain.
- [x] Spec and docs updated; tag `v4.1` at merge.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/v4_1/nested.la
```

## AMENDMENTS (during build, session 56)

- **Where E0111 points:** a string that runs past its line while an interpolation is open is reported at the innermost open `{` (the one containing that string), and every open interpolation ends there.
- **A line break in interpolation code** (`"{x` then a newline) pops that interpolation first, so it stays E0102 at top level, as before; it is E0111 only when an outer interpolation is still open.
- **`nested.la`** also covers escapes and `{{…}}` inside a nested string, and a nested `", "` separator.
- **No file splits;** `interp.rs` stays under 300 lines.
