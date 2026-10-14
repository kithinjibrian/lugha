## FEATURE: `format` — `format("{} has {:>4} items", name, n)` returns a `string` from a template literal (E0432); milestone 48.

**Status:** merged 2026-10-14 — session 99. This is part of the user's "in one go" (milestones 45–49), merged once CI is green.
**Milestone:** 48 (one PRP; tag `m48` at merge)
**Spec:** `Language v14 Draft.md`:
- decision 2, and the choices on `format`;
- §2 (a string literal right after `format` `(`);
- §9 (lowering);
- E0432;
- §11 (milestone 48).

**Decisions:** MEMORY 91 (v14), MEMORY 90 (format specifiers), MEMORY 85 (printing).

## OBJECTIVE
The §10 program prints `println(format("{} has {:>4} items", name, n))` and a few more templates: literal braces, every specifier kind, a struct argument, and a `format` inside an interpolation.

## CONTEXT

- **Starting state:**
  - `"{x:spec}"` interpolations format values (v13);
  - there is no way to keep a template apart from its values.
- **Ending state:** `format` is a reserved intrinsic taking a literal template and any number of printable values.
- **Related existing code:**
  - **Lexer:** `src/lexer/{string,interp}.rs`.
  - **AST, parser:** `src/ast/expr.rs`, `src/parser/primary.rs`, `src/parser/sexp`.
  - **Checker:** `src/check/{input,call,expr}.rs`, `src/check/liveness/walk.rs`.
  - **Codegen:** `src/codegen/{expr,input}.rs`.
  - **Shared:** `src/format.rs` (specifiers).
- **Open decisions that must be resolved first:** none.

### Design
- **Lexer:**
  - when a `"` starts a string and the two tokens before it are `format` and `(`, with no `.` before `format`, the string is a template;
  - a template lexes as one `Str` token: escapes are decoded, and `{` and `}` are kept as written (so `{{` stays `{{`);
  - this works inside an interpolation too (`"{format("{}", x)}"`).
- **Template parsing:** new `src/template.rs`, shared by the checker and codegen.
  - `pieces(text) -> Result<Vec<Piece>, String>`, with `Piece::Text(String)` and `Piece::Hole(Option<String>, offset)`;
  - `{{`/`}}` are literal braces, `{}` is a hole, and `{:spec}` is a hole with a specifier;
  - `{x}`, an unclosed `{` and a lone `}` are errors (E0432);
  - `parts(pieces, args) -> Vec<Part>` turns them into the interpolation's parts, with the argument expressions cloned (their `ExprId`s, so the side tables still apply).
- **Parser** (new `parser/format.rs`):
  - `format` `(` STR followed by `,` or `)` → `ExprKind::Format(Box<FormatCall>)` with `{ template, template_span, args }`; a trailing comma is allowed;
  - anything else after `format(` stays an ordinary call;
  - sexp `(format "…" a b)`.
- **Checker:**
  - **`ExprKind::Format`:** parse the template; any error, or a hole count unlike the argument count, is E0432 (with the arguments still checked). Otherwise the parts go through `interpolation`, which already checks printability (E0407), specifiers (E0431) and records `printed`. The result is `string`.
  - **A plain call to `format`:** E0432, "the template must be a string literal". `format` joins the reserved names in `input.rs::INPUT`, so declaring it is E0302.
  - **Specifier spans** point into the literal when it has no escapes, otherwise at the whole template.
- **Codegen:** `ExprKind::Format` builds the parts and lowers them like `"…"` interpolation.
- **Programs without `format`** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - §2 (templates);
  - §5 (`format`, next to interpolation and specifiers);
  - the names list (`format` reserved);
  - §9;
  - the E0432 row;
  - the §10 program;
  - §11;
  - the changes summary and compatibility.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 95, CONTEXT. The tag `m48` is added at the merge.

### Must NOT Do
- **No positional or named holes (`{0}`, `{name}`), no runtime templates, no user-defined variadic functions.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0432:**
  - `format(s, 1)` with a non-literal template;
  - `format()`;
  - two holes for three arguments, or three for two;
  - `{x}` in a template;
  - an unclosed `{` or a lone `}`.
- **E0431:** a bad specifier or one that doesn't fit its argument, as in v13.
- **E0407:** a `void` argument.
- **E0302:** `fun format() {}`.

## SECURITY CONSIDERATIONS
- The template is a compile-time literal, so a program can't be fed a format string at run time.
- No new runtime code, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — lexer: a template keeps `{x}` and `{{` raw; `x.format("{a}")` still interpolates; a template inside an interpolation.

Unit tests — `template.rs`: holes, specs, `{{`/`}}`, and each error.

Unit tests — parser: `(format "…" a b)`; `format(s)` stays a call.

Unit tests — checker:
- [x] The type is `string`.
- [x] Each E0432 case; E0431 inside a template; E0407; E0302.

Acceptance — `tests/programs/m48/`:
- [x] `format.la`: the §10 program.
- [x] `more.la`:
  - no arguments (`format("{{}}")`);
  - every specifier kind;
  - structs, arrays, `Option`, tuples and `char`s as arguments;
  - nested `format` and `format` inside an interpolation;
  - escapes in a template.
- [x] `e0432.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-077-format`, merged into `main` after CI passes; tag `m48`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m48/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The spec is at milestone 48; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m48/format.la
```

## AMENDMENTS (during build, session 99)

- **`template.rs` is crate-level** next to `format.rs`, since both the checker and codegen read it; `check::template_exact` says whether specifier spans can point inside the literal.
- **A non-literal `format(…)` call** reaches `input_intrinsic` (`format` joined `INPUT`) and is E0432 at its first argument, or at the call for `format()`.
- **IR:** the 165 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for both `m48` run programs.
- **No file splits:** the new code is in new files (`src/template.rs`, `parser/format.rs`, `check/templates.rs`).
