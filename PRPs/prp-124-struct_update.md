## FEATURE: Struct update syntax — `S { f: e, ..base }` builds a struct from some fields written and the rest copied from `base`; milestone 93 (v17).

**Status:** built in session 216; approved 2026-10-17 ("approved", session 216); proposed in session 215, with PRP-125.
**Milestone:** 93 (tag `m93` at merge).
**Spec:** `Language v17 Draft.md` decisions 24–29; v17 spec §3 (grammar), §4 (struct literals), §5 (evaluation order), §9 (diagnostics), §11 (row 93).

**Why:** the prerequisite of kivuli (`/home/kithinji/Documents/kithinji/kivuli/KICKOFF.md`, decision 11): Elm-style `update` functions return a changed copy of the model in every arm, and without `..m` each arm repeats every field.

**Decisions (discovery, session 215; the user's choice: Rust's rules):**
- `..base` comes last; `base` is an expression of exactly the literal's struct type (type arguments included).
- Explicit fields are evaluated first, left to right, then `base`, once; every field not written is taken from it.
- Visibility as for any literal: outside the struct's module every field must be visible, written or not (E0313).
- Decided by me, for review: a `base` of another type is a new E0440 (to be confirmed as the next unused code when building — the actors branch may claim codes first); `..base` not last, or twice, is a syntax error (E0201); every field written *and* `..base` is a warning, W0104 ("`..base` copies nothing"); `..base` in a struct *pattern* doesn't exist (Lugha has no struct patterns); tuples (`$tuple`) have no literal syntax, so nothing changes for them.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
struct Model { count: i64, name: string, items: string[] }

fun update(m: Model, msg: Msg): Model = match msg {
    Add => Model { count: m.count + 1, ..m },
    Named(n) => Model { name: n, ..m },
    Clear => Model { items: [], count: 0, ..m },
};

let p = Point { x: 1.0, ..origin() };    // origin() runs once, after `x`
let q = Point { x: 1.0, ..label };       // E0440: `label` isn't a Point
let r = Point { x: 1.0, y: 2.0, ..p };   // W0104: every field is written
```

## CONTEXT
- **Starting state:** struct literals `S { f: e, … }` must give every field exactly once (E0413 missing, E0414 twice); parsed in `parser/primary.rs` / `ast` `StructLiteral` (fields), checked in `check/struct_literal.rs` and `check/structs.rs`, lowered in `codegen/structs.rs` (literals). Copies follow spec §4's table ("a place used as a struct-literal field … gets its own copy"); move on last use in `check/liveness/`.
- **Ending state:**
  - **AST:** `StructLiteral.base: Option<Box<Expr>>`.
  - **Parser:** `..e` as the last entry of a literal's braces (also after a trailing comma); `..` elsewhere in the braces, or twice, E0201. In no-struct-literal mode nothing changes.
  - **Checker:** `base` checked with the literal's type expected (E0440 if it has another type, with the two types named); E0413 not reported when `base` is present; W0104 when every field is written; visibility of unwritten fields (E0313). Generic structs: type arguments inferred from the written fields and `base` together.
  - **Liveness:** `base` is a read like any other (moved on its last use when it is a local).
  - **Codegen:** evaluate the written fields in source order, then `base` once into a temporary; each unwritten field is read from it with the normal copy rules (a deep-copied type copied, arrays marked shared — `value_for_store`).
  - **Formatter:** `..base` printed last, as written; the language server: hover, completion inside the braces still work (completion after `..` offers values).
  - **Spec v17:** grammar, §4 struct literals, §5 evaluation order, E0440, W0104, §11 row 93, changes list. **Draft:** decisions 24–29, row 93.
  - **`docs/errors/E0440.md`, `W0104.md`** (`tests/explain.rs`).
- **Related existing code:** `src/ast/expr.rs`, `src/parser/{primary,expr}.rs`, `src/check/{struct_literal,structs,liveness/,errors/}`, `src/codegen/structs.rs`, `src/formatter/exprs.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first; byte-identical IR for programs without `..`.
- Check E0440's number against the v17 spec and the actors branch at build time; take the next unused one.
### Must NOT Do
- No structural `..base` from another type; no `..` in patterns; no change to literals without `..`.
### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- **E0440** (new): `base` of another type than the literal's.
- **W0104** (new): every field written as well as `..base`.
- **E0201**: `..` not last, or twice.
- **E0313**: an unwritten field not visible here.
- Existing E0414 (a field written twice) unchanged.

## SECURITY CONSIDERATIONS
- None beyond the compiler's usual: no `unsafe`, no new I/O.

## TESTS TO WRITE
- [ ] Parser: `S { a: 1, ..b }`, with a trailing comma before `..`, `..` first/twice (E0201), the s-expression form; formatter round trip.
- [ ] Checker: missing fields filled; E0440 (another struct, a non-struct, another instance of a generic struct); W0104; E0313 for a private unwritten field from another module; generic inference through `base`.
- [ ] `tests/programs/`: evaluation order (fields with `println` side effects, then base), copies (an array field taken from `base` then changed: the original unchanged), move on last use, nested (`Outer { inner: Inner { a: 1, ..o.inner }, ..o }`), generic.
- [ ] IR unchanged for the compiling `tests/programs` without `..`.

## ROLLBACK PLAN
- Branch `prp-124-struct_update` in its own worktree; abandoning it means deleting the branch.

## ACCEPTANCE CRITERIA
- [ ] The objective's `update` compiles and runs; E0440 and W0104 reported as specified.
- [ ] All tests pass; fmt, clippy clean; every `pub` item documented; CHANGELOG updated.

## VALIDATION
- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`

## BUILT (session 216)
- As planned, test-first. E0440 and W0104 confirmed unused (actors took E0439 only).
- `base` is checked first and **without** an expected type, so `..5` is E0440 rather than the literal's own E0401; its type arguments then fix a generic struct's, so the fields need no inference.
- A trailing comma after `..base` is accepted (`P { x: 1, ..p, }`), since the formatter breaks long literals with one.
- Actor states (milestone 92) can't be a base outside their actor: E0439 already rejects the state there.
- Tests: checker (6), parser (1), formatter (1), `tests/programs/m93` (`update.la`: Elm-style `update`, evaluation order, copies, generics, nesting; `e0440.la`); the two explain pages through `tests/explain.rs`. Byte-identical IR for 204 compiling programs.
