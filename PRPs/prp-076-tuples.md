## FEATURE: Tuples — `(A, B)` types and values, `t.0`, patterns in `match`/`if let`/`let`/`for`, `zip` → tuples, `Pair` removed (E0433); milestone 47.

**Status:** merged 2026-10-14 — session 98. This is part of the user's "in one go" (milestones 45–49), merged once CI is green.
**Milestone:** 47 (one PRP; tag `m47` at merge)
**Spec:** `Language v14 Draft.md`:
- decision 3, and the choices on tuples;
- §3 (`type`, `primary`, `member`, `alternative`, `var_decl`, `for_head`);
- §9;
- E0433;
- §11 (milestone 47).

**Decisions:** MEMORY 91 (v14), MEMORY 89 (`Pair`), MEMORY 75 (`let … else`).

## OBJECTIVE
The §10 program:
- returns `(i64, i64)` from `divmod`;
- destructures with `let (q, r) = …`;
- loops with `for (i, w) of xs.iter().zip(ws.iter())`;
- matches on a tuple;
- compares tuples;
- prints them as `(1, "a")`.

## CONTEXT

- **Starting state:**
  - no tuples;
  - `zip` yields the prelude struct `Pair`;
  - `let … else` needs its `else`;
  - `for` binds a name.
- **Ending state:** tuples throughout; `Pair` gone.
- **Related existing code:**
  - **Lexer, AST, parser:** `src/lexer/number.rs`, `src/ast/{expr,types,pattern,mod}.rs`, `src/parser/{primary,expr,generic,pattern,conditional,stmt}.rs`, `src/parser/sexp`.
  - **Checker:** `src/check/{env,types,patterns,enums,conditional,ops,bounds,expr,iter_helpers}.rs`, `src/check/liveness/walk.rs`.
  - **Codegen:** `src/codegen/{expr,patterns,conditional,show,equality,arith}.rs`.
  - **Elsewhere:** `src/prelude_more.la`, `tests/programs/m43/`.
- **Open decisions that must be resolved first:** none.

### Design
- **Representation:**
  - a tuple type of `n` elements is the generic struct `$tuple<n>` with parameters `T0…` and fields `0`, `1`, …, registered by the checker on first use (`check/tuples.rs::tuple_type`);
  - layout, copies, `==`, field reads and writes, generic instances, `.0` access and printing's machinery all reuse the struct paths;
  - `Type`'s `Display` prints it `(A, B)`, so LLVM types and symbols read `%"(i64, string)"` and `lugha_fn_max<(i64, i64)>`.
- **Lexer:** a number directly after a `.` token lexes as an integer only, so `t.0.1` is `t . 0 . 1`.
- **Parser** (new `parser/tuples.rs` with the helpers):
  - **types:** `(A, B, …)` → `TypeKind::Tuple`;
  - **expressions:** `(a, b, …)` → `ExprKind::Tuple`; a trailing comma is allowed, and `(a,)` is E0201;
  - **member:** an integer → a field named by the digits;
  - **patterns:** `(p, q, …)` → `PatternKind::Tuple`;
  - **`let (…) = e;`:** `StmtKind::LetElse` with `else_: Option<Box<Block>>` (`None`);
  - **`for (i, w) of e { body }`:** becomes `for $item<offset> of e { let (i, w) = $item<offset>; body }`.
- **Checker:**
  - **Tuple literals** take their element types from the expected tuple type or from the elements.
  - **Tuple patterns** type each element (not top-level, so names bind) and give `Pat::Ctor(Variant(0, "()"), …)`. `variants_of` gives a tuple type one variant `"()"`, so exhaustiveness and W0102 work unchanged, and witnesses print as `(_, false)`.
  - **`let` without `else`, and the `for` destructuring:** the pattern must be irrefutable. If `Wild` is still useful after its rows it is E0433 (new `errors/v14.rs`).
  - **`<`:** works when every element is `Ord`.
- **Codegen** (new `codegen/tuples.rs`):
  - **Literals:** `insertvalue` of the elements (stored forms).
  - **Patterns:** a tuple pattern tests each field's address as a subject, with no tag; `is_flat` is false for tuple arms.
  - **`<`:** `lugha_cmp_<type>(a, b) -> i32` (−1, 0, 1) element by element (numbers, `char`, `bool`, strings through `lugha_rt_str_cmp`, nested tuples), then the operator against 0.
  - **Printing:** `show_fields` prints a tuple as `(a, b)`.
  - **`let` without `else`:** a failed test is `unreachable`.
- **Prelude:** `Zip` yields `(A, B)`; `Pair` and its `ITER_TYPES` entry are removed. `m43` programs and the v14 spec's §10 copy of the more-helpers program change to tuples: a planned break.
- **Programs without tuples** give byte-identical IR, except those calling `zip`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - §3;
  - §4 (tuple types, equality and order);
  - §5 (literals, `t.0`, `let`/`for` destructuring, patterns);
  - the print table;
  - §9;
  - the E0433 row;
  - the helpers table (`zip` yields `(A, B)`);
  - `Pair` removed;
  - the §10 programs;
  - §11;
  - the changes summary and compatibility.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 94, CONTEXT. The tag `m47` is added at the merge.

### Must NOT Do
- **No one-element tuples, unit `()`, tuple structs, or variadic generics.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0433:** a refutable pattern in `let` without `else`, or in a `for` destructuring.
- **E0410:** `t.5` past the end (the existing no-field code).
- **E0403:** element types that don't match.
- **E0404:** `<` with a non-`Ord` element.
- **E0201:** `(a,)`.
- **E0417 and W0102:** cover tuple matches.

## SECURITY CONSIDERATIONS
- No new input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser:
- [x] `(a, b)` and `(i64, string)`; `t.0.1`; `(x, _)` patterns; `let (a, b) = t;`; `for (i, w) of xs { }` desugared.
- [x] E0201 for `(a,)`.

Unit tests — checker (`check/tuples.rs`):
- [x] **Types:** literal types, `t.1` types, a tuple return, generic `max<(i64, i64)>`.
- [x] **E0433** for `let (1, x) = t;`.
- [x] **E0417** for `match (true, 1) { (true, _) => 1 }`.
- [x] **E0404** for `<` on a tuple holding an array.

Unit tests — codegen: `%"(i64, string)"`; `lugha_cmp_` for `<`.

Acceptance — `tests/programs/m47/`:
- [x] `tuples.la`: the §10 program.
- [x] `more.la`:
  - nested tuples;
  - tuples in arrays, structs, `Option` and enums;
  - `t.0 = v`;
  - copies (a tuple holding an array);
  - `==`;
  - `<` on strings and floats;
  - `match` with guards;
  - `if let (Some(a), b) = …`;
  - generic functions over tuples;
  - `zip` and `enumerate` together.
- [x] `e0433.la`: reject golden.
- [x] **Updated:** `m43/helpers` and `m43/more` (`zip` yields tuples).
- [x] The §10 programs run from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR, except `zip` users.

## ROLLBACK PLAN

- Branch `prp-076-tuples`, merged into `main` after CI passes; tag `m47`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m47/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The spec is at milestone 47; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m47/tuples.la
```

## AMENDMENTS (during build, session 98)

- **IR:** earlier programs give byte-identical IR except `zip` users (`m43/helpers`) and the two `m46` programs. Those differ only in the line numbers of panics inside `prelude_text.la`, which is joined after `prelude_more.la`, now two lines shorter without `Pair`; nothing else changes. `-O2` was checked by hand for both `m47` run programs.
- **`let … else` without `else`:** the parser gives `StmtKind::LetElse { else_: None }` for any `let` whose target is a tuple pattern, so destructuring shares the `let … else` path; a failed test there is `unreachable` (E0433 makes it impossible).
- **No file splits:** the new code is in new files (`parser/tuples.rs`, `check/tuples.rs`, `check/errors/v14.rs`, `codegen/tuples.rs`).
