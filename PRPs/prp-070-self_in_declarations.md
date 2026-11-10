## FEATURE: `Self` in declarations — `Self` in struct fields and enum payloads; milestone 41.

**Status:** merged 2026-10-14 — session 91. This is part of the user's "in one go" (milestones 41–44), merged once CI is green.
**Milestone:** 41 (one PRP; tag `m41` at merge; `Language v13 Specification.md` begins)
**Spec:** `Language v13 Draft.md`:
- decision 1, and the choice on `Self` in declarations;
- §4;
- §9;
- §10 "`Self` in declarations";
- §11 (milestone 41).

**Decisions:** MEMORY 86 (v13), MEMORY 77 (`Self` in `impl` blocks as parser sugar).

## OBJECTIVE
The §10 program prints `3` then `Cons(1, Cons(2, Nil))`:

```
struct Tree<T> { value: T, kids: Self[] }
enum List { Cons(i64, Self), Nil }
```

## CONTEXT

- **Starting state:** `parser/self_type.rs` replaces `Self` inside `impl` blocks (`Parser::in_impl`). In a struct or enum declaration, `Self` is E0305.
- **Ending state:** struct and enum bodies replace it too; `Language v13 Specification.md` begins.
- **Related existing code:** `src/parser/{item,decl,self_type}.rs`, `src/check/{self_type,concrete_defaults}.rs` (tests that assert E0305 for `struct N { kids: Self[] }`).
- **Open decisions that must be resolved first:** none.

### Design
- **Parser:** `struct_decl` reads its fields, and `enum_decl` its variants, inside `in_impl(name, type parameter names)`. So `Self` becomes the declared type, or `T<P…>` for a generic one, exactly as in an `impl`.
- **Nothing else changes:** the recursion rules (E0307 for a struct holding itself directly, indirect enum slots) apply as if the name were written.
- **Programs without `Self` in declarations** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- The two parser changes.
- **Updated tests:** those asserting E0305 for `Self` in a declaration now assert the type, or E0307 for `struct S { me: Self }`.
- **Spec:** start `Language v13 Specification.md` from v12 (in progress: milestone 41); point `lib.rs` and `tests/cli.rs` at it. Fold in:
  - §4 `Self`;
  - §9;
  - the §10 program;
  - §11;
  - "Changes since v12".
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 87, CONTEXT. The tag `m41` is added at the merge.

### Must NOT Do
- **No `Self` in function signatures outside `impl`s, or in `extern`s.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0307:** a struct holding `Self` directly (unchanged rule).
- **E0305:** `Self` outside interfaces, `impl` blocks and struct and enum declarations.

## SECURITY CONSIDERATIONS
- None: parser sugar only. No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser (`parser/self_type.rs`):
- [x] `struct N { kids: Self[] }` prints with `N`; `struct T<A> { k: Self[] }` with `(applied T A)`; `enum L { C(i64, Self), E }` with `L`.
- [x] A function outside these declarations keeps `Self`.

Unit tests — checker: `struct S { me: Self }` is E0307; `fun f(): Self` is E0305.

Acceptance — `tests/programs/m41/`:
- [x] `self.la`: the §10 program.
- [x] `more.la`:
  - a generic enum with `Self`;
  - `Option<Self>` in a struct;
  - `Self` in a struct of another module used through `m.T`.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-070-self_in_declarations`, merged into `main` after CI passes; tag `m41`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m41/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] `Language v13 Specification.md` begins; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m41/self.la
```

## AMENDMENTS (during build, session 91)

- **`m41/more`** is a directory case: module `geo` with `Menu { items: Self[] }`.
- **A struct field of `Option<Self>`** would be E0307 (`Option` is stored inline), so `more` uses `Self[]`. The bare `Tree.Leaf` needs a type annotation, as for any generic enum.
- **IR:** the 149 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for both `m41` programs.
- **No file splits.**
