## FEATURE: Better patterns — or-patterns (E0428) and `mut` bindings; milestone 31.

**Status:** approved 2026-10-14 — session 77. The user asked for milestones 31–33 "in one go": each is its own PRP, merged once CI is green.
**Milestone:** 31 (one PRP; tag `m31` at merge; `Language v9 Specification.md` begins)
**Spec:** `Language v9 Draft.md`:
- decisions 2 and 3, and the choices "Or-patterns nest" and "`mut` binds wherever a name does";
- §3 (`pattern`, `alternative`);
- §4 "Or-patterns" and "`mut` bindings";
- §9;
- E0428;
- §10 "Or-patterns";
- §11 (milestone 31).

**Decisions:** MEMORY 72 (v9), MEMORY 37/38 (nested patterns, usefulness).

## OBJECTIVE
The §10 or-patterns program prints `3.0`, `3.0`, `stop`, `run`, `42`:

```
Circle(r) | Square(r) => r * 2.0,
"q" | "quit" | "exit" => "stop",
Some(mut n) => { n += 1; println(n); }
```

## CONTEXT

- **Starting state:**
  - `PatternKind` has `_`, variants and bindings, literals, and type patterns.
  - Bindings are immutable and borrowed.
  - Usefulness works on `Pat` rows.
  - Codegen has a flat `switch` and an arm chain.
- **Ending state:**
  - `PatternKind::Or` and `PatternKind::Mut`.
  - E0428.
  - Or-patterns in exhaustiveness and W0102.
  - `mut` bindings as owned copies.
  - `Language v9 Specification.md` begins.
- **Related existing code:**
  - `src/ast/pattern.rs`;
  - `src/parser/pattern.rs`, `src/parser/sexp/mod.rs`;
  - `src/check/{patterns,matching,type_patterns,usefulness}.rs`, `src/check/mod.rs` (`Binding`);
  - `src/codegen/{patterns,matching}.rs`;
  - `src/lib.rs`, `tests/cli.rs`.
- **Open decisions that must be resolved first:** none.

### Design
- **AST:**
  - `PatternKind::Or(Vec<Pattern>)`, with two or more alternatives;
  - `PatternKind::Mut(Box<Pattern>)`, around a binding: a bare name, or `name: T`;
  - a parenthesised pattern `(p)` is just `p`.
- **Parser** (new code in `parser/patterns_or.rs` if `pattern.rs` would pass 300 lines):
  - a pattern is alternatives separated by `|`;
  - `mut` must be followed by a name, with no `(`, or by `name: T`; anything else is a parse error (E0201);
  - the s-expression forms are `(\| a b)` and `(mut n)`.
- **Checker:**
  - **Bindings** become `(Ident, Type, bool)`, where the flag marks `mut`.
  - **`Or`:** each alternative is checked with its own bindings. Every alternative must bind the same names at the same types, compared with the first; a mismatch is E0428 at the alternative, naming the name. The first alternative's bindings are kept.
  - **`Mut`:**
    - around a bare name that resolves to a variant (always at the top level, where bare names are variants, or by the slot rule), it is E0428 ("`mut` needs a binding; `None` is a variant");
    - otherwise the binding is marked mutable;
    - at the top level, `mut x` binds the whole scrutinee.
  - **Declaring:** mutable bindings use `Binding::Let { mutable: true }`, and plain ones stay `Binding::Pattern` (E0501 on assignment).
  - **Usefulness** (new `check/or_patterns.rs`):
    - `Pat::Or(Vec<Pat>)`, and `expand(&Pat) -> Vec<Pat>`, the product over nested ors, so `usefulness.rs` never sees `Or`;
    - each arm's expansions become rows;
    - W0102 is per top-level alternative, at its span, when no value reaches it, and per arm as before.
- **Codegen:**
  - **Matches with or-patterns use the arm chain**, not the flat `switch` (`is_flat` is false), so there are no `switch` cases to share.
  - **`test` on `Or`:** it tries each alternative, jumping to the next alternative on a mismatch and to the arm's `next` after the last.
  - **One slot per name:** a binding already declared in the arm's scope is stored into, not redeclared, so every alternative fills one slot per name.
  - **`Mut`:** the binding is an owned local (`borrowed: false`), deep-copied if its type is deep, so changes never reach the scrutinee.
- **Programs without these patterns** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **AST, parser and sexp:** the two pattern kinds.
- **Checker:** the binding flag, E0428 (`errors/v9.rs`), `check/or_patterns.rs`, and the matching changes.
- **Codegen:** `test`, `bind` and `is_flat`.
- **Spec:** start `Language v9 Specification.md` from v8 (in progress: milestone 31). Fold in:
  - §3 patterns;
  - §4 or-patterns and `mut` bindings;
  - the E0428 row;
  - the §10 or-patterns program;
  - §11;
  - "Changes since v8".

  `lughac spec` prints it.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 73, CONTEXT. The tag `m31` is added at the merge.

### Must NOT Do
- **No `if let`, `while let` or `let … else`** (milestone 33), and **no `unwrap` or `%=`** (milestone 32).
- **No ranges in patterns, and no bindings shared by reference.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0428:** alternatives that bind different names or types, and `mut` on a variant.
- **E0501:** assigning to a plain binding (unchanged).
- **E0417 and W0102:** they cover or-patterns.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser:
- [x] `A | B => 1`, `Some(1 | 2)`, `(A | B)`, `mut n` and `Some(mut c: Circle)` parse and print.
- [x] `mut 1`, `mut A(x)` and a trailing `|` are E0201.

Unit tests — checker (`check/or_patterns.rs`):
- [x] **Allowed:**
  - `Circle(r) | Square(r)` binds `r: f64`;
  - literal alternatives;
  - nested `Some(1 | 2)`.
- [x] **E0428:**
  - a missing name;
  - a different type;
  - `mut None`.
- [x] **E0417:** `Red | Green` without `Blue`.
- [x] **W0102:**
  - an alternative covered by an earlier arm;
  - an arm whose alternatives are all covered.
- [x] **`mut`:**
  - `Some(mut n) => { n += 1; }` checks;
  - `Some(n) => { n += 1; }` is E0501;
  - `mut x =>` at the top level binds the scrutinee.

Unit tests — codegen (`codegen/patterns.rs`): an or-pattern match uses the arm chain, and both alternatives store into one slot.

Acceptance — `tests/programs/m31/`:
- [x] `or.la`: the §10 program.
- [x] `more.la`:
  - nested or-patterns;
  - or-patterns with guards;
  - integer and bool alternatives;
  - type-pattern alternatives (`_: Circle | _: Square`);
  - a `mut` binding of an array, changed in the arm while the scrutinee stays intact;
  - `mut` in a type pattern.
- [x] `e0428.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-060-better_patterns`, merged into `main` after CI passes; tag `m31`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m31/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] `Language v9 Specification.md` begins; docs updated.
- [x] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m31/or.la
```

## AMENDMENTS (during build, session 77)

- **No `Pat::Or`:** pattern typing returns each pattern's expansions (`Vec<Pat>`) instead, so `usefulness.rs` is unchanged. `check/or_patterns.rs` holds the alternatives and `mut` logic.
- **`mut` on a variant is E0428 at the top level too:** `mut None` checks the matched type's variants. A top-level `mut x` that isn't a variant binds the scrutinee.
- **The parser's `mut _` is E0201** (`_` binds nothing).
- **`-O2`** was checked by hand for `m31/or` and `m31/more`.
- **No file splits.** `codegen/or_patterns.rs` keeps `codegen/patterns.rs` under 300 lines.
