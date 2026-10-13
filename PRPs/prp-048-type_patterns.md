## FEATURE: Type patterns — `c: Circle =>` gets the concrete value out of an interface value.

**Status:** pre-approved 2026-10-13 — session 60 (built while the user was away; open questions answered with the recommended option, listed below for review; merge held for the user)
**Milestone:** 21 (one PRP; tag `m21` at merge)
**Spec:** `Language v5 Draft.md`:
- decision 4;
- §3 (`pattern = … | ( IDENT | "_" ) ":" type`);
- §4 "Type patterns";
- §9 (E0426; reused E0417, W0102);
- §10 "Type patterns";
- §11 (milestone 21).

**Decisions:** MEMORY 54 (v5 direction), MEMORY 55–56 (interface values: tables with an identity slot), MEMORY 37/38 (nested patterns, usefulness).

## OBJECTIVE
A `match` on an interface value can test which type it holds and bind that value:

```
fun describe(s: Shape): string = match s {
    c: Circle if c.r > 10.0 => "big circle",
    c: Circle => "circle of radius {c.r}",
    _: Square => "a square",
    _ => "some shape",
};
```

The §10 type-patterns program prints `big circle`, `circle of radius 1.5`, `a square`.

## CONTEXT

- **Starting state:**
  - Patterns are `_`, literals and variants (`PatternKind`).
  - A `match` on an interface value can only use `_`.
  - Every dispatch table already has the identity slot, `lugha_type_<Type>` (PRP-047).
- **Ending state:**
  - `PatternKind::Type`, parsed, checked and lowered.
  - E0426.
  - The v5 spec is at "in progress: milestone 21".
- **Related existing code:**
  - `src/ast/pattern.rs`, `src/parser/pattern.rs`, `src/parser/sexp/mod.rs`;
  - `src/check/patterns.rs` (`Pat`, `Ctor`), `src/check/usefulness.rs`, `src/check/satisfy.rs`;
  - `src/codegen/patterns.rs` (`is_flat`, `test`), `src/codegen/dispatch.rs`, `src/codegen/interfaces.rs` (`type_id`).
- **Open decisions that must be resolved first:** none.

### Discovery
- **No interview:** the user was away and pre-approved building milestones 21 and 22 within the approved v5 draft (session 60).
- **Draft gaps:** where the draft is silent, the decision below takes the recommended option.

### Decisions taken while away (for review)
1. **A type parameter as the pattern's type (`x: T`) is E0426.** A type pattern names a concrete type, as written in the program. Allowing `T` would make whether an arm can match depend on the instance. It can come later if needed.
2. **A generic type written without its type arguments (`s: Stack`) is E0426, as the draft says.** It is not E0418: in a pattern, the arguments can't be inferred. `s: Stack<i64>` works.
3. **E0426's three messages:**
   - "a type pattern needs an interface value, found `i64`";
   - "`Shape` is an interface; a type pattern names a concrete type";
   - "`Rect` doesn't satisfy `Shape`, so this arm can never match", with help listing what's missing.

   A type from another module is written as usual: `c: geo.Cat`.
4. **The identity check is a pointer comparison** of table slot 0 with `lugha_type_<T>`. A pattern type that is never converted anywhere still gets its `lugha_type_` global, and the comparison is simply never true.
5. **Bindings borrow,** as payload bindings do. `c` reads the box's value; storing it (`let d = c;`) copies it at the copy sites, so `mut self` calls through the interface value never show through `c`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **AST:** `PatternKind::Type(Option<Ident>, Annotation)`, where `None` means `_`.
- **Parser** (`parser/pattern.rs`): after `_` or a name without `(`, a `:` starts a type (`ty()`). The s-expression form is `(: c Circle)` or `(: _ Circle)`.
- **Checker:**
  - `check/patterns.rs`: `Ctor::Type(String)`, keyed by the type as it prints, with no payloads; the arm calls a new `type_pattern` in a new `check/type_patterns.rs` (E0426, satisfaction without reporting E0422, the binding);
  - `check/usefulness.rs`: `show` handles `Ctor::Type`. No other change is needed: an interface has no finite signature, so `_` is required and its witness is `_`, and a repeated unguarded type is W0102 through `specialize`;
  - `check/satisfy.rs`: `can_satisfy`, the same rule as `satisfies` without the report;
  - `check/errors/v5.rs`: the E0426 constructors.
- **Codegen:**
  - `codegen/patterns.rs`: `is_flat` is false for an arm with a type pattern, and `test` handles `PatternKind::Type` through a new `type_test`;
  - `codegen/dispatch.rs`: `type_test` (load slot 0, compare with `type_id(T)`, branch, then bind the box's value).
- **Spec:**
  - fold the draft's §3 pattern line, §4 "Type patterns", the E0426 row, the §10 type-patterns program and §11 milestone 21 into `Language v5 Specification.md`;
  - status "in progress: milestone 21"; "Changes since v4" gains type patterns.
- **Docs:** CLAUDE.md tree, README, TODO, CHANGELOG, MEMORY decision 57, CONTEXT. The tag `m21` waits for the merge.

### Must NOT Do
- **No default methods or `?` conversion** (milestone 22).
- **No `x: T` with a type parameter** (decision 1).
- **No downcasts outside `match`:** no `is` and no `as Circle`.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Mistakes are `Diagnostic`s:** E0426 for every misuse, E0417 for a missing `_`, W0102 for an unreachable arm. Checking continues after each.
- **Codegen** relies on the checker. The pattern's type is concrete (an `expect` states it).

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser (`parser/pattern.rs`):
- [ ] `match s { c: Circle => 1, _: geo.Sq => 2, Some(x: Stack<i64>) => 3, _ => 4 }` parses to `(: c Circle)`, `(: _ (. geo Sq))`, `(Some (: x Stack<i64>))`.

Unit tests — checker (`check/type_patterns.rs`):
- [ ] **Bindings:** `c: Circle => c.r` binds `c: Circle`, and `_: Square` binds nothing; nested `Some(c: Circle)` on an `Option<Shape>` works; guards see the binding.
- [ ] **E0426:**
  - on an `i64`;
  - with an interface as the type;
  - with a type that doesn't satisfy (help lists what's missing);
  - with `Stack` without arguments;
  - with a type parameter.
- [ ] **E0417:** a `match` on a `Shape` without `_`, whose witness is `_`.
- [ ] **W0102:** a second unguarded `c: Circle` is unreachable; after a guarded one it isn't.
- [ ] **Bindings are immutable:** `c.grow(1.0)` in the arm is E0501.

Unit tests — codegen (`codegen/dispatch.rs`):
- [ ] A type pattern loads slot 0 and compares it with `@lugha_type_Circle`.
- [ ] A `match` with type patterns uses the arm chain, not a `switch`.

Acceptance — `tests/programs/m21/` (run at `-O0`; `-O2` by hand, as for m20):
- [ ] `describe.la`: the §10 program.
- [ ] `nested.la`:
  - `Some(c: Circle)` and `None`;
  - an interface value in an array, matched in a loop;
  - a generic `Stack<i64>` pattern;
  - a type from another module (a directory case);
  - the bound value copied and changed while the original stays intact.
- [ ] `e0426.la` and `e0417.la`: reject goldens.
- [ ] The §10 type-patterns program runs from the spec.

Regression:
- [ ] Every earlier test passes; programs without type patterns give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-048-type_patterns`, held for the user's review, then merged into `main`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 type-patterns program and the `m21/` programs pass.
- [ ] The spec is at milestone 21; docs updated.
- [ ] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m21/describe.la
```
