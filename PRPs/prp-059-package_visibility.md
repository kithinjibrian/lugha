## FEATURE: Package visibility — `public`, package-default visibility (E0313), no leaking (E0318); milestone 30, part 2; v8 complete.

**Status:** approved 2026-10-14 — session 75. This is the second half of the user's "in one go" for milestone 30, merged once CI is green.
**Milestone:** 30, part 2 (completes v8; tags `m30` and `v8` at merge)
**Spec:** `Language v8 Draft.md`:
- decision 3, and the choice "No leaking";
- §3 (`public`);
- §6 "Visibility";
- E0313 and E0318 changed, E0317 retired;
- §10 "Packages" and the rejected package program;
- §11.

**Decisions:** MEMORY 69–70 (v8, package loading), MEMORY 34 (v2 visibility).

## OBJECTIVE
A package exposes only what it marks `public`. The §10 packages program prints `12.0`, and the same layout calling `c.diameter()` or writing `shapes.Circle { r: 1.0 }` is E0313:

```
error[E0313]: `diameter` is not `public`: it is only visible inside package `geo`
```

## CONTEXT

- **Starting state (after PRP-058):**
  - Packages load.
  - Visibility is v2's: `private` means the module, otherwise the whole program.
  - `public` is E0317.
- **Ending state:**
  - three levels;
  - E0317 is retired;
  - E0318 covers `public` declarations;
  - v8 is complete.
- **Related existing code:**
  - `src/check/visibility.rs` (`Privacy`, `item_visible`, `method_visible`, `method_usable`, `field_visible`, `literal_visible`, `visibility_rules`);
  - `src/check/errors/names.rs`;
  - `src/check/modules/mod.rs` (`Unit`, `enter`);
  - `tests/common/spec_programs.rs` and `tests/spec.rs` (the §10 extractor).
- **Open decisions that must be resolved first:** none.

### Design
- **Levels** (new `check/levels.rs`):
  - `Level { Private, Package, Public }`, from the written keyword;
  - `Owner { module, package, level }` per item;
  - methods and fields keep a `Level`, and their owner is their type's;
  - the prelude's items are `Public`.
- **The current package:** `Unit.package` gives the checker the current package (`Module::package`).
- **Who may use what** (`denied`):
  - `Public` everywhere;
  - `Package` in the same package;
  - `Private` in the same module.

  Every existing E0313 site goes through it, and so does interface satisfaction (`method_usable`).
- **Messages:**
  - **Private:** v2's E0313 message.
  - **Package:** "`helper` is not `public`: it is only visible inside package `geo`" (`errors/v8.rs::package_item`).
  - **Literals:** a struct literal naming a hidden field reports that field, with help to use one of the struct's functions. A hidden struct is reported once, for the type.
- **E0318** (`hidden_inside`): a declaration may not mention a type less visible than itself.
  - **Unchanged for v7's levels:** shallow, structs and enums.
  - **For `public` declarations:** the check also looks inside type arguments and interface types, and at a generic function's bounds. So no v7 program changes meaning.
- **E0317 is retired:** `public` is accepted on items, methods and fields. Its golden `m10/e0317` is removed, and `m30/public` shows the same code now builds.
- **The spec extractor** accepts `# lugha.toml` blocks and files in subfolders, writes them into folders, and lexes and parses only `.la` files.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Checker:** the items above, plus tests in `check/levels.rs`.
- **Spec** (`Language v8 Specification.md`):
  - §3;
  - §6 "Visibility";
  - the E0313, E0317 and E0318 rows;
  - the §10 packages program, plus a prose rejected case;
  - §11 milestone 30 done;
  - the title and status say v8 is complete;
  - §12 gains the v8 questions.
- **Docs:** CLAUDE.md (STACK, tree, anti-pattern 5), README, TODO, CHANGELOG, MEMORY decision 71, CONTEXT. The tags `m30` and `v8` are added at the merge.

### Must NOT Do
- **No re-exports** (`public import`) and **no `--package` flag:** both are v8 §12 questions.
- **No change to v7 programs' meaning:** the deeper E0318 search applies only to `public` declarations.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0313:** an item, method or field not visible here, with a message per level.
- **E0318:** a less visible type exposed.
- **E0422:** a hidden method doesn't satisfy a bound.

## SECURITY CONSIDERATIONS
- No external input beyond PRP-058's, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — `check/levels.rs` (multi-package, in memory):
- [x] Public items, methods, fields and interfaces cross packages, including through a bound.
- [x] **E0313 from another package:**
  - a function;
  - a method;
  - a field;
  - a literal with a hidden field;
  - a hidden struct, reported once.
- [x] A hidden method doesn't satisfy a bound (E0422).
- [x] **E0318:**
  - a `public` function returning a non-`public` struct;
  - a type argument;
  - a bound naming a non-`public` interface;
  - a package-level function is unaffected.

Unit tests — `check/visibility.rs`: v2's tests still pass, and `public` is allowed (it replaces the E0317 test).

Integration: `tests/modules.rs` packages use `public`.

Acceptance — `tests/programs/m30/`:
- [x] `packages/` with `public`.
- [x] `public.la`: `public` in a single program builds.
- [x] `e0313/`: reject golden.
- [x] The §10 packages program runs from the spec (extractor with `lugha.toml` and subfolders).

Regression:
- [x] Every earlier test passes, apart from the retired `m10/e0317`, which is removed.
- [x] Earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-059-package_visibility`, merged into `main` after CI passes; tags `m30` and `v8`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 packages program and the `m30/` programs pass.
- [x] The v8 spec is complete; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m30/packages/main.la
```

## AMENDMENTS (during build, session 75)

- **Process:** the code was written before this PRP file and its branch existed, uncommitted on `main`. That was caught before any commit: the work moved to `prp-059-package_visibility`, and this PRP records the design as built.
- **Literal errors name the hidden field** (`` `r` is not `public` ``), with help to use one of the struct's functions. Naming the struct was misleading when the struct itself is `public`.
- **The checker's in-memory test loader** models folders (for manifests), as the loader tests do.
- **No file splits.**
