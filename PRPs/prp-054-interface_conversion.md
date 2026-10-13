## FEATURE: Converting between interfaces — a `Pet` value becomes a `Named` when `Pet` declares `Named`'s items; through `?` too; milestone 26, v6 complete.

**Status:** approved 2026-10-13 — session 68
**Milestone:** 26 (one PRP; completes v6; tags `m26` and `v6` at merge)
**Spec:** `Language v6 Draft.md`:
- decision 4, and the choice "Conversion between interfaces needs every item, defaulted ones included";
- §4 "Converting between interfaces";
- §7;
- §9 "Converting `J` to `I`", **amended below**;
- §10 "Converting between interfaces";
- §11 (milestone 26).

**Decisions:** MEMORY 59 (v6), MEMORY 55–58 (interface values, tables, type patterns, defaults, `?`), MEMORY 61 (`Applied`, `InterfaceShape`).

## OBJECTIVE
The §10 program prints `good dog, rex`, `woof`, `hello, tom`, `meow`:

```
let pets: Pet[] = [Dog { called: "rex" }, Cat { called: "tom" }];
for p of pets {
    println(greet(p));     // greet(n: Named): a `Pet` converts; `d: Dog` still matches inside
    println(p.sound());
}
```

`?` converts an interface error the same way. A `Result<T, ParseProblem>` converts in a function returning `Result<U, Problem>` when `ParseProblem` declares every item of `Problem`.

## CONTEXT

- **Starting state:**
  - A value of interface type `J` where another interface `I` is expected is E0403 (`errors::interface_to_interface`).
  - `?` rejects interface errors (`try_converts`).
  - A generic instance whose `T` is an interface value converts it by boxing the interface value itself (`lugha_vt_<J>.<I>`). That box hides the concrete type from type patterns.
- **Ending state:**
  - `J` converts to `I` under the draft's rule.
  - A converted value keeps its box and its concrete type.
  - v6 is complete.
- **Related existing code:**
  - `src/check/iface_values.rs` (`convert`), `iface_tables.rs` (`try_converts`), `applied.rs` (`items`), `errors/v5.rs`;
  - `src/codegen/interfaces.rs` (`converted`, `table`, `method_of`), `codegen/defaults.rs` (`box_as`), `codegen/dispatch.rs` (`parts`, `slot`), `codegen/lower.rs` (`lower`).
- **Open decisions that must be resolved first:** none.

### Discovery
- **No interview:** the approved draft settles the behaviour (decision 4, §4). One compilation detail is amended; it needs your approval as part of this PRP (below).

### Design
- **Checker:**
  - **The rule:** where `I` is expected, a value of interface type `J` with a different key converts when `J` declares every item of `I`, defaulted ones included, with the same name, receiver and signature, after substituting both interfaces' arguments.
    - The check compares declared items (`declares_all`), not what a concrete type offers.
    - A missing or mismatched item is **E0422**, "`Pet` doesn't declare everything `Named` needs", with help listing each problem.
    - The same interface at other arguments (`Source<i64>` where `Source<string>` is expected) stays E0403.
  - **Recording:** the conversion goes in `Checked.conversions`, as for any conversion. No reachability is needed: every method an `I` table needs is one that `J`'s tables already use.
  - **`?`:** `try_converts` accepts an interface error `J` under the same rule. That lifts v5's PRP-049 decision 4.
- **Codegen:**
  - **Derived tables:** for a concrete type `C` and its `J` table, the `I` table takes:
    - the identity and copy slots of `C`'s `J` table;
    - for each `I` item, the function `C`'s `J` table has for that item.

    So each method behaves exactly as it did through `J`, including a default `J` supplies. It is named `lugha_vt_<C>.<J>.<I>`, because it can differ from a direct `lugha_vt_<C>.<I>` in which default it uses. Derived tables of derived tables keep the root `J` (`<C>.<J>.<K>`), so chains stay finite, and converting back to the root gives the root table.
  - **Finding the table at run time (amends draft §9):** a conversion calls `lugha_up_<J>.<I>(table) -> table`. It compares the value's table with each `J` table the program has and returns the matching `I` table.
    - **Why not the draft's slots:** the draft put one slot per target interface in each `J` table. But `J` tables are emitted lazily, while lowering, before every conversion of `J` is known. Growing a table afterwards would mean replacing a global, and deleting one needs `unsafe` in inkwell. The lookup functions instead get their bodies at the end of lowering, when every table is known.
    - **Cost:** one comparison per type converted to `J`, instead of one load.
    - **Chains:** building the up-functions creates derived tables, which other up-functions may need, so this repeats until no new table appears.
  - **The box** is copied as for any conversion of a value still in use, and paired with the found table.
  - **A generic instance whose `T` is an interface value** now converts it this way, not by boxing it again. So a type pattern sees the concrete type, as the draft wants. Programs that did this give different IR, but the same output.
  - **`?`** with an interface error converts the same way (`box_as` handles an interface source).
- **Programs that never convert one interface to another** get no `lugha_up_` functions and no derived tables.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Checker:**
  - `declares_all` and the E0422 diagnostic;
  - `convert` and `try_converts` accept `J` to `I`;
  - same key with other arguments stays E0403.
- **Codegen** (new file `codegen/upcast.rs`):
  - a registry of tables, `(C, root, interface)`;
  - `upcast` (call `lugha_up_…`, copy the box, pair it);
  - derived tables;
  - the fixpoint, and the up-function bodies at the end of `lower`;
  - `converted` and `box_as` route interface sources to `upcast`.
- **Spec** (`Language v6 Specification.md`):
  - fold in §4 "Converting between interfaces", replacing v5's "Not between interfaces" and the `?` rule;
  - §7, and §9 with the amended lookup;
  - the §10 program;
  - §11 milestone 26 done;
  - the title and status say v6 is complete, plus the compatibility sentence;
  - §12 gains the draft's two v6 questions.
- **Draft:** a note in §9 that the slots became lookup functions (this PRP).
- **Docs:**
  - CLAUDE.md: STACK says v6 is complete; rule 7 gains `lugha_up_<J>.<I>` and derived tables; anti-pattern 5; the tree;
  - README, TODO, CHANGELOG, MEMORY decision 64, CONTEXT.

  The tags `m26` and `v6` are added at the merge.

### Must NOT Do
- **No conversion inside other types:** a `Pet[]` is not a `Named[]` (E0403).
- **No `==` on interface values.**
- **No `unsafe`:** that's why the lookup functions replace the draft's slots.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0422:** "`Pet` doesn't declare everything `Named` needs", with help such as "no `name`; `size` should be `fun size(self): i64`".
- **E0403:** `Source<i64>` where `Source<string>` is expected; `Pet[]` where `Named[]` is expected.
- **E0424:** `?` whose interface error doesn't convert.
- **Codegen invariant:** the up-function's last block is `unreachable`. A value's table is always one the program built, since every interface value comes from a conversion that emitted its table.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (new `check/upcast.rs`, which holds `declares_all`):
- [x] **Allowed:**
  - `Pet` to `Named` in `let`, arguments, array elements, fields, `Option` payloads and returns;
  - a generic `Source<i64>` to `Sized`, when `Source` declares `size`.
- [x] **E0422:**
  - a missing item;
  - a mismatched item;
  - a defaulted item `J` lacks (defaults count).
- [x] **E0403:**
  - `Source<i64>` to `Source<string>` (unchanged);
  - `Pet[]` to `Named[]`.
- [x] **`?`:** `Result<i64, ParseProblem>` into `Result<i64, Problem>` checks; with a `J` that lacks an item, E0424.

Unit tests — codegen (`codegen/upcast.rs`):
- [x] Converting a `Pet` to `Named` calls `@"lugha_up_Pet.Named"`. Its body compares with `@lugha_vt_Dog.Pet` and returns `@lugha_vt_Dog.Pet.Named`.
- [x] The derived table's slots are `@lugha_vt_Dog.Pet`'s identity, copy and `name` entries.
- [x] A program without such conversions has no `lugha_up_` symbol.

Acceptance — `tests/programs/m26/`:
- [x] `pets.la`: the §10 program.
- [x] `chains.la`:
  - a chain `J` to `I` to `K`, and back to `J`;
  - a default kept from `J`;
  - copies are independent (`mut self` through the new value);
  - type patterns after conversion;
  - a generic function converting `T = Pet` to `Named`;
  - `?` with interface errors.
- [x] `e0422.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes.
- [x] Earlier programs give byte-identical IR, except programs that convert an interface-valued type argument, whose output is unchanged.

## ROLLBACK PLAN

- Branch `prp-054-interface_conversion`, merged into `main` after CI passes; tags `m26` and `v6`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m26/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The v6 spec is complete; docs updated.
- [x] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m26/pets.la
```

## AMENDMENTS (during build, session 68)

- **No exceptions to byte-identical IR:** no earlier test program converted an interface-valued type argument, so every earlier program's IR is unchanged.
- **Two v5 unit tests updated** to the v6 rule: `iface_values::nothing_converts_without_an_interface_expected` (another interface now converts, or is E0422) and `results::question_mark_converts_into_an_interface_error` (an interface error now converts).
- **`interfaces.rs`:** `table_type`, `method_of` and `box_copy` became `pub(super)`, and `table` registers each table it builds (one line).
- **`-O2`** was checked by hand for `m26/pets` and `m26/chains`.
- **No file splits.**
