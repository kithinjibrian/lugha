## FEATURE: Enums, `match` and `Option<T>` — non-recursive enums as values, exhaustive flat matching, and the first v1 specification file.

**Status:** merged into `main` 2026-10-12 — session 26
**Milestone:** 6, second of three PRPs (019 syntax ✓, 020 this, 021 recursive enums)
**Spec:** `docs/specs/Language v1 Draft.md` §2–§10 (enum parts), folded into a new `docs/specs/Language v1 Specification.md`
**Decisions:**
- MEMORY 23 (v1 direction) and 24 (M6 plan, gate);
- MEMORY 16/17/22 (copies, struct layout, move on last use);
- PRP-013 (no `unsafe` addressing).

## OBJECTIVE
Programs can declare enums, build values (`Shape.Circle(2.0)`, `Shape.Empty`, `Some(x)`, `None`), and take them apart with `match` on enums, integers, `bool`s and strings.

A `match` that misses a case is E0417, listing what's missing. An arm that can never run is warning W0102.

Enums are values. Their payloads can never be changed in place, so copies of an enum share payload arrays, like strings, and no program can tell. The copies that matter still happen: building a variant from a variable (`Job.Batch(xs)` copies `xs`), and storing a payload taken out by `match`.

`lughac spec` prints the new v1 specification, whose §10 shapes program prints `12.56636`, `6.0` and `0.0`. Recursive enums still stop with "not implemented yet: recursive enums (milestone 6)" until PRP-021.

## CONTEXT

- **Starting state:**
  - All v1 syntax parses (PRP-019).
  - The driver gate stops enums, `match` and `Option` (milestone 6), and `impl`/`self` (milestone 7).
  - `lugha::SPEC` is the v0 spec.
- **Ending state:**
  - **Spec:** `Language v1 Specification.md` exists (v0 plus the non-recursive enum sections), and `lugha::SPEC` points at it. The v0 file is frozen.
  - **Checker:** knows `Type::Enum` and `Type::Option`, `Checked.enums`, construction, `match`, E0309/E0311/E0415/E0416/E0417/W0102.
  - **Codegen:** lays out, builds and matches enums.
  - **Gate:** only recursive enums and methods remain.
- **Related existing code:**
  - `src/check/{types,mod,env,call,access,expr,flow,liveness,structs}.rs`, `src/check/errors/*`;
  - `src/codegen/{value,structs,copy,control,function,expr,lower}.rs`;
  - `src/driver/gate.rs`;
  - `src/lib.rs`;
  - `tests/common/{spec_programs,v1_programs}.rs`, `tests/spec.rs`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 26)
- **Layout units:** the payload area is an array of integers of the largest alignment (`%Shape = type { i32, [2 x i64] }`), not of `double`s; the layout and access are the same.
- **`bool` matches** with both `true` and `false` are exhaustive (only integer and string matches need `_`). E0417 for those says "other values of `i64` are not covered", with help "add `_ => …` as the last arm".
- **Gate:** it also stops a *struct* cycle through an enum or `Option` (`struct N { next: Option<N> }`), since it needs indirect slots too. Plain struct cycles stay E0307.
- **`Box<T>`:** `Name<T>` types now parse (recording `Name`), so `Box<List>` reaches the checker's E0305 help instead of a parse error.
- **Approved move:** `type_span`, `is_type_name` and `check_local_name` (E0311), with their test, moved to `check/env.rs`; `enums.rs` was 317 lines.
- **Bug found and fixed:** a function tail `None` was looked up as a local by the return-move check. `Scopes::find` is the non-panicking lookup.
- **Tests:** `tests/codegen.rs` cross-checks `m6/`, skipping programs whose expected exit is 2 (gate stops, which happen in the driver). `tests/spec.rs` checks every rejected §10 program; the JSON line stays with the v0 one.
- **Out of scope, logged in TODO:** a Rust panic in lughac exits 101, not 2 (rule 6).

### Discovery answers (session 26)
1. **Recursive enums are gated by name reachability.** The driver gate follows type names from each enum's payloads, through struct fields, enum payloads and `Option<T>` but not arrays. An enum that reaches itself is "not implemented yet: recursive enums (milestone 6)". PRP-021 deletes the check.
2. **Value semantics for enums carrying arrays are handled in this PRP.** Asked as "deep-copy helpers now".
   - **Revised while drafting:** payloads can't be mutated in place (there is no payload place syntax, `match` bindings are immutable, and `mut self` can only replace the whole value). So enum values can share their payloads safely, and no enum deep-copy helper is needed.
   - **Copies kept:** the copies that keep value semantics are at construction (payloads go through `value_for_store`) and when a `match` binding is stored (bindings are borrowed).
   - **Spec:** the v1 spec states this, refining draft §4/§7, which said enum copies deep-copy their payloads.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**The v1 specification file**
- Create `docs/specs/Language v1 Specification.md`: the v0 text, retitled "Lugha v1 Specification (in progress)", with these sections folded in from the draft:
  - §2: the keywords `enum` and `match`, `=>`, `::`, and the built-in names.
  - §3: the grammar for `enum_decl`, `match_expr` and patterns, plus `Option<T>`.
  - §4: Enums, `match`, `Option`, E0311 name resolution, the copy-table rows that concern enums, and a sentence that enum payloads are immutable, so enum values share them.
  - §5: `match` evaluation.
  - §6: pass 1 and pass 2 additions, and the shared namespace.
  - §7: the non-recursive enum layout, and "copying an enum copies its tag and payload bytes; payload arrays are shared, never mutated".
  - §8: no enums or `Option` across C.
  - §9: `match` lowering, and the codes E0207, E0309, E0311, E0415 (variants), E0416, E0417 and W0102.
  - §10: the shapes program and a rejected enum program (E0417), each with exact expected output.
  - §11: the milestone 6–8 rows.
- **Left in the draft:** methods (`impl`, `self`, E0310), growable arrays, and recursive enums.
  - In the v1 file, `impl`/`self` stay reserved keywords.
  - A note in §1 says which v1 parts are still to come.
- **Draft upkeep:** the draft gets a "Folded into the v1 specification" marker on each moved section.
- **Embedding:** `lugha::SPEC` embeds the v1 file. The v0 file stays as the frozen v0 definition.
- **Tests:**
  - `spec_programs.rs` supports several rejected programs. Each one with an expected-output block is checked against human output (file `main.la`), and the §9 JSON line still belongs to the E0401 program.
  - `tests/common/v1_programs.rs` drops the programs now extracted from the spec.

**Checker** — new `check/enums.rs` (declarations, construction) and `check/matching.rs` (`match`)
- **Types:**
  - `Type::Enum(String)` and `Type::Option(Box<Type>)`, displayed `Shape` and `Option<i64>`.
  - `contains_array` is false for `Enum` and `Option`. Their values are never deep-copied, because payloads are immutable; payload arrays themselves are still copied into a variant from a place.
  - `Checked` gains `enums: Enums` (variant names and payload types, in declaration order).
- **Declarations:**
  - Enum names join pass 1 and the shared global namespace (E0302 against functions, structs, intrinsics, `Option`, `Some`, `None`).
  - Payload types resolve in pass 2.
  - A variant declared twice is **E0309**, with "first declared here".
- **E0311:** a `let`, a parameter, a `for` variable or a `match` binding named like a struct, enum or `Option` is "`Shape` is the name of a type", with help "rename the variable".
- **`Box`:** `Box<…>` (written `Box` as an unknown type name) gets E0305's help "recursive enums need no `Box`; write the type itself".
- **Construction:**
  - `E.V` is a payload-less variant; `E.V(args)` takes payloads, each expected at its declared type.
  - The wrong number of payloads is **E0416**; an unknown variant is **E0415** "no variant `Circel` on `Shape`".
  - `Some(x)` is `Option<T>`, using the expected type if there is one. `None` needs an expected `Option` type (E0412 "cannot infer the type of `None`"). `Some`/`None` with arguments other than one or zero are E0416.
- **Operators:** `==` and other operators on enums or `Option` are E0404. Casts are E0408. Printing them is E0403 via the intrinsic rule. Fields on an enum are E0410.
- **`match`:**
  - The scrutinee is any value.
  - **Variant patterns** need an enum or `Option` scrutinee; their names must be variants (E0415) with the right payload count (E0416).
  - **Literal patterns** need a scrutinee of the literal's type (E0403); an integer pattern takes the scrutinee's integer type, with range checks (E0402).
  - **Bindings** are immutable and borrowed (a new `Binding::Pattern`, treated like `LoopVar` for mutability and moves). Each is in scope in its own arm only.
  - **Type:** arms unify like `if`/`else` (E0403 with both types labelled); diverging arms fit.
- **Exhaustiveness:**
  - An enum `match` without `_` must name every variant. Otherwise it is **E0417** "non-exhaustive match: `Rect`, `Empty` not covered", on the `match` keyword through the scrutinee, with help "add the missing arms, or `_ => …`".
  - Literal matches always need `_`, and E0417 says so.
  - An arm after `_`, or a repeated variant or literal, is **W0102** "unreachable match arm".
- **Statement form:** a `match` statement with a non-`void` value is E0505, like `if`.
- **Liveness:** pattern bindings are mentions like any local, and are never moved from.
- **Returns:** a `match` diverges when every arm does (Type::Never), so return checking (§6) works unchanged.

**Codegen** — new `codegen/enums.rs` (layout, LLVM types, construction) and `codegen/matching.rs` (`match`)
- **Layout:**
  - An enum `E`, and each `Option<T>` (named `Option<T>`), is a named struct `{ i32, [k x iA] }`.
  - `A` is the largest payload alignment (minimum 1). `k × A` is at least the largest variant's payload size, with payloads laid out as a struct at natural alignment.
  - `structs::layout` gains `Enum` and `Option` with the same arithmetic: size `round_up(round_up(4, A) + k·A, max(4, A))`, alignment `max(4, A)`. Arrays of enums therefore use it.
  - `None` is tag 0 and `Some` is tag 1. Variants are numbered in declaration order.
- **Construction:**
  - Store the tag and each payload, through `build_struct_gep` on the variant's payload struct type (`bool` as `i8`), at the payload field of an entry-block slot, then load the whole value.
  - Payloads are evaluated left to right, each through `value_for_store` (copied from a place, moved on last use).
- **`match`:**
  - The scrutinee is stored to a slot once.
  - **Enums:** an LLVM `switch` on the tag; each variant arm loads its bindings from the payload (borrowed slots); `_` is the default, otherwise the default block is `unreachable`.
  - **Integers and `bool`:** a `switch` on the value.
  - **Strings:** a chain of `lugha_rt_str_eq` tests in arm order.
  - **Results:** each non-diverging arm stores the result in a slot, which is loaded at the join.
- **Calls:** enum and `Option` values are passed as pointers to caller storage, like structs (§7). A shared `is_aggregate(ty)` helper replaces the struct-only checks in `function.rs`.
- **Copies:** none for enum values (see discovery answer 2). Payloads go through `value_for_store` at construction, and `copy.rs` is unchanged.

**Gate** — `driver/gate.rs`
- The enum, `match` and `Option` entries are removed.
- Added: a recursive enum → "recursive enums (milestone 6)", found by name reachability (discovery answer 1).
- `impl`/`self` stay at milestone 7.

**Docs**
- CLAUDE.md:
  - the tree (`check/{enums,matching}.rs`, `codegen/{enums,matching}.rs`, the v1 spec file);
  - architecture rule 1 now points at the v1 specification file.
- MEMORY, CHANGELOG, TODO, CONTEXT.

### Must NOT Do
- No recursive enums or indirect slots (PRP-021); no methods (M7); no growable arrays (M8).
- No nested patterns, guards, or float literal patterns (draft §12).
- No `==` on enums.
- No `unsafe`: payload access uses `build_struct_gep` on known types, at constant indexes.
- No new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- Every new rule reports a code and keeps going. `Type::Error` suppresses cascades: a `match` on an `Error` scrutinee checks its arms without exhaustiveness errors.
- Codegen asserts its invariants (`unreachable!` for gated or rejected forms), as before.

## SECURITY CONSIDERATIONS

- **Bounded payload access:** a payload is read only after its tag has been compared, through constant-index GEPs on the variant's payload struct, inside the enum's own storage.
- **Unreachable defaults:** an exhaustive `match` reaches `unreachable` only if a tag is corrupt, which ordinary code can't produce.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests:
- [x] **Checker:**
  - enums in any order;
  - E0302, E0309, E0311 (let, param, for, binding);
  - `Box<…>` help;
  - construction types, E0415, E0416;
  - `Some`/`None` inference and E0412;
  - E0404 on `==`;
  - `match` types and arm mismatches;
  - E0417, listing every missing variant, and for a literal `match` without `_`;
  - W0102;
  - bindings: borrowed, immutable (E0501), scoped to their arm;
  - E0505 for a discarded `match` statement;
  - a diverging `match`.
- [x] **Codegen IR:**
  - `%Shape = type { i32, [2 x double] }`;
  - a `switch i32` on the tag;
  - a string `match` calls `lugha_rt_str_eq`;
  - an enum argument passed as `ptr`;
  - `let b = a;` on an enum with an array payload emits no copy, while `Job.Batch(xs)` with `xs` used later copies `xs`.
- [x] **Layout:** sizes of `{ Circle(f64), Empty }` (16), `{ A(u8), B }` (8), and an enum inside a struct.
- [x] **Gate:**
  - recursive enums (direct, through a struct, through `Option`) are gated;
  - recursion through an array is not;
  - non-recursive enums pass.

Acceptance — `tests/programs/m6/` (also at `-O2`) and the spec:
- [x] **Spec §10:**
  - the shapes program (`12.56636`, `6.0`, `0.0`);
  - the E0417 rejected program, with exact human output.
- [x] **`enum_basics.la`:**
  - payloads of every primitive type, plus string and struct payloads;
  - `Option` from a function, `Some`/`None` in `match`;
  - integer, string and `bool` matches with `_`;
  - a `match` used as an expression, and one used as a statement;
  - enums in arrays and in struct fields.

  Expected stdout written by hand.
- [x] **`enum_values.la`:**
  - `Job.Batch(xs)` then mutating `xs` leaves the payload unchanged;
  - a payload taken out by `match` into `let mut w = v;` and mutated leaves the enum unchanged;
  - two copies of an enum both print the original payload.
- [x] **Reject programs:** `e0309.la`, `e0311.la`, `e0415.la`, `e0416.la`, `e0417_literal.la`, and `w0102.la` (warning only: exit 0 with stderr).
- [x] **Recursion gate:** `lughac check` on the linked-list program exits 2 with "recursive enums (milestone 6)".
- [x] All earlier tests pass; `lughac spec` prints the v1 file.

## ROLLBACK PLAN

- Branch `prp-020-enums_and_match`, merged into `main` after green CI.
- To abandon: delete the branch; the gate keeps enums stopped.

## ACCEPTANCE CRITERIA
- [x] The shapes program prints its three lines; the rejected program reports E0417 exactly as the spec shows.
- [x] Every test above passes; recursive enums stop at the gate.
- [x] The v1 specification file exists and is what `lughac spec` prints; CLAUDE.md, MEMORY.md, TODO.md, CHANGELOG.md updated.
- [x] No file over 300 lines (with the splits); no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run <shapes>` prints the three lines; `cargo run -q -- spec | head -1` shows the v1 title
