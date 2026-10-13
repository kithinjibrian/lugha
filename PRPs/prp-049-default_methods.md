## FEATURE: Default methods and `?` error conversion — milestone 22; v5 complete.

**Status:** pre-approved 2026-10-13 — session 61 (built while the user was away; open questions answered with the recommended option, listed below for review; merge held for the user; stacked on `prp-048-type_patterns`)
**Milestone:** 22 (one PRP; completes v5; tags `m22` and `v5` at merge)
**Spec:** `Language v5 Draft.md`:
- decisions 5–6;
- §3 (`iface_item` bodies);
- §4 "Default methods" and "The `?` operator";
- §9 (default-method symbols; reused E0415, E0422, E0424);
- §10 "Errors as interfaces";
- §11 (milestone 22).

**Decisions:** MEMORY 54–57 (v5, interface values, type patterns), MEMORY 50 (`?`), MEMORY 36 (instances).

## OBJECTIVE
An interface method may have a body that a type gets unless it has the method itself. `?` converts an error into an interface error. The §10 errors-as-interfaces program prints `42`, `error: not a number: x`, `range error: 500 is out of range`. With this, the v5 spec is complete.

```
interface Error {
    fun message(self): string;
    fun report(self): string = "error: {self.message()}";
}

fun read(s: string): Result<i64, Error> {
    let n = parse(s)?;    // Result<i64, ParseError>: the ParseError becomes an Error
    Ok(check(n)? * 2)
}
```

## CONTEXT

- **Starting state:**
  - Interface items are signatures only.
  - `?` needs the same error type (E0424).
  - Interface values, tables and type patterns work (PRP-046–048).
- **Ending state:**
  - Default methods are checked once and compiled per type that uses them.
  - `?` converts into an interface error.
  - The v5 spec is complete.
- **Related existing code:**
  - `src/ast/header.rs` (`InterfaceItem`), `src/parser/interface.rs`, `src/parser/sexp/item.rs`;
  - `src/check/interfaces.rs` (`Requirement`), `satisfy.rs` (`unmet`, `through_bound`), `instances.rs` (`bound_target`), `iface_tables.rs` (`conversion_uses`), `methods.rs` (`method_bodies`), `method_calls.rs` (E0415), `generics.rs` (`resolve_named`), `results.rs` (`try_expr`), `mod.rs`;
  - `src/codegen/instances.rs` (`find`, `enter`, `symbol_of`), `interfaces.rs` (`method_of`), `methods.rs` (calls through bounds), `results.rs` (`try_value`), `lower.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery
- **No interview:** the user was away and pre-approved milestones 21 and 22 within the approved v5 draft (session 60).
- **Draft gaps:** where the draft is silent, the decision below takes the recommended option.

### Design
- **A default method is a generic method of the interface:**
  - its key is `<Interface>.<method>`, with one hidden type parameter `Self: <Interface>`, which is the receiver's type;
  - it is checked once, with `self: Self`, so the body can use the interface's items on `self` and nothing else;
  - it is compiled once per type that uses it, as `lugha_fn_<Interface>.<method><Type>` (`lugha_fn_Error.report<ParseError>`), the draft's §9 symbol.
- **Satisfaction:** a missing item that has a default is satisfied. A type's own method with that name must match the signature, or it is E0422 (unchanged).
- **Which body runs:** the type's own method when it has one, else the default's instance:
  - in a dispatch table slot;
  - for a call through a bound (`x.report()` with `T: Error`). The checker records the callee `<Interface>.<method>` so codegen can fall back to the default.
- **Reachability:** the instance pass reaches the default's instance from a bound call (`bound_target`) and from a conversion (`conversion_uses`), so only used defaults compile.
- **`?` conversion:**
  - **When it applies:** `e?` on `Result<T, E>` in a function returning `Result<U, F>`, where `F` is an interface that `E` satisfies;
  - **Checker:** records `Checked.try_conversions` (the operand's id → `F`) and the conversion's reachability;
  - **Codegen:** the failure branch boxes the `E` payload into an `F` before building `Err`.

### Decisions taken while away (for review)
1. **Only methods take a default body.** An associated function with a body is E0201 ("a default needs `self`"). A body without `self` couldn't know its type without `Self`, which is out of scope.
2. **`Self` stays out of scope.** The hidden parameter is named `Self` internally, but a written `Self` in a type position is still an unknown type (E0305), even inside a default body.
3. **Defaults on concrete types:** `c.report()` on a type without its own `report` is E0415, with help naming the interface whose default it is and saying to call it through a value or a bound. This is as the draft says; its §12 question stays open.
4. **An error already of interface type `J` doesn't convert to another interface `F` with `?`.** Only a concrete `E` converts, matching the rule for values.
5. **Any type can use a default,** including primitives satisfying an interface whose remaining items they have (for example one with only default methods). The default's instance receives a pointer to the value, as every method does.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **AST and parser:**
  - `InterfaceItem.default: Option<FunDecl>`;
  - `interface_item` accepts `= expr;` or a block after the signature, and an associated function with a body is E0201;
  - the s-expression form prints the body.
- **Checker:**
  - `Requirement.default: Option<String>`, the interface key when the item has a body;
  - `unmet` forgives a missing item with a default;
  - a new `check/defaults.rs`: registering defaults as methods of the interface key (generics `[Self: I]`, `outer` 1), checking their bodies (called from `method_bodies`), the bound-call callee, and the E0415 help;
  - `bound_target` falls back to the default's instance;
  - `conversion_uses` records the default's instance when the type lacks the method;
  - `resolve_named` ignores the hidden `Self`;
  - `results.rs`: `?` conversion (`Checked.try_conversions`).
- **Codegen:**
  - `instances.rs` finds default bodies (an interface owner: subst `Self`, receiver = the type, symbol `lugha_fn_<I>.<m><T>`);
  - `interfaces.rs::method_of` falls back to the default's instance;
  - `methods.rs`: a bound call falls back to it;
  - `results.rs` boxes a converted error (`interfaces.rs::box_as`).
- **Spec** (`Language v5 Specification.md`):
  - fold in default methods (§3, §4), `?` conversion (§4, amending v4's rule), the symbol (§9) and the §10 program;
  - §11 milestone 22 done;
  - the title drops "(in progress…)" and the status says v5 is complete;
  - the open questions (§12) gain the draft's two v5 items.
- **Docs:**
  - CLAUDE.md (STACK: v5 complete; rule 7: default symbol; anti-pattern 5; tree), README, TODO, CHANGELOG, MEMORY decision 58, CONTEXT;
  - tags `m22` and `v5` wait for the merge.

### Must NOT Do
- **No `Self` in written types, no generic interfaces, no interface-to-interface conversion.**
- **No default associated functions** (decision 1).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **New mistakes reuse existing codes:** E0201 (a default without `self`), E0415 (a default on a concrete type), E0422 (a conflicting own method), E0424 (`?` that neither matches nor converts).
- **Codegen invariants** are `expect`s: a default's instance exists when a table or bound call needs it.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser (`parser/interface.rs`):
- [ ] `fun describe(self): string = "x";` and a block body parse, and print with their bodies.
- [ ] `fun make(): i64 = 1;` in an interface is E0201.

Unit tests — checker (`check/defaults.rs`):
- [ ] **Satisfaction:** a type without the defaulted method satisfies the interface; one with a conflicting signature is E0422.
- [ ] **Body:**
  - the body can call the interface's other methods on `self`;
  - a field of `self` is E0410;
  - an unknown method is E0421;
  - a written `Self` is E0305.
- [ ] **Concrete calls:** `c.report()` without its own method is E0415, with help naming the interface.
- [ ] **Reachability:** converting a type that lacks the method makes `Error.report<T>` an instance, and a type with its own doesn't. A bound call does the same.

Unit tests — checker (`check/results.rs`):
- [ ] **Conversion:** `?` on `Result<i64, ParseError>` in a function returning `Result<i64, Error>` checks.
- [ ] **E0424:** an `E` that doesn't satisfy `F`; an `E` of another interface type.

Unit tests — codegen:
- [ ] A table's slot for a defaulted method points at `lugha_fn_Error.report<ParseError>`.
- [ ] `?` with conversion calls `lugha_rt_alloc` and stores `lugha_vt_ParseError.Error` on the failure path.

Acceptance — `tests/programs/m22/` (`-O0`; `-O2` by hand):
- [ ] `errors.la`: the §10 program.
- [ ] `defaults.la`:
  - a default through a value, through a bound, and overridden;
  - a default calling another default;
  - a generic type using a default;
  - an interface of only defaults satisfied by an `i64`.
- [ ] `e0415.la`: reject golden (a default called on a concrete type).
- [ ] The §10 errors-as-interfaces program runs from the spec.

Regression:
- [ ] Every earlier test passes; programs without defaults or conversions give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-049-default_methods` (on top of `prp-048-type_patterns`), held for the user's review, then merged into `main` after PRP-048.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 errors-as-interfaces program and the `m22/` programs pass.
- [ ] The v5 spec is complete; docs updated.
- [ ] No file over 300 lines (an existing file needing a split: stop and leave it for the user); no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m22/errors.la
```
