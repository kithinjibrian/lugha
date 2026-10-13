## FEATURE: Generic interfaces as bounds — `interface Source<T>`, `S: Source<T>`, with `T` inferred.

**Status:** approved 2026-10-13 — session 64
**Milestone:** 23, the first of two PRPs: **050 is bounds**, 051 is generic interface values (which completes the milestone and adds the tag `m23`)
**Spec:** `Language v6 Draft.md`:
- decision 3;
- §3 (`interface_decl` with `type_params`, `bound` with arguments);
- §4 "Generic interfaces", except "As a value type";
- §9 (the symbol for a default method of a generic interface; reused E0412, E0418, E0419, E0422).

**Decisions:** MEMORY 59 (v6 direction), MEMORY 58 (default methods as generic methods of the interface), MEMORY 36 (instances).

## OBJECTIVE
An interface can take type parameters, and a bound can pass it arguments. A parameter that appears only inside a bound is worked out from the concrete type's methods:

```
interface Source<T> {
    fun next(mut self): Option<T>;
    fun skip(mut self): bool = self.next().is_some();   // default, per (type, T)
}

fun collect<T, S: Source<T>>(src: S): T[] { … }

collect(Count { n: 0, end: 3 })               // S = Count, T = i64 from Count.next
collect(Words { xs: ["a", "b"], at: 0 })      // T = string
```

The program builds and runs. Using a generic interface as a value type (`let s: Source<i64> = …`) is still rejected, with E0418, until PRP-051.

## CONTEXT

- **Starting state:**
  - Interfaces have no type parameters, and a bound is a name (`ast::Bound { module, name }`).
  - `generics::Bounds.interfaces` is a list of keys.
  - A requirement's types are concrete.
- **Ending state:**
  - Interfaces have type parameters, and bounds carry arguments.
  - Every place that reads a bound's items gets them with the arguments substituted.
  - Inference goes through bounds.
  - Default methods of generic interfaces compile per (type, arguments).
  - `Language v6 Specification.md` begins (in progress: milestone 23).
- **Related existing code:**
  - AST and parser: `src/ast/header.rs` (`InterfaceDecl`, `Bound`), `src/parser/interface.rs` (`interface_decl`, `bound`), `src/parser/sexp/item.rs`.
  - Checker:
    - `src/check/interfaces.rs` (`InterfaceInfo`, `resolve_interfaces`, `bounds_of`, `check_bounds`);
    - `generics.rs` (`Bounds`, `resolve_named`, `resolve_applied`);
    - `satisfy.rs` (`bound_item`, `unmet`, `offer`);
    - `bounds.rs` (`meets_bounds`);
    - `infer.rs` (`infer`, `unify`);
    - `defaults.rs` (`register_defaults`, `default_target`);
    - `instances.rs` (`bound_target`);
    - `iface_values.rs` (`interface_type`).
  - Codegen: `src/codegen/instances.rs` (`find`, `enter`, `symbol_of`).
  - Spec and its callers: `src/lib.rs` (`SPEC`), `tests/cli.rs`, `docs/specs/`.
- **Open decisions that must be resolved first:** none.

### Discovery (session 64)
- **Split:** milestone 23 is two PRPs, generic interfaces as bounds and then as values, each running end to end with no gate. Calls through a bound are already compiled per instance, so bounds need almost no codegen.

### Design
- **An applied interface:** a new `check::Applied { key, args }` replaces the bare key in `Bounds.interfaces`. A non-generic interface has `args: []`, so every v5 bound reads the same.
  - **`Checker::items(&Applied)`** returns the interface's requirements with its parameters replaced by `args`, using `generics::subst`.
  - **Every reader of bound items goes through it:** `bound_item`, `unmet`/`offer`, `check_bounds` (E0423), `default_target` and `default_help`.
- **Declarations:**
  - `InterfaceInfo.generics` holds the interface's parameters, with bounds.
  - In pass 2 they are in scope (`type_params`) while the items resolve, so `Option<T>` in an item becomes `Option<Param("T")>`.
  - A parameter name is checked like a struct's (E0316), and `Self` is a bad name (E0316), since milestone 25 gives it a meaning.
- **Bounds with arguments:**
  - `bounds_of` resolves each bound's argument types with the declaration's parameters in scope, so `S: Source<T>` can name `T`.
  - A wrong count, or a generic interface written bare as a bound, is E0418.
  - An argument that misses the interface parameter's bound is E0419, checked once at the declaration.
  - A bound's argument types follow the usual name and visibility rules (E0305, E0313).
- **Inference through bounds** (new file `check/bound_infer.rs`, since `infer.rs` is at 291 lines):
  - **When:** after `infer` has bound what the arguments and expected type decide, and before the E0412 loop.
  - **The step:** for each parameter `S` that is bound, take each bound `Source<X…>` whose arguments mention a parameter that isn't bound yet. Unify each item's declared signature (arguments substituted, unknowns kept) with what `S`'s type offers for that item:
    - for a struct or enum, its method with the `impl`'s parameters substituted;
    - for a type parameter, its bounds' items;
    - for a non-generic interface value, its items.
  - **Order:** the step repeats until nothing new is bound, so `T` can come from `S`, and `U` from `T`.
  - **What's left:** a parameter still unbound is E0412. A binding that the arguments also fix, but differently, fails the satisfaction check (E0422), exactly as today.
- **Default methods of generic interfaces:**
  - They are registered with the generics `[Self: Source<T>, T]` (Self first, then the interface's parameters), with `outer` equal to 1 plus the number of parameters.
  - An instance's arguments are `[concrete, X…]`.
  - **Codegen:** `enter` maps `Self` and then the interface's parameter names. `symbol_of` prints `lugha_fn_Source<i64>.skip<Count>`, which is the draft's §9 symbol. A non-generic interface keeps `lugha_fn_Error.report<ParseError>`.
- **Generic interfaces as value types wait for PRP-051.** `Source<i64>` in a type position is E0418 with the message "generic interface `Source` can't be a value type yet", and a bare `Source` there is E0418 as for structs. PRP-051 replaces the first case.
- **Not affected:**
  - interface values of non-generic interfaces;
  - their tables, `Checked.interfaces` and conversions, since generic interfaces aren't converted yet;
  - type patterns.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **AST and parser:**
  - `InterfaceDecl.type_params: Vec<TypeParam>` and `Bound.args: Vec<Type>`, where the span covers the arguments;
  - `interface_decl` reads `type_params`, and `bound` reads `<type, …>`;
  - the s-expression form prints `(interface (Source T) …)` and `(S (Source T))`.
- **Checker:**
  - `Applied`;
  - `InterfaceInfo.generics`;
  - `items`;
  - each reader above switched to `items`;
  - bound resolution with E0418 and E0419;
  - `bound_infer.rs`;
  - defaults over the interface's parameters;
  - E0418 for a generic interface as a type;
  - `bound_target` and `default_target` carry the interface's arguments.
- **Codegen:** `instances.rs`: `Source.interface` carries the interface's parameter names, and `enter` and `symbol_of` handle them.
- **Spec:** create `Language v6 Specification.md` from v5 (v5 is then frozen), titled "Lugha v6 Specification (in progress: milestone 23)". Fold in:
  - the draft's §3 grammar lines;
  - §4 "Generic interfaces", without "As a value type", plus a sentence that a generic interface is not yet a type;
  - the §9 default-method symbol;
  - "Changes since v5";
  - §11: milestone 23 in progress, with v6's out-of-scope list.

  `lughac spec` prints it (`lib.rs`, `tests/cli.rs`).
- **Docs:**
  - CLAUDE.md: STACK says v6 is in progress, plus the tree and anti-pattern 5;
  - README, TODO, CHANGELOG, MEMORY decision 60, CONTEXT.

### Must NOT Do
- **No generic interface values:** no conversion, no tables, no `Type::Interface` arguments. That is PRP-051.
- **No `Self` and no `Iter`:** those are milestones 25 and 24.
- **No item-level type parameters in interfaces:** they are still syntax errors, as in v5.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **No new codes.** The existing ones cover every mistake:
  - **E0418:** a bound or type with the wrong number of arguments; a bare generic interface; a generic interface as a type (until PRP-051).
  - **E0419:** a bound argument that breaks the interface parameter's bound.
  - **E0412:** a parameter that nothing fixes.
  - **E0422:** a type that doesn't satisfy `Source<X>`, with help showing the substituted signature (`` `next` should be `fun next(mut self): Option<i64>` ``).
  - **E0423:** two bounds that clash after substitution.
  - **E0316:** a bad interface parameter name.
- **Codegen invariants** are `expect`s: a default's instance has the interface's arguments.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser (`parser/interface.rs`):
- [x] `interface Source<T: Eq> { fun next(mut self): Option<T>; }` and `fun f<T, S: Source<T> + geo.Sink<T[]>>(s: S)` parse and print.

Unit tests — checker (new `check/bound_infer.rs`; `check/interfaces.rs` and `satisfy.rs` for the rest):
- [x] **Declarations:**
  - items use the interface's parameters;
  - `interface I<T, T>` and `interface I<Self>` are E0316;
  - an unknown type in an item is E0305.
- [x] **Bounds:**
  - `S: Source<i64>` and `S: Source<T>` check;
  - `S: Source` and `S: Source<i64, i64>` are E0418;
  - `S: Sorted<Point>`, with `interface Sorted<T: Ord>`, is E0419;
  - `S: Source<i64> + Source<string>` is E0423.
- [x] **Calls through bounds:** `s.next()` has type `Option<T>` with the bound's arguments, and `x.next()` with `S: Source<i64>` gives `Option<i64>`.
- [x] **Satisfaction:**
  - `Count` satisfies `Source<i64>`;
  - `Count` doesn't satisfy `Source<string>`: E0422, whose help shows the substituted signature;
  - a generic `Repeat<T>` satisfies `Source<T>` at each instance;
  - a parameter `U: Source<i64>` satisfies `Source<i64>`.
- [x] **Inference:**
  - `collect(Count {…})` gives `T = i64`;
  - inference chains (`T` from `S`, then `U` from `T`);
  - a parameter in no argument and no bound is E0412;
  - `first<T, S: Source<T>>(s: S, d: T)` called with `d = "x"` on a `Count` is E0422.
- [x] **Defaults:**
  - a default of a generic interface checks with `self.next()` typed `Option<T>`;
  - a call through a bound reaches the instance `Source.skip` with `[Count, i64]`.
- [x] **Not yet a type:** `let s: Source<i64> = …` and `fun f(s: Source<i64>)` are E0418 with the "not a value type yet" message, and a bare `Source` is E0418.

Unit tests — codegen (`codegen/instances.rs`):
- [x] A generic interface's default called through a bound defines `lugha_fn_Source<i64>.skip<Count>`.

Acceptance — `tests/programs/m23/`:
- [x] `bounds.la`:
  - `collect` over `Count` and `Words`;
  - a default method of a generic interface, overridden by one type and inherited by another;
  - a generic `Repeat<T>` source;
  - inference chained through two bounds.
- [x] `modules/` (a directory case): an interface `geo.Source<T>` from another module, used as `S: geo.Source<T>`.
- [x] Reject goldens:
  - `e0422.la`: `Count` passed where `S: Source<string>` is needed;
  - `e0418.la`: a generic interface as a value type.

Spec and CLI:
- [x] `lughac spec` starts with `# Lugha v6 Specification` (`tests/cli.rs`).
- [x] The v5 §10 programs still pass through the new spec (`tests/spec.rs`).

Regression:
- [x] Every earlier test passes, and programs without generic interfaces give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-050-generic_interface_bounds`, merged into `main` after CI passes.
- To abandon: delete the branch. `main` stays at v5.

## ACCEPTANCE CRITERIA
- [x] The `m23/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] `Language v6 Specification.md` begins (in progress: milestone 23); docs updated.
- [x] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m23/bounds.la
```

## AMENDMENTS (during build, session 64)

- **Struct and enum bounds are recomputed in pass 2.** Bound arguments can name types that pass 1 hasn't collected yet, so `bounds_of` resolves them quietly (`Checker::quietly`), and `resolve_structs`/`resolve_enums` rebuild their generics once every type is known. Interface parameter bounds come from a new pass, `interface_params`, after `collect_enums`.
- **`Checked.default_args`** (a new side table): a bound call that may reach a generic interface's default records the interface's arguments, in the caller's parameters. Codegen substitutes them to name the instance. The callee key alone couldn't say which `Source<X>` instance to call.
- **S-expressions:** interface parameters print as `(type-params …)`, as for structs: `(interface Source (type-params (T Eq)) …)`.
- **`impl` parameter renaming** (`generics::renamed`) also renames bound arguments, so `impl W<A, B>` keeps `B: Source<A>`.
- **New error constructor** `errors/v6.rs::generic_interface_value` (E0418), removed by PRP-051.
- **`-O2`** was checked by hand for `m23/bounds`.
- **No file splits.**
