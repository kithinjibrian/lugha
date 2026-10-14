## FEATURE: `Self` everywhere — `Self` in `impl` blocks, and default methods on concrete types (E0430); milestone 34.

**Status:** approved 2026-10-14 — session 81. This is part of the user's "in one go" (milestones 34–36), merged once CI is green.
**Milestone:** 34 (one PRP; tag `m34` at merge; `Language v10 Specification.md` begins)
**Spec:** `Language v10 Draft.md`:
- decisions 1 and 5, and the choices on `Self` and on default methods;
- §3 (`Self`);
- §4 "`Self` in `impl` blocks" and "Default methods on concrete types";
- §9;
- E0430;
- §10 "`Self` in `impl` blocks";
- §11 (milestone 34);
- §12 (the v5 question on default methods, now decided).

**Decisions:** MEMORY 76 (v10), MEMORY 63 (`Self` in interfaces), MEMORY 49 (default methods).

## OBJECTIVE
The §10 `Self` program prints `(3, 4)`, `this is (3, 4)`, `green`:

```
impl Point {
    fun origin(): Self = Self { x: 0, y: 0 };
    fun unit(): Self = Self.origin().plus(Self { x: 1, y: 1 });
}
println(p.describe());   // a default method of `Describe`, on a `Point`
```

## CONTEXT

- **Starting state:**
  - `Self` is the hidden type parameter of interface items and default bodies (`check/defaults.rs::SELF`). Anywhere else it is E0305.
  - A default method called on a concrete type is E0415, with help naming the interface (`Checker::default_help`).
- **Ending state:** `Self` works in `impl` blocks; defaults are callable on structs and enums; `Language v10 Specification.md` begins.
- **Related existing code:**
  - `src/parser/{mod,decl,primary,generic}.rs`;
  - `src/check/{method_calls,defaults,satisfy,bound_infer,visibility}.rs`, `src/check/modules/mod.rs` (`key`, `imports`);
  - `src/codegen/{methods,defaults}.rs`: `default_method` already resolves a call whose callee the checker recorded as `Interface.method`.
- **Open decisions that must be resolved first:** none.

### Design
- **`Self` in `impl` blocks is resolved by the parser,** not the checker. While it reads `impl T<P…> { … }`, the parser knows the type as written. It replaces each `Self`, keeping the span:
  - in a type position, by `T`, or `T<P…>` for a generic `impl`;
  - as an expression name or a struct literal's name (`Self { … }`, `Self.f()`, `Self.Red`), by `T`, exactly as if `T` were written there.

  So the checker and codegen see ordinary programs, and their rules (inference for `Stack { … }`, E0418, visibility) apply unchanged. The draft's §9 says "resolved by the checker"; the spec text states the visible behaviour only, so this mechanism is an implementation choice.
  - Outside `impl` blocks and interfaces nothing changes: `Self` stays a plain name (E0305 as a type, E0301 as a value).
  - `--emit=ast` shows the replaced name.
- **Default methods on concrete types** (new `check/concrete_defaults.rs`): in `on_value`, when a struct or enum receiver has no method `name`:
  - **Candidates:** every interface visible here that declares a default `name` and that the receiver satisfies (`unmet` is empty). Visible means:
    - declared in the current module, in a module it imports, or in the prelude;
    - not denied by visibility (v8 levels).

    For a generic interface, its arguments are read off the receiver's methods (`read_off`); if any stay unknown, it isn't a candidate.
  - **Exactly one:** check the call against the item with `Self` replaced by the receiver (`items_for`), and record:
    - the callee `Interface.name`;
    - `default_args` for a generic interface;
    - a use of the default's instance at `[receiver, args…]` (`used`), so `monomorphize` lists it.

    Codegen's existing `default_method` then calls `lugha_fn_<Interface>.<name><Type>`.
  - **None:** E0415 with the existing help.
  - **Two or more:** E0430 at the method name, naming the first two (sorted).
  - **Other receivers** (numbers, strings, arrays, `Option`) keep E0415. They have no `impl` blocks, and codegen sends their method calls elsewhere. The spec says so.
- **Programs without these** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Parser:** a field `impl_self: Option<(Ident, Vec<Ident>)>`, set while reading an `impl` body; the replacement in `named_type` and `name_or_struct` (helpers in a new `parser/self_type.rs`).
- **Checker:** `check/concrete_defaults.rs`, its hook in `method_calls.rs::on_value`, and E0430 in a new `errors/v10.rs`.
- **Spec:** start `Language v10 Specification.md` from v9 (in progress: milestone 34), and point `lib.rs` `SPEC` and `tests/cli.rs` at it. Fold in:
  - §3 and §4 (`Self` in `impl` blocks; default methods on concrete types);
  - §9;
  - the E0430 row;
  - the §10 program;
  - §11;
  - §12;
  - "Changes since v9".
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 77, CONTEXT. The tag `m34` is added at the merge.

### Must NOT Do
- **No `Self` in struct or enum declarations,** and none in patterns (`Self.Red =>` in a `match`): `Self` is replaced only in types and expressions.
- **No default methods on primitives, strings, arrays or `Option`.**
- **No change to calls through bounds or interface values.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0430:** a default method that two visible interfaces supply.
- **E0415:** none supplies it (unchanged, with its help).
- **E0305:** `Self` as a type outside interfaces and `impl` blocks (unchanged).
- **Inside `impl` blocks,** errors about `Self` are those of the type it stands for: E0418 never arises, since a generic `impl`'s `Self` carries its parameters.

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — parser (`parser/self_type.rs`):
- [x] In `impl Point`:
  - `fun o(): Self = Self { x: 0 };` prints with `Point` in both places;
  - `Self.origin()` and `Self.Red` print as `Point.origin()` and `Point.Red`.
- [x] In `impl Stack<T>`, `fun f(self, o: Self): Self[]` gives `Stack<T>` types.
- [x] Outside an `impl`, `Self` stays `Self`.

Unit tests — checker (`check/concrete_defaults.rs`):
- [x] **Allowed:**
  - a default called on a struct and on an enum;
  - the type's own method wins;
  - a generic interface's default, its arguments read off the type.
- [x] **E0430:** two interfaces with the same default.
- [x] **E0415:**
  - an interface the type doesn't satisfy;
  - an `i64` receiver.
- [x] **`Self` in an `impl`:** checks as the type; E0305 in a struct declaration.

Acceptance — `tests/programs/m34/`:
- [x] `self.la`: the §10 program.
- [x] `more.la`:
  - `Self` in a generic `impl Stack<T>`;
  - `Self` in closures and arrays inside methods;
  - `Self.Variant` with payloads;
  - a default from a generic interface on a concrete type;
  - a default called on `self` inside an `impl`;
  - a default that calls another default.
- [x] `e0430.la`: reject golden.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-063-self_everywhere`, merged into `main` after CI passes; tag `m34`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m34/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] `Language v10 Specification.md` begins; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m34/self.la
```

## AMENDMENTS (during build, session 81)

- **Two earlier tests asserted the old rule:**
  - `check/defaults.rs`: a default on a concrete type was E0415;
  - `check/self_type.rs`: `Self` in an `impl` was E0305.

  They now assert the v10 behaviour. The `m22/e0415.la` golden now has `Sq.area` return `i64`, so `Sq` no longer satisfies `Shape`, and its stderr is unchanged.
- **`read_off`** in `check/bound_infer.rs` became `pub(super)`.
- **A rejected program has no `.exit` file,** only `.stderr`.
- **IR:** the 133 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for `m34/self` and `m34/more`.
- **No file splits.**
