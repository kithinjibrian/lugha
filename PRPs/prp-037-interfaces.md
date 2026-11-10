## FEATURE: Interfaces — structural bounds on type parameters; v2 complete.

**Status:** merged 2026-10-13 — session 46
**Milestone:** 12 (one PRP; completes milestone 12 and the v2 spec, tag `m12`)
**Spec:** `Language v2 Draft.md`:
- decision 10;
- §3 (`interface_decl`, `iface_item`, `bound`);
- §4 "Interfaces";
- §9 (E0421, E0422; E0305 reused);
- §10 (the interfaces program);
- §11 (milestone 12).

Also `docs/decisions/generics.md` (the owner's note, as adapted in session 43).

**Decisions:**
- MEMORY 40 (interfaces amendment: bounds only, structural, concrete signatures, no `Self`);
- MEMORY 41, 42 (generic checking and compiling);
- MEMORY 37 (visibility), MEMORY 39 (keys, symbols).

## OBJECTIVE
Generic code can call methods through interface bounds, and the §10 interfaces program prints `15.0`, `9.0`, `square`.
- **Declaring:** `interface Shape { fun area(self): f64; fun name(self): string; }` lists methods (with `self` or `mut self`) and associated functions (without).
- **Using:** an interface is a bound, `fun total<T: Shape>(xs: T[])`, combined freely with `Eq` and `Ord` (`T: Shape + Eq`), and may come from another module (`T: shapes.Shape`).
- **Structural:** any type whose visible methods match satisfies an interface; there is no `impl Shape for`.
- **Compiled per instance:** inside each instance, `s.area()` is a plain call to the concrete type's method.
- **v2 complete:** the spec drops "in progress".

## CONTEXT

- **Starting state:**
  - Generics are checked and compiled (milestone 11).
  - A bound other than `Eq`/`Ord` is E0315.
  - `interface` is an ordinary identifier, and bounds are bare names (`TypeParam.bounds: Vec<Ident>`).
- **Ending state:**
  - `interface` is a keyword with its own item.
  - Bounds may name interfaces, and generic bodies call through them.
  - Uses are checked for satisfaction, and codegen calls the concrete methods.
  - The spec is complete.
- **Related existing code:**
  - `src/lexer/token.rs`;
  - `src/ast/{mod,header}.rs`;
  - `src/parser/{item,generic,decl}.rs` and `sexp/`;
  - `src/check/` (`generics.rs`, `bounds.rs`, `method_calls.rs`, `instances.rs`, `env.rs`, `visibility.rs`, `modules/lookup.rs`, `errors/`);
  - `src/codegen/{methods,instances}.rs`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 46)
1. **One PRP:** syntax, checking, compiling and the spec fold together; tag `m12`.
2. **Clashing bounds:** if two bounds of one parameter declare the same name with different signatures, it is **E0423** at the bound list ("`A` and `B` both declare `size` with different signatures"). Identical duplicates are fine.

### Design choices for review
- **What a type parameter has:** the items of all its interface bounds. A parameter used as a type argument satisfies an interface when its bounds provide every item with the same signature; this is structural, as for any other type.
- **Generic types satisfy too:** `Stack<i64>` satisfies `interface Sized { fun size(self): i64; }` through `impl Stack<T> { fun size(self): i64 … }`, with the `impl`'s parameters substituted. A method with its own type parameters never matches, because interface signatures are concrete.
- **Types without methods:** primitives, strings, arrays and `Option` have no methods (the array built-ins don't count), so they satisfy only an interface with no items.
- **Visibility:** a method counts only where the bound is checked. A `private` method of another module's type doesn't satisfy an interface there (E0422 names it as missing).
- **Instances:** a call through a bound is recorded like a generic call. When the instance pass substitutes the concrete type, it resolves the call to that type's method, adding an instance if that method is itself generic (`Stack.size<i64>`). Codegen does the same lookup with the substituted receiver type, so it still infers nothing.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Syntax**
- **Lexer:** `interface` becomes a keyword. A program using it as a name now fails to parse; the spec's compatibility sentence says so.
- **Grammar** (draft §3):
  - `item = [ visibility ] ( … | interface_decl ) | impl_decl`;
  - `interface_decl = "interface" IDENT "{" { iface_item } "}"`;
  - `iface_item = "fun" IDENT "(" [ receiver [ "," params ] | params ] ")" [ ":" type ] ";"`;
  - `bound = "Eq" | "Ord" | IDENT [ "." IDENT ]`.

  An interface item with a body or type parameters is a parse error, with the existing codes.
- **AST:** `Item::Interface(InterfaceDecl)`; a bound becomes `{ module: Option<Ident>, name: Ident }`.
- **S-expressions:** `(interface Shape (fun area (self) f64) …)`, plus `T: m.Shape` in `type-params`.

**Checker** — new `check/interfaces.rs` (the declarations and satisfaction), with calls through bounds in `method_calls.rs` or a new file
- **Declarations:**
  - interfaces share their module's item namespace (E0302 for duplicates, built-in names and intrinsics) and follow `private`/E0313/E0318 like structs;
  - an interface's item names are unique within it (E0310);
  - signatures resolve in pass 2 and use concrete types only (a type parameter there is E0305, since none is in scope).
- **Bounds:**
  - `T: Shape` and `T: m.Shape` resolve to the interface's key. A name that is neither `Eq`, `Ord` nor an interface is E0315 ("unknown bound"); a type name there gets help "only interfaces, `Eq` and `Ord` can be bounds".
  - E0423 is reported at the bound list for a name declared by two of its interfaces with different signatures.
- **Interface as a type:** writing one in a type position (`let s: Shape = …`, a parameter, a field) is E0305, with help "use it as a bound: `<T: Shape>`".
- **Calls through a bound** (in a generic body):
  - `x.m(args)` with `x: T` checks against the signature the bound declares (receiver kind included: a `mut self` item needs a mutable place, as today);
  - `T.f(args)` works likewise for an associated item;
  - a name no bound declares is **E0421** "no bound of `T` declares `m`", with help "add an interface that declares `m`, e.g. `T: Shape + …`".
- **Satisfaction at a use:** wherever E0419 is checked (inferred arguments, written type arguments), a type argument that doesn't satisfy an interface bound is **E0422**. It names the type and the interface, and lists each missing or mismatched item.
- **Instances:** calls through bounds join the instance pass (above). Generic methods reached this way get instances, and E0420 still applies.

**Codegen**
- A method or associated call whose receiver or type is a substituted type parameter calls the concrete type's method (an instance symbol when that method is generic). No other change.

**Spec** — `Language v2 Specification.md` (approved draft text folded in)
- **§2:** `interface` among the keywords.
- **§3:** the grammar lines above.
- **§4:** "Interfaces" (declaring, satisfying, using as a bound, at a use, not a type, names and visibility), with clashing bounds (E0423).
- **§6:** interfaces in each module's namespace.
- **§9:** E0421, E0422 and E0423 in the code table (E0423 is new; the draft's table gets it too), and E0305's row extended.
- **§10:** the interfaces program.
- **§11:** milestone 12 marked done.
- **Completion:**
  - the title drops "(in progress: …)";
  - the status paragraph says v2 is complete;
  - "Changes since v1.2" gains interfaces;
  - the compatibility sentence adds `interface` as a reserved name.

**Docs**
- CLAUDE.md:
  - STACK (the spec is complete) and the tree;
  - anti-pattern 5: v2 is complete, so further language features need a spec change (v3).
- README, TODO (milestone 12 ✅, v2 complete), CHANGELOG, MEMORY decision 43, CONTEXT; tags `m12` and `v2`.

### Must NOT Do
- **Not in v2:** interface values or dynamic dispatch, `Self`, generic interfaces, default method bodies, operator overloading, explicit type arguments, `.len()`.
- **No `impl Shape for T`:** satisfaction is structural only.
- **No inference in codegen.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Records:** E0421, E0422, E0423 and the extended E0305/E0315 are `Diagnostic` records; checking continues after each.
- **Codegen invariants:** a call through a bound that has no concrete method in an instance is a compiler bug (an `expect` stating the invariant, exit 2).

## SECURITY CONSIDERATIONS
- No `unsafe`, no new input surfaces. Satisfaction checks are bounded by the program's own declarations.

## TESTS TO WRITE

Unit tests — lexer/parser:
- [x] `interface` lexes as a keyword.
- [x] `interface Shape { fun area(self): f64; fun make(r: f64): Circle; }` parses and prints as an S-expression.
- [x] `T: Shape + Eq` and `T: m.Shape` parse.
- [x] An interface item with a body is a parse error.

Unit tests — checker (`check/interfaces.rs` and the call file):
- [x] **Accepted:** the draft's interfaces program checks.
- [x] **Declarations:** a duplicate interface (E0302); a duplicate item (E0310); an unknown type in a signature (E0305).
- [x] **Bounds:** `T: Circle` (a struct) is E0315 with the help; `T: m.Shape` from an import works; a `private` interface from another module is E0313.
- [x] **E0423** for `T: A + B` with `size(self): i64` and `size(self): f64`; identical duplicates are accepted.
- [x] **Interface as a type** is E0305 with the help.
- [x] **Calls through bounds:**
  - `s.area()` types as `f64`;
  - `T.make(1.0)` works;
  - `s.perimeter()` is E0421 with the help;
  - wrong argument types are the usual E0401/E0403;
  - a `mut self` item called on an immutable binding is the usual E0501.
- [x] **E0422:**
  - `total_area([1, 2])` (i64 has no `area`);
  - a type whose `area` returns `i64` (mismatched);
  - a `self` item met only by `mut self` (receiver kind);
  - a `private` method of another module's type.

  The message lists each missing or mismatched item.
- [x] **Generic types satisfy:** `Stack<i64>` meets `Sized` through `impl Stack<T>`; a parameter `U: Shape` passed on to `T: Shape` satisfies it.
- [x] **Instances:** a call through a bound on a generic receiver adds the method's instance (`Stack.size<i64>`).

Acceptance — `tests/programs/m12/` (run, `-O0` and `-O2`):
- [x] `shapes.la`: two shape types through `total_area<T: Shape>` and `describe<T: Shape>`, with an associated item through `T.make`.
- [x] `generic_impl.la`: `Stack<i64>` and `Stack<string>` satisfying `Sized` through a generic `impl`.
- [x] `bounds.la`: `T: Shape + Ord`-style combinations (`Eq` and an interface together), and a `mut self` item changing the caller's value.
- [x] `modules/`: an interface declared in one module, satisfied by a type of a second, used as a bound in the main file.
- [x] `e0421.la`, `e0422.la`, `e0423.la`: reject goldens.
- [x] The §10 interfaces program runs from the spec (`tests/spec.rs`).

Regression:
- [x] Every earlier test passes. Non-interface programs are unchanged, except that the name `interface` is now reserved.

## ROLLBACK PLAN

- Branch `prp-037-interfaces`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 interfaces program and the `m12/` programs pass.
- [x] E0421, E0422, E0423 and the E0305/E0315 helps are reported as specified.
- [x] The spec is complete (no "in progress"); CLAUDE.md, README, TODO, CHANGELOG and MEMORY are updated; tags `m12` and `v2`.
- [x] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m12/shapes.la
```

## AMENDMENTS (during build, session 46)

- **Split (approved):** the checker's test helpers moved verbatim from `check/mod.rs` to `check/test_util.rs`, to make room for the interface state.
- **Parse error code:** an interface item with a body is E0201 at `{` (the parser's "expected `;`"), not E0202.
- **Wrong kind of call:** calling a required associated function on a value, or a method on the type, is E0415 with a hint, as for concrete types.
- **E0422 wording:** "`i64` doesn't satisfy `Shape`", with help "no method `area`" or "`area` should be `fun area(self): f64`".
- **No E0318 for bounds:** bounds aren't types in signatures. A non-private function may name a private interface as a bound of its own module, since callers elsewhere never write it.
