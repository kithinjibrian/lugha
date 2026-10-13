## FEATURE: Compiling generics — instances, monomorphization, E0420, the gate removed, the spec folded.

**Status:** approved 2026-10-13 — session 45
**Milestone:** 11 (second of two PRPs: 035 checking, **036 compiling**); completes milestone 11 (tag `m11`)
**Spec:** `Language v2 Draft.md`:
- decisions 3–5;
- §4 ("Generic declarations", "Using a generic", "`Option<T>`", "Generic recursion", "Copy table");
- §6 ("Generic functions");
- §7, §8 (generic instances lay out like concrete types; no generic types in externs);
- §9 (the symbol table's "Generic instance" row, "Monomorphization", E0420, E0304 for a generic `main`);
- §10 (the generics program);
- §11 (milestone 11);
- §12 (the depth limit, 64).

**Decisions:**
- MEMORY 41 (generic checking, `Checked.instances`), MEMORY 39 (keys, symbols, side tables);
- MEMORY 40 (interfaces are milestone 12);
- MEMORY 34 (the gate).

## OBJECTIVE
Generic programs compile and run. The §10 generics program prints `b`, `true`, `plum`.
- **Instances:** each generic function and method compiles once per set of type arguments the program uses. Generic structs and enums get one layout per set of arguments.
- **Too deep:** instantiation nested more than 64 deep is E0420, found by `lughac check`.
- **Gate:** removed; `driver/gate/` is deleted.
- **Spec:** the generics rules move from the draft into `Language v2 Specification.md`, and `Option<T>` is described as a predeclared generic enum.

## CONTEXT

- **Starting state (PRP-035):**
  - The checker accepts generics and records each generic use's type arguments in `Checked.instances` (call, associated call, struct literal or construction → arguments).
  - Types carry arguments: `Type::Struct(key, args)`, `Type::Enum(key, args)`, `Type::Param(name)`.
  - `build`, `run` and `--emit=ir` stop at the gate before codegen.
- **Ending state:**
  - A new checker pass lists every instance reachable from the compiled code and reports E0420.
  - Codegen compiles that list by substitution, and the gate is gone.
  - The spec says what the draft says, and milestone 11 is done.
- **Related existing code:**
  - `src/check/` (`mod.rs`, `infer.rs`, `generics.rs`, `env.rs` for `main`, `modules/`);
  - `src/codegen/` (`lower.rs`, `declare.rs`, `function.rs`, `methods.rs`, `value.rs`, `structs.rs`, `enums.rs`, `construct.rs`, `copy.rs`, `equality.rs`, `matching.rs`, `patterns.rs`);
  - `src/driver/{pipeline.rs, gate/}`;
  - `tests/cli.rs`, `tests/spec.rs`, `tests/common/`.
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 45)
1. **Instances are listed in the checker:** a pass walks the instantiations and reports E0420 as an ordinary diagnostic, so `lughac check` finds it (exit 1). Codegen compiles the list it is given.
2. **`Option<T>` changes in the spec only:** the spec describes `Option` as `enum Option<T> { Some(T), None }`, predeclared. The compiler keeps `Type::Option`, which already behaves exactly like that. Programs, messages and output are unchanged.
3. **One PRP:** this one finishes milestone 11, spec included; tag `m11`.

### Design choices for review
- **Roots:** every non-generic function and method is compiled, as today, whether or not `main` reaches it. The instance pass therefore starts from all non-generic bodies, not just `main`. A generic that nothing uses is checked but not compiled.
- **Substitution, not inference:** while compiling an instance, codegen keeps a map from each type parameter to its concrete type. It applies the map to every type it reads from the checker (the expression table, annotations, `Checked.instances`). Nothing is inferred, so architecture rule 4 holds.
- **Symbols** (architecture rule 7, extended): the instance's arguments are spelled as types print, and the whole name goes through `check::symbol`, so module paths use `/`:

  | Instance | Symbol |
  | --- | --- |
  | `max` at `i64` | `lugha_fn_max<i64>` |
  | `Stack<string>.push` | `lugha_fn_Stack<string>.push` |
  | `max` of module `a.b` at its `Point` | `lugha_fn_a/b/max<a/b/Point>` |
  | `first` at `i64[]` | `lugha_fn_first<i64[]>` |

  Copy and equality helpers follow: `lugha_copy_Stack<i64>`, `lugha_eq_Pair<i64, string>`. LLVM struct types are named the same way (`Stack<i64>`). A non-generic item's symbol is unchanged.
- **Depth:** an instance's depth is 1 + the depth of the instance whose body requested it (roots are 0). Depth 65 is E0420, reported once at the use that would create it, with the chain's first few instances in a note. Recursion at the same arguments (`len<T>` calling `len<T>`) creates nothing new, so it never counts.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Checker: the instance list** — new `check/instances.rs`
- **Uses by body:** while checking, record for each function and method body the generic uses inside it: their `ExprId`, the callee's key, and the recorded (possibly abstract) arguments.
- **The pass:** after every body is checked and only if there are no errors, run a worklist from the roots:
  - substitute the current instance's arguments into each use's arguments;
  - add each new (key, concrete arguments) pair to the list, with its depth.
- **Output:** `Checked.monos: Vec<Instance>`, where `Instance { key, args }` has `args` covering the `impl`'s parameters and then the method's own. It is sorted by first discovery, so builds are deterministic.
- **E0420** "generic instantiation too deep" at the use's span. The note names the chain (`f<i64> -> f<i64[]> -> …`). The pass stops after the first E0420.
- **E0304:** a generic `main` (`fun main<T>()`) is rejected, as the draft says.

**Codegen: compiling instances**
- **Declaring instances:** every `Checked.monos` entry is declared alongside the ordinary functions and methods, with its parameter and return types substituted, and defined from the same AST body under the substitution.
- **Calls:** a call, associated call or method call with an entry in `Checked.instances` calls the instance symbol for its (substituted) arguments.
- **Types:** `Stack<i64>` and `Tree<string>` lower to their own LLVM struct types, with fields and payloads substituted. The indirect-slot rule (heap slots for recursion) applies per instance. Every concrete instance a compiled body mentions is declared before it is used.
- **Helpers:** deep copies (`lugha_copy_*`) and deep equality (`lugha_eq_*`) work on instances, keyed by the instance's name.
- **Struct literals and constructions** of generic types build the instance's layout.
- **Splits:** `copy.rs` (300), `runtime.rs` (295), `control.rs` (292), `heap.rs` (283) and `equality.rs` (285) are near the limit. New logic goes in new files (likely `codegen/instances.rs`). Any split of an existing file is proposed and approved first.

**Driver**
- The gate is removed: `driver/gate/` is deleted, along with `stop_gated` in `pipeline.rs`. Everything that checks now compiles.

**Spec** — `Language v2 Specification.md` (the approved draft text, folded in)
- **§4:**
  - "Generic declarations", "Using a generic", "Generic recursion" (with the depth limit, 64);
  - `Option<T>` as predeclared `enum Option<T> { Some(T), None }`, behaviour unchanged;
  - the copy-table note.
- **§6:** generic functions.
- **§7, §8:** the instance layout and extern lines.
- **§9:**
  - the symbol table's "Generic instance" row, with the module spelling above;
  - "Monomorphization" in the lowering notes;
  - E0420 joins the code table, and E0304 mentions a generic `main`.
- **§10:** the generics program, so `tests/spec.rs` runs it.
- **§11:** milestone 11 marked done.
- **Changes since v1.2:** the generics sentence says generics are complete. The title stays "in progress: milestone 12".
- The draft's §12 depth question is marked resolved (64).

**Docs**
- CLAUDE.md:
  - architecture rule 7: instance symbols;
  - the tree: new files, `driver/gate/` removed, `tests/programs/m11/`;
  - KNOWN ISSUES: the gate line removed.
- README (generics usable), TODO, CHANGELOG, MEMORY decision 42, CONTEXT; tag `m11`.

### Must NOT Do
- **No interfaces** (milestone 12), explicit type arguments, or `Self`.
- **No compiler rewrite of `Option`** (discovery answer 2).
- **No inference in codegen:** only substitution of what the checker recorded.
- **No change to non-generic output:** symbols, IR and diagnostics stay as they are.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0420** is a `Diagnostic` record from the checker (exit 1), human and JSON alike.
- **Codegen invariants:** a missing instance or an unsubstituted `Type::Param` reaching codegen is a compiler bug. It stays an `expect`/`unreachable!` with the invariant stated, and is exit 2 through the guard.

## SECURITY CONSIDERATIONS
- No `unsafe`, no new input surfaces.
- **Bounded growth:** the depth limit (64) also stops a hostile program from making the compiler run without end.

## TESTS TO WRITE

Unit tests — `check/instances.rs`:
- [ ] **One per argument set:** `max(3, 7)` and `max("a", "b")` give `max<i64>` and `max<string>`; a second `max(1, 2)` adds nothing.
- [ ] **Through generic bodies:** `largest(["a"])`, whose body calls `first(xs)`, gives `largest<string>` and `first<string>`.
- [ ] **Methods:** `s.push("a")` on `Stack<string>` and `Stack.new()` give `Stack.push<string>` and `Stack.new<string>`; a method's own parameter is appended after the `impl`'s.
- [ ] **Unused generics** produce nothing.
- [ ] **Recursion at the same arguments** (`fun count<T>(l: List<T>): i64`, recursive) terminates with one instance.
- [ ] **E0420** for `fun f<T>(x: T) { f([x]); }` called with `f(1)`, at `f([x])`, with the chain in the note.
- [ ] **E0304** for `fun main<T>() {}`.

Integration — `tests/codegen.rs` / `tests/modules.rs` (IR):
- [ ] `--emit=ir` defines `@"lugha_fn_max<i64>"` and `@"lugha_fn_Stack<string>.push"` and calls them.
- [ ] A generic function of module `a.b` at its own struct is `@"lugha_fn_a/b/max<a/b/Point>"`.
- [ ] A non-generic program's IR is byte-for-byte unchanged (an existing IR test is kept).

Acceptance — `tests/programs/m11/` (run, `.stdout`/`.exit`), built at `-O0` and `-O2` by the runner:
- [ ] `stack.la`: a generic `Stack<T>` at `i64` and at `string`, with `push`/`pop` and `Option<T>`.
- [ ] `list.la`: a recursive `enum List<T> { Nil, Cons(T, List<T>) }` with a generic `count`, at two types (indirect slots per instance).
- [ ] `copies.la`: a generic struct holding an array, copied and mutated, so the copies stay independent (deep copy helpers per instance).
- [ ] `equality.la`: `contains<T: Eq>` over structs and over arrays (deep `==` helpers per instance), and `largest<T: Ord>` over `f64` and `string`.
- [ ] `modules/`: a generic function and a generic struct in an imported module, used at a type of the main file.
- [ ] `e0420.la`: reject golden.
- [ ] The §10 generics program runs from the spec (`tests/spec.rs`), printing `b`, `true`, `plum`.

CLI — `tests/cli.rs`:
- [ ] The gate test becomes "generics run": `run` exits 0 with the expected output. The gate's unit tests go with `driver/gate/`.

Regression:
- [ ] Every earlier test passes; every non-generic program's IR, output and diagnostics are unchanged.

## ROLLBACK PLAN

- Branch `prp-036-generic_compiling`, merged into `main` after green CI.
- To abandon: delete the branch. PRP-035's gate is still in `main` until the merge.

## ACCEPTANCE CRITERIA
- [ ] The §10 generics program passes, and the `m11/` programs pass at `-O0` and `-O2`.
- [ ] E0420 is reported by `lughac check`; a generic `main` is E0304.
- [ ] The gate is gone; `driver/gate/` is deleted.
- [ ] The spec has the generics rules; CLAUDE.md, README, TODO, CHANGELOG, MEMORY are updated; tag `m11`.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m11/stack.la
```
