## FEATURE: Closures — `fun(x) = e` literals that capture by copy.

**Status:** merged 2026-10-13 — session 50
**Milestone:** 15 (one PRP)
**Spec:** `Language v3 Draft.md`:
- decisions 2–3;
- §3 (`closure`, `cparam`, E0209);
- §4 "Closures";
- §5 (evaluation order);
- §7 (environments);
- §9 (`lugha_closure_*`, monomorphization, E0209, E0511);
- §10 "Closures";
- §11 (milestone 15);
- §12 (empty captures).

**Decisions:** MEMORY 44 (v3 direction), MEMORY 46 (function values: `{ code, env }`, environment first), MEMORY 42 (instances).

## OBJECTIVE
Functions can be written inline and remember the values around them. The §10 closures program prints `13`, `1`, `20`.
- **Literals:** `fun(x) = x + k`, `fun(x: i64): i64 { … }`.
- **Inference:** parameter types come from an expected function type, and the return type from the body.
- **Capture by copy, read-only:** a closure copies what it mentions when it is created, and can't assign it (E0511).
- **Values everywhere:** closures can be returned, stored and passed like any function value, including inside generic code.

## CONTEXT

- **Starting state (milestone 14):**
  - Function values are `{ code, env }` with a null environment.
  - Named functions are called through `lugha_ref_` wrappers that take the environment first.
- **Ending state:**
  - A closure literal makes a value whose environment is a GC object holding copies of its captures.
  - Its body is compiled as `lugha_closure_<enclosing>.<n>`.
- **Related existing code:**
  - `src/parser/primary.rs`, `src/ast/expr.rs`, `sexp/`;
  - `src/check/` (`expr.rs`, `fun_values.rs`, `body.rs`, `flow.rs`, `assign.rs`, `ops.rs` `defers`, `liveness/walk.rs`, `mod.rs`);
  - `src/codegen/` (`fun_values.rs`, `function.rs`, `scope.rs`, `copy.rs`, `expr.rs`).
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 50)
1. **A closure body is its own function:**
   - `return` leaves the closure and is checked against its return type;
   - `break` and `continue` reach only loops inside the closure, so one aimed at an outer loop is E0504.

### Design choices for review
- **Inferring the return type:** with no written or expected return type, the closure's return type is the body's type. Such a closure may not use `return e`, since the result's type isn't known yet: it is E0412 "can't infer the return type of a closure that uses `return`", with help to write it (`fun(x: i64): i64 { … }`).
- **Parameter types:**
  - written ones are used;
  - otherwise they come from an expected function type in the same position (E0403 if the counts differ);
  - otherwise E0412 "can't infer the type of parameter `x`".
- **Inference order in generic calls:** a closure with an unwritten parameter type waits, like a literal, `None` or `[]`. So `apply(fun(x) = x + 1, 41)` binds `T` from `41` first.
- **Captures:**
  - every local, parameter, loop variable, pattern binding or `self` of an enclosing scope that the body mentions;
  - copied, in order of first mention, when the literal is evaluated;
  - nested closures capture through their enclosing closure.
  - **Arrays are always copied (deep), never moved,** even when it's their last use. This differs from the draft's "(moved on last use)". It's correct and simpler, and a capture's last use can be inside the closure, which may run many times. Moving can be added later without any visible change.
- **E0511** "can't change `count`: closures capture by copy", with help to return a value instead. It covers assigning a captured variable, its fields or its elements, a `mut self` call or `push` on one, and `swap` with one.
- **Inside the body:** a captured variable is never moved from, since it behaves like a parameter. The closure's own `let mut` locals work as usual.
- **Code:**
  - the body becomes `lugha_closure_<enclosing>.<n>`, where `<enclosing>` is the enclosing function's symbol without its prefix and `n` counts that function's closures from 0 (`lugha_closure_main.0`, `lugha_closure_main.0.0` for a nested one, `lugha_closure_max<i64>.0` in an instance);
  - its first parameter is the environment;
  - the environment is one `lugha_rt_alloc` object laid out as an LLVM struct of the captures' types;
  - a closure that captures nothing uses a null environment (the draft's §12 question, resolved yes);
  - inside the body, captures are read in place from the environment, which is never written after creation.
- **AST:** `ExprKind::Closure(Box<Closure>)`, holding parameters (`Ident` and an optional type), an optional return type, and a body `Block` (`= e` becomes a block with a tail, as for functions). It prints as `(closure ((x i64) (y _)) i64 (block …))`.
- **Side table:** `Checked.captures: ExprId → Vec<String>`, the captured names in order, so codegen never resolves names itself.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Syntax**
- **`primary` accepts a closure:** `fun ( [ cparam { , cparam } ] ) [ : type ] ( block | = expr )`, where `cparam = IDENT [ : type ]`. The `= expr` body takes the longest expression.
- **E0209:** a closure parameter that isn't a name with an optional type (`fun(self) = 1`, `fun(mut x) = x`), or a closure with type parameters.

**Checker** — new `check/closures.rs`
- **Typing:** a closure literal types as above. Its body is checked as a function body with its own return type, loop depth and dead-code state, saved and restored around it. A closure in statement position is an ordinary expression statement.
- **Captures:** they are recorded per closure, through nested closures, into `Checked.captures`. A mention of a captured variable is recorded with a binding that is never moved from.
- **E0511** applies to every place-changing operation whose root is a capture (assignment, compound assignment, `swap`, a `mut self` receiver, `push`/`pop`/`clear`).
- **Return and loops:** `return` checks against the closure's return type, and `break`/`continue` against loops inside it (E0504).
- **Inference:** `defers()` is true for a closure with an unwritten parameter type.
- **Liveness:** the walk enters closure bodies, so their own lets can move; captured names are never moved.

**Codegen** — closures in `codegen/fun_values.rs` or a new `codegen/closures.rs`
- **Creating the value:** lowering a closure literal copies the captures into a new environment (deep copies for arrays, as at any copy site), emits the body function, and builds `{ code, env }`.
- **The body function:** lowered with the builder, scopes, loops, return type and current function saved and restored. Captures are bound as borrowed locals pointing into the environment.
- **Instances:** each instance compiles its own closures under its substitution.

**Spec** — `Language v3 Specification.md`
- **The draft's text, folded in:**
  - §3: `closure` and `cparam`, plus E0209 in the parse-error table;
  - §4: "Closures", including discovery answer 1, the return-type rule above, and always copying;
  - §5: evaluation order;
  - §7: environments;
  - §9: `lugha_closure_` in the symbol table, and E0511 and E0209 in the code table;
  - §10: the closures program;
  - §11: milestone 15 done.
- **The draft's §12** empty-capture question is resolved (yes). The "moved on last use" note becomes "always copied" (design choice above, if approved).
- **The status line** becomes "in progress: milestone 15".

**Docs**
- CLAUDE.md: architecture rule 7 gains `lugha_closure_`, plus the tree.
- README, TODO, CHANGELOG, MEMORY decision 47, CONTEXT.

### Must NOT Do
- **No changing captures:** closures never change what they capture, and there are no shared references.
- **No recursive closures, and no closures with type parameters.**
- **No array helpers** (`map`, `filter`, …; milestone 16).

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Records:** E0209 (parser), and E0511 and the new E0412 cases (checker), are `Diagnostic` records; checking continues.
- **Codegen invariants:** a closure whose captures can't be found in scope is a compiler bug (an `expect` stating the invariant, exit 2).

## SECURITY CONSIDERATIONS
- **Memory:** environments are GC objects written once at creation. A closure never writes through its environment pointer, so it is safe to share.
- No `unsafe`, no new inputs.

## TESTS TO WRITE

Unit tests — parser:
- [x] `fun(x) = x + 1`, `fun(x: i64, y): i64 { x }`, `fun() {}` and `fun(f: fun(i64): i64) = f(1)` parse and print.
- [x] `sort(xs, fun(a, b) = a > b)` ends the body at the `)`.
- [x] E0209 for `fun(self) = 1` and `fun(mut x) = x`.

Unit tests — checker (`check/closures.rs`):
- [x] **Types:**
  - `let f = fun(x: i64): i64 = x * 2;` is `fun(i64): i64`;
  - `let g: fun(i64): i64 = fun(x) = x + 1;` infers `x`;
  - `fun(x: i64) = x > 1` is `fun(i64): bool`;
  - `fun() {}` is `fun()`.
- [x] **E0412** for `let h = fun(x) = x;`, and for a closure using `return` without a known return type.
- [x] **Return and loops:**
  - `return` inside a closure is checked against the closure's return type (E0403);
  - `break` inside a closure inside a loop is E0504.
- [x] **Captures:**
  - `count` captured and read is fine;
  - E0511 for `count += 1`, `p.x = 1`, `xs[0] = 1`, `xs.push(1)` and `swap(a, b)` on captures;
  - a closure's own `let mut` local is assignable.
- [x] **Recorded captures:**
  - `captures` records names in order of first mention;
  - a nested closure's captures are recorded on the outer one too;
  - a closure's own parameters and locals aren't captures.
- [x] **Generic calls:** `apply(fun(x) = x + 1, 41)` with `apply<T>(f: fun(T): T, x: T)` infers `T = i64`.
- [x] **Closures in generic bodies** check once, and their instances compile.

Codegen / IR:
- [x] `lugha_closure_main.0` is defined with a `ptr` first parameter.
- [x] A capturing closure allocates its environment with `lugha_rt_alloc`.
- [x] A non-capturing one passes `ptr null`.

Acceptance — `tests/programs/m15/` (run at `-O0` and `-O2`):
- [x] `adder.la`: `adder(k)` returning a closure, called repeatedly; closures in arrays and fields.
- [x] `capture.la`: copy semantics, i.e. changing the outer variable after creation leaves the closure's copy unchanged, including for an array (deep copy).
- [x] `nested.la`: a closure returning a closure that captures through both; `return` inside a closure; closures in a loop each keeping their own iteration's value.
- [x] `generic.la`: closures passed to generic functions and created inside generic functions at two types.
- [x] `e0511.la`, `e0209.la`: reject goldens.
- [x] The §10 closures program runs from the spec.

Regression:
- [x] Every earlier test passes; programs without closures give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-040-closures`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 closures program and the `m15/` programs pass.
- [x] E0209, E0511 and the E0412 cases are reported as specified.
- [x] The spec is folded (in progress: milestone 15); CLAUDE.md, README, TODO, CHANGELOG and MEMORY are updated; tag `m15`.
- [x] No file over 300 lines (splits proposed first); no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m15/adder.la
```

## AMENDMENTS (during build, session 50)

- **Inference order:** closures are checked after every other waiting argument, not just after the arguments typed alone. In `apply(fun(x) = x + 1, 41)` both the closure and `41` wait, and `41` must fix `T` first.
- **The draft is amended** to "always deep-copied" (decision 2 and §4, as approved), and its §12 empty-capture question is resolved yes.
- **Return type for a diverging body:** a closure whose body never finishes, with no written or expected return type, returns nothing (`Void`).
- **No file splits were needed.**
