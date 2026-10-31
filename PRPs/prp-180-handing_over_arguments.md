## FEATURE: Handing over arguments: `x = f(x)` stops copying `x`'s arrays when `f` keeps its parameter; milestone 149, v17.

**Status:** approved 2026-10-31 ("build"); amended and re-approved mid-build ("approved"): a keeping function gets a second, owning instance `lugha_fn_<symbol>.$own` called only by direct calls, instead of every caller marking for the one instance (internal codegen paths — derive blocks, printing, ordering, map hashing, helpers, operators, actors — call the ordinary instance unchanged, so none can skip the marking). Proposed 2026-10-31, from TODO's copy-on-write bug, after a short discussion (1 question, three options compared; the user chose option 1, "the caller decides while compiling", over a run-time ownership flag passed with every argument and over leaving it).

## OBJECTIVE
```lugha
struct Bag { items: i64[] }

fun insert(b: Bag, k: i64): Bag {
    let mut out = b;            // today: marks b's array shared…
    out.items.push(k);          // …so this clones it, on every call
    out
}

fun main() {
    let mut bag = Bag { items: [] };
    for k in 0..200000 {
        bag = insert(bag, k);   // today quadratic (each call copies the whole array); now linear
    }
    println(bag.items.len);     // 200000
    let more = insert(bag, 1);  // `bag` is still used below: it is protected, as now
    println(bag.items.len);     // 200000
    println(more.items.len);    // 200001
}
```
Nothing a program prints changes; only how often arrays are cloned.

## CONTEXT
- Read first: `src/check/liveness/` (PRP-018's move on last use: `moves`, `dead_after`, the loop rule), `src/codegen/copy.rs` (`value_for_store`, `value_for_return`, `deep_copy`), `src/codegen/cow.rs` (`mark_shared`), `src/codegen/function.rs` (parameters are `borrowed` locals), `src/codegen/fun_values.rs` (`lugha_ref_` wrappers), `src/codegen/interfaces.rs` (vtables), `src/codegen/methods.rs`/`dispatch.rs`; DECISIONS 010, 012/106–109 (copy-on-write; "move on last use stays possible"), 2073's entry (the prelude's `mut self` workaround).
- Spec: §4 "Array copies" (line ~638: "function arguments are never copied" stays true; a parameter a function keeps is now a store the caller pays for), §7/§9 "Copy-on-write" (who marks), §9's lowering notes, §11 row 149, "Changes since v16". The v17 Draft before the build. DECISIONS: a new entry (handing over arguments; the caller marks), referring to 010/012.
- Decisions: none open. Core decision 6: no new prefix, one new suffix: `lugha_fn_<symbol>.$own`, the owning instance (`$` names can't be written by programs, so it can't collide); recorded in DECISIONS and §9's symbol notes. No dependency, no runtime change, not security-sensitive.

## REQUIREMENTS

### Must Do
- **Kept parameters:** the checker records, per function and method (receivers included: `fun with(self, k): Bag { let mut b = self; … }`), which parameters its body *keeps*: stores at a copy site on the parameter's last use (a `let`, an assignment, a literal's element or field, a `push`, the result), or passes on its last use to a parameter its callee keeps. Found once per declaration, iterated to a fixed point over recursion.
- **Inside the function:** a kept parameter's store on its last use moves (no mark), as a dead `let` local does today. Any other store of a parameter is marked, as now.
- **At a direct call** (`f(…)`, `m.f(…)`, `x.m(…)`, `T.f(…)`, a monomorphised bound call): for each argument holding arrays passed to a kept parameter, the caller marks it shared unless the argument is a place on its last use; a last use is handed over unmarked. Arguments to parameters that aren't kept are never marked, exactly as now.
- **Last use, widened:** besides PRP-018's rule, a mention of a local `x` inside the right-hand side of `x = …` (the whole local, not a field or index of it) is a last use when no later mention of `x` in that right-hand side follows it, loops or not: the assignment overwrites `x` before anything reads it again. Parameters stay non-movable at call sites unless kept (a kept parameter's arrays are the function's own or marked, so passing it on at its last use is safe).
- **Two instances:** a function or method with a kept parameter is compiled twice: `lugha_fn_<symbol>`, exactly as today (it marks what it stores), and `lugha_fn_<symbol>.$own`, where kept parameters are the function's own (their last-use stores move, and it may hand them on). Only direct calls (`f(…)`, `m.f(…)`, `x.m(…)`, `T.f(…)`, monomorphised bound calls) call the `.$own` instance, marking each kept argument that isn't handed over. Every other path — function values, interface dispatch, derive blocks, printing, ordering, maps, prelude helpers, operator methods, actors, `spawn` — calls the ordinary instance, as today.
- **Liveness covers methods and derive-block items** too, not only free functions (today `moves` walks `Item::Fun` bodies only).
- **Correctness:** an argument still read after the call (`insert(bag, 1); println(bag)`, `f(x, x)`, `x = f(x, x.len)`) is marked; a panic inside the callee leaves nothing observable. Every `tests/programs` output unchanged.
- **Performance:** the OBJECTIVE's loop at `-O2` goes from quadratic to linear (recorded in BUILT, with the array clone count from a debug counter or timing at 50 000 / 100 000 / 200 000); the m101 map-insert guard and a read-only-call benchmark (`total(xs)` in a loop that then writes `xs`) stay within 5% of `main`.
- Milestone 149; tag `m149`.

### Must NOT Do
- A run-time ownership flag or any new hidden argument (option 2, declined).
- Handing over field or index places (`m.items = f(m.items)`, `xs[i] = f(xs[i])`): only whole locals.
- Change the prelude's `mut self` maps back to `self = f(self, …)` (they could now; a later chore if wanted).
- Change C calls, extern signatures or the runtime.

## DESIGN
- **Checker:** `check/liveness/` gains the assignment rule (`walk.rs`, when walking `StmtKind::Assign` whose place is a bare `Name`) and a second output, `hands: HashSet<ExprId>` (call arguments on their last use); a new `check/keeps.rs` computes `keeps: HashMap<String /* function key */, Vec<bool>>` (receiver first) from the moves and hands, iterating until nothing changes. Both go into `Checked` (`Checked::hands`, `Checked::keeps`). Parameters of a kept index become movable roots in `dead_after` (a new `Binding::Param` case gated on `keeps`).
- **Codegen:** the three places signatures are declared (`declare.rs`, `methods.rs`, `instances.rs`) also declare the `.$own` instance when the declaration keeps a parameter, and body lowering runs a second time with `owning` set; `value_for_store`/`value_for_return` move a kept parameter on its last use only while `owning`. `function.rs::call` and `methods.rs::call_method` (direct calls only) call `.$own` with kept arguments marked unless handed over (`hands`, and in an owning body a kept parameter of its own).
- **Why it is sound:** copy-on-write's rule is "an unmarked array has one holder". A handed-over argument's only other holder is a local the caller never reads again (or overwrites first), so the callee becomes its one holder; every other path reaches the ordinary instance, which marks on store as before.
- Sizes: liveness +60, `keeps.rs` ~150 with tests, codegen +80. `check/liveness/mod.rs` (213) and `walk.rs` (166) stay under 500.

## TESTS
- [ ] Unit (`check/liveness`): `x = f(x)` hands `x` over in and out of loops; `x = f(x, x.len)` and `f(x); println(x)` don't; field places don't; methods and derive items are walked.
- [ ] Unit (`check/keeps.rs`): a parameter stored on its last use is kept; one only read isn't; passed on to a keeping callee is kept; mutual recursion reaches a fixed point; receivers.
- [ ] Codegen unit: a keeping function has both instances; a direct call to it calls `.$own`, marks a live argument and doesn't mark a handed-over one; a call to a non-keeping function marks nothing and has no `.$own`; a function value uses the ordinary instance.
- [ ] `tests/programs/m149/`: `hand_over.la` (the OBJECTIVE, plus the same through a method `bag = bag.with(k)`, a function value, an interface method, a recursive builder, and aliasing checks proving a protected argument is unchanged after the callee writes its copy), same at `-O2`; `quadratic.la`: 200 000 inserts through `x = f(x)` inside the runner's timeout.
- [ ] `--test programs` and `--test spec` unchanged.

## ACCEPTANCE
- [ ] The OBJECTIVE prints `200000`, `200000`, `200001`, and its loop is linear (BUILT).
- [ ] `cargo test --lib`, `--test programs`, `--test spec`, `--test codegen`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; DECISIONS (new entry), spec (§4, §9, §11 row 149, "Changes since v16"), the v17 Draft, CHANGELOG (under Fixed), TODO (the bug removed) updated.

## BUILT — milestone 149
- As amended (two instances). Checker: `check/liveness/` now returns `Liveness` (`moves`, `param_moves`, `hands`, `param_hands`, `keeps`) from `analyse(units, mentions, callees, deep)`; parameters are slots (receiver first, `mut self` never kept); a function's tail and `return` values are stores for parameters; closures count as captures (never handed); only values whose type may hold arrays (`maybe_deep`: deep, or mentioning a type parameter) keep a parameter, so `insert(b, k)` keeps `b` and not `k`. The fixed point uses the checker's `callees`; a slot mismatch could only cost marks, since codegen decides from the callee it actually calls. Tests moved to `check/liveness/tests.rs` (`mod.rs` 264, `walk.rs` 252).
- Codegen: `codegen/owning.rs` (`declare_owning` at the three declaration sites, filtered again by the instance's real parameter types; `define_with` lowers `define_body` twice; `moved`, `handed`, `protect`, `owned_arguments`). `function.rs::call` and `methods.rs::call_method` (the receiver through `receiver_copy`) call `.$own`; nothing else does. Interface calls need nothing: their tables hold the ordinary instance.
- Measured, `-O2`: the OBJECTIVE (200 000 inserts) 1 min 50 s on `main` → 7 ms; `tests/programs/m101/many_inserts` 0.04 s both; a read-only `total(g)` loop then writing `g` (400 000 calls over 1 000 elements) 0.17 s both, same output.
- Tests: liveness unit tests (overwrite rule in and out of loops, read-again and field targets, closures, kept vs read-only parameters, results, hand-ons through recursion, receivers and methods); codegen unit tests (both instances, two direct calls to `.$own` with one protected and one handed over, no `.$own` for a read-only function, `lugha_ref_` calls the ordinary instance); `tests/programs/m149/hand_over.la` (100 000 inserts by function and 50 000 by method, protected arguments unchanged after the callee writes, the same argument twice, a function value, an interface bound, a recursive builder; same output at `-O2` and on `main`). The PRP's separate `quadratic.la` is covered by `hand_over.la`'s loops (on `main` they would take ~35 s).
