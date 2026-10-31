## FEATURE: An iterator chain's expected type reaches `map`'s closure through `collect` and the helpers between; milestone 150, v17.

**Status:** approved 2026-10-31 ("build"); proposed 2026-10-31, from TODO's last known limit, after a short discovery (1 question, answered with the recommended option): (1) the element type travels through the whole chain: `collect`, `collect_set` and `collect_map` turn their expected type into an element type, `filter`, `take`, `skip`, `take_while` and `skip_while` pass it on, and `map` gives it to its closure as the result type.

## OBJECTIVE
```lugha
fun main() {
    let xs = [1, 2, 3];
    let n: Option<i64>[] = xs.iter().map(fun(x) = None).collect();            // today: E0412
    let m: Option<i64>[] = xs.iter().map(fun(x) = None).filter(fun(o) = true).take(2).collect();
    let s: Set<Option<i64>> = xs.iter().map(fun(x) = None).collect_set();
    let k: Map<i64, Option<string>> = xs.iter().map(fun(x) = (x, None)).collect_map();
    println("{n} {m} {s.len} {k.len}");                                        // [None, None, None] [None, None] 1 3
}
```
A closure that types alone (`fun(x) = Some(x)`) is unchanged; nothing expected still leaves `fun(x) = None` E0412.

## CONTEXT
- Read first: `src/check/iter_helpers.rs` (`iter_helper_call`: each helper is the prelude function `iter_<name>`, inferred with `infer`), `src/check/method_calls.rs` (receivers are checked before the call; `receiver_expect`), `src/check/siblings.rs` (milestone 148's rule, for methods), `src/check/iterators.rs` (`next_of`: an iterator's element), `src/prelude/iter.la` and `more.la` (`iter_map<T, U>(it: Iter<T>, f: fun(T): U): Mapped<T, U>`, `iter_collect<T>(it: Iter<T>): T[]`), `src/prelude/maps.la` (`iter_collect_map`, `iter_collect_set`).
- Why not an expected type: an `Iter<E>` expected of the receiver would convert it to an interface value (`Checker::expr` converts where an interface is expected), changing what codegen builds. The element type goes as a separate hint, keyed by the receiver's `ExprId`.
- Spec: §4 "Inference" (a sentence after milestone 148's), the iterator helpers' section (§5, where `collect` is described), §11 row 150, "Changes since v16". The v17 Draft before the build.
- Decisions: inference stays implicit (no explicit type arguments, DECISIONS M11). None open. No dependency, no codegen or runtime change, not security-sensitive.

## REQUIREMENTS

### Must Do
- **Sources:** `r.collect()` expected to be `E[]`, `r.collect_set()` expected `Set<E>` and `r.collect_map()` expected `Map<K, V>` give `r` the element hint `E` (`(K, V)` for maps).
- **Passing on:** `r.filter(f)`, `r.take(n)`, `r.skip(n)`, `r.take_while(f)` and `r.skip_while(f)` with an element hint give `r` the same hint.
- **Use:** `r.map(f)` with an element hint `E`, when it is the prelude helper, fixes `U` to `E` before its arguments are checked, so `f` is checked as a `fun(T): E`; a closure that disagrees is the same error as an annotated one (E0401/E0403 at the closure), once.
- **Only helpers:** the hint is read only by the helper `map` (`iter_helper_call`); a type with its own `map`, `filter` or `collect` never sees one. Every step between a consuming `map` and the source is then a prelude iterator type, which declares none of these names, so the chain is all helpers.
- Nothing changes when there is no expected type, when the closure types alone, or for array `map` (`xs.map(f)`, already typed by its expected type).
- Milestone 150; tag `m150`.

### Must NOT Do
- Interface conversion of a receiver, or any change to what codegen sees.
- `fold`, `flat_map`, `enumerate`, `zip`, `chain` and other helpers (they stay as they are; a later PRP if wanted).
- Inference through `next()`, `unwrap()` on a chain (milestone 148 covers methods, not helpers).

## DESIGN
- `Checker` gains `element_hints: HashMap<ExprId, Type>`. In `method_call`, before checking the receiver: if the call has an expected type and its name is a source, or the call itself has a hint and its name passes on, insert the receiver's hint. `iter_helper_call` for `map` reads (and removes) the hint for its call, works out the helper result's element in terms of its own parameters (`next_of` on `Mapped<T, U>` is `U`), `unify`s it with the hint into `bound`, then infers as now.
- Source shapes: `E[]` → `E`; `Set<E>` (`$prelude.Set`) → `E`; `Map<K, V>` (`$prelude.Map`) → `(K, V)`. Anything else gives no hint.
- Sizes: `iter_helpers.rs` +40 (its tests move to `iter_helpers_tests.rs` if it passes 500, proposed then), `method_calls.rs` +15.

## TESTS
- [ ] Unit: the OBJECTIVE's four lines type; a closure returning `Some(x)` unchanged; with nothing expected, E0412 as now; a disagreeing closure (`let n: string[] = xs.iter().map(fun(x) = 1).collect();`) one error; a struct with its own `map` and `collect` methods unaffected.
- [ ] `tests/programs/m150/chains.la`: the OBJECTIVE, with `.stdout`.
- [ ] `--test programs` (m94's `none_closures`, iterator programs) unchanged; the TODO limit removed.

## ACCEPTANCE
- [ ] The OBJECTIVE prints `[None, None, None] [None, None] 1 3`.
- [ ] `cargo test --lib`, `--test programs`, `--test spec`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; spec (§4, the helpers, §11 row 150, "Changes since v16"), the v17 Draft, CHANGELOG, TODO updated.

## BUILT — milestone 150
- As planned. `Checker::element_hints`; `iter_helpers.rs::pass_element_hint` (called from `method_call` before the receiver is checked) and, in `iter_helper_call`, `map` unifies its result's element (`next_of(Mapped<T, U>)` = `U`) with the hint into `bound` before `infer`. A map's `(K, V)` hint is built with `tuple_type`. `iter_helpers.rs` is 341 lines, `method_calls.rs` 422.
- Tests: four unit tests in `iter_helpers.rs` (every source and pass-through helper, a closure that types alone unchanged, E0412 with nothing expected and through `enumerate`, one error for a disagreeing closure, a struct's own `map`/`collect` unaffected); `tests/programs/m150/chains.la` (the OBJECTIVE).
