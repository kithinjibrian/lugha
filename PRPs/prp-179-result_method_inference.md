## FEATURE: A waiting generic call takes its type through any method whose result reveals it: `json.decode(v).unwrap()`, `.expect(…)`, a program's own methods; milestone 148, v17.

**Status:** approved 2026-10-31 ("build"); proposed 2026-10-31, from TODO's "inference through `.unwrap()`/`.expect()`", after a short discovery (1 question, answered with the recommended option): (1) one general rule, not `unwrap`/`expect` alone: when a method's result mentions its receiver's type parameters, the expected type flows back to a waiting receiver; milestone 96's "the method returns the receiver's own type" becomes one case of it.

## OBJECTIVE
```lugha
import std.json;

struct Person { name: string, age: i64 }
fun main() {
    let v = json.parse("{{\"name\": \"ada\", \"age\": 36}}").unwrap();
    let p: Person = json.decode(v).unwrap();                 // today: E0412
    let q: Person = json.decode(v).expect("a person");      // today: E0412
    println(p.name);                                         // ada
    println(json.decode(v).unwrap() == q);                   // true: the sibling `q` types it
    let people: Person[] = [json.decode(v).unwrap()];        // the list's element type
}
```
A program's own methods follow the same rule:
```lugha
struct Pair<T> { a: T, b: T }
impl Pair<T> { fun first(self): T = self.a; }
fun zeros<T: Num>(): Pair<T> = Pair { a: 0, b: 0 };

fun main() {
    let n: u8 = zeros().first();                             // `zeros`'s T is u8; today: E0412
}
```
`let p = json.decode(v).unwrap();` with nothing wanting a type stays E0412, as `let p = json.decode(v);` is.

## CONTEXT
- Read first: `src/check/siblings.rs` (milestone 96: `waits`, `waiting_head`, `waiting_receiver`, `returns_self`, `receiver_expect`), `src/check/method_calls.rs` (where `receiver_expect` is consulted before the receiver is checked), `src/check/builtins.rs` (`unwrap`/`expect` on `Option` and `Result`), `src/check/infer.rs` (`unify`), `src/check/results.rs` (`try_hint`: `?`'s version of the same idea, `Type::Error` leaving a type argument open).
- Spec: §4 "Inference" (the sentence on a method call whose receiver waits, line ~796) — it widens from "returns the receiver's own type" to "whose result mentions the receiver's type parameters"; §5 `json.decode` ("`T` comes from context — an annotation, a return type, or `?`" gains "or `.unwrap()`/`.expect()`"); §11 row 148; "Changes since v16". The v17 Draft before the build.
- Decisions: core decision on inference (type arguments always inferred, never written; DECISIONS 3/M11) followed, not changed: this only adds a place the expected type reaches. None open. No dependency, no runtime or codegen change, not security-sensitive.

## REQUIREMENTS

### Must Do
- **The rule:** in `r.m(…)` where `r` waits (a generic call only its result can type, as milestone 96 defines it) and `m`'s declared result mentions type parameters of `r`'s type (its `impl`'s parameters), the call's expected type is matched against `m`'s result; the parameters it fixes give `r` an expected type (`Result<Person, _>` for `unwrap`, the unfixed ones left open as `?` leaves the error type open), and `r` is checked with it.
- **Which methods:** a program's or library's own (`impl Pair<T> { fun first(self): T }`, `fun wrap(self): Option<T>`), the prelude's, and the built-ins `unwrap`/`expect` on `Option<T>` and `Result<T, E>` (result `T`). Milestone 96's case (result exactly the receiver's type) is the same rule and keeps working unchanged.
- **Such a call waits too**, so siblings type it as they type its receiver: `json.decode(v).unwrap() == q`, `assert_eq(json.decode(v).unwrap(), q)`, a list's element, another call's argument.
- **Chains:** the rule applies at each step: `make().wrap().unwrap()` where each method's result mentions its receiver's parameters.
- Nothing changes when the receiver can be typed alone, when nothing is expected (E0412 as now), or when the method's result doesn't mention the receiver's parameters (`.len()`-like methods: E0412 as now).
- A mismatch is reported once, where it is today (E0401/E0403 at the call), never twice.
- Milestone 148; tag `m148`.

### Must NOT Do
- Explicit type arguments (`json.decode<Person>(v)`): still ruled out (DECISIONS M11).
- Inference from later uses of a `let` without an annotation (`let p = json.decode(v).unwrap(); greet(p);`).
- Typing `match json.decode(v) { Ok(p) => … }` from its arms.

## DESIGN
- `siblings.rs`: `returns_self(head, name)` becomes `result_of(receiver_head, name) -> Option<(Type, Vec<String>)>`: the method's declared result and the receiver type's own parameters, from `self.methods` (the first `outer` generics) or, for `unwrap`/`expect`, a fixed answer for `Option<T>`/`Result<T, E>`. `waiting_head` learns `Option` (a waiting call returning `Option<…>`, head `Option`) beside structs and enums.
- `receiver_expect`: `unify(result, expected, params)`; if it binds at least one parameter, the receiver's expected type is its head at the bound arguments, `Type::Error` for the rest (as `try_hint` does). `waiting_receiver` (what makes the call itself wait) uses the same test: the result mentions one of the receiver's parameters.
- Sizes: `siblings.rs` 330 → ~400 (with tests); a split isn't needed. Unit tests go in `siblings.rs`'s test block; if it would pass 500 lines, they move to a new `check/siblings_tests.rs` (proposed then, not assumed).

## TESTS
- [ ] Unit (`check/siblings.rs`): `unwrap`/`expect` on a generic call returning `Result<T, E>` and one returning `Option<T>`, at a `let` annotation, a return type, a function argument, `==` with a sibling, a list element; a program's own `fun first(self): T` and `fun wrap(self): Option<T>` chained with `unwrap`; milestone 96's `pad` chain unchanged; still E0412 with nothing expected and through a method whose result doesn't mention the parameters; one error for `let s: string = json.decode(v).unwrap();` where `decode`'s `T` has to meet `Json` (it does: no error) and for a concrete mismatch.
- [ ] `tests/programs/m148/unwrap.la`: the OBJECTIVE's lines that run (decode through `unwrap`, `expect`, `==`, a list), with `.stdout`.
- [ ] `--test programs` (m96, m140 unchanged).

## ACCEPTANCE
- [ ] The OBJECTIVE type-checks and prints `ada`, `true`.
- [ ] `cargo test --lib`, `--test programs`, `--test spec`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; spec (§4, §5 `json.decode`, §11 row 148, "Changes since v16"), the v17 Draft, CHANGELOG, TODO (the `.unwrap()` clause removed) updated.

## BUILT — milestone 148
- As planned. `siblings.rs`: `returns_self` gave way to `result_of(head, name)` (a method's declared result, the receiver type's own parameters, and the receiver's type at them; `unwrap`/`expect` on `Option`/`Result` answered there, with `$`-named parameters that can't clash); `waiting_head` returns the declared type (struct, enum or `Option`); `receiver_expect` unifies the method's result with the expected type (`infer::unify`, `known` now `pub(super)`) and leaves unbound parameters open.
- Tests: two unit tests in `check/siblings.rs` (`unwrap`/`expect` on `Result` and `Option` at a `let`, a return type, an argument, a list, `==` both ways; a program's own `first`, `wrap`, chained with `unwrap`; E0412 with nothing expected and through `size(): i64`; one error for a concrete mismatch); `tests/programs/m148/unwrap.la` (on `main` it has 7 E0412s). `siblings.rs` is 408 lines.
