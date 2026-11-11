## FEATURE: arithmetic between different types: several `mul`s per type, and `2.0 * v` (milestones 158 and 159)

## OBJECTIVE

```
struct V { x: f64, y: f64 }

impl V {
    fun mul(self, w: V): V = V { x: self.x * w.x, y: self.y * w.y };   // v * w
    fun mul(self, k: f64): V = V { x: self.x * k, y: self.y * k };     // v * 2.0
    fun add(self, w: V): V = V { x: self.x + w.x, y: self.y + w.y };
}

impl f64 {
    fun mul(self, v: V): V = v * self;                                  // 2.0 * v
}

impl u8 {
    fun mul(self, v: V): V = v * (self as f64);                         // b * v
}

fun main() {
    let v = V { x: 1.0, y: 2.0 };
    let w = 2.0 * v + v * v;                // V { x: 3.0, y: 8.0 }
    let b: u8 = 3;
    let s = b * v;                          // V { x: 3.0, y: 6.0 }
    let bad = 2 * v;                        // error: `2` fits both `f64` and `u8`
}
```

The arithmetic operators choose their method by both operands' types, not only the left one. That lets a vector, matrix or array type have `v * w` and `v * 2.0` at once, and lets a number be on the left. Nothing else is overloaded.

## CONTEXT

- Read first: `src/check/operators.rs` (`operator_call`, `operator_method`, `operator_assign`), `src/check/methods.rs` (`method`: one method per type and name), the duplicate-method check, `src/check/visibility.rs`, `src/parser/` for `impl` heads, `src/codegen/operators.rs` (`operator_method`, `operator_on`), the symbol naming in `src/codegen/declare.rs`, `src/prelude/ops.la` (`Add`…`Num`), and the LSP hover and definition code for methods.
- Spec: §4 Operator overloading (the **Types** bullet says "Each type has one method per operator; the left operand decides, so `2.0 * v` is E0404"), §4 Compound assignment, §8 symbol names and lowering, §12 new rows, §11 rows 158 and 159, "Changes since v16".
- DECISIONS: this amends 100–101 (one method per operator, left operand decides). The user decided it in discovery (session of 2026-11-12) after weighing `r*` methods and full method overloading. It gets a new DECISIONS entry on approval. Core decision 6 still holds: symbols keep `lugha_fn_`, and only an overloaded method's name gets its right-hand type appended. Nothing open blocks this.

## DECISIONS MADE IN DISCOVERY (approved in conversation; listed for review)

1. **Only `add`, `sub`, `mul`, `div` and `rem` may be declared more than once on a type,** each with a different right-hand parameter type, and each with the operator shape (plain `self`, one parameter, a result). `neg`, `eq`, `cmp`, `index`, `set_index` and every other method stay one per name. Two `mul`s with the same parameter type are the existing duplicate-method error. A `mul` without the operator shape (a set's `fun add(mut self, x)`) next to another `add` is a duplicate too.
2. **Exact types only.** A candidate fits when its parameter type is exactly the right operand's type. No conversion counts, not even to an interface value, so nothing is ranked.
3. **A literal (or other deferring operand: `[]`, `None`) must fit exactly one candidate, on either side.** Otherwise it is a new error naming the candidates, with help to write `2.0` or `2 as u8`. So adding a `mul` can never silently change which one an existing expression calls; at worst it becomes an error.
4. **A left type that isn't yours:** `impl T { fun mul(self, v: V): V }`, where `T` is a built-in number or a type from another module, is allowed only for these five methods, only in the module that declares `V`, and only when the parameter's type is `V` (or a type built from it: `V[]`, `Option<V>`). Two packages can't both define `W * V`, since each would have to import the other. Anywhere else it is a new error saying where the method has to live. Ordinary methods can't be added to other people's types this way.
5. **Bounds don't change:** `T: Mul` still means `T * T → T`. A type satisfies `Mul` when one of its `mul`s takes `Self`. Mixed-type operators work on concrete types only; a bound with a parameter (`T: Mul<f64>`) is a separate decision.
6. **An explicit call chooses the same way:** `v.mul(2.0)` picks the overload exactly as `v * 2.0` does, so a method means one thing however it's called. `2.0.mul(v)` is the `impl f64` one.
7. **Compound assignment** picks by the same rule; the result must still have the place's type (E0403), so `k *= v` with `k: f64` is E0403.
8. **Generic types:** overloads are chosen after the receiver's type arguments are known. If two candidates become the same type for some arguments (`impl P<A, B> { fun mul(self, a: A); fun mul(self, b: B) }` used as `P<f64, f64>`), the use is the ambiguity error of decision 3, reported where it is used.
9. **Evaluation order is unchanged:** left operand, right operand, then the call.
10. **Interfaces declare at most one `mul`,** as today. A type with several satisfies the interface when one of them matches its signature, and the interface table points at that one.
11. **No IR change for programs with one method per operator:** a type with a single `mul` keeps the symbol `lugha_fn_V.mul`; only a type with several gets `lugha_fn_V.mul(f64)`, `lugha_fn_V.mul(V)`, and a left type that isn't yours gets its own name, e.g. `lugha_fn_f64.mul(V)`.

## REQUIREMENTS

### Must Do
- **Milestone 158:** decisions 1–3 and 5–11: several arithmetic methods per type, chosen by the right operand's type; the new ambiguity error.
- **Milestone 159:** decision 4: `impl f64 { … }` and `impl other.W { … }` for the five methods, in the right-hand type's module; the new placement error.
- Spec §4 (Operator overloading, Compound assignment), §8 (symbols, lowering), §12 (the two new codes), §11 rows 158 and 159, "Changes since v16"; their `docs/errors/` pages; the Draft; DECISIONS entry; CHANGELOG; TODO.
- LSP: hover and go-to-definition on an operator or `.mul(…)` show the overload chosen; completion lists each `mul` with its parameter.

### Must NOT Do
- No overloading of other methods or free functions; no conversions while matching; no `r*` methods; no bounds with parameters; no new operators; no overloading of the bit operators.

## DESIGN

- Checker: the method table keeps, for these five names only, a list of overloads keyed by right-hand type. `operator_call` (and an explicit method call on these names) resolves the right operand's type first (a literal against the candidates, decision 3), then picks; the choice is recorded for codegen. `impl` heads accept a built-in number or a type from another module, checked against decision 4.
- Codegen: reads the recorded choice; symbol names as in decision 11.
- Security: none. No external input, no `unsafe`, no new dependency.

## TESTS

- [ ] `tests/programs/interfaces/mixed_arithmetic/`: the OBJECTIVE (without the error line), `v * w` and `v * 2.0` together, `+=` choosing by type, an explicit `v.mul(2.0)`, a generic `NdArray`-style type with `mul(self, o: Self)` and `mul(self, k: T)`, a `T: Mul` function used with `V` (the `Self` one), an interface with one `mul` satisfied by a type with two.
- [ ] Milestone 159 cases: `f64 * V`, `u8 * V`, `W * V` with `W` from another module of the program, and a package case in the existing package tests.
- [ ] Rejected programs: ambiguous literal on each side (new code), `2 * v` with `f64` and `u8` left impls, no match (E0404 listing what exists), two `mul(f64)` (duplicate), `impl f64 { fun mul(self, k: i64) }` with no type of the module involved and `impl f64 { fun scale(…) }` (placement error), `k *= v` (E0403), `P<f64, f64>` collapse (decision 8).
- [ ] Unit tests in the checker for resolution and placement; an IR test that a single-`mul` program's IR is unchanged.

## ACCEPTANCE
- [ ] The OBJECTIVE prints `V { x: 3.0, y: 8.0 }` and `V { x: 3.0, y: 6.0 }` without the last line; with it, that line is the ambiguity error.
- [ ] `cargo test --lib`, `--test programs`, `--test explain` and the LSP tests pass; fmt and clippy are clean.
- [ ] Spec, Draft, DECISIONS, CHANGELOG and TODO are updated; tags `m158` and `m159`.
