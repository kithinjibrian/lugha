## FEATURE: A method may share its name with a field, or (with `self`) with a variant, wherever the syntax tells them apart; milestone 123 (v17).

**Status:** approved 2026-10-20 (session 294) with the design agreed in chat: the user asked to lift E0310 for a field and a method; Rust's behaviour was checked with rustc 1.99.0 (it accepts every pair but silently prefers the method over a function field and the variant over an associated function, which becomes uncallable); the user chose the principle below over Rust's silent precedence.

## OBJECTIVE
> Two members of a type may share a name when every use shows which one is meant; where the same text would mean either, it is E0310.

```lugha
struct P { name: string, area: fun(i64): i64 }
impl P { fun name(self): string = "Dr. {self.name}"; }      // allowed (E0310 before)

enum Shape { Circle(f64), Square(f64) }
impl Shape { fun Circle(self): bool = match self { Shape.Circle(_) => true, _ => false }; }  // allowed

p.name         // the field          p.name()   // the method
Shape.Circle(1.0)  // the variant    s.Circle() // the method
```
Still E0310: a method named like a field whose declared type is a function type (`q.area(5)` could be either); an associated function (no `self`) named like a variant (`Shape.Circle(1.0)` could be either); two methods with one name.

## CONTEXT
- `src/check/methods.rs` `member_named` reports E0310 for any field or variant; `errors::method_named_like` (`check/errors/names.rs`).
- `src/check/fun_values.rs` `fun_field`: `p.f(x)` calls a function-typed field only when there is no method `f`; unchanged, and with this rule a declared function field never meets a method.
- Spec v17 §4 "Methods" (line ~691: "may not repeat a field or variant name (E0310)"); `docs/errors/E0310.md`.
- Language server: references, hover and rename key members by declaration span (`driver/lsp/references.rs` `Key::Decl`), so a field and a method of one name stay distinct when each use records its own declaration.
- Open decisions: none.

## DECISIONS (draft decisions 269–275)
269. **The principle** above, stated in spec §4 "Methods".
270. **Field + method**: allowed unless the field's declared type is a function type (`fun(…): …`). `p.name` is the field, `p.name()` the method. A field of generic type `T` doesn't count as a function field; `s.f(x)` on it calls the method, as `fun_field` already does.
271. **Variant + method**: allowed for a method with `self` or `mut self`; `Shape.Circle`, written or as a value, is always the variant, and `s.Circle()` the method. An associated function named like a variant stays E0310.
272. **E0310** keeps its code; its message and page say which pair clashes and why (`` `P` already has a field `area` holding a function: `p.area(…)` could call either ``).
273. **Tooling**: hover, go-to-definition, Find References, Rename and completion treat the field and the method as two members. Rename refuses a new name only where the result would be E0310 (its in-memory re-check does this).
274. **Not**: overloading methods by arity, fields named like associated functions on the type path (fields have no type path), changes to interfaces (items are methods only).
275. **Docs**: spec §4 "Methods", "Changes since v16", §11 row 123, the E0310 row of the error table; `docs/errors/E0310.md`; Draft; CHANGELOG (Changed). **Success**: milestone 123, tag `m123`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `member_named` reports only a function-typed field, or a variant when the method has no receiver.
- `p.name` resolves to the field and `p.name()` to the method in every checker path (field access, method call, method value if any, `mut self` calls, through generic instances).
- `Shape.Circle` resolves to the variant everywhere (construction, patterns, variant values), never the method.
- Each use records its own declaration for the language server.

### Must NOT Do
- No precedence rule between same-text uses (that is what E0310 is for).
- No new diagnostic code.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- E0310 (reworded) for the pairs that remain errors; nothing else new.

## SECURITY CONSIDERATIONS
- None: checker rules only.

## TESTS TO WRITE
- [ ] Checker (`check/methods.rs`): a field and a method `name` both type (`p.name` string, `p.name()` its result); a method on `mut self` named like a field changes the field; a generic struct's field and method; E0310 for a function-typed field; a variant and a `self` method; E0310 for an associated function named like a variant; E0310 for two methods.
- [ ] End to end `tests/programs/m123/shared_names.la` (+ `.stdout`): the OBJECTIVE program printing each use, a method reading its namesake field, through an interface, a function field without a method still called as `p.area(5)`.
- [ ] `tests/programs/m123/e0310_fun_field.la` + `.stderr`.
- [ ] LSP: hover and go-to-definition on `p.name` reach the field, on `p.name()` the method; Find References on each lists only its own uses (`tests/lsp_references.rs`, `tests/lsp_definition.rs`).
- [ ] `tests/explain.rs`: the rewritten E0310 page's examples.

## ROLLBACK PLAN
- Branch to return to: `main`. The feature lives on `prp-155-field_method_names` until merged.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE program runs; the still-E0310 cases are reported
- [ ] All existing tests pass (`--lib`, `programs`, `explain`, the LSP suites touched)
- [ ] New tests written and passing
- [ ] `cargo clippy --all-targets -- -D warnings`, `cargo fmt --check` pass
- [ ] Spec, Draft, E0310 page and CHANGELOG updated; no file over 500 lines

## VALIDATION
- `cargo fmt --check`; `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`; `cargo test --test programs --test explain --test lsp_references --test lsp_definition --test lsp_hover`
