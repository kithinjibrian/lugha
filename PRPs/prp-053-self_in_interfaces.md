## FEATURE: `Self` in interfaces — the implementing type in signatures and default bodies; bounds-only interfaces (E0427); milestone 25.

**Status:** approved 2026-10-13 — session 67
**Milestone:** 25 (one PRP; tag `m25` at merge)
**Spec:** `Language v6 Draft.md`:
- decision 2, and the choice "Inside an interface, `Self` always means the implementing type";
- §4 "`Self`";
- §9 (E0427);
- §10 "`Self`";
- §11 (milestone 25).

**Decisions:** MEMORY 59 (v6), MEMORY 58 (defaults are generic over a hidden `Self`), MEMORY 60–61 (`Applied`, `items`).

## OBJECTIVE
An interface can name the type that satisfies it. The §10 `Self` program prints `1999` and `2.0`:

```
interface Ordered {
    fun less(self, other: Self): bool;
    fun max(self, other: Self): Self = if self.less(other) { other } else { self };
}
impl Money { fun less(self, other: Money): bool = self.cents < other.cents; }

fun largest<T: Ordered>(xs: T[]): T { … best = best.max(x); … }

let xs: Ordered[] = …;     // E0427: `Ordered` uses `Self`, so it can only be a bound
```

## CONTEXT

- **Starting state:**
  - Default methods are generic over a hidden parameter named `Self` (`check::SELF`).
  - A written `Self` is E0305 everywhere (`generics::resolve_named`).
  - An interface parameter named `Self` is E0316 (PRP-050).
- **Ending state:**
  - `Self` resolves inside interfaces.
  - Satisfaction, calls through bounds and inference replace it with the type in question.
  - E0427 exists, and the spec is at milestone 25.
- **Related existing code:**
  - `src/check/generics.rs` (`resolve_named`);
  - `src/check/interfaces.rs` (`InterfaceInfo`, `resolve_interfaces`);
  - `src/check/applied.rs` (`items`);
  - `src/check/satisfy.rs` (`bound_entry`, `unmet`);
  - `src/check/bound_infer.rs` (`read_off`);
  - `src/check/iface_values.rs` (`interface_type`);
  - `src/check/iface_tables.rs` (`interface_methods`);
  - `src/check/defaults.rs`;
  - `src/check/errors/`.
- **Open decisions that must be resolved first:** none.

### Discovery
- **No interview:** the approved v6 draft settles every behaviour of this milestone (decision 2, §4 "`Self`"). The design below only maps it onto the compiler.

### Design
- **Resolving `Self`:** while an interface's items resolve, and while its default bodies are checked, `Self` is in scope as the type parameter `SELF`. That is the same hidden parameter defaults already have. So `other: Self`, `: Self`, `Self[]` and `Option<Self>` become `Param("Self")` there. Anywhere else, a written `Self` is still E0305.
- **The items for a type:** `items_for(applied, self_ty)` gives `items(applied)` with `Self` replaced by `self_ty`. Every place that compares or calls items passes the type in question:
  - **satisfaction** (`unmet`): the argument type, so `Money` meets `less(self, other: Money)`, and `Stack<i64>` meets it at its arguments;
  - **calls through a bound** (`bound_entry`): the type parameter. So with `T: Ordered`, `a.less(b)` needs `b: T`, `a.max(b)` returns `T`, and `T.zero()` returns `T`;
  - **inference through bounds** (`read_off`): the bound parameter's type;
  - **an interface value as a type argument:** that interface type.
- **E0427:**
  - **What it means:** `InterfaceInfo.uses_self` records the first method that mentions `Self` outside its receiver.
  - **When:** such an interface written as a type is E0427, naming that method. It is reported where E0425 is, and E0425 is reported first if both apply.
  - **Tables:** such interfaces get no entry in `Checked.interfaces`, since they never have values.
- **Default methods:** unchanged. Their generic parameter `Self` is the receiver's type, so a default can take and return `Self` (`max`), and its instance substitutes the concrete type.
- **Codegen:** no changes. Bound calls and default instances are already compiled per concrete type.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Checker:**
  - `Self` in scope in interface items and default bodies;
  - `items_for` and its four uses above;
  - `InterfaceInfo.uses_self`;
  - E0427 (a new constructor in a new `errors/v6.rs`);
  - `interface_methods` skips interfaces with `uses_self`.
- **Spec** (`Language v6 Specification.md`):
  - fold in §4 "`Self`", replacing "no `Self`" in "Declaring one" and v5's default-body sentence;
  - the E0427 row;
  - the §10 `Self` program;
  - §11 milestone 25 done;
  - "Changes since v5".
- **Docs:** CLAUDE.md (anti-pattern 5's list, STACK, tree), README, TODO, CHANGELOG, MEMORY decision 63, CONTEXT. The tag `m25` is added at the merge.

### Must NOT Do
- **No `Self` outside interfaces** (in `impl` blocks): it is out of scope for v6.
- **No converting between interfaces:** that is milestone 26.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **New code E0427:** "`Ordered` can't be a type: `less` uses `Self`", with help "two `Ordered` values could hold different types; use it as a bound: `<T: Ordered>`".
- **E0305:** `Self` outside an interface.
- **E0422:** a type whose method doesn't match with `Self` replaced, with substituted help (`` `less` should be `fun less(self, other: Money): bool` ``).
- **E0403:** a call through a bound with the wrong argument type (`a.less(1)`).

## SECURITY CONSIDERATIONS
- No external input, no `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (new `check/self_type.rs`, which holds `items_for`):
- [ ] **Resolution:**
  - `Self` resolves in item signatures (`Self`, `Self[]`, `Option<Self>`) and in default bodies;
  - in a function, struct field or `impl`, it is E0305.
- [ ] **Satisfaction:**
  - `Money` and a generic `Stack<i64>` satisfy `Ordered`;
  - a `less(self, other: i64)` is E0422, with the substituted help.
- [ ] **Bounds:**
  - with `T: Ordered`, `a.less(b)` checks and `a.less(1)` is E0403;
  - `a.max(b)` has type `T`;
  - `fun zero(): Self` gives `T.zero(): T`.
- [ ] **E0427:**
  - `fun f(x: Ordered)`, `Ordered[]` and `Option<Ordered>` are E0427, with help;
  - an interface whose only `Self` is the receiver is still a type;
  - an interface with both an associated function and `Self` reports E0425.
- [ ] **Inference:** `fun g<T, O: Pick<T>>(o: O): T` with `interface Pick<T> { fun pick(self, other: Self): T; }` infers `T` from the concrete type.

Acceptance — `tests/programs/m25/`:
- [ ] `self.la`: the §10 program.
- [ ] `more.la`:
  - `Self` returns through a bound (`fun twice(self): Self`);
  - an associated `fun zero(): Self` through `T.zero()`;
  - `Self[]` and `Option<Self>` parameters;
  - a generic `Pair<T>` satisfying `Ordered`;
  - a default method using `Self`, overridden by one type.
- [ ] `e0427.la`: reject golden.
- [ ] The §10 `Self` program runs from the spec.

Regression:
- [ ] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-053-self_in_interfaces`, merged into `main` after CI passes; tag `m25`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The §10 `Self` program and the `m25/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [ ] The spec is at milestone 25; docs updated.
- [ ] No file over 300 lines: new code goes in new files, and any split of an existing file is proposed first. No new dependencies, no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m25/self.la
```
