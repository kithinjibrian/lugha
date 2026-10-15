## FEATURE: Shared boxes — `Ref<T>`, the one shared, changeable object, opening Lugha v16; milestone 67.

**Status:** proposed 2026-10-15 — session 135.
**Milestone:** 67 (one PRP; tag `m67` at merge). The first milestone of v16; v15 is declared complete and frozen.
**Spec:** `Language v16 Draft.md` (decisions 1–5); spec §4 (types, equality, copies), §5 (method calls), §7 (memory model), §9 (lowering), §10 (a program), §11 (row 67), §12.

**Decisions:**
- the first exception to value semantics (MEMORY 16), deliberately narrow;
- builds on copy-on-write and loop ownership (MEMORY 107, 109), whose invariants it keeps;
- no open entries in DECISIONS.md apply.

## OBJECTIVE
```
let n = Ref.new(0);
let tick = fun(): i64 {
    n.update(fun(x: i64): i64 = x + 1);
    n.get()
};
tick(); tick();
println(n.get());                     // 2

struct Node { value: i64, next: Option<Ref<Node>> }
```
Every other type behaves exactly as in v15.

## CONTEXT
- **Starting state:**
  - every value is copied (copy-on-write since M55);
  - closures can't change captures (E0511);
  - `Option`, `Result` and `Ordering` are the predeclared generic and prelude types;
  - arrays have built-in methods (`push`, `pop`, `clear`) checked in `check/builtins.rs`.
- **Ending state:** decisions 1–4.
- **Related existing code:**
  - **Checker (`src/check/`):** `types.rs` (`Type`, `is_deep`, `Display`), `env.rs` (predeclared names), `builtins.rs`/`method_calls.rs` (built-in methods), `call.rs`/`construct.rs` (`T.f` associated calls), `bounds.rs` (`eq_ok`), `slots.rs` (recursion and indirect slots), `printing.rs`, `generics.rs` (substitution), `layouts.rs`, `closures.rs` (E0511), `errors/`.
  - **Codegen (`src/codegen/`):** `value.rs` (`llvm_type`), `copy.rs` (copy sites: a `Ref` is a pointer copy), `equality.rs`, `show.rs` (printing with a cycle stack), a new `refs.rs` (`new`/`get`/`set`/`update`/`is`).
  - **Runtime:** a tiny cycle stack for printing (`lugha_rt_show_enter` / `lugha_rt_show_leave`), or the same in generated code.
  - **Spec files:** `src/lib.rs` (`SPEC` → v16), `docs/specs/`.
- **Open decisions that must be resolved first:** none.

### Design
1. **The type:** a new `Type::Ref(Box<Type>)`, predeclared as `Ref<T>` (a module's own `Ref` shadows it). It prints `Ref<i64>` in types. `is_deep` is false: copying a `Ref` copies the pointer and marks nothing.
2. **Recursion:** `Ref` breaks the inline-recursion rule like arrays do (`slots.rs`), so a struct may hold `Option<Ref<Self>>` directly.
3. **Built-ins:**
   - `Ref.new(v)` is an associated call on the predeclared name, checked like `Option`'s constructors.
   - `get`, `set`, `update` and `is` are built-in methods with plain `self` receivers (decision 4), so a `let` binding and a captured `Ref` may use them. `set(v)` stores `v` by the normal store rules (copying from a place, moving on last use). `update(f)` evaluates `f(r.get())`, then stores it.
   - No other method, field or index applies to a `Ref`: E0415 / E0410.
4. **Equality (decision 3):** `eq_ok` is false for `Ref` and for any type holding one. `==` gives E0404 with help naming `is` and `get`. Bounds give E0419.
5. **Codegen:**
   - a `Ref<T>` is a `ptr` to a GC object holding `T` in element layout;
   - `new` is `lugha_rt_alloc(size)` plus a store, and `get` is a load plus `value_for_store`, so arrays inside are marked shared when handed out;
   - `set` is `value_for_store` plus a store, and `is` is an `icmp eq` on the pointers.
   - The copy helpers treat `Ref` as a plain pointer and never recurse into it.
6. **Printing:** `lugha_show_Ref<T>` checks whether its box is on a stack of boxes being printed. If it is, it writes `Ref(…)`; otherwise it pushes the box, writes `Ref(` + contents + `)`, and pops. The stack is a runtime array of pointers, reset at each top-level print.
7. **Version change:** `Language v15 Specification.md` is frozen (status line: complete, milestones 51–66). `Language v16 Specification.md` starts as its copy, with the v16 header and changes summary. `lugha::SPEC` points at it, and CLAUDE.md, README and `tests/spec.rs` follow.

### Risk
- **Breaking value semantics by accident:** a `Ref` copied by deep copy would split one box into two; a box's contents written in place would break copy-on-write. Hazard programs check both:
  - two handles see one box after copies through structs, arrays, interface values, closures and function returns;
  - a value from `get` never changes when the box is `set`, including for arrays inside.
- **Printing cycles:** a test prints a two-node cycle and a self-cycle.
- **E0511 interplay:** a test that a closure may `set` a captured `Ref` but not reassign it.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec:**
  - freeze v15;
  - `Language v16 Specification.md`: §4 (`Ref<T>`, equality, copies table), §5 (the methods), §7 (memory model and ownership sentence), §9 (lowering), §10 program "Shared boxes", §11 row 67, §12 (the open questions from the draft), the header and changes summary;
  - `lughac spec` prints v16.
- **Docs:** CLAUDE.md (STACK: v16; tree; spec files), README, TODO (v16 section; the "shared references" item done as `Ref`), CHANGELOG, MEMORY 125 (plan) and 126 (built), CONTEXT; tag `m67`.

### Must NOT Do
- **No change to any v15 program's output** (barring one that declares its own `Ref`, which still means its own).
- **No in-place access to a box's contents,** no references (`&`), no classes.
- **`==` never compares boxes;** `Ref` isn't `Eq`.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0404:** `r == s`, with help.
- **E0419:** a `Ref`, or a type holding one, where `Eq` is required.
- **E0415:** an unknown `Ref` method.
- **E0410:** a field or index through a `Ref`.
- **E0501:** `r = …` when `r` isn't `let mut`, as for any binding.
- **E0511:** unchanged.

## SECURITY CONSIDERATIONS
- **No `unsafe`, dependencies or external input.**
- **Memory safety:** boxes are GC objects, so no dangling handles are possible.

## TESTS TO WRITE
**Checker unit tests:**
- [ ] `Ref.new(1)` is `Ref<i64>`; `r.get()`, `r.set(2)`, `r.update(f)` and `r.is(s)` type-check, with plain `let` bindings.
- [ ] A closure may call `set`/`update` on a captured `Ref`; assigning the captured `r` is E0511.
- [ ] `r == s` is E0404 with help; a struct holding a `Ref` in `==` is E0404; `T: Eq` with `Ref<i64>` is E0419.
- [ ] `r.value`, `r[0]` and `r.push(1)` are rejected.
- [ ] `struct Node { next: Option<Ref<Node>> }` is accepted.
- [ ] A module's own `Ref` shadows the predeclared one.

**Acceptance (`tests/programs/m67/`):**
- [ ] **`refs.la`:**
  - a counter closure;
  - two handles through a struct field, an array element, an interface value and a function's return all see one box;
  - `get` copies (an array taken out, then the box `set`: the copy is unchanged; and the reverse);
  - `update`, `is`, generic `fun bump<T: Num>(r: Ref<T>)`.
- [ ] **`linked.la`:** a doubly linked list built and walked both ways; a self-cycle and a two-node cycle printed with `Ref(…)`.
- [ ] **`e0404.la`:** a rejected program.
- [ ] **The §10 program "Shared boxes".**

**Regression:**
- [ ] Every v15 program passes unchanged.
- [ ] `lughac spec` prints v16.

## ROLLBACK PLAN
- Branch `prp-096-ref`; tag `m67`.
- To abandon: delete the branch (v15 stays the current spec).

## ACCEPTANCE CRITERIA
- [ ] All tests pass at `-O0`; `m67/` at `-O2`.
- [ ] v15 frozen; the v16 spec at milestone 67; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo run -q -- run tests/programs/m67/refs.la
```
