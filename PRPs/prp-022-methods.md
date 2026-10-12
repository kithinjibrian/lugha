## FEATURE: Methods — `impl` blocks with `self` / `mut self` receivers, associated functions, and method calls; the v1 gate is removed. Completes milestone 7.

**Status:** implemented 2026-10-12 — session 28 (branch `prp-022-methods`, awaiting merge)
**Milestone:** 7 (one PRP). Done-when: the methods program prints `5.0` then `6.0`; then tag `m7`.
**Spec:** `docs/specs/Language v1 Draft.md` §4 "Methods" and copy-table additions, §5 method-call evaluation, §6 symbols, §9 E0310/E0415, §10 methods program, folded into `Language v1 Specification.md`
**Decisions:**
- MEMORY 23 (v1 direction: dot access, `impl` blocks);
- MEMORY 24/25/26 (gate, enums);
- MEMORY 16/22 (copy sites, move on last use).

## OBJECTIVE
Structs and enums can have methods.
- **`self` methods:** `v.len_sq()` reads the receiver.
- **`mut self` methods:** `v.scale(2.0)` changes the caller's variable in place, which must be `let mut`.
- **Associated functions:** `Vec2.new(1.0, 2.0)` builds values.
- **Errors:**
  - unknown methods are E0415;
  - duplicate methods, or methods named like fields, are E0310;
  - passing a value to a `mut self` method called on that same value is E0508.
- **The gate goes:** every v1 construct is now checked, so `driver/gate.rs` is deleted.

The methods program prints `5.0` and `6.0`.

## CONTEXT

- **Starting state:**
  - `impl` blocks, receivers and `self` parse (PRP-019).
  - The driver gate stops them as "methods (milestone 7)".
  - The checker and codegen have `unreachable!` arms for `Item::Impl`.
- **Ending state:**
  - New `check/methods.rs` and `codegen/methods.rs`.
  - The receiver rules, E0310/E0415/E0508.
  - `driver/gate.rs` deleted.
  - The v1 spec gains methods and the methods program.
- **Related existing code:**
  - `src/check/{env,call,expr,assign,array,mod,liveness}.rs`, `src/check/errors/*`;
  - `src/codegen/{function,structs,expr,lower}.rs`;
  - `src/driver/{gate,pipeline,mod}.rs`;
  - `tests/{cli,codegen}.rs`, `tests/common/v1_programs.rs`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 28)
- **Approved split:** `check/methods.rs` reached 357 lines, so calls moved to a new `check/method_calls.rs` (162 + 211 lines).
- **E0501 wording:** for a `mut self` receiver it reads "cannot call `mut self` method `bump` on `p`: it is not mutable" (`check_place_for` with the method name).
- **E0508's help** reads "copy it first: `let w = v;` and pass `w`".
- **Function lowering:** `define` became `define_with(f, signature, receiver)`, and argument lowering is shared (`arguments`), so methods reuse both.
- **IR names:** LLVM prints `lugha_fn_V.len_sq` unquoted.
- **Element receivers:** the IR test checks the call sits in the `bounds.ok` block, because the failure block is laid out last.
- **Gate removal:** `tests/codegen.rs` drops the exit-2 skip, and the CLI gate test now runs an `impl` program.

### Discovery answers (session 28)
1. **One PRP** for the whole milestone.
2. **`mut self` aliasing is rejected at compile time.**
   - **Rule:** in a `mut self` call, an argument that mentions the receiver's root variable is **E0508** "cannot pass `v` to a `mut self` method called on `v`", with a label "`v` is edited by this call" and help "copy it first: `let w = v; v.add(w);`".
   - **Why:** otherwise the method could observe its own edits through an argument, which arguments, passed uncopied, would expose.
   - **Overlap** is conservative, like E0507: any mention of the root counts.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Checker** — new `check/methods.rs`
- **Collection (pass 1, after types):**
  - Every `impl T` must name a struct or enum declared in the file. Anything else is E0305, including `Option`, arrays and primitives.
  - A type may have several `impl` blocks.
  - Method signatures resolve in pass 2, like functions.
  - A method name repeated for the same type, or equal to a field of the struct, is **E0310**, with "first declared here".
- **Calls `x.m(args)`:** `x`'s type has a method `m`.
  - An unknown method is **E0415** "no method `m` on `T`". Arrays and other built-ins have no methods until milestone 8, so `xs.push(1)` is E0415 for now.
  - A method without a receiver called through a value is E0415, with help "call it on the type: `T.m(…)`".
  - Argument count is E0405; argument types are E0401/E0403, as for functions.
- **Associated calls `T.f(args)`:**
  - `T` must name a struct or enum, not a local (E0311 guarantees the distinction), and `f` a method of `T` without a receiver.
  - A method with a receiver called this way is E0415, with help "call it on a value: `x.f(…)`".
  - Enum variants keep priority for `E.V(…)`; a variant and a method of the same name is E0310.
- **`self` receivers:**
  - **`self`** is bound like a parameter: immutable and borrowed. Storing or returning it copies (spec §4).
  - **`mut self`** is a new `Binding::SelfMut`.
    - Assigning to `self`, its fields and its elements is allowed.
    - It counts as borrowed for moves, since it is the caller's data: returning or storing it copies it, and it is never moved from.
    - A `mut self` call needs `x` to be a place with a `let mut` root (E0501, as for assignment; a non-place is E0502). A chain through a `string` is E0506. A receiver in a `for … of` iteration overlap is E0507.
- **E0508:** for a `mut self` call, any argument whose expression mentions the receiver's root variable.
- **`self` outside a method** is E0301 "cannot find `self` in this scope" (a free function has no `self`).
- **Return checking, E0503 and flow rules** apply to method bodies as to functions.

**Codegen** — new `codegen/methods.rs`
- **Symbols:** methods and associated functions are `lugha_fn_<Type>.<name>`, declared with every function before bodies are lowered, in source order.
- **Receivers:**
  - **`self`:** a struct or enum receiver is passed like any aggregate argument, as a pointer to a temporary copy of the caller's value. The callee uses it as its slot.
  - **`mut self`:** the caller passes the address of the receiver place itself (`place_address`: the slot, field address, or bounds-checked element). The callee's `self` slot is that pointer, so writes go straight to the caller's place.
- **Evaluation order (spec §5):** the receiver, then the arguments left to right. A `mut self` receiver's place is evaluated once, before the arguments.
- **Returns:** struct and enum returns by value, as for functions.

**Driver**
- Delete `driver/gate.rs` and its call in `pipeline::front`.
- Update `tests/cli.rs`: the gate test becomes a test that an `impl` program builds and runs.
- `tests/codegen.rs` drops the "exit 2 is the gate" skip.

**Spec** — fold into `Language v1 Specification.md`
- **§2:** `impl` and `self` are no longer "reserved for milestone 7".
- **§3:** the `impl_decl`, `method` and `receiver` grammar, and `self` in `primary`.
- **§4:** Methods (from the draft), the E0508 rule, and the copy-table rows for receivers.
- **§5:** method-call evaluation.
- **§6:** methods in their type's namespace; the `lugha_fn_<Type>.<name>` symbols.
- **§9:** E0310, E0415 (methods), E0508; the method lowering note.
- **§10:** the methods program, which prints `5.0` then `6.0`.
- **§11:** the milestone 7 row is done.
- **Draft:** folding progress updated.
- `tests/common/v1_programs.rs` drops `METHODS`.

**Docs**
- CLAUDE.md:
  - tree: the new files and the removed `driver/gate.rs`;
  - the known-issue line about the gate is removed;
  - architecture rule 7 (symbol prefixes) gains `lugha_fn_<Type>.<name>`.
- MEMORY, CHANGELOG, TODO, CONTEXT. Tag `m7` after merging.

### Must NOT Do
- No methods on built-in types (`Option`, arrays, primitives); no growable-array methods (milestone 8).
- No generic methods, traits or interfaces; no method overloading.
- No implicit copies of arguments (discovery answer 2 rejects the alias instead).
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- Every new rule reports a code and keeps going. An unknown method's arguments are still checked; a call with errors has type `Error`.
- **Codegen invariants:**
  - every method call resolves to a declared function;
  - every `mut self` receiver is a place.

## SECURITY CONSIDERATIONS

- **`mut self` pointers:** they point at the caller's own place for the duration of the call only. They are never stored (a `self` that is stored or returned is copied).
- **Element receivers:** they are bounds-checked before their address is formed, as for assignment.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests:
- [x] **Checker:**
  - methods in any order and across several `impl`s;
  - an `impl` on an unknown or built-in type → E0305;
  - E0310 (duplicate method, method named like a field, method named like a variant);
  - E0415 (unknown method, a receiverless method through a value, a receiver method through the type, `xs.push`);
  - argument count and types;
  - `self` immutable (E0501 on assignment) and `mut self` assignable;
  - a `mut self` call on an immutable root → E0501;
  - on a non-place → E0502;
  - E0508 for `v.add(v)` and `v.add(v.part)`, not for `v.add(w)`;
  - E0507 for a `mut self` call on an iterated array;
  - `self` in a free function → E0301.
- [x] **Codegen IR:**
  - `define … @"lugha_fn_Vec2.len_sq"(ptr`;
  - a `mut self` call passes the variable's own `alloca`, with no temporary copy;
  - an element receiver `ps[i].scale(2.0)` is bounds-checked before the call.

Acceptance:
- [x] **Spec §10:** the methods program prints `5.0` then `6.0` (from the spec, at `-O0`/`-O2`).
- [x] **`m7/methods.la`:**
  - methods on a struct and on an enum;
  - associated constructors;
  - a `mut self` method that reassigns `self` whole;
  - `mut self` through a field (`r.min.shift(1.0)`) and through an element (`ps[1].scale(2.0)`);
  - method chains on rvalues (`Vec2.new(1.0, 1.0).len_sq()`);
  - a method calling another method on `self`;
  - a method returning `self`, then mutated by the caller while the original is unchanged.

  Expected stdout written by hand.
- [x] **Reject programs:** `e0310.la`, `e0415.la`, `e0508.la`, `e0501_receiver.la`. Goldens captured and reviewed.
- [x] **CLI:** an `impl` program runs (the gate is gone).
- [x] All earlier tests pass.

## ROLLBACK PLAN

- Branch `prp-022-methods`, merged into `main` after green CI; then tag `m7`.
- To abandon: delete the branch; the gate keeps methods stopped.

## ACCEPTANCE CRITERIA
- [x] The methods program prints `5.0` then `6.0`; every test above passes.
- [x] `driver/gate.rs` is gone; the v1 spec covers methods; CLAUDE.md, MEMORY.md, TODO.md, CHANGELOG.md updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac run` on the methods program prints `5.0` and `6.0`
