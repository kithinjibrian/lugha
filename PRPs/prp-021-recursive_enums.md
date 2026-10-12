## FEATURE: Recursive enums — an enum (or a struct, through an enum or `Option`) may contain itself; only the slots that lead back are stored on the heap. Completes milestone 6.

**Status:** implemented 2026-10-12 — session 27 (branch `prp-021-recursive_enums`, awaiting merge)
**Milestone:** 6, last of three PRPs (019 syntax ✓, 020 enums ✓, 021 this). Done-when: the linked-list program prints `6`, then tag `m6`.
**Spec:** `docs/specs/Language v1 Draft.md` §4 ("Recursion"), §6 (pass 2), §7 (indirect slots), §10 (linked list), folded into `Language v1 Specification.md`
**Decisions:**
- MEMORY 23 (implicit recursion: no `Box`, only recursive slots on the heap);
- MEMORY 25 (enum payloads are shared, never deep-copied);
- MEMORY 24 (the gate).

## OBJECTIVE
`enum List { Nil, Cons(i64, List) }`, `enum Tree { Leaf, Node(Tree, i64, Tree) }` and `struct Node { value: i64, next: Option<Node> }` compile and run, with no `Box` and nothing to annotate. Only the payload slot that leads back to the enum (the inner `List`) holds a hidden pointer, so `Nil` allocates nothing. Because enum payloads are shared, copying a list of any length is constant time. The linked-list program prints `6`, and milestone 6 is complete.

## CONTEXT

- **Starting state:**
  - Non-recursive enums work (PRP-020).
  - The driver gate stops recursive enums, and struct cycles through an enum or `Option`, as "recursive enums (milestone 6)".
  - The v1 spec says an enum "may not contain itself yet".
- **Ending state:**
  - A shared `indirect_slot` rule decides which payload slots live on the heap.
  - Codegen lays out, builds and reads such slots.
  - The gate stops only methods.
  - The v1 spec gains recursion, indirect slots, a stack-depth note, and the linked-list program.
- **Related existing code:**
  - `src/check/types.rs`;
  - `src/codegen/{enums,matching,structs}.rs`;
  - `src/driver/gate.rs`;
  - `docs/specs/Language v1 {Specification,Draft}.md`;
  - `tests/programs/m6/`, `tests/common/v1_programs.rs`, `tests/cli.rs`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 27)
- **Approved split:** `codegen/enums.rs` reached 326 lines, so construction (`constructed_variant`, `construct`, with its IR test) moved to a new `codegen/construct.rs`.
- **Gate:** with recursion gone, it no longer walks types at all; it only looks for `impl` and `self`.
- **Allocation size:** a heap copy is the payload type's whole layout size, e.g. 24 bytes for a `List` value (tag, padding, head, pointer). The IR test checks `lugha_rt_alloc(i64 24)`.
- **Spec:** the §4 Structs paragraph now points recursive structs at enums and `Option`. §6 pass 2 marks indirect slots. §7 gains "Indirect slots" and "Recursion depth".

### Discovery answers (session 27)
1. **Deep recursion is documented, not handled.**
   - The spec says recursion depth is bounded by the native stack, and that too-deep recursion crashes the program (signal, exit 139) instead of panicking. Very long lists should be walked with a loop.
   - TODO gains an idea for a `panic: stack overflow` later.
   - Acceptance tests build and walk long lists with loops.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**The indirect-slot rule** — `check/types.rs`, public so codegen applies the same rule (one source, like `contains_array`)
- `pub fn indirect_slot(owner: &Type, slot: &Type, structs, enums) -> bool`.
- **When it holds:** the enum or `Option` `owner` is reachable from the payload type `slot` by inline containment: struct fields, enum payloads, and `Option`'s payload. It is never reached through arrays, which are pointers.
- **Cycles:** a visited set guarantees termination on cyclic tables.
- **Examples:**
  - `Cons(i64, List)`: the `List` slot is indirect, the `i64` slot is not.
  - `struct Node { next: Option<Node> }`: `Some`'s `Node` slot in `Option<Node>` is indirect.
  - Mutually recursive enums mark each other's slots.

**Checker**
- No new rule.
  - E0307 already applies only to struct cycles through structs.
  - Cycles through an enum or `Option` are now valid.
- Every other enum rule from PRP-020 applies unchanged.

**Codegen** — `codegen/enums.rs`, `codegen/matching.rs`
- **Layout:**
  - An indirect slot occupies 8 bytes with alignment 8: a pointer.
  - `payload_area` and the variant struct use `ptr` for it, so `layout()` terminates on every recursive type. `List` is `{ i32, [2 x i64] }`.
- **Construction:** the value for an indirect slot is stored in a fresh `lugha_rt_alloc(size)` object (the payload type's own layout size), and the slot holds its pointer.
- **Reading:** a `match` binding to an indirect slot loads the pointer, then the value. Bindings stay borrowed: storing one copies the value, which is constant time because payloads are shared.
- **Copies:** none added; enum values keep sharing their payloads (MEMORY 25).
- **Option types:** `Option<T>` types are collected and declared as before. `Option<Node>` inside `Node` works because the slot is a pointer.

**Gate** — `driver/gate.rs`
- Remove the recursive-enum entry and its reachability helpers. Only methods (milestone 7) remain.

**Spec** — fold into `Language v1 Specification.md`
- **§4 Enums:** replace "An enum may not contain itself yet" with the recursion rules.
  - An enum may contain itself directly or through structs, enums or `Option`; no `Box` is needed (`Box<…>` is E0305 with help).
  - A struct may contain itself through an enum or `Option`.
  - E0307 applies to cycles through structs only.
- **§4 Structs:** the "needs a boxed or optional type, planned for v1" sentence now points to enums and `Option`.
- **§6:** pass 2 marks indirect slots.
- **§7:** describe indirect slots: an 8-byte pointer to a GC-heap copy, made when the variant is built, never visible to programs.
- **§5 or §7:** the stack-depth note (discovery answer 1).
- **§10:** the linked-list program (milestone 6), which prints `6`.
- **§11:** the milestone 6 row says "done".
- **Draft:** folding progress updated; recursion moved.
- `tests/common/v1_programs.rs` drops `LIST`.

**Docs:** CLAUDE.md (the known-issue line now names only methods), MEMORY, CHANGELOG, TODO (tick PRP-021; the stack-overflow idea), CONTEXT. Tag `m6` after merging.

### Must NOT Do
- No `Box`, `indirect` keyword or annotation of any kind.
- No reference counting, and no deep copy of enum values.
- No stack-overflow handling (discovery answer 1).
- No methods or growable arrays.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- No new diagnostics. A recursive type that can't have a finite layout can't be written: every cycle passes through a struct (E0307) or an enum slot, and enum slots on a cycle are indirect.
- Allocation failure already panics with "out of memory" (runtime).

## SECURITY CONSIDERATIONS

- Indirect slots point only at objects from `lugha_rt_alloc`, written once at construction and never mutated. Boehm keeps them alive while any enum value refers to them (the payload area is scanned conservatively).
- Deep recursion can exhaust the native stack, which crashes the process (documented). There is no memory-safety issue: the guard page faults.
- No `unsafe`.

## TESTS TO WRITE

Unit tests:
- [x] **`indirect_slot`:**
  - `List`'s `List` slot yes, `i64` no;
  - `Tree`'s two `Tree` slots;
  - `Node`'s `Option<Node>` slot via `Some`;
  - mutual recursion `A { B(B) }` / `B { A(A), End }`;
  - an array of the enum (`kids: List[]`) is not a cycle;
  - non-recursive enums have no indirect slot.
- [x] **Codegen IR:**
  - `%List = type { i32, [2 x i64] }`;
  - building `List.Cons(1, List.Nil)` calls `lugha_rt_alloc`;
  - building `List.Nil` doesn't.
- [x] **Gate:** recursive enums now pass; `impl` is still gated.

Acceptance:
- [x] **Spec §10:** the linked-list program prints `6` (from the spec, at `-O0`/`-O2`).
- [x] **`m6/recursive.la`:**
  - a 1 000 000-node list built and summed with loops;
  - a binary search tree with recursive insert and an in-order sum;
  - `struct Node { value, next: Option<Node> }` chained three deep and walked;
  - a copy of a list, with a new head consed onto the copy, leaving the original's length unchanged.

  Expected stdout written by hand.
- [x] `m6/recursive_gate.*` is removed. The CLI gate test uses an `impl` program (methods, milestone 7).
- [x] All earlier tests pass.

## ROLLBACK PLAN

- Branch `prp-021-recursive_enums`, merged into `main` after green CI; then tag `m6`.
- To abandon: delete the branch; the gate keeps recursive enums stopped.

## ACCEPTANCE CRITERIA
- [x] The linked-list program prints `6`; every test above passes.
- [x] The v1 spec covers recursion; the draft is updated; CLAUDE.md, MEMORY.md, TODO.md, CHANGELOG.md updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac run` on the linked-list program prints `6`
