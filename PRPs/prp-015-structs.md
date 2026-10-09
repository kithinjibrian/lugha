## FEATURE: Structs — declarations in any order, literals, field reads and writes, structs inside arrays and arrays inside structs, and value-semantics copies.

**Status:** approved 2026-10-09 — session 18
**Milestone:** 5, third of four PRPs. Done-when: the spec §10 centroid program prints `centroid: 2.0, 1.0`.
**Spec:** §3 (struct declarations and literals, no-struct-literal conditions), §4 (structs, copies table, deep copies, value semantics), §5 (field evaluation order), §6 (two-pass globals, shared namespace), §7 (stack layout, struct parameters and returns), §8 (no structs across C), §9 (lowering notes, copy helpers, codes)
**Decisions:** MEMORY 14 (codegen reads the checker's types), MEMORY 16 (array value semantics and copy sites), PRP-013 (integer address arithmetic, no `unsafe`)

## OBJECTIVE
Lugha programs can declare structs and use them as values:
- `struct Point { x: f64, y: f64 }`, declared before or after use;
- `Point { y: 2.0, x: 1.0 }`, `p.x`, `p.x = 3.0`, `pts[i].x += 1.0`, `w.data[0] = 5`;
- structs as parameters, returns, array elements and fields of other structs.

Structs behave as values (§4): `let q = p; q.x = 9.0;` never changes `p`, and the arrays inside a struct are deep-copied at the same copy sites as plain arrays. The §10 centroid program prints `centroid: 2.0, 1.0`.

## CONTEXT

- Starting state:
  - Struct declarations and literals stop the checker with "not implemented yet: structs (milestone 5)"; codegen has the same `Unsupported` stop.
  - The parser already handles the full §3 syntax, including no-struct-literal conditions (E0205).
  - Arrays, deep copies (`codegen/copy.rs`) and the E0507 guard's `Path` (which already has `Field` steps) are in place.
- Ending state:
  - `check::Type::Struct(String)`; `Checked` gains a `structs` table (fields in declaration order).
  - New `check/structs.rs` (collection, field resolution, E0307/E0308, literals E0413/E0414), plus field access and field places in `check/access.rs` and `check/assign.rs`.
  - New `codegen/structs.rs` (named LLVM types, layout, literals, field reads, field places); `copy.rs` learns structs; `function.rs` passes struct arguments by pointer.
  - New `m5/` programs; spec §4 clarification and §9 codes.
- Related existing code: `src/check/{types,env,mod,access,assign,array,expr,ops,errors}.rs`, `src/codegen/{value,heap,copy,array,function,stmt,expr,scope}.rs`.
- Open decisions that must be resolved first: none.

### Discovery answers (session 18)
1. Codes:
   - **E0307** a struct that contains itself, with the cycle in the label (`Node -> Pair -> Node`).
   - **E0308** a field declared twice in one struct.
   - **E0413** a literal missing fields, listing every missing one.
   - **E0414** a literal giving a field twice.
   - Reused: E0302 (duplicate struct name, shared namespace with functions and intrinsics), E0305 (a literal or annotation naming something that isn't a struct), E0410 (unknown field in `p.z` or in a literal), E0404 (`==` and other operators), E0408 (casts), E0409 (structs in `extern` signatures), E0501 (field assignment needs a `let mut` root), E0507 (assigning to an overlapping place while iterating).
2. Copy helpers name struct types **length-prefixed**: `Point` → `lugha_copy_5Point`, `Point[]` → `lugha_copy_5Point_arr`. Existing names (`lugha_copy_i64_arr_arr`) are unchanged.
3. Codegen holds structs as **SSA aggregates**: literals build with `insertvalue`, reads use `extractvalue`, field writes store through `build_struct_gep` (constant index). Struct arguments are spilled to an entry-block `alloca` and passed as a pointer.

### Needs approval with this PRP — spec clarification
The spec forbids a struct containing itself "directly or through other structs" because it would have infinite size. A struct reaching itself only **through an array** (`struct Node { value: i64, kids: Node[] }`) has a finite size, since the array is a pointer. This PRP **allows** it and adds a sentence to §4 saying so. Its copy helpers are mutually recursive (`lugha_copy_4Node` ↔ `lugha_copy_4Node_arr`), which the on-demand generation already supports.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Types**
- `check::Type::Struct(String)`, displayed as its name.
- `contains_array` must see through structs, so it takes the struct table: `ty.contains_array(&structs)`. For a struct, it is true if any field's type contains an array; the walk terminates because only arrays can close a cycle.
- `Checked { types, structs }`, where `structs: HashMap<String, Vec<(String, Type)>>` lists the fields in declaration order. Codegen reads layouts from it, never from the AST (MEMORY 14).

**Checker globals** — `check/env.rs`, `check/structs.rs` (spec §6)
- Pass 1 collects struct names alongside function signatures. A duplicate name is E0302 against any function, struct or intrinsic.
- Pass 2 resolves every field type:
  - unknown names → E0305;
  - a repeated field name → **E0308** "field `x` is declared twice in `Point`", with a label "first declared here".
- Then cycle detection over direct struct-typed fields (arrays break the edge): **E0307** "struct `Node` contains itself", primary span on the struct's name, label with the path `Node -> Pair -> Node`. Each cycle is reported once, at its first struct in source order.
- Annotations naming a struct resolve to `Type::Struct`; `Point[]` works through the existing recursion.
- `extern` parameters or returns that are or contain a struct → E0409 "structs can't cross the C boundary" (§8).

**Checker expressions** — `check/structs.rs`, `check/access.rs`
- **Literal `S { f: e, … }`:**
  - `S` must name a struct, otherwise E0305 "unknown type `S`".
  - Fields are checked in source order, each expecting its declared type.
  - Unknown field → E0410 "no field `z` on `Point`"; a repeated one → **E0414** "field `x` is given twice", with a label "first given here".
  - Missing fields → **E0413** "missing fields `y`, `z` in `Point`" (declaration order), on the whole literal.
  - The type is `Struct(S)`, even after reporting errors, so uses stay quiet.
- **`p.f`** on a struct → the field's type; an unknown field → E0410. `.len` on a struct is just an unknown field unless it declares one.
- `==`, `!=`, arithmetic and comparisons on structs → E0404; `as` → E0408.

**Checker assignment** — `check/assign.rs`
- Field places (`p.x`, `pts[i].x`, `w.data[0]`, `a.b.c`) follow the PRP-014 rules: the root must be `let mut` (E0501), a chain through a `string` is E0506, and `.len` of an array or string is still not a place (E0502).
- The E0507 guard already compares `Field` steps by name: `for p of pts { pts[0].x = 1.0; }` is E0507.

**Codegen layout** — `codegen/structs.rs` (spec §7)
- Each struct gets a named LLVM type `%S`, created opaque for every struct first and given its body second, so declaration order doesn't matter.
- Field storage types match array elements: `bool` is `i8` (zero-extended into the aggregate, truncated on `extractvalue`); arrays and strings are `ptr`; nested structs are inline.
- `element_size` for a struct is its size computed by hand with natural alignment (fields at aligned offsets, the total rounded up to the largest alignment), stated in the code as the 64-bit layout LLVM uses for this target. Arrays of structs store them inline at `8 + i * size`; the alignment is at most 8, so the header keeps elements aligned.

**Codegen operations** — `codegen/structs.rs`, `stmt.rs`, `function.rs`
- **Literal:** evaluates fields in source order (§5), each through `value_for_store` (a place is copied, §4), then builds the aggregate in declaration order with `insertvalue`.
- **`p.f` read:** lowers `p` and uses `extractvalue`.
- **Field place:** a place's address is computed by walking the chain once:
  - `Name` → its slot;
  - `.f` → `build_struct_gep` on the base's address;
  - `[i]` → the base evaluated as an array value, then the existing bounds-checked `element_place`.

  Then the value is evaluated and stored; compound assignment loads, applies the checked operator at `op_span` and stores once (§5).
- **Parameters:** a struct parameter's LLVM type is `ptr`. The callee uses that pointer as the parameter's slot (no copy; parameters are immutable) and marks it borrowed.
- **Arguments:** each struct argument is evaluated, stored to an entry-block temporary, and its pointer is passed (§7, §9). It is never deep-copied.
- **Returns:** structs return by value as `%S`.
- `for p of pts` over a struct array loads each element into the loop slot, as for other element types.

**Copies** — `codegen/copy.rs` (spec §4, §9)
- `deep_copy` handles structs that contain arrays: an internal `lugha_copy_<mangled>(%S) -> %S` that deep-copies each array-holding field and reinserts it. Structs without arrays are already copied by the store itself.
- Mangling: struct `S` → `<len(S)>S`; arrays append `_arr` as today (`lugha_copy_5Point_arr`, `lugha_copy_4Wrap`).
- Copy sites are unchanged from PRP-014. "Contains arrays" now sees through structs, and struct-literal fields are a copy site (`Wrap { data: xs }` copies `xs`).

**Spec and docs**
- §4: a struct may contain itself through an array (pending approval above).
- §9: code table gains E0307, E0308, E0413, E0414 and E0409's struct case; the copy-helper note gives the length-prefixed struct mangling.
- CLAUDE.md file tree: `check/structs.rs`, `codegen/structs.rs`. If `CodegenError::Unsupported` has no producers left, remove the "outermost unsupported construct" known issue.

**Acceptance** — `tests/programs/m5/`, also cross-checked at `-O2`:
- `centroid.la`: the spec §10 program → `centroid: 2.0, 1.0`.
- `struct_basics.la`:
  - structs declared after use, literals with fields in any order;
  - fields of every primitive type including `bool` and `u8`;
  - a nested struct; `p.x = v`; `pts[i].x += 1.0`; `w.data[0] = 5`;
  - a struct parameter and a struct return;
  - `for p of pts` summing.
- `struct_copies.la`:
  - `let q = p; q.x = …` leaves `p` unchanged;
  - a struct holding an array, copied and then mutated;
  - `Wrap { data: xs }` then mutating `xs`;
  - a function returning its struct parameter, mutated by the caller;
  - an array of structs with arrays inside, copied;
  - a `Node { kids: Node[] }` tree copied deeply.

  Each prints the original unchanged; expected stdout is written by hand.
- Reject mode: `e0307.la`, `e0308.la`, `e0413.la`, `e0414.la`, `e0501_field.la`. Goldens captured and reviewed.

### Must NOT Do
- No struct `==`, methods, generics, struct printing or `to_string` of structs (§11 v1 or not in v0).
- No `lughac spec` (PRP-016).
- No copy-on-write (DECISION-010 stays deferred).
- No `unsafe`; field addresses use `build_struct_gep` (constant indexes), array elements the existing integer arithmetic.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- Checker rules report and keep going; a struct with errors still has a type, and `Type::Error` suppresses cascades.
- A recursive struct stops nothing else: its fields resolve, and later uses still check. Codegen never sees one, since checker errors stop the pipeline.
- The IR verifies on every test; a hand-computed layout that disagreed with LLVM's would show up as wrong field values at `-O0` vs `-O2`, which the acceptance cross-check catches.

## SECURITY CONSIDERATIONS

- Field access uses constant-index struct GEPs, so it can't go out of bounds. Array element access keeps its unsigned bounds check.
- Copy helpers read only fields and `len` elements of objects they were given.
- No `unsafe`.

## TESTS TO WRITE

Unit tests:
- [ ] Checker:
  - structs declared after use; E0302 for a struct named like a function;
  - E0305 for an unknown field type and for a literal of a non-struct;
  - E0308; E0307 direct (`A { a: A }`) and indirect (`A -> B -> A`) with the path label; no error for `Node { kids: Node[] }`;
  - literals: field types, any order, E0410, E0413 (all missing listed), E0414;
  - `p.x` type; E0410 on `p.z`; E0404 for `p == q`; E0409 for a struct extern parameter;
  - E0501 on `p.x = 1.0` with an immutable root; E0507 for `pts[0].x` inside `for p of pts`.
- [ ] Codegen IR:
  - `%Point = type { double, double }`;
  - a `bool` field is `i8`;
  - a struct parameter is `ptr` and the argument is an `alloca` pointer;
  - field writes use a struct GEP with a constant index;
  - `lugha_copy_4Wrap` exists and is called on `let w2 = w;`, but not for a struct without arrays;
  - the `Node`/`Node[]` copy helpers are generated once each.
- [ ] Layout: hand-computed sizes for `{ u8, i64 }` (16), `{ i32, u8 }` (8) and a nested struct.

Acceptance:
- [ ] The three run programs and five reject programs pass; all earlier tests pass.

## ROLLBACK PLAN

- Branch `prp-015-structs`, merged into `main` on acceptance.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] `lughac run tests/programs/m5/centroid.la` prints `centroid: 2.0, 1.0`.
- [ ] Every test above exists and passes.
- [ ] Spec §4 and §9, CLAUDE.md, CHANGELOG.md, TODO.md and MEMORY.md updated.
- [ ] No file over 300 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `cargo run -q -- run tests/programs/m5/centroid.la` → `centroid: 2.0, 1.0`
- `grep -rn "unsafe" src/` → nothing
