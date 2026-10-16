## FEATURE: C structs by value — `extern struct Name { … }` has C's layout and is passed to and from C (externs and callbacks) the way C does on x86-64 Linux; milestone 90.

**Status:** approved 2026-10-16 ("yes build all three", session 190); proposed in session 189 with PRP-119 and PRP-121 as one batch.
**Milestone:** 90 (tag `m90` at merge).
**Spec:** `Language v16 Draft.md` decisions 120–125; spec §3 (grammar), §4 (structs), §8 (allowed types, the calling convention), §9 (lowering, diagnostics), §11 (row 90).

**Decisions (discovery, session 189; the user's choices):**
- an explicit `extern struct`, not any struct with plain fields: the layout is a promise, so it is declared.
- Decided by me, for review: fields are numbers, `bool`, handles, `Option` of a handle and other extern structs (no strings, arrays, enums, functions, generics); passed by value in extern parameters and results and in callback parameters and results; x86-64 System V only (lughac's one platform, spec §9); pointers to structs and arrays of structs are later work.
- No open entries in DECISIONS.md apply.

**Why:** raylib's whole API takes `Color`, `Vector2`, `Rectangle`, `Camera2D` by value; without this, every call needs a C shim.

## OBJECTIVE
```
extern struct Color { r: u8, g: u8, b: u8, a: u8 }
extern struct Vector2 { x: f32, y: f32 }
extern struct Camera2D { offset: Vector2, target: Vector2, rotation: f32, zoom: f32 }

extern fun DrawCircleV(center: Vector2, radius: f32, color: Color);
extern fun GetMousePosition(): Vector2;
extern fun BeginMode2D(camera: Camera2D);

let red = Color { r: 230, g: 41, b: 55, a: 255 };
DrawCircleV(GetMousePosition(), 20.0, red);
```

## CONTEXT
- **Starting state:** structs (`check/structs.rs`, `codegen/structs.rs`) are LLVM named structs with natural alignment (a `bool` field as `i8`), passed between Lugha functions as a pointer to the caller's copy (spec §9); in externs a struct is E0409. LLVM does no C ABI lowering itself: a frontend must split or indirect aggregates as clang does.
- **Ending state:**
  - **Syntax:** `[visibility] extern struct Name { field: T, … }` (no type parameters); AST `StructDecl` gains `extern_: bool` (or a span); parser, formatter, s-expression printer, language server (hover shows `extern struct Name`).
  - **Checker:** an extern struct is a struct in every other way (literals, fields, `==`, methods, printing). Its fields must be extern-struct-able (E0437, new, at the field type: "an `extern struct` holds numbers, `bool`, handles and other extern structs"); a plain struct in an extern signature stays E0409 with help "declare it `extern struct`". Extern structs allowed by value in extern parameters and results and in callback parameters and results.
  - **Codegen** (`codegen/c_abi.rs`, new): the x86-64 System V classification of an extern struct — size over 16 bytes → memory (an argument passed `byval` with its alignment; a result through a hidden `sret` pointer); otherwise each eightbyte is INTEGER or SSE by its fields, and the struct travels as clang lowers it (`{u8 × 4}` → `i32`, `{f32, f32}` → `<2 x float>`, `{f32 × 4}` → `{ <2 x float>, <2 x float> }`, `{i64, f64}` → `{ i64, double }`, `{f32, i32}` → `i64`, `{f64, f64}` → `{ double, double }`). Declarations use the lowered types; calls store the value to a slot and load it in the lowered shape (and back for results); callback entries do the reverse.
  - **Spec v16:** §3 grammar; §4 (extern structs); §8 (allowed types, "Passing structs" with the classification table and examples); §9 (lowering, E0437 row, E0409 row); §11 row 90; changes list. **Draft:** decisions 120–125, row 90.
  - **`docs/errors/E0437.md`**; E0409's page mentions `extern struct`.
- **Related existing code:** `src/ast/mod.rs`, `src/parser/item.rs`, `src/check/{structs,env,callbacks}.rs`, `src/codegen/{structs,declare,function,externs,callbacks}.rs`, `src/formatter/items.rs`, `src/driver/lsp/hover.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first; the classification is unit-tested against the clang lowering of each shape (cases listed above, nested structs, a `bool` field, a 16-byte and a 17-byte struct, a handle field).
- New code in new files; `codegen/callbacks.rs` and `codegen/declare.rs` stay under 500 lines (split by proposal if not).
### Must NOT Do
- No other ABI (AArch64, Windows): on another target lughac reports an internal error naming the target rather than guessing.
- No pointers to structs, no arrays of structs to C, no packed structs, no unions, no bit-fields.
- No change to how plain structs are laid out or passed between Lugha functions; byte-identical IR for programs without extern structs.
### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- **E0437** (new): a field of an `extern struct` that C can't hold; generic parameters on an `extern struct` (or E0201 in the parser — decided while building, recorded in BUILT notes).
- **E0409**: a plain struct by value in an extern (help names `extern struct`).

## SECURITY CONSIDERATIONS
- A wrong `extern struct` (fields not matching C's) is undefined behavior, as any wrong extern declaration (spec §7 "Safety"). No `unsafe` in lughac.

## TESTS TO WRITE
- [ ] Parser and formatter: `extern struct`, `public extern struct`; generics rejected.
- [ ] Checker: allowed fields, every E0437 case, a plain struct in an extern still E0409, extern structs in callbacks.
- [ ] `codegen/c_abi.rs` unit tests: the classification table above.
- [ ] `tests/extern_structs.rs` (C files through `[link]`): every shape as argument and result, a C function adding the fields and echoing the struct back; nested; a 24-byte `Camera2D`-like struct both ways; a callback receiving and returning structs; mixed with other arguments (structs after six integer arguments go to the stack); -O0 and -O2.
- [ ] IR unchanged for the compiling `tests/programs`.

## ROLLBACK PLAN
- Branch `prp-120-extern_structs` in worktree `../lugha-prp-120`.

## ACCEPTANCE CRITERIA
- [ ] Every shape round-trips through C with the C side computing the same values.
- [ ] All tests pass; fmt and clippy clean; every `pub` item documented; CHANGELOG updated.

## VALIDATION
- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`
