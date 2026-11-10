## FEATURE: Names across modules — `m.f` / `m.T`, `private` (E0313), `public` (E0317), no leaking (E0318), and module symbols — completes milestone 10.

**Status:** merged into `main` 2026-10-13 — session 42 (tag `m10`)
**Milestone:** 10 (second of two PRPs: 033 front end, **034 names, visibility and symbols**). Milestone 10 is done when the draft's two-module program passes.
**Spec:** `Language v2 Draft.md`:
- decision 9;
- §5 (qualified names, private items);
- §6 (names, visibility, no leaking, `main`);
- §8 (externs);
- §9 (symbols, codes);
- §10 (two-module program).

`docs/decisions/visibility.md`. These fold into `Language v2 Specification.md`.
**Decisions:**
- MEMORY 37 (visibility), MEMORY 38 (module loading, one offset space);
- MEMORY 34 (gate);
- architecture rules 4 (codegen never infers types) and 7 (symbol prefixes).

## OBJECTIVE
A program can span several files and actually run.
- **Using another module:**
  - its function: `geometry.origin()`;
  - its type: `geometry.Vec2` in annotations, `geometry.Vec2 { … }`, `shapes.Shape.Circle(1.0)`, `geometry.Vec2.new()`.
- **Within a module:** its own items are used unqualified.
- **Visibility, everywhere:**
  - `private` items, methods and fields are usable only in their own module (E0313);
  - writing `public` is E0317 until v3 packages exist;
  - a non-`private` signature can't expose a `private` type (E0318).
- **Codegen:** every module's code lands in one executable under module-qualified symbols.

## CONTEXT

- **Starting state (after PRP-033):**
  - All modules load and parse into one offset space.
  - The checker and codegen see only the main file's `Program`, and the gate stops `import`, `private`/`public` and qualified names.
  - Each module numbers its `ExprId`s from 0.
  - Codegen resolves annotation types (`annotation_type`) and callee names by bare name itself.
- **Ending state:**
  - **Checking:** all modules are checked together, items are keyed by qualified names, and visibility is enforced.
  - **Side tables:** the checker records what each annotation and each call resolved to; codegen reads those and stops resolving names.
  - **Codegen:** all modules are lowered into one LLVM module.
  - **Gate:** the M10 entries are gone.
  - **Spec:** milestone 10 is folded in.
- **Related existing code:**
  - `src/check/` (`env`, `call`, `structs`, `enums`, `methods`, `method_calls`, `access`, `assign`, `errors/names`);
  - `src/codegen/` (`function`, `methods`, `structs`, `enums`, `value`, `construct`, `lower`, `mod`);
  - `src/modules/`, `src/parser/mod.rs`;
  - `src/driver/{pipeline,gate}`;
  - `tests/common/spec_programs.rs`, `tests/spec.rs`.
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 42)
- **Associated calls:** `T.f()` and `m.T.f()` record the method key (`geometry.Vec2.new`) in `Checked.callees`, so codegen finds the method without resolving the owner itself.
- **Call dispatch:** codegen checks the callee table first. Without that, `m.f()` (whose base is a non-value, like an enum's) would be mistaken for a variant constructor.
- **`impl` headers:** `impl m.T` is E0305, worded like an unknown type. An `impl` must be in its type's module.
- **E0311 wording:** "`x` is the name of an imported module" when the clash is with a module.
- **Approved splits:**
  - `check/modules.rs` (442 lines) became `check/modules/{mod,lookup}.rs`;
  - struct literals moved to `check/struct_literal.rs`;
  - function declaration and the C ABI moved to `codegen/declare.rs`.

  A script confirmed the moved code is unchanged, apart from visibility qualifiers and one signature rustfmt rewrapped.
- **Tests:**
  - **Where they went:** the codegen symbol tests are in `tests/modules.rs` (`--emit=ir`), because `codegen/function.rs` was at the limit.
  - **Test order:** the checker and visibility tests were written first and failed first. The IR test, acceptance programs and spec test were written after the code.
- **§10 example sentence:** it uses the standard "Prints …" form so the extractor treats it as a run program.

### Discovery answers (session 42)
1. **Symbols:** the module path's segments and the item are joined with `/`, and methods keep `.`.
   - `lugha_fn_a/b/c` is function `c` of module `a.b`; `lugha_fn_a/b.c` is method `c` of struct `b` in module `a`; `lugha_fn_shapes/circle/Circle.area` is a method in a nested module.
   - The main file's symbols are unchanged (`lugha_fn_main`, `lugha_fn_Point.dist`).
   - This replaces the draft's `lugha_fn_a.b.f`, which could collide.
2. **Type names in messages:** a type from another module is shown with its full module path (`shapes.circle.Circle`). A main-file type keeps its bare name, so every existing message is unchanged.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**One id space**
- The parser takes a first `ExprId` (`parser::parse_from(tokens, first)`), and `parse(tokens)` stays a wrapper starting at 0.
- The loader parses each module starting where the previous one ended, so ids are unique across the program. The last module's `expr_count` sizes the checker's table.

**Qualified keys** (the checker's internal names, also `Type::Struct`/`Type::Enum` and LLVM type names)
- **Rule:** an item `X` of module `a.b` is keyed `a.b.X`; a main-file item is keyed `X`. Names can't contain `.`, so a key has exactly one reading.
- **Shared helpers in `check`:**
  - `qualify(path, name)`;
  - `symbol(key)`, which turns `.` into `/` for the module part, giving `a/b/X`.
- **Messages:** they print keys, which is discovery answer 2.

**Checker** — new `check/modules.rs` (the current module, imports, qualified lookup) and `check/visibility.rs` (E0313, E0317, E0318)
- **API:** `check_modules(&[Unit { path, program }])` checks the whole program. The existing `check(&Program)` is a one-module wrapper, so every existing unit test is unchanged.
- **Pass 1 (draft §6), over all modules:** structs, enums, functions, externs and methods are collected under qualified keys.
  - **Duplicates:** E0302, within a module only.
  - **Reserved names:** intrinsics and built-in names stay reserved in every module.
  - **`main`:** it must be in the main file. A `main` in another module is E0304 with "`main` belongs in the main file".
  - **Externs:** two modules declaring the same C function with different signatures is E0306, extended.
- **Lookups:** each module checks its bodies with its own imports (the import's last segment → module path).
  - **Unqualified names:** locals, then the module's own items (`qualify(current, name)`), then intrinsics and built-ins.
  - **`m.f(…)`:** a call of function `f` in the module imported as `m`, when `m` isn't a local.
  - **`m.T`:** a type in annotations; `m.T { … }` a struct literal; `m.T.f(…)` an associated function; `m.T.V(…)` / `m.T.V` a variant.
  - **Unknown names:** an unknown `m` is E0301, an unknown `f` or `T` in a known module is E0301 / E0305, each naming the module.
  - **E0311:** a local named like an imported module (extended).
- **Side tables in `Checked`** (codegen reads these instead of resolving names, architecture rule 4):
  - `callees: HashMap<ExprId, String>`, the qualified key of each user function or extern called;
  - `annotations: HashMap<Span, Type>`, the resolved type of every written type, keyed by span. Spans are unique in the one offset space.
- **Visibility** (`check/visibility.rs`):
  - **E0313** "`X` is private to module `m`", with help naming the fix (for example "use `range.Range.new(…)`" for a literal). It covers using another module's:
    - `private` function, extern, struct, enum or associated function;
    - `private` method, in a method call;
    - `private` field, when read, assigned (including through `swap` and `mut self` receivers), or named in a struct literal.

    A struct literal outside the module needs every field visible.
  - **E0317** "`public` needs packages (v3)", at every `public` keyword, with help "remove it: everything is visible to the whole program".
  - **E0318:** a non-`private` declaration whose signature mentions a `private` type, looking inside arrays and `Option`. "Signature" means:
    - a function's parameters and return type;
    - an extern's parameters and return type;
    - a method's parameters and return type;
    - a non-`private` field of a non-`private` struct;
    - a payload of a non-`private` enum.

    It is reported at the type, with a label at the `private` declaration.

**Codegen**
- **API:** `emit_ir` / `emit_object` take all modules (`&[Unit]`). The single-`Program` forms stay for tests.
- **Declarations:** structs, enums, functions and methods are declared from every module under their keys.
  - **Symbols:** `lugha_fn_<symbol(key)>`, with methods `lugha_fn_<symbol(type key)>.<method>`.
  - **Externs:** they keep their C name and are declared once.
- **Name resolution goes away:**
  - calls use `Checked.callees`;
  - `annotation_type` reads `Checked.annotations`;
  - `self.functions` is keyed by qualified key.
- **Unchanged:** the C `main` still calls `lugha_fn_main`. Copy and equality helpers already use length-prefixed type names, which stay unique with keys.

**Driver**
- The pipeline passes every module to the checker and codegen.
- Panic locations: `SourceInfo` gives codegen the file name and text for an offset in any module, so a panic in `geometry.la` says `geometry.la:3:5`.

**Gate**
- The `modules` and `visibility` entries, their walker hits and their tests are deleted. Only `generics` (milestone 11) remains.

**Spec**
- **`Language v2 Specification.md`:**
  - the title becomes "in progress: milestone 11";
  - **"Changes since v1.2"** gains modules and visibility;
  - **§2:** the keywords;
  - **§3:** the grammar for `import`, `visibility` and `field`;
  - **§5:** qualified names, private items and fields;
  - **§6:** modules, names, visibility, no leaking, `main`;
  - **§8:** externs;
  - **§9:** the symbols table with the `/` form, and E0313, E0317, E0318 and the extended codes;
  - **§10:** the two-module program, written as one paragraph with two code blocks whose first lines are `// geometry.la` and `// main.la`;
  - **§11:** milestone 10 done.
- **`Language v2 Draft.md`:** §9's symbol rows are updated to the `/` form (discovery answer 1).
- **Spec tests** (`tests/common/spec_programs.rs`): a §10 program whose code blocks start with `// <file>.la` is written as files to a temp directory and run from `main.la`. `tests/spec.rs` checks the two-module program's title.

**Docs**
- CLAUDE.md:
  - architecture rule 7 (the `/` module symbols);
  - the tree (`check/modules.rs`, `check/visibility.rs`);
  - the KNOWN ISSUES gate line (generics only).
- README (modules).
- TODO (milestone 10 done; tag `m10` after merging).
- CHANGELOG, MEMORY decision 39, CONTEXT.

### Must NOT Do
- No packages, `public` semantics, re-exports, selective imports or interfaces (v3).
- No generics (milestone 11).
- No change to single-file programs' behaviour, messages or symbols.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- **Diagnostics:** E0301, E0302, E0304, E0305, E0306, E0311, E0313, E0317 and E0318 are records, and checking continues.
- **One mistake, one diagnostic:** a private item reported once as E0313 isn't also reported as unknown.

## SECURITY CONSIDERATIONS

- **No new input surface:** modules were already loaded and bounded in PRP-033.
- **Symbols:** they are built only from identifiers and `/`/`.`, so they can't collide with `lugha_rt_`/`lugha_copy_`/`lugha_eq_` or with externs (externs can't start with `lugha_`, E0306).
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests — `check/modules.rs` (several in-memory modules through `check_modules`):
- [x] **Using another module:**
  - `geometry.origin()`, `geometry.Vec2` in a parameter and in a return type;
  - `geometry.Vec2 { … }`, `shapes.Shape.Circle(1.0)` and `geometry.Vec2.new()`;
  - a method call on a value from another module.
- [x] **Within a module:** its own items are used unqualified, and two modules may each declare `Point` with no clash.
- [x] **Errors:**
  - E0301 for an unknown module or function;
  - E0305 for an unknown type in a known module;
  - E0311 for a local named like a module;
  - E0304 for `main` in a module;
  - E0306 for one extern with two signatures.
- [x] **Messages:** a type mismatch between modules prints full paths (`geometry.Vec2`).
- [x] **Single-file programs:** unchanged; all existing checker tests pass as they are.

Unit tests — `check/visibility.rs`:
- [x] **E0313** for another module's:
  - private function, struct and associated function;
  - private method;
  - private field read, field assignment and `swap` of a field;
  - struct literal with a private field.

  Each is allowed inside its own module.
- [x] **E0317** on items, methods and fields.
- [x] **E0318:**
  - a function returning a private struct;
  - a public field of a private type;
  - an enum payload of a private type.

  None for a `private` function using a private type.

Codegen IR:
- [x] **Symbols:**
  - `define … @"lugha_fn_geometry/origin"`;
  - a method `@"lugha_fn_shapes/circle/Circle.area"`;
  - the main file's `@lugha_fn_main` unchanged;
  - an extern declared in two modules declared once.
- [x] **Resolution:** calls use the checker's callee table, so two modules' `f` call different symbols.

Acceptance — `tests/programs/m10/` (cross-checked at `-O0`/`-O2`; expected output written by hand):
- [x] **`two_modules/`:** the draft program, which prints `3.5`.
- [x] **`modules/`:** three files covering:
  - a nested module (`shapes/circle.la`);
  - a module cycle;
  - a `private` helper used inside its module;
  - a `Range` with private fields built through `Range.new`;
  - two modules each with a `Point`;
  - an enum from another module matched in `main`;
  - a panic location inside a module.
- [x] **Goldens:** `e0313_field/` and `e0318/`, plus `e0317.la` (single file). Captured and reviewed.
- [x] **Spec:** the spec's §10 two-module program runs from `lughac spec`'s text.
- [x] **Regression:** every earlier test passes. The gate's M10 tests are removed, and `tests/modules.rs`'s gate test becomes a run test.

## ROLLBACK PLAN

- Branch `prp-034-module_names`, merged into `main` after green CI; then tag `m10`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The two-module program passes from the spec's §10, and milestone 10 is done.
- [x] Qualified names, visibility (E0313, E0317, E0318) and module symbols behave as above; single-file programs are unchanged.
- [x] The spec and draft are updated; CLAUDE.md, README, TODO, CHANGELOG, MEMORY updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac run tests/programs/m10/two_modules/main.la` prints `3.5`
