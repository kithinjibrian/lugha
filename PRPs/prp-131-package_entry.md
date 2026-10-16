## FEATURE: Package entry modules — `import kivuli;` imports a package's `lib.la`; milestone 99 (v17).

**Status:** proposed in session 227 (the user: "should libraries have an entry like lib.la or even declared in manifest" → recommended a fixed `lib.la`, no manifest key; "yes write it up").

## OBJECTIVE
A library exposes one front door. `import kivuli;` binds the module in `src/lib.la` (the package's `source` folder, or its folder without one) as `kivuli`, so an app writes `kivuli.column([...])` or `import kivuli { text, column };` instead of choosing among the package's modules. `lughac init --lib` starts libraries this way. Packages without a `lib.la` work exactly as today.

## CONTEXT
- Starting state: a package is imported a module at a time (`import kivuli.ui;`); `import kivuli;` is E0312 ("`kivuli` is a package, not a module", `modules/packages.rs`). `lughac init --lib` writes `src/N.la` and an example importing `N.N`. TODO (v16 §12 list) holds "a package entry module (`import glowworm;`)".
- Ending state: the loader resolves a one-segment package import to `<module root>/lib.la` as the module `N`; spec v17 §6 (packages), §9 (`init`), draft decisions 51–56 and row 99; `init` writes `src/lib.la`.
- Related existing code: `src/modules/{packages,resolve,layout,mod}.rs` (resolution, E0312, E0328, `module_path`), `src/driver/init.rs`, `src/driver/test_json.rs` (`module_name`), `src/check/named_imports.rs`, `src/check/reexports.rs` (unchanged, but exercised).
- Open decisions: none (see decisions below, for review).

## DECISIONS (proposed; draft decisions 51–56)
51. **The entry is `lib.la`** at the package's module root (its `source` folder, or its folder when there is no `source`). No manifest key: one place to look, nothing to validate, and renaming the package moves nothing.
52. **`import N;`** binds the entry module as `N` (or `import N as k;`, `import N { X, y };`, `public import N;`); its module path is `N`, so its items are `N.f` and symbols `lugha_fn_N/f`. Without a `lib.la`, `import N;` stays E0312, its help now saying to add `lib.la` or import a module (`import N.<module>;`).
53. **One name:** the entry is reached only as `N`. `import N.lib;` is E0312 with help "the entry is imported as `import N;`". Modules in a `lib/` folder (`N.lib.x`) are unaffected.
54. **Everything else is unchanged:** `import N.module;` still reaches every other module, with the same visibility; the entry is an ordinary module (re-exports, `private`, package-level items, module docs, hover, references and rename all apply).
55. **Where it shows:** the implicit package (a library's own `examples/`, `test/`) can `import N;`; `lughac test src/lib.la` and `check` treat it as the module `N`, tests named `N.name`; std gets no entry (`import std;` stays E0312).
56. **`lughac init --lib`** writes `src/lib.la` (the `greet` function) and `examples/hello.la` with `import N;` and `N.greet("world")`. Existing libraries made by older `init` (`src/N.la`) keep working as `N.N`.
57. **Success:** milestone 99, tag `m99`. A package with `lib.la` imported as `N` from a dependent program, its own example and its tests; kivuli can rename `ui.la` to `lib.la`.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- `Packages::resolve`: a one-segment import of a listed package (or the implicit package) whose module root holds `lib.la` → `Target { path: [N], file: root/lib.la, package: Some(N) }`; else E0312 with the new help.
- `import N.lib;` → E0312 (decision 53).
- `layout::module_path`: `src/lib.la` → `[N]` (and `test_json::module_name` through it).
- `init --lib` layout (decision 56); `tests/init.rs` updated.
- Spec v17 §6 (packages: the entry), §9 (`init` table, `test` names), changes list, row 99; draft decisions and row; E0312's page (`docs/errors/E0312.md`) mentions the entry.

### Must NOT Do
- A manifest key for the entry, or any other file name (decision 51).
- An entry for `std`, or for a program's main folder without a package.
- Change what `import N.module;` means.

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- E0312 (existing code) for `import N;` without `lib.la` and for `import N.lib;`, each with its own help. No new codes.

## SECURITY CONSIDERATIONS
- None: resolution reads one more file name inside the package's own module root, through the same `read` and path checks as every module.

## TESTS TO WRITE
- [ ] `modules/tests.rs` (in-memory): `import geo;` loads `geo/lib.la` as `[geo]`; with `source = "src"`, `geo/src/lib.la`; without `lib.la` E0312 with the new help; `import geo.lib;` E0312; `import geo.lib.x;` (a `lib/` folder) still loads.
- [ ] `tests/package_layout.rs`: a dependent program `import glowworm;` and `import glowworm { zeros };` runs; the library's own `examples/` uses `import glowworm;`; `public import` in `lib.la` re-exports a sibling module's items; `lughac test src/lib.la` names tests `glowworm.name`.
- [ ] `tests/init.rs`: `init --lib` writes `src/lib.la`, the example imports `N`, `run examples/hello.la` prints the greeting.
- [ ] `tests/lsp_definition.rs` or `lsp_hover.rs`: hovering `kivuli` in `import kivuli;` shows `module kivuli` and `lib.la`'s module doc; Go to Definition opens `lib.la`.
- [ ] `tests/explain.rs`: E0312's page examples still report E0312.

## ROLLBACK PLAN
- Branch to return to: `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] `import N;` works from a dependent program, the package's own examples and tests.
- [ ] Packages without `lib.la` unchanged; `--test programs` passes.
- [ ] Targeted tests (`--lib`, `package_layout`, `modules`, `init`, `lsp_hover`, `lsp_definition`, `explain`, `programs`), fmt, clippy pass; CHANGELOG updated.

## VALIDATION
- `cargo test --lib`, `cargo test --test package_layout --test modules --test init --test lsp_hover --test lsp_definition --test explain --test programs`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`
