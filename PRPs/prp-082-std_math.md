## FEATURE: The std library and `std.math` — bundled `std.<module>` modules, `std` reserved, the `Float` bound, and `std.math`; milestone 53.

**Status:** approved 2026-10-14 — session 111, with the milestone 53 additions to `Language v15 Draft.md`, "in one go".
**Milestone:** 53 (one PRP; tag `m53` at merge)
**Spec:** `Language v15 Draft.md` (decisions 14–18, the milestone 53 choices, §10 M53, §11 row 53).

**Decisions:** MEMORY 98 (v15), MEMORY 99 (package layout), MEMORY 100–101 (operators, `Num`), session 109 (std direction, no opt-out).

## OBJECTIVE
```
import std.math;

fun hypot<T: Float>(a: T, b: T): T = math.sqrt(a * a + b * b);

fun main() {
    println(math.sqrt(2.0));             // 1.4142135623730951
    println(hypot(3.0, 4.0));            // 5.0
    println(math.max(2, 7));             // 7
    println(math.abs(-3));               // 3
    println(math.round(2.5));            // 3.0
    println(math.pi());                  // 3.141592653589793
}
```

## CONTEXT

- **Starting state:**
  - built-ins are intrinsics or the prelude;
  - there are no bundled modules;
  - generic code can't use float math.
- **Ending state:** decisions 14–18, implemented.
- **Related existing code:**
  - **Loader:** `src/modules/{mod,packages,resolve,parse,layout}.rs` (the embedded prelude is `with_prelude`).
  - **Lexer:** `src/lexer/mod.rs` (identifiers).
  - **Checker:**
    - `src/check/{call,operators,bounds,literal,iterators}.rs`;
    - the operator bounds in `src/prelude/ops.la`.
  - **Codegen:** `src/codegen/{function,arith}.rs` (LLVM intrinsics through `Intrinsic::find`).
  - **Driver:** `src/driver/source.rs` (file names in diagnostics).
  - **Docs:** `src/lib.rs` (`PRELUDE`), the spec.
- **Open decisions that must be resolved first:** none.

### Design
- **Sources:**
  - **Files:** `src/std/math.la`, embedded as `lugha::STD`, a table of `(module, source)`, with one entry for now.
  - **Loader:** an import whose first segment is `std` (in any module of any package) resolves to that table. The module path is `std.math`, and the package is a hidden `std` package whose items are visible everywhere they are `public`.
  - **Files and offsets:** the source joins the shared offset space like a file, named `<std>/math.la` in diagnostics. It is read once, like any module.
  - **Errors:** a missing module is E0312, with help listing the std modules.
- **Reserved name:**
  - a manifest entry named `std` is E0321 (`manifest.rs`);
  - a folder `std/` or file `std.la` beside the main file is E0320, through the existing clash check, applied to `std` as if listed.
- **`$` built-ins:**
  - the lexer accepts an identifier starting with `$` (`$float`) only when lexing a bundled source (the prelude or std), passed as a flag. In a program, `$` stays E0101;
  - a call `$float.sqrt(x)` (and `pow`, `exp`, `ln`, `log10`, `sin`, `cos`, `tan`, `atan2`, `floor`, `ceil`, `round`) is checked in a new `check/float_ops.rs`: its arguments must be one `T` with `T: Float`, or `f32`/`f64`, and the result is that type.
- **`Float`:**
  - **Where:** declared in `src/prelude/ops.la` as an interface with `Num`'s items, and named in `is_operator_bound`.
  - **What satisfies it:** `builtin_operator` lets `f32`/`f64` satisfy it, and nothing else (E0419 with its own help).
  - **What it allows:** `has_num` also counts `Float`, so `T: Float` allows the operators, `==`, `<` and `0`/`1`.
- **`std.math`** is written in Lugha: the float functions wrap `$float.*`, `abs`/`min`/`max`/`clamp` use `Num`/`Ord`, and `pi()`/`e()` return literals.
- **Codegen** (new `codegen/float_ops.rs`): `$float.op(args)` at a concrete `f32`/`f64` lowers to `llvm.<op>.<f32|f64>`:
  - `ln` is `llvm.log`;
  - `round` is `llvm.round` (halves away from zero);
  - `atan2` is `llvm.atan2`;
  - `tan` is `llvm.tan`.

  The math library (`-lm`) is already linked.
- **Programs that don't import std** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - the changes summary;
  - §2 (`$` names in bundled sources only);
  - §4 (`Float`);
  - §6 (the std library: loading, reserved name, no opt-out);
  - a new "Standard library" section listing `std.math`'s functions and their behaviour (NaN/inf, `round`, `abs` overflow, `clamp` panic);
  - §9 (embedding, symbols `lugha_fn_std/math/sqrt<f64>`, lowering);
  - the E0312/E0320/E0321/E0419 rows;
  - the §10 program;
  - §11 row 53.
- **Docs:** CLAUDE.md (tree: `src/std/`, new files; STACK), README, TODO, CHANGELOG, MEMORY decision 102, CONTEXT. The tag `m53` is added at the merge.

### Must NOT Do
- **No opt-out,** no `deny` list, no `const` items.
- **No `std.fs`, `std.env` or `std.process` yet.**
- **No `$` names in programs.**
- **No integer `sqrt`;** call `math.sqrt(n as f64)`.

### New dependencies
None (LLVM's float intrinsics; `-lm` is already linked).

## ERROR HANDLING REQUIREMENTS
- **E0312:** `import std.nope;`, with help listing `std.math`.
- **E0320:** a `std/` folder or `std.la` beside the main file.
- **E0321:** `std = "…"` in a manifest.
- **E0419:** `math.sqrt(2)` (`i64` isn't `Float`), with help to write `2.0` or use `as f64`.
- **E0101:** `$` in a program.
- **E0313:** a non-`public` std item (there are none, but the rule holds).

## SECURITY CONSIDERATIONS
- std sources are compiled into `lughac`; nothing is read from disk for them, and a program can't replace them (`std` is reserved).
- `$` built-ins can't be named by programs.
- No `unsafe`, no new dependencies, no new runtime code.

## TESTS TO WRITE

Unit tests:
- [x] **Lexer:** `$float` in bundled mode; E0101 otherwise.
- [x] **Loader:**
  - `import std.math;` loads `std.math` once;
  - E0312 for `std.nope`, with the help;
  - E0320 for a local `std.la`;
  - E0321 for a `std` entry.
- [x] **Checker:**
  - the `Float` bound;
  - `$float.*` typing;
  - E0419 for `math.sqrt(2)`;
  - `T: Float` with operators and `0`/`1`.
- [x] **Codegen:** `math.sqrt(2.0)` lowers to `llvm.sqrt.f64`; an `f32` instance to `llvm.sqrt.f32`.

Acceptance — `tests/programs/m53/`:
- [x] `math.la`: the §10 program, including the objective.
- [x] `more.la`:
  - every function at `f64` and some at `f32`;
  - `NaN`, `inf`;
  - `round` of negative halves;
  - `abs`/`min`/`max`/`clamp` on integers and floats;
  - `import std.math as m;` and `import std.math { sqrt };`;
  - a generic `hypot<T: Float>`.
- [x] `clamp_panic.la`: `clamp` with `lo > hi` panics.
- [x] `e0419.la` and `e0312.la`: reject goldens.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-082-std_math`, merged into `main` after CI passes; tag `m53`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The objective's program prints the values shown.
- [x] The `m53/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The spec is at milestone 53; docs updated.
- [x] No file over 500 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m53/math.la
```

## AMENDMENTS (during build, session 111)

- **Loader:** `Packages::std_target` resolves `std.<module>` (E0312 with the module list, E0320 for a local `std`). `read_manifest` skips the `std` package, and the loader takes a std module's bytes from `lugha::STD` instead of `read`. Module files are named `<std>/<module>.la`.
- **Lexing:** `lexer::lex_bundled` (also used by `--emit=tokens` for std modules); `parse_module` picks it for `$prelude` and `std` paths.
- **`Float`** is declared in `src/prelude/ops.la` with `Num`'s items; `has_num` counts it, so `T: Float` gets the operators and `0`/`1`. `has_float` gates the `$float` built-ins.
- **One module, one import per file** still applies (E0314), so `import std.math as m { sqrt };` combines an alias and names.
- **`tests/programs/m53/clamp_panic`:** the panic location is `<std>/math.la:40:9`.
- **IR:** the 171 earlier programs give byte-identical IR. `-O2` was checked by hand for `m53/math` and `m53/more`.
