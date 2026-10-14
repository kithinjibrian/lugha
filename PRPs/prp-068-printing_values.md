## FEATURE: Printing values — `Display`/`show`, the built-in form, quoting, generic `T`; milestone 39.

**Status:** merged 2026-10-14 — session 88. This is part of the user's "in one go" (milestones 39–40), merged once CI is green.
**Milestone:** 39 (one PRP; tag `m39` at merge; `Language v12 Specification.md` begins)
**Spec:** `Language v12 Draft.md`:
- decisions 1–3, and the choices on `show`, the built-in form, recursion and escapes;
- §4;
- §5 "How values print";
- §9;
- §10 "Printing values";
- §11 (milestone 39).

**Decisions:** MEMORY 83 (v12).

## OBJECTIVE
The §10 printing-values program prints its seven lines: `Point { x: 1, y: 2 }`, `Circle(1.5)`, `[Some(1), None]`, `Team { name: "red", members: ["ann", "bo"] }`, `$5.25`, `wallet: Wallet { cash: $5.25 }`, `Stack { items: [3] }`.

## CONTEXT

- **Starting state:**
  - `print`/`println`/`eprint`/`eprintln` take numbers, `bool` and strings; `to_string` takes numbers and `bool`; interpolation takes numbers, `bool` and strings. Everything else is E0403 (`check/call.rs`, `check/input.rs`).
  - Codegen prints through `lugha_rt_print_*` and `lugha_rt_to_string_*` (`codegen/runtime.rs`, `codegen/input.rs::text_of`).
- **Ending state:**
  - every type but `void` prints, interpolates and goes through `to_string` — directly, except interface and function values, which come in milestone 40;
  - codegen can print every type, including interface values (a generic `T` may be one).
- **Related existing code:**
  - `src/check/{call,input,instances}.rs`;
  - `src/codegen/{runtime,input,equality,copy,enums,interfaces,upcast,lower}.rs`;
  - `runtime/print.c`, `src/link.rs`, `src/prelude_iter.la`.
- **Open decisions that must be resolved first:** none.

### Design
- **Prelude:** `interface Display { fun show(self): string; }` in `src/prelude_iter.la`'s new tail (renamed in a comment as the prelude's v10–v12 part). It is visible like `Iter` (`or_prelude_interface` grows `Display`).
- **Checker** (new `check/printing.rs`):
  - **What prints:** `printable(ty)` is every type except `void` and, until milestone 40, interface and function values. It is used by `call.rs` (print family, `to_string`) and `input.rs` (interpolation).
  - **Uses:** each print site records `printed(at, ty)`, a use of the special key `$print` at `[ty]`. `monomorphize` expands such a use, at concrete types, into `show_instances(ty)`: for every generic struct or enum reached inside `ty` (through fields, payloads, elements, `Option`, and the types converted to an interface) whose own `show` is `fun show(self): string`, its instance at that type's arguments. Non-generic `show` methods are compiled anyway.
- **Codegen** (new `codegen/show.rs`, `codegen/show_iface.rs`):
  - **`text(value, ty)`:** the top-level text. A string is itself; numbers and `bool` go through `text_of`; everything else goes through `part`.
  - **`part(value, ty)`:** the text inside a built-in form. A string is `lugha_rt_str_quote(s)`; a function value is `"<fun>"`; structs, enums, arrays, `Option` and interface values call `lugha_show_<mangle>(value) -> string`, generated once per type.
  - **Bodies:**
    - a struct or enum whose method (or listed instance) `show` has receiver `self`, no parameters and a `string` result calls it, through a stack slot for the receiver;
    - a struct gives `Name { f: part, … }`;
    - an enum or `Option` switches on the tag, giving `Variant(part, …)` or `Variant`;
    - an array gives `[part, …]`;
    - names are bare: the key's last segment, without type arguments.
  - **Interface values:** `lugha_show_<I>` is declared at first use, and its body is built after `finish_upcasts`. Every table registered for that interface becomes an identity check on the table's type slot (`lugha_type_<C>`), then a load from the box and `C`'s helper. Building those may declare more interface helpers, so this repeats until no body is missing.
  - **Print sites:** `print(x)` of a non-primitive becomes `print_str(text(x))`; interpolation and `to_string` use `text`.
- **Runtime:** `lugha_rt_str_quote` in `runtime/print.c` (`\"`, `\\`, `\n`, `\t`, `\r`, `\u{XX}` for other bytes below 0x20); `RUNTIME_SYMBOLS` grows to 35.
- **Programs printing only numbers, `bool` and strings** give byte-identical IR.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** the prelude interface, `check/printing.rs` and its hooks, `codegen/show.rs`, `codegen/show_iface.rs`, and the runtime function.
- **Spec:** start `Language v12 Specification.md` from v11 (in progress: milestone 39); point `lib.rs` and `tests/cli.rs` at it. Fold in:
  - §4 printing and `Display`;
  - §5 the print table;
  - §6 the intrinsics table (`print`, `to_string` take any value);
  - §9;
  - the E0403 row;
  - the §10 program;
  - §11;
  - "Changes since v11".
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 84, CONTEXT. The tag `m39` is added at the merge.

### Must NOT Do
- **Not yet:** printing interface or function values directly, and `unwrap` showing the error (milestone 40).
- **Never:** format specifiers or a separate debug form.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0403:** printing `void`; and, until milestone 40, a direct interface or function value.
- **A `show` with another signature** is ignored silently: no error.

## SECURITY CONSIDERATIONS
- `lugha_rt_str_quote` sizes its buffer from a first pass over the bytes, with checked arithmetic, and never reads past `len`.
- No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests — checker (`check/printing.rs`):
- [x] Structs, enums, arrays, `Option`, `Result` and a `T` print, interpolate and go through `to_string`.
- [x] E0403 for `void`, an interface value and a function value.
- [x] A generic type's `show` instance is listed when printed directly, inside an array, and through a generic `T`.

Unit tests — codegen (`codegen/show.rs`):
- [x] A struct print calls `lugha_show_5Point`.
- [x] A string field is quoted with `lugha_rt_str_quote`.
- [x] A type with `show` calls its method.

Acceptance — `tests/programs/m39/`:
- [x] `values.la`: the §10 program.
- [x] `more.la`:
  - nested structs and enums;
  - a recursive enum;
  - `Result`;
  - escapes;
  - an empty struct and an empty array;
  - `f64` fields;
  - a module's type (bare name);
  - `show` on an enum;
  - a `show` with the wrong signature ignored;
  - `to_string`, `eprintln`;
  - a generic `T` that is an interface value (`dump(s)` with `s: Shape`).
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-068-printing_values`, merged into `main` after CI passes; tag `m39`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The §10 program and the `m39/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] `Language v12 Specification.md` begins; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m39/values.la
```

## AMENDMENTS (during build, session 88)

- **`Display`** is in `src/prelude_iter.la` (the prelude's second file). `or_prelude_interface` resolves `Iter` and `Display` by one rule.
- **Printing `void` is E0407,** the existing code for a `void` value, not E0403.
- **Four earlier checker tests changed** to the v12 behaviour:
  - `bounds` (`println(a)` on a bare `T`);
  - `call` (`to_string("s")`);
  - `input` (`eprint([1])`, `"{p}"`, `"{[1]}"`).

  Their error cases now use function values.
- **`monomorphize`'s use loop** now builds a list of callees per use, so a `$print` use can expand into several `show` instances.
- **The more program is a directory case,** `m39/more_mod/`, with module `geo` for the bare-name check. Its `eprintln` output is checked by `more_mod.stderr`.
- **Codegen tests** live in `codegen/show_iface.rs`, keeping `show.rs` under 300 lines.
- **`input.rs::call_runtime_value`** became `pub(super)`.
- **IR:** the 145 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for `m39/values` and `m39/more_mod`.
- **No file splits.**
