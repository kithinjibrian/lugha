## FEATURE: Strings from C — an extern may return `string` or `Option<string>`, take `Option<string>`, and callbacks may receive them; C's text is copied into a Lugha string; milestone 89.

**Status:** approved 2026-10-16 ("yes build all three", session 190); proposed in session 189 with PRP-120 and PRP-121 as one batch.
**Milestone:** 89 (tag `m89` at merge).
**Spec:** `Language v16 Draft.md` decisions 115–119; spec §8 (allowed types, the string rule, callbacks), §9 (runtime list, diagnostics), §11 (row 89).

**Decisions (discovery, session 189; the user's choices):**
- C text is accepted in both places: an extern's result and a callback's parameters;
- invalid UTF-8 from C becomes U+FFFD, as Rust's `to_string_lossy` does;
- the three GUI follow-ups are discovered together, approved once, and built in order (strings → structs → GTK package), each merged once local checks pass.
- Decided by me, for review: `NULL` is `None` for `Option<string>` and a panic for `string`; C keeps its memory (Lugha copies, never frees); a callback may not *return* a string (C would hold GC memory); an extern may take `Option<string>` (`None` → `NULL`).
- No open entries in DECISIONS.md apply.

## OBJECTIVE
```
extern fun getenv(name: string): Option<string>;
extern type GtkWidget;
extern fun gtk_editable_get_text(entry: GtkWidget): string;
extern fun gtk_button_set_label(button: GtkWidget, label: Option<string>);
extern fun on_line(cb: fun(string, data), user: data);

fun main() {
    match getenv("HOME") { Some(h) => println(h), None => println("no home") }
    on_line(fun(line: string) { println(line.len()); });
}
```

## CONTEXT
- **Starting state:** spec §8: "A `string` may not be an extern return type, because C strings aren't allocated by the GC and have no length header" (E0409); strings go to C as `const char *` (`codegen/function.rs` `c_string`). Callbacks (milestone 88) take numbers, `bool`, handles and `Option` of a handle (`check/callbacks.rs` `crosses_into_callback`, `codegen/callbacks.rs` `c_body`). `std.env` already copies C bytes in the runtime (`lugha_rt_env`). Lugha's UTF-8 decoding is `utf8_decode` in the prelude (v15).
- **Ending state:**
  - **Checker:** `string` and `Option<string>` allowed as an extern's result; `Option<string>` as an extern parameter; `string` and `Option<string>` as a callback's parameters. Still E0409: a callback returning a string, strings inside arrays or structs.
  - **Runtime** (`runtime/input.c` or a new `cstr.c`): `lugha_rt_str_from_c(const char *text) -> LughaString *` copies the NUL-terminated bytes into a new GC string, replacing each invalid UTF-8 sequence (as the prelude's `utf8_decode` defines one) with U+FFFD; `NULL` panics with `panic: C gave NULL for a \`string\`; declare it \`Option<string>\`` (no location: the runtime can't tell which call). `RUNTIME_SYMBOLS` updated.
  - **Codegen** (`codegen/externs.rs`): a `string` result → `lugha_rt_str_from_c`; `Option<string>` result → `NULL` check, then `None` or `Some(copy)`; `Option<string>` argument → `NULL` or the string's bytes (`c_string`). Callback entries convert their `string`/`Option<string>` parameters the same way.
  - **Spec v16 §8:** the allowed-types table (`string` results, `Option<string>`), the copy and ownership rule ("lughac copies; C keeps and frees its own memory; a C function returning memory the caller must free leaks unless declared as a handle and freed through C"), callbacks' types; §9 runtime list; E0409 row; §11 row 89; changes list. **Draft:** decisions 115–119, row 89.
  - **`docs/errors/E0409.md`:** the string-return example becomes valid (`Option<string>`); the remaining cases stay.
- **Related existing code:** `src/check/env.rs` (`extern_string_return`, the E0409 loop), `src/check/callbacks.rs`, `src/codegen/{externs,callbacks,function}.rs`, `runtime/`, `src/link/mod.rs`, `src/prelude/text.la` (`utf8_decode`, the decoding to match).
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Test-first; the runtime's UTF-8 replacement matches the prelude's decoding byte for byte (a test feeds the same invalid sequences to both).
### Must NOT Do
- Never free C's memory; never hand C a pointer to a GC string it could keep (no string results from callbacks).
- No change to how strings go *to* C.
- Byte-identical IR for programs without string results, `Option<string>` or string callbacks.
### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- **E0409** narrowed: no longer for `string`/`Option<string>` extern results or `string`/`Option<string>` callback parameters; still for a callback's string result (help: "C would keep a pointer into Lugha's memory; return a handle or a number") and other types.
- Runtime panic (exit 101) for `NULL` given as a plain `string`.

## SECURITY CONSIDERATIONS
- The copy reads until the first NUL: a C function returning a non-terminated buffer is C's undefined behavior, as any wrong extern declaration is (spec §7 "Safety").
- No `unsafe` in lughac; the runtime copy is plain C with a length from `strlen`.

## TESTS TO WRITE
- [ ] Checker: `string`/`Option<string>` results and `Option<string>` params allowed; callback `string` params allowed; callback string result E0409.
- [ ] `tests/c_strings.rs` (C files through `[link]`): a result copied (C then overwrites its buffer; Lugha's copy is unchanged); `NULL` → `None`; `Option<string>` argument `None` → `NULL` and `Some` → the text; invalid UTF-8 → `U+FFFD` (same output as `string_from_utf8` would give through the prelude's replacement); a callback receiving `string` and `Option<string>`; `NULL` as a plain `string` → exit 101 with the message; -O0 and -O2.
- [ ] IR unchanged for the compiling `tests/programs`.
- [ ] `tests/explain.rs` (the E0409 page's examples).

## ROLLBACK PLAN
- Branch `prp-119-strings_from_c` in worktree `../lugha-prp-119`; abandoning it means deleting the branch.

## ACCEPTANCE CRITERIA
- [ ] `getenv("HOME")` returns `Some(…)` through a plain extern.
- [ ] All tests pass; fmt and clippy clean; every `pub` item documented; CHANGELOG updated.

## VALIDATION
- `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`
