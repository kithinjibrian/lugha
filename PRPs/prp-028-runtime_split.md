## FEATURE: Split the C runtime into three source fragments, joined into one `lugha_rt.c` when lughac is built.

**Status:** merged into `main` 2026-10-12 — session 34
**Milestone:** none; housekeeping (CLAUDE.md file size rule: `runtime/lugha_rt.c` is 358 lines)
**Spec:** §9 ("Link", "Runtime library"). No spec change: programs are still linked against one embedded `lugha_rt.c`.
**Decisions:** DECISION-009 / MEMORY 15 (runtime embedded as source, compiled in the single `cc` link call); MEMORY 30 (stack overflow handler).

## OBJECTIVE
No runtime source file exceeds 300 lines. What lughac embeds, writes and compiles stays byte-for-byte what it is today, apart from the fragments' file comments. The link step, the private temp file and the generated programs are unchanged.

## CONTEXT

- **Starting state:**
  - `runtime/lugha_rt.c` (358 lines) holds all of the runtime: core, stack overflow handler, formatting and printing.
  - `link::RUNTIME_SOURCE` is `include_str!` of it.
  - `link::RuntimeFile` writes it to one `create_new` temp file for `cc`.
- **Ending state:** three files in `runtime/`, joined in order by `concat!` into `RUNTIME_SOURCE`. `RuntimeFile` and the `cc` command are untouched.
- **Related existing code:**
  - `src/link.rs` (`RUNTIME_SOURCE`, its symbol test);
  - `tests/codegen.rs` (`runtime_compiles_without_warnings`, `-Werror`);
  - `src/codegen/runtime.rs` (`RUNTIME_SYMBOLS`).
- **Open decisions that must be resolved first:** none.

### Amendments during implementation (session 34)
- **Sizes:** the core is 143 lines, `stack.c` 72 and `print.c` 163, all smaller than estimated because the core no longer holds the handler.
- **Check:** a diff of the old file, with the handler block moved after the core, against the joined parts shows only the three added comments.

### Discovery answers (session 34)
1. **Split as fragments joined at build:** not separate C modules with a header (that would need a temp directory and a spec change), and not an exemption from the rule.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Runtime files.** Code moves verbatim, with no behaviour change.
- **`runtime/lugha_rt.c` (~205 lines), the core.** It comes first because it holds `#define _GNU_SOURCE` and every `#include`. It contains:
  - the file comment, now also listing the parts and their order;
  - the includes;
  - `LughaString` and `LughaArray`;
  - the public declarations;
  - `FORMAT_CAP`;
  - the forward declaration of `install_stack_overflow_handler`;
  - `lugha_rt_init`, `die`, `lugha_rt_alloc`, `lugha_rt_array_grow`;
  - the two panics, `str_concat` and `str_eq`.
- **`runtime/stack.c` (~70 lines).** The stack overflow handler: `stack_low`, `alternate_stack`, `GUARD_WINDOW`, `on_segv`, `install_stack_overflow_handler`.
- **`runtime/print.c` (~160 lines).** `put`, `format_f64`, `make_string`, and the `lugha_rt_print_*` and `lugha_rt_to_string_*` functions.
- **Fragment headers.** `stack.c` and `print.c` each start with a comment: a fragment of `lugha_rt.c`, joined after the core by `link.rs`, not compiled on its own, and relying on the core's includes and types.

**Embedding** — `src/link.rs`
- `RUNTIME_PARTS: [(&str, &str); 3]` holds the file name and source of each part, in order.
- `RUNTIME_SOURCE` is `concat!` of the same three `include_str!`s. Its type and meaning don't change.
- The `//!` and `///` docs name the parts.

**Docs**
- CLAUDE.md: the tree (`runtime/` lists the three files), the STACK line, and the `link.rs` line.
- MEMORY decision 15 gets a one-line note. TODO, CHANGELOG, CONTEXT.

### Must NOT Do
- No change to `RuntimeFile`, the temp file name or permissions, the `cc` command, or the spec.
- No code edits inside moved functions; no new runtime functions.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- No new error paths. A missing part is a compile error in lughac itself (`include_str!`).

## SECURITY CONSIDERATIONS

- **Temp file:** the security-relevant handling (one `create_new` temp file) is unchanged by design. This was the reason for choosing fragments over separate modules.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests — `link.rs` (written first; they fail until `RUNTIME_PARTS` exists):
- [x] **Order and joining:** `RUNTIME_SOURCE` is exactly the parts joined in order. The core comes first, and `#define _GNU_SOURCE` appears before its first `#include`.
- [x] **Includes:** only the core has `#include` lines, since the fragments rely on its includes.
- [x] **Size:** every part is non-empty and at most 300 lines.
- [x] **Existing:** every `RUNTIME_SYMBOLS` entry is still defined.

Existing integration tests (the safety net for a verbatim move):
- [x] **`runtime_compiles_without_warnings`:** the joined source compiles with `-std=c11 -Wall -Wextra -Werror`.
- [x] **End to end:** every program under `tests/programs/` (printing, floats, panics, stack overflow, growth) and every spec §10 program pass at `-O0` and `-O2`.

## ROLLBACK PLAN

- Branch `prp-028-runtime_split`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] No file in `runtime/` exceeds 300 lines; the joined source is the old file's code in the same order.
- [x] CLAUDE.md, MEMORY.md, TODO.md, CHANGELOG.md updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `wc -l runtime/*.c` — all ≤ 300
