## FEATURE: Callbacks and handles — Lugha functions and closures passed to C as callbacks, and opaque handle types for C pointers (`extern type`); milestone 88.

**Status:** proposed 2026-10-16 (session 187).
**Milestone:** 88 (one PRP; tag `m88` at merge).
**Spec:** `Language v16 Draft.md` decisions 105–114; spec §2/§3 (`extern type`, the grammar), §4 (handle types), §7 (pinned closures), §8 (C interop: handles, callbacks, `data`, `release`), §9 (symbols, trampolines, the runtime, diagnostics), §11 (row 88).

**Decisions (discovery, session 187; the user's choices):**
- named functions **and** closures can be passed to C (option 1, `user_data`, as Rust, Swift, Zig and Go do — no run-time code generation);
- an extern says where C passes user data back with a `data` marker; the caller never writes the `data` argument;
- closures handed to C stay alive until the program exits, unless the extern has a `release` parameter, which lughac fills with a function C calls when it drops the callback;
- opaque handle types for C pointers are part of this PRP ("just do it now").
- Found while writing the example: GTK connects every signal through `g_signal_connect_data`, each with its own handler type, and one C symbol may be declared only once (E0306). So this PRP adds `extern "c_symbol" fun lugha_name(…)`: a Lugha name for a C function, which several declarations may share with different signatures.
- Decided by me, for review: no strings in callbacks yet; a callback without `data` takes only a named function or a closure that captures nothing; callbacks run on the thread that registered them; no `unsafe` keyword (the `extern` declaration stays the trust point, spec §7 "Safety").
- No open entries in DECISIONS.md apply.

**Why:** milestone 87 made any C library linkable; GUI toolkits also need C to call back into the program (GTK signals, timers) and need values for C objects (`GtkWidget*`). This makes a GTK 4 window with a working button possible in plain Lugha.

## OBJECTIVE
A GTK 4 counter, in plain Lugha:
```
extern type GtkApplication;
extern type GtkWidget;

extern fun gtk_application_new(id: string, flags: i32): GtkApplication;
extern fun gtk_application_window_new(app: GtkApplication): GtkWidget;
extern fun gtk_button_new_with_label(label: string): GtkWidget;
extern fun gtk_button_set_label(button: GtkWidget, label: string);
extern fun gtk_window_set_child(window: GtkWidget, child: GtkWidget);
extern fun gtk_window_present(window: GtkWidget);
extern fun g_application_run(app: GtkApplication, argc: i32, argv: i64): i32;

// One C function, two Lugha declarations: each signal's handler has its own type.
extern "g_signal_connect_data" fun on_activate(app: GtkApplication, signal: string,
    handler: fun(GtkApplication, data), user: data, destroy: release, flags: i32): u64;
extern "g_signal_connect_data" fun on_clicked(button: GtkWidget, signal: string,
    handler: fun(GtkWidget, data), user: data, destroy: release, flags: i32): u64;

fun main(): i32 {
    let app = gtk_application_new("org.lugha.counter", 0);
    on_activate(app, "activate", fun(app: GtkApplication) {
        let window = gtk_application_window_new(app);
        let button = gtk_button_new_with_label("click me");
        let clicks = Ref.new(0);
        on_clicked(button, "clicked", fun(b: GtkWidget) {
            clicks.update(fun(n) = n + 1);
            gtk_button_set_label(b, "clicked {clicks.get()} times");
        }, 0);
        gtk_window_set_child(window, button);
        gtk_window_present(window);
    }, 0);
    g_application_run(app, 0, 0)
}
```
The caller never writes `user` or `destroy`: lughac passes the pinned closure and the function that frees it.

Named functions go to any callback, closures to callbacks with `data`:
```
extern fun qsort_ints(xs: IntBuf, n: i64, cmp: fun(i64, i64): i32);   // no data: named functions only
fun descending(a: i64, b: i64): i32 = if a > b { -1 } else if a < b { 1 } else { 0 };
qsort_ints(buf, 10, descending);
```

## CONTEXT
- **Starting state:** externs (`ast::ExternDecl`, `parser/item.rs`, `check/env.rs`, `codegen/declare.rs`) take numbers, `bool` and `string` parameters (E0409 otherwise). Function values are `{ ptr code, ptr env }`; `code` takes the environment first (`lugha_ref_<symbol>` for named functions, `lugha_closure_…` bodies; `codegen/fun_values.rs`, `closures.rs`). `Type` (`check/types.rs`) has no pointer type; `Ref<T>` (milestone 67, PRP-096) is the template for adding a kind of type. `[link]` C files (milestone 87) let tests ship their own C side.
- **Ending state:**
  - **Syntax:** `extern type Name;` — a top-level item with a visibility (`public extern type GtkWidget;`). AST `Item::ExternType`; the parser, the formatter (`formatter/items.rs`), the s-expression printer, and the language server (hover, definition, completion list it as a type) follow.
  - **Handle types** (`Type::Handle(key)`): a pointer-sized value from C; distinct per declaration (a `GtkWidget` isn't a `GtkApplication`); usable anywhere a type is (locals, fields, arrays, `Option`, generics); `==`/`!=` compare addresses (`Eq`); no ordering, arithmetic, casts, literals, fields or methods (E0403/E0408/E0410/E0415 as for other types); print as `<GtkWidget>`; copying copies the pointer. Lugha can't make one: only an extern returns or a callback receives one. In extern signatures and callbacks `Option<H>` maps C's `NULL` to `None` and back.
  - **Renamed externs:** `extern "symbol" fun name(…)` declares the C function `symbol` under the Lugha name `name`. Several declarations may name one symbol with different signatures (E0306 then applies only to two declarations with the same Lugha name); `symbol` follows C identifier rules and the `lugha_` reservation (E0306). LLVM declares the symbol once and each call uses its own declaration's function type.
  - **Callback types in externs:** a parameter `fun(T…): R` is a C function pointer. `T`/`R` are extern types (numbers, `bool`, handles, `Option` of a handle; `R` may be omitted), plus at most one parameter written `data`. An extern parameter written `data` is the user data C hands back; one written `release` is a C function `void (*)(void *)` C calls when it drops the callback. Rules (E0435 at the declaration): `data` and `release` appear only in extern signatures; an extern with a `data` parameter has exactly one callback containing `data`, and the reverse; `release` needs `data`; at most one of each; no `string`, array, struct, enum or function type inside a callback; no callback as an extern's return or inside another callback.
  - **Calls:** the caller omits `data` and `release` arguments (arity counts without them). The argument for a callback is a function value whose type is the callback's without `data`. For a callback **without** `data` it must be a function's name (`f`, `m.f`) that isn't generic, or a closure literal that captures nothing; anything else is E0436 ("C can't keep this closure's captures: this callback has no `data`; pass a named function"). For a callback **with** `data`, any function value.
  - **Codegen:**
    - a named function or capture-free closure passed to C becomes `lugha_cfn_<symbol>`, a C-ABI function with the callback's C signature calling `lugha_fn_<symbol>` (or the closure body with a null environment); emitted once;
    - a callback with `data` passes the trampoline `lugha_cb_<n>` (one per distinct callback signature, numbered in order of first use; its C signature takes `data` as `ptr` where declared) and, as the `data` argument, `lugha_rt_cb_pin(code, env)`; the trampoline loads `{ code, env }` from `data` and calls `code(env, args…)`, converting handles and `Option`s;
    - `release` arguments are `lugha_rt_cb_release`.
  - **Runtime** (`runtime/callbacks.c`, a new fragment): `lugha_rt_cb_pin` copies `{ code, env }` into `GC_MALLOC_UNCOLLECTABLE` memory (scanned by Boehm, never collected, so the closure and its environment stay alive while C holds them); `lugha_rt_cb_release` frees it with `GC_FREE`. `RUNTIME_PARTS`, `RUNTIME_SOURCE`, `RUNTIME_SYMBOLS` updated.
  - **Spec v16:** §2 (a string after `extern`; no new keyword: `type` after `extern`; `data`/`release` are names, special only as extern parameter types), §3 grammar (`extern_type_decl`, callback parameter types), §4 handles, §7 pinned closures and threads, §8 the boundary table (handles, `Option<H>`, callbacks), §9 symbols (`lugha_cfn_`, `lugha_cb_`, rule 7 in CLAUDE.md), the runtime list, E0435/E0436 and the widened E0409, §11 row 88, the changes list. **Draft:** decisions 105–114, row 88.
  - **`docs/errors/E0435.md`, `E0436.md`**; E0409's page gains handles and callbacks.
  - **Docs:** CLAUDE.md (tree, rule 7 prefixes, STACK), README, TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/ast/mod.rs`, `src/parser/item.rs`, `src/check/{types,env,fun_values,closures,call,arguments,bounds,printing}.rs`, `src/check/errors/`, `src/codegen/{declare,value,function,fun_values,closures,equality,show,copy}.rs`, `runtime/`, `src/link/mod.rs`, `src/formatter/items.rs`, `src/driver/lsp/{hover,definition,completion}.rs`.
- **Open decisions that must be resolved first:** none.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The ending state above, test-first; handles first (they stand alone), then callbacks without `data`, then with `data` and `release`.
- A file over 500 lines is split by proposal first (FILE SIZE RULE); new checker and codegen code goes in new files (`check/handles.rs`, `check/callbacks.rs`, `codegen/handles.rs`, `codegen/callbacks.rs`).

### Must NOT Do
- No run-time code generation, no executable memory, no libffi.
- No `unsafe` keyword; no pointer arithmetic, dereferencing or casts to or from handles (handles are not integers).
- No strings, arrays, structs or enums crossing into callbacks.
- No change to how function values work inside Lugha, and byte-identical IR for programs without handles or callbacks.
- No threads: nothing makes callbacks safe to call from another thread.
- No new crate.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS
- **E0409** (widened): a type not allowed in an extern signature now also covers a `string`, array, struct, enum or function inside a callback, and a callback as a return type.
- **E0435** (new): a malformed callback declaration — `data`/`release` outside an extern signature, two of them, `data` without a callback using it (or the reverse), `release` without `data`, a callback inside a callback.
- **E0436** (new): a closure with captures (or a function value from a variable, or a generic function) passed where the callback has no `data`.
- Existing codes for misuse of handles: E0403 (operators), E0408 (casts), E0410/E0415 (fields, methods), E0419 (`Ord` bounds), E0406 (an extern as a value, unchanged).
- No new `.expect()` beyond codegen invariants the checker guarantees (stated in each message).

## SECURITY CONSIDERATIONS
- **No `unsafe` in lughac** (Rust), and the generated code adds no new way for ordinary Lugha code to cause undefined behavior: only a wrong `extern` declaration can (spec §7 "Safety"), as today.
- **Lifetimes:** a pinned closure is never freed while C can call it, unless C calls `release` and then calls the callback anyway (C's bug). Double `release` is C's bug. Documented in §7/§8.
- **Threads:** a callback called on another thread is undefined behavior (Boehm doesn't know the thread); documented.
- **`release`'s C type** is `void (*)(void *)`; GLib's `GClosureNotify` passes a second argument, which the x86-64 System V ABI lets a one-parameter function ignore; documented as the one platform assumption.

## TESTS TO WRITE
- [ ] Parser: `extern type T;`, `public extern type T;`, `extern "sym" fun f(…);`, callback parameter types with `data`; errors for `extern type` with a body or generics; formatter round trip; s-expression output.
- [ ] Checker unit tests: handle equality, no arithmetic/cast/field/method/`<`, distinct handle types, `Option<H>` in externs, printing type names; every E0435 case; every E0436 case; arity without `data`/`release`; argument type matching against the callback without `data`.
- [ ] `tests/callbacks.rs` (through `lughac`, with a `[link]` C file per case):
  - [ ] two renamed externs on one C symbol with different callback types, both called;
  - [ ] handles: C returns a pointer, Lugha stores it in a struct and passes it back; `NULL` → `None` and `None` → `NULL`; `==`; printing `<H>`;
  - [ ] a named function to a callback without `data` (`qsort`-style), called during the extern call;
  - [ ] a capture-free closure literal to it;
  - [ ] a closure with captures and a `Ref` through `data`, stored by C and called later, several times, after a `GC_gcollect()` forced from C (the closure survives);
  - [ ] two closures registered on one signature keep their own captures;
  - [ ] `release`: C calls it, then a counter in C confirms it was called once; a later forced collection doesn't crash;
  - [ ] a callback receiving and returning handles, `bool`, `f64`, `i32`;
  - [ ] a panic inside a callback exits 101 with the usual message;
  - [ ] E0409, E0435, E0436 rejected programs with their spans.
- [ ] A program without handles or callbacks gives byte-identical IR (an existing `tests/programs` IR comparison or a new check).
- [ ] Hover, definition and completion show `extern type` names (`tests/lsp_hover.rs` etc., one case each).
- [ ] `tests/explain.rs` covers E0435/E0436 automatically.

## ROLLBACK PLAN
- Branch to return to: `main` before the merge.
- State: the feature lives on branch `prp-118-callbacks_handles` in worktree `../lugha-prp-118`; abandoning it means deleting the branch.

## ACCEPTANCE CRITERIA
- [ ] Manual: a GTK 4 window with a button counting its clicks, in plain Lugha, linked through `[link]` (a `vendor/lib` folder with `libgtk-4.so`, `libgobject-2.0.so`, `libgio-2.0.so` symlinks to the installed `.so.N` files, since the GTK development package isn't installed). The window is the user's to look at.
- [ ] Every test above passes; IR without handles or callbacks unchanged.
- [ ] All existing tests pass
- [ ] New tests written and passing
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- the GTK counter (manual)
