# Lugha v16 — Draft (delta against v15)

**Status:** milestone 67 (shared boxes, `Ref<T>`) approved in session 136 (proposed in session 135), and built. Milestone 68 (changing the working directory) approved in session 138 (proposed in session 137), and built. Milestone 69 (`lughac init`) approved in session 142 (proposed in session 139), and built. Milestone 70 (`lughac lsp`, live diagnostics) approved in session 145 (proposed in session 144), and built. Milestone 71 (hover) approved in session 147 (proposed in session 146), and built. Milestone 72 (go-to-definition) approved in session 149 (proposed in session 148), and built. Milestone 73 (completion) approved in session 152 (proposed in session 151), and built. Milestone 74 (signature help) approved in session 155 (proposed in session 154), and built.
**Base:** `Language v15 Specification.md`, complete (milestones 51–66). It stays the source of truth, and what `lughac spec` prints, until v16's first milestone starts `Language v16 Specification.md`. That file then grows milestone by milestone, and v15 is frozen.

This draft lists only what changes, in the order of the v15 sections. Everything not mentioned keeps its v15 meaning.

**Compatibility.** Every valid v15 program stays valid, unless it declares its own type named `Ref`. A module's own `Ref` still means its own, as for `Iter` and `Ordering`.

## Decisions behind milestone 67 (discovery, session 135)

Found while writing firefly and oyster, and discussed in session 135: without any shared reference, a closure can't keep state (E0511), and linked or graph-shaped data must use index handles. The user chose a small, explicit shared box over classes or borrowed references, and to open v16 with it.

1. **`Ref<T>` is a shared box:** one GC object holding a `T`. Copying a `Ref` (assigning, passing, storing, capturing) copies the handle, never the box, so every copy sees the same contents. Every other type keeps value semantics; sharing appears only where a `Ref` is written.
2. **Copy in, copy out:**
   - `Ref.new(v: T): Ref<T>` makes a box;
   - `r.get(): T` gives a copy of the contents;
   - `r.set(v: T)` replaces them;
   - `r.update(f: fun(T): T)` is `r.set(f(r.get()))`;
   - `r.is(other: Ref<T>): bool` tells whether both are the same box.

   Nothing reaches inside a box in place: no `r.value.x = 1`, no field or index access through a `Ref`. So copy-on-write (milestone 55) and loop ownership (milestone 56) stay sound unchanged: a box is one holder of whatever it contains.
3. **Not `Eq`:**
   - `r == s` is E0404, with help: `r.is(s)` for the same box, or `r.get() == s.get()` for the same contents;
   - a struct, enum, tuple, array or `Option` holding a `Ref` isn't `Eq` either (E0419 for `T: Eq`), as for interface values.

   **Printing:** a box prints as `Ref(<contents>)`. A box already being printed (a cycle) prints as `Ref(…)`, so printing always ends.
4. **Changing a box needs no `let mut`:** `set` and `update` take plain `self`, because they change the box, not the handle. `let r = Ref.new(0); r.set(1);` is valid, and a closure may `set` or `update` a `Ref` it captured; E0511 still forbids changing the captured variable itself. `let mut r` is needed only to point `r` at another box (`r = Ref.new(5)`).
5. **Success:** milestone 67, tag `m67`. v15 is declared complete and frozen; `Language v16 Specification.md` starts and `lughac spec` prints it. Tests cover:
   - checker rules (not `Eq`, the `let` rules, generics, recursion through `Ref`);
   - programs: a stateful closure, a doubly linked list, `Ref` in structs, arrays and interface values, values taken out with `get` never changing when the box does, and cyclic printing;
   - a §10 program.

   No library changes.

## Decisions behind milestone 68 (discovery, session 137)

Found building oyster, a shell in Lugha: `cd` was impossible, because v15's `std.env` was read-only (session 127, decision 48) and `std.process` takes no directory.

6. **`env.set_current_dir(path): Result<i64, string>`** changes the program's working directory (`chdir`): `Ok(0)`, or `Err` with the system's message (a missing folder, a file, no permission); a NUL byte in `path` is `Err("path contains a NUL byte")`. It is process-wide, so relative paths in the file intrinsics and `std.fs` resolve against it, and every program `std.process` starts afterwards runs there. Environment variables stay read-only.
7. **Shell conveniences are the program's job:** `~`, `cd` alone and `cd -` are oyster's, not the language's.
8. **Success:** milestone 68, tag `m68`. Tests: `current_dir`, a relative `read_file`, `std.fs` and a child process all see the new directory; `Err` for a missing folder, a file and a NUL. Then oyster's `cd` (`cd dir`, `cd` to `$HOME`, `cd -`, `~`), left uncommitted with the rest of oyster.

## Decisions behind milestone 69 (discovery, session 139)

There is no command to start a Lugha project: `lugha.toml` and the first source file are written by hand. A library and a program need different starting files.

9. **`lughac init [folder] [--lib] [--name N]`** starts a project in `folder` (default: the current one), making it if needed. Without `--lib` it's a program; with it, a library. It never prompts.
10. **The layouts:**
    - **both:** `lugha.toml` (`[package]` `name`, `version = "0.1.0"`, `source = "src"`) and a `.gitignore`;
    - **a program:** `src/main.la`, a `fun main()` printing a greeting;
    - **a library:** `src/<name>.la`, a documented `public fun`, and `examples/hello.la`, a program importing the library by name (`import <name>.<name>;`), runnable at once through v15's package layout. Naming the module after the package fits a later entry module (`import <name>;`).
11. **Safety:**
    - an existing `lugha.toml` stops it before anything is written (exit 2);
    - no file is ever overwritten: one that exists is kept and reported as `kept`;
    - the name comes from the folder unless `--name` is given; it must be an identifier that is neither a keyword nor `std`, else exit 2 with a hint (`--name my_shell`);
    - no `git`, no network.
12. **Success:** milestone 69, tag `m69`. Tests in `tests/init.rs` (PRP-098: `tests/cli.rs` is near the file limit): each layout created, then checked, built and run; each refusal; `kept` files untouched. The spec's CLI table gains `init`.

## Decisions behind milestone 70 (discovery, session 144)

The VS Code extension (PRP-099) checks a file only when it is saved, by running `lughac check`. The user chose a language server, built in three steps: live diagnostics (milestone 70), then hover types, then go-to-definition. This milestone is the first.

13. **`lughac lsp`** speaks the Language Server Protocol over stdin and stdout, for any editor. It reports the same diagnostics as `lughac check`, for the text in the editor, saved or not, a moment after typing stops. It never runs `git` (always offline), never writes a file (not even `lugha.lock`), and opens no network port.
14. **What is checked:** a file with `fun main` checks itself. A module checks through every main file listed in the client's `lugha.entries` setting and every main file the server has already checked that loaded it; with none, it is checked alone and E0303 is dropped.
15. **A panic inside the compiler** while checking is caught: the server stays up and shows one internal-error diagnostic at the top of the file.
16. **Crates:** `lsp-server` and `lsp-types` (rust-analyzer's; MIT/Apache), with `serde_json`. No async runtime. They are used only by the server; nothing else in lughac changes.
17. **Success:** milestone 70, tag `m70`. Tests in `tests/lsp.rs` drive `lughac lsp` over stdio. The spec's CLI table gains `lsp`.

## Decisions behind milestone 71 (discovery, session 146)

The second language-server step: hovering shows types.

18. **`textDocument/hover`** answers for any expression (its type) and any declared name: a `let`, parameter, `for` or `match` binding (`let total: i64`), a function or method at its declaration or call (`fun Circle.area(self): f64`), a struct field (`x: f64`). The innermost expression or name under the cursor wins.
19. **The text** is a Lugha code block written as code would write it, then short notes when they apply: `instance: max<f64>` for a generic use, `converted to Shape` for a value converted to an interface. Types print as diagnostics print them.
20. **With errors:** the checker's partial results are used, so hover works in a program with errors, except inside a module that doesn't parse and on the expression in error. `lughac check`, its output and codegen are unchanged: the partial results come through a separate entry point.
21. **Fresh answers:** a hover first runs any check still waiting for typing to pause, so it never answers from older text.
22. **Success:** milestone 71, tag `m71`. Tests in `tests/lsp.rs` (or a new `tests/lsp_hover.rs` if that file would pass 500 lines). The spec's `lughac lsp` paragraph gains hover.

## Decisions behind milestone 72 (discovery, session 148)

The third language-server step: Go to Definition (F12, Ctrl+click).

23. **`textDocument/definition`** jumps from a variable or parameter to its binding; a function or method (declaration, call, value) to its declaration; a type name (annotation, literal, `impl`, construction) to its struct, enum or interface; `p.x` and a literal's `x:` to the field; `Shape.Circle` to the variant; a module in `import geo;` or `geo.f` to the module's file. The smallest name under the cursor wins; on a declaration it answers the declaration itself.
24. **Interface calls:** `s.area()` on an interface value jumps to the interface's method declaration. Implementations are a later feature (Go to Implementations).
25. **Bundled sources:** a definition in `std` or the prelude has no file: nothing happens (`null`). A read-only view of bundled sources may come later.
26. **As hover:** the checker's partial results, so it works while the program has errors; a pending check runs first.
27. **Success:** milestone 72, tag `m72`. Tests in `tests/lsp_definition.rs`. The spec's `lughac lsp` paragraph gains go-to-definition.

## Decisions behind milestone 73 (discovery, session 151)

The fourth language-server step: completion.

28. **`textDocument/completion`**, triggered by `.` and by typing a name. After `.`: a value's fields and methods (built-in array and string methods too), a module's items (`geo.` → `double`), an enum's variants and a type's associated functions (`Shape.` → `Circle`, `Stack.` → `new`). A plain name: the locals and parameters declared before the cursor in the enclosing function, the program's functions, types and imported modules, the intrinsics (`println`, …), `Some`/`None`/`Ok`/`Err`, and the keywords. Each item carries its type or header as detail, as hover shows them. No snippets.
29. **Incomplete code:** completion reads the last check that parsed. Its offsets are trusted only while the text before the receiver (or before the word being typed) is unchanged since that check — the usual case when typing `p.` after `p`. Otherwise member completion offers nothing that time and plain names come from the last good check without locals. The parser is unchanged.
30. **Visibility:** another module's members are offered as code may use them — what `private` hides is left out.
31. **Ordering and filtering** are the editor's: the server returns every candidate for the position, with a kind (field, method, function, variable, type, module, enum member, keyword).
32. **Success:** milestone 73, tag `m73`. Tests in `tests/lsp_completion.rs`. The spec's `lughac lsp` paragraph gains completion.

## Decisions behind milestone 74 (discovery, session 154)

The fifth language-server step: signature help.

33. **`textDocument/signatureHelp`**, triggered by `(` and `,`: the signature of the call the cursor is inside, with the current parameter marked. Every callable: functions, module functions (`geo.double(`), methods (`p.scale(`), associated functions (`Shape.new(`), variants with payloads (`Shape.Circle(` → `Circle(f64)`), built-in methods and prelude helpers (`xs.push(`), and the intrinsics (`println(`).
34. **The text:** a declared function's header as written; a generic one with its type parameters filled in when the last good check knows the instance at that call (`fun max(a: f64, b: f64): f64`, with the written header as a note), as written otherwise. Built-ins and variants show types only; intrinsics show the spec's call form (`println(x)`, `read_file(path)`).
35. **Finding the call:** from the cursor back to the unclosed `(` of the current line or lines, commas at that depth giving the parameter; the callee is the name (or `x.name`, `m.name`, `T.name`) before the `(`, resolved as completion resolves a receiver — from the last check that parsed, by its shared prefix or by name.
36. **Nothing found** (not in a call, an unknown callee, a string or comment) → no signature help (`null`).
37. **Success:** milestone 74, tag `m74`. Tests in `tests/lsp_signature.rs`. The spec's `lughac lsp` paragraph gains signature help.

## §4 Type system — changes

- **`Ref<T>`**, predeclared like `Option`, for any `T`. Recursion through a `Ref` is allowed, as through an array: `struct Node { value: i64, next: Option<Ref<Node>> }` needs no indirect slot.
- **Equality:** decision 3.
- **Copies:** the Array copies table gains a row: a `Ref` is never copied, only its handle.

## §7 Memory model — changes

- **A `Ref` is a pointer** to one GC object holding its contents, laid out as the contents would be in an array element.
- **Two kinds of shared object:** boxes are the only objects two Lugha values may share and change (strings are shared but immutable). The ownership sentence (copy-on-write) reads: every array has one holder, a box counting as one.

## §9 Compilation model — changes

- **`Ref.new`:** an allocation through `lugha_rt_alloc` and a store.
- **`get`:** a load and a copy site (copy-on-write marks arrays).
- **`set`:** a store following the normal store rules.
- **`is`:** a pointer comparison.
- **Printing:** `lugha_show_Ref<T>` keeps a stack of boxes being printed.

## §10 Example programs — additions

- **M67:** a counter closure over a `Ref`, a two-node cycle printed with `Ref(…)`, and `get` giving a copy that later `set`s don't change.

## §11 Milestones — additions

| # | Milestone | Scope |
| --- | --- | --- |
| 67 | Shared boxes | `Ref<T>`: `new`, `get`, `set`, `update`, `is`; not `Eq`; cyclic printing |
| 68 | Changing directory | `env.set_current_dir(path)` |
| 69 | `lughac init` | program and library layouts; never overwrites |
| 70 | `lughac lsp` | live diagnostics over the Language Server Protocol; the VS Code extension uses it |
| 71 | Hover | types of expressions and declared names, also while the program has errors |
| 72 | Go to definition | variables, functions and methods, types, fields, variants, modules |
| 73 | Completion | members after `.`, plain names in scope, keywords; from the last check that parsed |
| 74 | Signature help | every callable; generic instances filled in when known; intrinsics in the spec's call form |

## §12 Open questions — additions

- [ ] **Editing a box in place** (v16). `r.with(fun(mut x: T) { … })` could avoid the copy `update` makes of big contents, if the closure can be kept from reaching the same box.
- [ ] **A bounds-checked byte buffer** (v16, session 135): the safe answer to pointer-style code over raw bytes.
