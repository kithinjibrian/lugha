# Lugha v16 — Draft (delta against v15)

**Status:** milestone 67 (shared boxes, `Ref<T>`) approved in session 136 (proposed in session 135), and built. Milestone 68 (changing the working directory) approved in session 138 (proposed in session 137), and built. Milestone 69 (`lughac init`) approved in session 142 (proposed in session 139), and built. Milestone 70 (`lughac lsp`, live diagnostics) approved in session 145 (proposed in session 144), and built. Milestone 71 (hover) approved in session 147 (proposed in session 146), and built. Milestone 72 (go-to-definition) approved in session 149 (proposed in session 148), and built. Milestone 73 (completion) approved in session 152 (proposed in session 151), and built. Milestone 74 (signature help) approved in session 155 (proposed in session 154), and built. Milestone 75 (partial syntax trees) approved in session 157 (proposed in session 156), and built. Milestone 76 (block comments) approved in session 159 (proposed in session 158), and built. Milestone 77 (doc comments) approved in session 161 (proposed in session 160), and built. Milestone 78 (snippets) approved in session 163 (proposed in session 162), and built. Milestone 79 (the add-missing-arms quick fix) approved in session 167 (proposed in session 166), and built. Milestone 80 (`lughac fmt`) approved in session 169 (proposed in session 168), and built. Milestone 81 (formatting in the editor) approved in session 171 (proposed in session 170), and built. Milestone 82 (more quick fixes) approved in session 173 (proposed in session 172), and built. Milestone 83 (Find References and Rename) approved in session 175 (proposed in session 174), and built. Milestone 84 (`lughac test`) approved in session 179 (proposed in session 178), and built. Milestone 85 (tests in the editor) approved in session 181 (proposed in session 180), and built. Milestone 86 (`lughac explain`) approved in session 183 (proposed in session 182), and built. Milestone 87 (linking C libraries) approved in session 185 (proposed in session 184), and built. Milestone 88 (callbacks and handles) approved in session 188 (proposed in session 187), and built. Milestone 89 (strings from C) approved in session 190 (proposed in session 189), and built. Milestone 90 (C structs by value) approved in session 190 (proposed in session 189), and built in session 191.
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

## Decisions behind milestone 75 (discovery, session 156)

The language server's features read the checker's findings, and the checker needs a tree: until now one syntax error anywhere left none. The parser already recovers at statements and items to report more errors, and discarded what it had built.

38. **Partial trees, for the language server only:** the parser can hand back the program it recovered: every import, item and statement that parsed, without the broken statements and items. `lughac check`, `build` and `run` are unchanged: a syntax error stops them before checking, with the same diagnostics.
39. **The server checks the current text** whenever every module lexes, syntax errors or not. Hover, go-to-definition, completion and signature help then answer from it everywhere but the broken statements. Only when a module doesn't lex do completion and signature help fall back to the last check that produced a tree (milestone 73's rule).
40. **Diagnostics:** while a module has a syntax error, the server publishes only the syntax errors, as before; what the checker finds in the partial program is used, not shown, since a dropped statement makes later names look undefined.
41. **Success:** milestone 75, tag `m75`. Tests: the parser's partial results, and the server's features in files with syntax errors.

## Decisions behind milestone 76 (discovery, session 158)

The user wants JSDoc-style documentation, `/** … */`, shown by the editor. Lugha has had only `//` comments (spec §2: "v0 has no block comments"), so block comments come first, as a language change; doc comments in the language server follow as milestone 77.

42. **`/* … */` is a comment** wherever whitespace may be: between any two tokens, over any number of lines. Its text is ignored, `//` and `"` inside it included; inside a string, `/*` is text.
43. **They nest:** `/* a /* b */ c */` is one comment (as Rust, Swift, Kotlin), so code holding a block comment can be commented out. `/**` and `/*!` are ordinary block comments to the compiler.
44. **Errors:** a `/*` never closed is E0113 at the outermost one still open, with help to add `*/`; lexing ends there. Inside an interpolation, a block comment must end on its line, as the string must: a line break there ends the string, as any line break inside an interpolation does (E0102). A `*/` outside a comment stays the tokens `*` and `/`.
45. **Compatibility:** every valid program stays valid with the same meaning. Outside strings and `//` comments, `/*` never occurs in valid code today, since `*` is never a prefix operator (`a / *b` is a syntax error).
46. **Success:** milestone 76, tag `m76`. Lexer tests, a `tests/programs/m76/` case and a rejected one, a §10 program; the VS Code grammar highlights them (nested) and `Shift+Alt+A` toggles them.

## Decisions behind milestone 77 (discovery, session 160)

The second step the user asked for: JSDoc-style documentation.

47. **A doc comment** is a `/** … */` block comment (not `/**/`) followed, apart from whitespace and a `public`/`private`, by a declaration: a function, method, extern, struct, field, enum, variant, interface or interface method. To the compiler it stays a comment; nothing about a program changes.
48. **Its text** is Markdown, with each line's leading `*` removed. `@param name text` documents a parameter, `@returns text` the result; other `@tags` show as written.
49. **Where it shows:** hover (under the header; hover now also answers on type and variant names: `struct Point`, `Circle(f64)`), completion items, and signature help (the description, and the active parameter's `@param` text).
50. **std and the prelude are documented:** every public `std` function and every prelude helper completion offers gets a doc comment, its text from the spec's tables; a test fails on one without.
51. **Success:** milestone 77, tag `m77`. Tests in `tests/lsp_docs.rs`; the spec's §2 names the convention.

## Decisions behind milestone 78 (discovery, session 162)

Snippets for the editor; no language change.

52. **Static snippets** in the VS Code extension: declarations (`fun`, `main`, `struct`, `enum`, `impl`, `interface`, `import`, a `/** … */` doc comment) and control flow (`if`, `ifelse`, `while`, `for … of`, `for … in a..b`, `if let`, `let … else`, `match`). Each expands to valid Lugha once its placeholders are filled with their defaults; a test checks it with `lughac check`.
53. **Call placeholders:** choosing a function, method, associated function or variant with a payload from completion inserts its call with a placeholder per parameter (`max(${1:a}, ${2:b})`), when the editor supports snippets and no `(` already follows.
54. **Postfix `.match`:** on a value whose type is an enum, `Option` or `Result`, completion after `.` offers `match`, which replaces `value.` with a `match value { … }` holding every variant, a placeholder per payload slot.
55. **Success:** milestone 78, tag `m78`. Tests: the snippets file expands and checks; `tests/lsp_snippets.rs` for the server's.

## Decisions behind milestone 79 (discovery, session 166)

The language server's first code actions: fixing a non-exhaustive `match`.

56. **On an E0417**, `textDocument/codeAction` offers two quick fixes: **Add missing arms** — one arm per missing pattern, every one the checker found (not only the three the message lists), each pattern complete (`Rect(_, _)`, `Some(Dot)`); and **Add a `_` arm** — one catch-all. A match on integers, strings or interface values gets only the second.
57. **Each new arm's body is `panic("not handled")`:** it fits any match's type, so the error clears at once, and running into one says why.
58. **Placement:** the arms go before the match's closing `}`, one per line at its arms' indentation; a comma is added after the last existing arm if it lacks one. Only offered when the file's current check is free of syntax errors.
59. **Success:** milestone 79, tag `m79`. Tests in `tests/lsp_code_actions.rs`.

## Decisions behind milestone 80 (discovery, session 168)

A formatter, tree-based like rustfmt (the user's choice over a token-based one); the editor's Format Document and format-on-save follow as milestone 81.

60. **`lughac fmt [paths…] [--check]`** rewrites each `.la` file (a folder: every `.la` file under it) in the one canonical style; `--check` writes nothing and lists the files that would change (exit 1 if any). A file with syntax errors is reported, not formatted.
61. **The style:** 4-space indents; at most 100 characters a line; `{` on the line of what opens it; a call, array, tuple, struct literal, parameter list or `import { … }` that doesn't fit breaks one element per line with a trailing comma; one space around binary operators and `=>`, after `,` and `:`; one blank line at most between items or statements, kept where written; parentheses only where precedence needs them; literals and names exactly as written.
62. **Comments:** a comment on its own line goes above the item, statement, field, variant or arm after it; a comment at the end of a line stays at the end of that construct's line. A comment anywhere else (inside a multi-line expression) leaves that whole statement or item exactly as written.
63. **Safety:** before writing, the formatter re-parses its output and compares the tree with the original's (positions ignored) and the comments' texts with the original's; on any difference it writes nothing and fails with an internal error (exit 2).
64. **Idempotent:** formatting formatted code changes nothing.
65. **Success:** milestone 80, tag `m80`. Tests: every program in the repo formats, verifies and is idempotent; style cases one by one.

## Decisions behind milestone 81 (discovery, session 170)

Formatting in the editor, through the language server and `lughac fmt`'s formatter (the user's choices).

66. **Format Document:** `lughac lsp` answers `textDocument/formatting` for the whole document, from the editor's text (unsaved changes included), with the formatter of milestone 80. No range or on-type formatting.
67. **Format on save** is the user's setting (`editor.formatOnSave`); the VS Code extension only makes itself Lugha's default formatter, and its README shows the setting.
68. **Failures:** a document with syntax errors gets no edits and no message (its diagnostics already show the errors, and a save still saves); one that fails the formatter's verification gets no edits and one error message saying it was left as written.
69. **Safety:** the server still writes no file and runs no `git`: formatting returns edits the editor applies.
70. **Success:** milestone 81, tag `m81`. Tests in `tests/lsp_formatting.rs`; the extension's version bumped.

## Decisions behind milestone 82 (discovery, session 172)

Three more quick fixes in `lughac lsp` (the user's choice of three of four candidates; a general "did you mean" for any name is left out).

71. **Make it mutable (E0501):** when the binding assigned to — the place's root name, `x` in `x = 1`, `x.f = 1` or `x[0] = 1`, or `for v of mut x` — was declared by a `let` without `mut`, **Make `x` mutable** inserts `mut ` after that `let` (in the same file). Parameters, loop variables, `self` and `match` bindings get no fix: they can't be made `mut` there.
72. **Fix a misspelled variant (W0103):** when a `match` binding that nothing uses is spelled within 2 edits of a variant of the type it matches (or differs from one only in case), **Change to `V`** replaces it with that variant — the closest one; on a tie, none. Otherwise there is no fix.
73. **Add a std import (E0301):** when the undefined name is one of the std modules (`math`, `random`, `env`, `fs`, `process`), used as `name.` and not already bound by an import, **Add `import std.name;`** inserts the import after the file's last import, or at the file's start when it has none.
74. **Each fix is marked preferred** (the editor's auto-fix applies it) and comes only from a check of the current text that parsed, as milestone 79's.
75. **Success:** milestone 82, tag `m82`. Tests in `tests/lsp_code_actions.rs`; each fixed program checks clean of the fixed diagnostic.

## Decisions behind milestone 83 (discovery, session 174)

Find References and Rename in `lughac lsp` (the user's choices; Go to Implementations later).

76. **Find References** (`textDocument/references`): from a name — at a use or its declaration — every place written that resolves to the same declaration, in every file the server has checked (open files, their imports, the mains in `lugha.entries` and the mains already seen), with the declaration itself when the client asks for it. Locals, parameters, functions, methods, types, fields, variants and interface methods; a module (`geo` in `geo.f`) finds its uses too. Names declared in `std` or the prelude: their uses in the checked files.
77. **Rename** (`textDocument/prepareRename`, `textDocument/rename`): the same places, each replaced by the new name. Refused, with the reason as the error message, for: a name declared in `std` or the prelude; a module (it would rename a file); a new name that isn't an identifier or is a keyword; a module-level item visible outside its file (not `private`) declared in a module no checked main imports ("open or list in `lugha.entries` a main file that uses it").
78. **Self-check:** before answering, the server applies the rename in memory and checks every affected main again. Every renamed place must resolve to the renamed declaration, nothing else may newly resolve to it, and no file may gain an error. Otherwise the rename is refused with the first problem found ("`total` would clash with …", "a use would point elsewhere").
79. **Safety:** as before — the server writes no file and runs no `git`; the edits go to the client.
80. **Success:** milestone 83, tag `m83`. Tests in `tests/lsp_references.rs` and `tests/lsp_rename.rs`.

## Decisions behind milestone 84 (discovery, sessions 177–178)

A test runner, `lughac test` (the user's choices: `@test` over `test fun` and a naming convention, with `@` kept for Python-style decorators later).

81. **`@test`** before a top-level `fun` marks a test. `@` starts an attribute; `@test` is the only one (any other, `@inline`, is E0210 "unknown attribute"; general decorators are a later milestone). A test takes no parameters, has no type parameters and returns nothing; `@test` on anything else (a method, an extern, a struct, a function with parameters or a return type) is E0434. Outside `lughac test`, tests are checked like other functions but never compiled or run, and a program may still have no `main` only under `lughac test`.
82. **Where:** tests may be in any `.la` file, beside the code they test (so they reach its `private` items) or in a `tests/` folder through imports. `lughac test [paths…]` takes files and folders like `lughac fmt` (default: the current folder; hidden folders and `target/` skipped); each file holding tests is built, with its imports, into one test executable; a `main` in it is ignored.
83. **Assertions,** usable in any code: `assert(cond)`, `assert_eq(a, b)` and `assert_ne(a, b)` (`a` and `b` of one type that is `Eq` and printable). A failure panics like `panic` does, at the call: `assertion failed`, or `assert_eq failed: left 41, right 42`. Any panic, overflow, bounds error or stack overflow fails the running test.
84. **Running:** each test runs in its own process (the file's executable, told which test to run), so one failure never stops another; up to one per CPU at once, each with a time limit (`--timeout SECS`, default 10) after which it is killed and fails. `--filter TEXT` runs only tests whose `module.name` contains `TEXT`. What a test prints is captured and shown only when it fails.
85. **Output and exit:** one line per test in a fixed order (file, then declaration order) — `test geo.area_of_unit ... ok` / `FAILED` with the panic message, location and captured output — then `N tests: P passed, F failed`. Exit 0 when every test passes (also when there are none, with `0 tests`); 1 when a test fails or a file has errors (printed as diagnostics; its tests don't run); 2 for lughac's own errors.
86. **Success:** milestone 84, tag `m84`. Tests in `tests/test_runner.rs` and `tests/programs/`; the editor's "Run test" link is a later milestone.

## Decisions behind milestone 85 (discovery, session 180)

Running tests from VS Code (the user's choices: VS Code's Testing view and gutter over "Run test" links; on click only, not in untrusted folders).

87. **Listing:** `lughac test --list [paths…]` prints every test the paths hold — `module.name`, file, line and column of its name — without building anything (lex and parse only); with `--json`, one JSON object per line.
88. **Results for tools:** `lughac test --json` prints one JSON object per test as it finishes — name, file, line, `passed`/`failed`/`timed_out`, the panic message and its location when there is one, what the test printed, and how long it took — then a summary object; diagnostics stay on stderr. `--exact` makes each `--filter` match a whole name, and `--filter` may be given more than once.
89. **The extension** shows every test of the workspace folders' `.la` files (hidden folders and `target/` skipped) in VS Code's Testing view, with a ▶ in the gutter beside each `@test`, refreshed when a `.la` file is saved, created or deleted. Running one, a file's or all tests runs `lughac test --json --exact --filter …` in the workspace folder; results mark each test passed or failed, a failure's message shows at its panic location (else at the test), and the test's output goes to the Testing view's output. Cancelling stops the run.
90. **Trust:** tests run only when asked (a ▶, the Testing panel, or VS Code's own continuous run if the user turns it on). In a folder VS Code doesn't trust (Restricted Mode) the extension starts nothing for tests — they are neither listed nor run — since running them runs that folder's code.
91. **Safety:** the extension starts only `lugha.path` (the user's setting, machine scope), with arguments it builds, through `execFile`/`spawn` without a shell; `lughac lsp` is unchanged and still runs nothing.
92. **Success:** milestone 85, tag `m85`. Tests: `tests/test_runner.rs` (the new flags), the extension's unit tests (parsing the JSON, building the arguments, mapping results).

## Decisions behind milestone 86 (discovery, session 182)

Longer explanations of diagnostic codes (the user's choices: every code, examples checked; links from the editor into a VS Code tab; a hint line after diagnostics, as rustc gives).

93. **`lughac explain CODE`** prints the explanation of an error or warning code (`E0501`, `e0501`, `W0103`): its title, what it means and why, an example that reports it and the example fixed, in Markdown. `lughac explain` alone lists every code with its title. An unknown code is exit 2 ("no code E9999"; bad usage).
94. **Every code** of spec §9's table has an explanation, kept in `docs/errors/<CODE>.md` and embedded in `lughac`. Each `lugha` example in them is checked by the tests: one marked `// error: CODE` must report that code (and only diagnostics that follow from it), every other must check without errors. Examples that need several files or a `lugha.toml` (module, package and git codes) are written as `text` blocks and not checked.
95. **The hint:** after the diagnostics of a command in the human format, one line names the codes reported: `For more about E0501, run \`lughac explain E0501\`.`, or for several `For more about E0403 and E0501, run \`lughac explain <code>\`.` (codes in order of first appearance, errors and warnings alike, internal errors excluded). The JSON format is unchanged.
96. **In the editor,** each diagnostic's code links (LSP `codeDescription`) to `lugha-explain:<CODE>.md`; the VS Code extension shows that document read-only in a tab, from the installed `lughac explain`, offline. Explaining runs no program code, so it works in untrusted folders too.
97. **Success:** milestone 86, tag `m86`. Tests: `tests/explain.rs` (every code explained, every example checked, the command, the hint), the updated expected stderr of `tests/programs/`, the extension's unit tests.

## Decisions behind milestone 87 (discovery, session 184)

Linking C libraries, the first step toward GUI programs (the user's choices: declared in `lugha.toml`; pkg-config included; every used package's table applies; packages may ship C files; fixable link problems are diagnostics, exit 1).

98. **`[link]`** is a third manifest table, at most once, in any order with the others. Its lines are `key = ["…", …]`, a one-line array of double-quoted strings, each key at most once: `libs` (library names, linked as `-l<name>`), `paths` (folders searched for libraries, `-L`), `include` (folders searched for headers by this package's C files, `-I`), `c` (C files compiled into the program) and `pkg-config` (package names for the system `pkg-config`). A name is `[A-Za-z0-9_+.-]+`, not starting with `-`; a path is relative, with `/`, inside the package's folder (no absolute path, no `..` leaving it); a `c` file ends in `.c`. Anything else is E0321; a missing folder or file is E0322.
99. **Which tables apply:** the program's own and those of every package with at least one module in the build, in load order, the program first. Libraries are linked once each, in order of first appearance, after every `-L` folder.
100. **pkg-config:** at build time (`build`, `run`, `test`), each package's names go to `pkg-config --libs`, and to `pkg-config --cflags` when that package has C files, run without a shell. Only `-l…`, `-L…` and `-pthread` may come from `--libs`, and only `-I…`, `-D…` and `-pthread` from `--cflags`; any other flag, a missing `pkg-config` or a failing one is E0329 at the `pkg-config` line.
101. **C files** are each compiled on their own (`cc -c`, with that package's `include` folders and `--cflags`), never in the runtime's compile, and linked with the program. One that doesn't compile is E0330 at the `c` line, with the compiler's message. They can't call Lugha functions, and §8's rules for extern calls hold unchanged.
102. **Link failures:** when a build with any `[link]` input fails to link, that is E0331 with the linker's message, exit 1; without `[link]` input it stays an internal error, exit 2, and the command is unchanged.
103. **Nothing runs while checking:** `lughac check` and the language server read and check `[link]` (E0321, E0322) but never start `pkg-config` or the C compiler.
104. **Success:** milestone 87, tag `m87`. Tests: `tests/link_libs.rs` (a static library built by the test, a path package carrying `[link]`, a C file, a fake `pkg-config`, E0321/E0322/E0329–E0331, `check` starting nothing), unit tests for the table, the order and the flag filter; manually, the system zlib.

## Decisions behind milestone 88 (discovery, session 187)

Callbacks from C and handles for C objects, the second step toward GUI programs (the user's choices: named functions and closures, through `user_data` as Rust, Swift, Zig and Go do, never run-time code generation; a `data` marker the caller doesn't write; closures kept alive until exit unless C releases them; handle types in the same milestone).

105. **`extern type Name;`** declares a handle type: an opaque C pointer (`GtkWidget*`), with the usual visibility. Each declaration is its own type. Handles go anywhere a type goes; `==`/`!=` compare addresses (they are `Eq`); nothing else applies — no order, arithmetic, casts, literals, fields or methods. They print as `<Name>`. Only C makes them: an extern's result, or a callback's parameter.
106. **`Option<H>`** of a handle `H` may appear in extern signatures and callbacks: C's `NULL` is `None`, and `None` passes `NULL`. A plain `H` from C is assumed non-null.
107. **A callback parameter** in an extern is written as a function type, `fun(T…): R`, and is a C function pointer. Its parameter and result types are extern types (numbers, `bool`, handles, `Option` of a handle), and one parameter may be written `data`: the `void *` user data C hands back. No strings, arrays, structs, enums or functions inside; no callback as a result or inside another callback (E0409).
108. **`data` and `release` parameters:** an extern parameter written `data` receives the user data, and one written `release` a function `void (*)(void *)` that C calls when it drops the callback. Callers write neither; lughac fills them. An extern has a `data` parameter exactly when one of its callbacks has `data`, at most one of each, and `release` only with `data`; `data` and `release` mean nothing outside extern signatures (E0435).
109. **Arguments:** a callback's argument is a function value of the callback's type without `data`. Where the callback has no `data`, it must be a non-generic function's name or a closure literal that captures nothing, since C keeps no captures (E0436). Where it has `data`, any function value.
110. **Renamed externs:** `extern "symbol" fun name(…)` declares the C function `symbol` under the Lugha name `name`, so one C function can be declared several times with different callback types (GTK's `g_signal_connect_data`). E0306 compares declarations by their Lugha names; `symbol` must be a C identifier not starting with `lugha_`.
111. **Lifetimes:** a closure passed to C is copied into memory Boehm scans but never collects, so it and its captures stay alive while C may call it: until the program exits, or until C calls the `release` function. Calling a callback after releasing it, or releasing it twice, is C's error.
112. **Threads and panics:** C must call a callback on the thread that registered it (calling it from another thread is undefined). A panic inside a callback ends the program as anywhere else.
113. **No `unsafe`:** the `extern` declaration stays the one place a program vouches for C (§7 "Safety"); a wrong callback type is undefined behavior, as a wrong extern signature already is.
114. **Success:** milestone 88, tag `m88`. Tests: `tests/callbacks.rs` (C files through `[link]`: handles, `NULL`, named functions, closures kept across collections, `release`, renamed externs, E0409/E0435/E0436), unit tests; byte-identical IR for programs without them; manually, a GTK 4 window counting clicks.

## Decisions behind milestones 89 and 90 (discovery, session 189)

The rest of the GUI work (the user's choices: discover strings, structs and a GTK package together, approve once, build in order; strings from C both as results and in callbacks, invalid UTF-8 replaced; an explicit `extern struct`).

115. **Strings from C:** an extern may return `string` or `Option<string>`, and a callback's parameters may be `string` or `Option<string>`. lughac copies C's NUL-terminated text into a new string; C keeps and frees its own memory, and Lugha never frees it.
116. **UTF-8:** each invalid sequence in C's text becomes U+FFFD, as `utf8_decode` in the prelude defines a sequence.
117. **`NULL`:** `None` for `Option<string>`; for a plain `string`, a panic: `C gave NULL for a \`string\`; declare it \`Option<string>\``.
118. **`Option<string>` arguments:** `None` passes `NULL`, `Some(s)` the text. A callback may not return a string: C would keep a pointer into Lugha's memory (E0409).
119. **Success:** milestone 89, tag `m89`. Tests: `tests/c_strings.rs` (C files through `[link]`: copies, `NULL`, U+FFFD, callbacks, the panic).
120. **`extern struct Name { … }`** declares a struct with C's layout: fields in order with C's alignment. Its fields are numbers, `bool`, handles, `Option` of a handle and other extern structs; it has no type parameters (E0437). Otherwise it is an ordinary struct: literals, fields, `==`, methods, printing.
121. **By value:** extern structs may be extern parameters and results and callback parameters and results; a plain struct there is still E0409, whose help names `extern struct`.
122. **The calling convention** is C's on x86-64 System V: a struct over 16 bytes goes in memory (`byval` arguments, `sret` results); a smaller one is split into eightbytes, each in integer or SSE registers by its fields, as a C compiler would. lughac targets only x86-64 Linux; another target is an internal error, never a guess.
123. **Not yet:** pointers to structs, arrays of structs to C, packed structs, unions, bit-fields.
124. **No change** to how plain structs are laid out or passed between Lugha functions.
125. **Success:** milestone 90, tag `m90`. Tests: the classification against clang's lowering, `tests/extern_structs.rs` (every shape both ways, nested, over 16 bytes, past six integer arguments, callbacks).

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
| 75 | Partial syntax trees | the language server checks text with syntax errors; the CLI unchanged |
| 76 | Block comments | `/* … */`, nested; E0113 |
| 77 | Doc comments | `/** … */` before declarations, `@param`/`@returns`; shown by the language server; std and the prelude documented |
| 78 | Snippets | static declaration and control-flow snippets; call placeholders; postfix `.match` |
| 79 | Add missing arms | quick fixes on E0417: every missing arm, or a `_` arm, bodies `panic("not handled")` |
| 80 | `lughac fmt` | the tree-based formatter: canonical style, comments kept, self-verified, `--check` |
| 81 | Formatting in the editor | `textDocument/formatting` in `lughac lsp`; the extension the default Lugha formatter |
| 82 | More quick fixes | `let mut` for E0501, a misspelled variant for W0103, a std import for E0301 |
| 83 | Find References and Rename | every use of a name across the checked files; renaming them, refused unless safe and self-checked |
| 84 | `lughac test` | `@test` functions, `assert`/`assert_eq`/`assert_ne`, each test in its own process, in parallel, with a time limit |
| 85 | Tests in the editor | `lughac test --list`/`--json`/`--exact`; VS Code's Testing view and gutter; never in untrusted folders |
| 86 | `lughac explain` | an explanation for every code, examples checked; the hint after diagnostics; code links in the editor |
| 87 | Linking C libraries | `[link]` in `lugha.toml`: `libs`, `paths`, `include`, `c`, `pkg-config`; every used package's table; E0329–E0331 |
| 88 | Callbacks and handles | `extern type`; functions and closures passed to C (`data`, `release`); `extern "symbol" fun`; E0435, E0436 |
| 89 | Strings from C | `string` and `Option<string>` extern results, `Option<string>` arguments, string callback parameters; copies, U+FFFD |
| 90 | C structs by value | `extern struct`; passed by value to and from C (externs, callbacks) as on x86-64 System V; E0437 |

## §12 Open questions — additions

- [ ] **Editing a box in place** (v16). `r.with(fun(mut x: T) { … })` could avoid the copy `update` makes of big contents, if the closure can be kept from reaching the same box.
- [ ] **A bounds-checked byte buffer** (v16, session 135): the safe answer to pointer-style code over raw bytes.
