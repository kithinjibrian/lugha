# Lugha for VS Code

Syntax highlighting and live `lughac` diagnostics for [Lugha](../../README.md) (`.la` files).

- **Highlighting:** keywords, types, numbers, `//` and `/* … */` comments (nested ones too), strings with `{…}` interpolation and format specifiers (`{pi:.2}`), `format("{} {:>4}", …)` templates, `char` literals.
- **Editing:** `Ctrl+/` toggles `//`, `Shift+Alt+A` toggles `/* … */`; brackets and quotes close themselves.
- **Snippets:** type a prefix and Tab — `fun`, `main`, `struct`, `enum`, `impl`, `interface`, `import`, `doc`, `if`, `ifelse`, `while`, `forof`, `forin`, `iflet`, `letelse`, `match`. Picking a function, method or variant from completion inserts its call with each parameter as a tab stop; on an enum, `Option` or `Result` value, `value.match` writes out every variant. Needs a `lughac` from milestone 78 for the completion ones.
- **Doc comments:** `/** … */` above a function, method, type, field or variant (Markdown, `@param name text`, `@returns text`) shows under its header in hover and completion, and in signature help with the active parameter's text. Hovering a type or variant name shows it too. std and the prelude are documented. Needs a `lughac` from milestone 77.
- **While a file has a syntax error** (a missing `;`, a half-typed line), all of the below keep working everywhere but the broken statement, and only the syntax errors are underlined. Needs a `lughac` from milestone 75.
- **Signature help:** while you type a call's arguments, its signature with the current parameter highlighted — functions, methods, module functions, variants, built-ins, intrinsics; a generic call with its types filled in when they're known. Needs a `lughac` from milestone 74.
- **Completion:** after `.`, a value's fields and methods (built-in and prelude ones too), a module's items, an enum's variants and associated functions; otherwise the names in scope, the module's functions and types, imports, intrinsics and keywords — each with its type or header. It works while the line you're typing doesn't parse yet, from the last check that did. Needs a `lughac` from milestone 73.
- **Go to Definition** (F12, Ctrl+click): from a variable, function or method, type, field, variant or module (`import geo;`, `geo.f`) to its declaration, across files; an interface call goes to the interface's method. Nothing for `std` and prelude names. Needs a `lughac` from milestone 72.
- **Hover:** a value's type, a binding's declaration (`let total: i64`), a function's or method's header as written (`fun Circle.area(self): f64`), a field's type; with `instance: max<f64>` for a generic call and `converted to Shape` for a value converted to an interface. It works while the program has errors (not while a file doesn't parse). Needs a `lughac` from milestone 71.
- **Diagnostics as you type:** the extension starts `lughac lsp`, a language server that checks the program a moment after you stop typing, saved or not. Unsaved changes in other open files count too. Errors and warnings appear in the files they belong to, with their codes (`E0403`…), labels as related information and `help` in the message.

Needs a `lughac` from milestone 70 or later (one with `lughac lsp`).

## Build and install

Needs Node 22.

```bash
cd editors/vscode
npm ci
npm test                    # server-command and grammar tests
npm run package             # lugha-0.3.1.vsix
code --install-extension lugha-0.3.1.vsix
```

**After any change in `editors/vscode/`**, bump the version in `package.json` (`npm version minor --no-git-tag-version`), rebuild the `.vsix` and reinstall it, then run **Developer: Reload Window**. VS Code skips a `.vsix` whose version is already installed, so an unbumped rebuild needs `code --install-extension … --force`. The language-server features come from `lughac` itself: reinstall it too (`cargo install --path .`) when they changed.

## Settings

| Setting | Default | Meaning |
| --- | --- | --- |
| `lugha.path` | `lughac` | The compiler whose `lsp` command serves diagnostics: a name on `PATH` or an absolute path. Set in user settings only, so a folder you open can't choose what runs. |
| `lugha.entries` | `[]` | Main files, relative to the workspace folder, e.g. `["src/main.la", "examples/demo.la"]`. |

**Which file is checked.** A file with `fun main` is checked itself. A module (no `main`) is checked through the main files in `lugha.entries` and through every main file the server has already checked that imports it — so opening `src/main.la` once is usually enough. With neither, the module is checked alone and the "no `main`" error (E0303) is hidden. `lughac` has no default entry point, so nothing scans the folder for one.

**Offline.** The server never runs `git` and never writes a file. A git package not yet in the cache shows E0325 in `lugha.toml`: run `lughac update main.la` (or a build) in a terminal once.

**Problems.** If `lughac lsp` can't start (a missing `lughac`, or one from before milestone 70), one notification offers to set `lugha.path`. The **Lugha** output channel (`Lugha: Show Output`) holds the server's log. A bug inside the compiler shows as one "internal compiler error" at the top of the file; the server keeps running.

## No suggestions popping up?

If your user settings turn suggestions off (`"editor.quickSuggestions"` off, or `"editor.suggestOnTriggerCharacters": false`), completion still answers **Ctrl+Space** but never pops up on its own. Turn it back on for Lugha only in your user `settings.json`:

```json
"[lugha]": {
    "editor.quickSuggestions": { "other": "on", "comments": "off", "strings": "off" },
    "editor.suggestOnTriggerCharacters": true
}
```

Also make sure the `lughac` VS Code starts is from milestone 73 or later (`cargo install --path .` in the lugha repo), then run **Developer: Reload Window**.

## Manual check before a release

1. Install the `.vsix`; open `tests/programs/m21/nested/main.la`: it is highlighted.
2. Type `let z: i64 = "x";` inside a function, **without saving**: the error appears; delete it: it goes.
3. Open `geo.la` from the same folder, add that line there without saving: the error appears in `geo.la`, with no E0303.
4. Undo it without saving: `main.la` and `geo.la` are clean again.
5. Hover `let`-bound names, a method call and a generic call: their types and headers appear; add an error elsewhere and hover again: still there.
6. Ctrl+click a variable, a method call, a type and an `import`: each opens its declaration (the module's file for the import).
7. Type `p.` after a struct value, `geo.` after an import, `Shape.` and a plain word: the suggestions list fields and methods, the module's items, the variants, and the names in scope.
8. Type `twice(`, `p.scale(`, `max(1.5, ` and `println(`: the signature appears with the right parameter highlighted, moving on each `,`.
9. Delete a `;` in a function: only that syntax error is underlined, and hover, Ctrl+click and `p.` completion still work on the other lines.
10. Write a `/* … */` comment with a nested one inside: all of it is coloured as a comment; select lines and press `Shift+Alt+A`: they become a block comment.
11. Write `/** Doubles. @param n the number */` above a function: hover it, complete it and type its call — the doc shows each time; hover `math.sqrt` and `xs.map`: their library docs show.
12. Type `fun` then Tab, and `forof` then Tab: the snippets expand with tab stops; complete `max` and press Enter: `max(a, b)` with `a` selected; type `s.match` on an enum value and pick `match`: every variant is written out.
13. Set `lugha.path` to `/nonexistent`: one notification.
