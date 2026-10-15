# Lugha for VS Code

Syntax highlighting and `lughac` diagnostics for [Lugha](../../README.md) (`.la` files).

- **Highlighting:** keywords, types, numbers, `//` comments, strings with `{…}` interpolation and format specifiers (`{pi:.2}`), `format("{} {:>4}", …)` templates, `char` literals.
- **Editing:** `Ctrl+/` toggles `//`; brackets and quotes close themselves.
- **Diagnostics:** opening or saving a file runs `lughac check --offline --diagnostics=json`. Errors and warnings appear in the files they belong to, with their codes (`E0403`…), labels as related information and `help` in the message.

## Build and install

Needs Node 22 and a built `lughac`.

```bash
cd editors/vscode
npm ci
LUGHAC=../../target/debug/lughac npm test   # unit, runner and grammar tests
npm run package                              # lugha-0.1.0.vsix
code --install-extension lugha-0.1.0.vsix
```

## Settings

| Setting | Default | Meaning |
| --- | --- | --- |
| `lugha.path` | `lughac` | The compiler: a name on `PATH` or an absolute path. Set in user settings only, so a folder you open can't choose what runs. |
| `lugha.entries` | `[]` | Main files, relative to the workspace folder, e.g. `["src/main.la", "examples/demo.la"]`. |

**Which file is checked.** A file with `fun main` is checked itself. A module (no `main`) is checked through `lugha.entries`, so its errors appear as the main files see them; with no entries it is checked alone, and the "no `main`" error (E0303) is hidden. `lughac` has no default entry point, so the extension doesn't guess one.

**Offline.** Checks never fetch git packages. A package not yet in the cache shows E0325 at its import: run `lughac update main.la` (or a build) in a terminal once.

**Problems.** A missing compiler shows one notification. If `lughac` fails (exit 2), its output goes to the **Lugha** output channel and a status-bar warning appears; earlier diagnostics stay.

## Manual check before a release

1. Install the `.vsix`; open `tests/programs/m21/nested/main.la`: it is highlighted.
2. Add `let z: i64 = "x";` to a function in `geo.la`, save `main.la`: the error appears in `geo.la`.
3. Set `"lugha.entries": ["tests/programs/m21/nested/main.la"]`, save `geo.la`: the same error, no E0303.
4. Set `lugha.path` to `/nonexistent`, save: one notification.
