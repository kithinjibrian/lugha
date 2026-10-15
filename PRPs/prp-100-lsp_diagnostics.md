## FEATURE: `lughac lsp` — a language server with live diagnostics, and the VS Code extension switched to it; milestone 70.

**Status:** proposed 2026-10-15 — session 144 (discovery the same session); waits for approval.
**Milestone:** 70 (one PRP; tag `m70` at merge). The first of three language-server PRPs: hover types and go-to-definition follow, each its own PRP.
**Spec:** `Language v16 Draft.md` decisions 13–17; spec §9 (the CLI table, diagnostics, exit codes), §11 (row 70).

**Decisions (discovery, session 144):**
- three PRPs, live diagnostics first;
- crates `lsp-server` + `lsp-types` (with `serde_json`), no async runtime;
- the extension uses `vscode-languageclient` and drops its own on-save runner (no fallback to `lughac check`);
- a module is checked through `lugha.entries` and every main file the server has already checked that loaded it; else alone without E0303;
- a compiler panic is caught per check and shown as one internal-error diagnostic; the server stays up.
- No open entries in DECISIONS.md apply.

## OBJECTIVE
Type in a `.la` file and, a moment after you stop, errors and warnings appear or disappear, without saving. Unsaved changes in other open files count too: fix a function's type in `geo.la` without saving, and the error it caused in `main.la` goes away. Errors still land in the files they belong to, with codes, labels and help, exactly as `lughac check` reports them.

```
$ lughac lsp      # speaks LSP on stdin/stdout; editors start it, people don't
```

## CONTEXT
- **Starting state:**
  - the extension (PRP-099) runs `lughac check --offline --diagnostics=json` on open and save, with its own target planning (`targets.ts`), ownership (`store.ts`), UTF-16 conversion (`diagnostics.ts`) and process handling (`runner.ts`);
  - `driver::pipeline::loaded` reads files with `std::fs::read` and writes a changed `lugha.lock`; `modules::load` already takes any `Read` function;
  - `driver::guard` turns a panic into an internal error for the CLI;
  - checking real programs takes ~30–50 ms in a debug build (oyster, glowworm, the largest test programs), so a whole-program re-check after each pause is fast enough without incremental checking.
- **Ending state:**
  - New `src/driver/lsp/`:
    - `mod.rs` — `serve(stdin, stdout)`: the `lsp-server` connection, `initialize` (capabilities: full text sync; `positionEncoding` UTF-16), the message loop, debouncing, `shutdown`/`exit`;
    - `documents.rs` — open documents (URI → text, version) and the overlay `Read` (open text first, then disk);
    - `targets.rs` — has `fun main` (from the lexer's tokens: `fun` `main` `(` at brace depth 0), entries, seen mains;
    - `publish.rs` — `Diagnostic` + `Source` → `lsp_types::Diagnostic` (UTF-16 ranges, code, `help`, related labels), ownership per check (which check reported which file), `<std>`/`<prelude>` files dropped;
    - `check.rs` — one check of a main file under `catch_unwind`, offline, never writing a lock;
  - `src/driver/mod.rs`: `Command::Lsp`; `src/driver/pipeline.rs`: `loaded` and `front` take the `Read` and whether a lock may be written (the CLI passes `std::fs::read` and yes, unchanged);
  - `tests/lsp.rs` — `lughac lsp` over real stdio;
  - `Cargo.toml`: `lsp-server`, `lsp-types`, `serde_json`;
  - **Extension** `editors/vscode/` 0.2.0:
    - `src/extension.ts` rewritten around `LanguageClient` (server `lugha.path lsp`, documents `{ language: "lugha", scheme: "file" }`, configuration section `lugha` synchronized), a clear notification when the server can't start;
    - `src/server.ts` — pure: the server command and arguments from settings (tested without `vscode`);
    - deleted: `src/runner.ts`, `src/store.ts`, `src/diagnostics.ts`, `src/targets.ts`, their tests and `test/fixtures/` (the logic moves to Rust, where `tests/lsp.rs` covers it);
    - `package.json`: `vscode-languageclient` as the first runtime dependency, `engines.vscode` `^1.91.0` (its requirement), `@types/vscode` `~1.91.0`; `.vscodeignore` stops excluding `node_modules/` so the dependency ships in the `.vsix`;
  - docs: spec v16 (§9 CLI table and a `lughac lsp` paragraph, §11 row 70, changes summary), CLAUDE.md (STACK crates, COMMANDS, tree), README, the extension's README, TODO, CHANGELOG, MEMORY, CONTEXT.
- **Related existing code:** `src/driver/pipeline.rs`, `guard.rs`, `source.rs` (offsets → files, `line_col`), `json.rs` (the record the extension reads today), `src/modules/mod.rs` (`Read`, `load`), `src/lexer/`; `editors/vscode/src/*.ts` (the rules being moved).
- **Open decisions that must be resolved first:** none.

### Design
1. **Protocol.** `lughac lsp` takes no arguments. Over stdio it handles `initialize`/`initialized`, `textDocument/didOpen`, `didChange` (full text), `didSave`, `didClose`, `workspace/didChangeConfiguration` (section `lugha`: `entries`), `shutdown`, `exit`; it sends `textDocument/publishDiagnostics`. Unknown requests get `MethodNotFound`; unknown notifications are ignored. `initializationOptions` may carry `{ "entries": [...] }`. Exit code 0 after `shutdown` + `exit`, 1 after `exit` without `shutdown` (as the protocol says).
2. **When it checks:** on open, save and configuration change at once; on change after **200 ms** with no further change (`recv_timeout` in the loop, so no threads). Each pending file is planned once per round.
3. **What it checks** (`targets.rs`), for a changed file `F`:
   - `F` has `fun main` → `F`;
   - else every entry (workspace-relative `.la` inside its folder, existing — the rules `targets.ts` had), plus every main file whose last check loaded `F`; with none, `F` alone, dropping E0303.
   - After each check of a main `M`, the server records the files `M` loaded, replacing what it had for `M`.
4. **A check** (`check.rs`): `pipeline::front` on `M` with the overlay `Read` (open documents' text, else disk), `Mode { offline: true, .. }`, lock writing off, inside `catch_unwind`. Results:
   - checked or failed with program errors → the diagnostics and warnings, by file;
   - an internal failure (an unreadable main file…) → its message as one error at the top of `M`;
   - a panic → `internal compiler error: <message>` at the top of `M` with the guard's help text, and a `window/logMessage`.
5. **Publishing** (`publish.rs`): ranges in UTF-16 from each file's text (overlay or disk); `code`, `source: "lughac"`, message + label + `help: …` as the extension shows today, labels as `relatedInformation`. Each check owns the files it reported; a new result republishes the union of old and new files, merged across checks and deduplicated. Files in `<std>` or `<prelude>` are skipped. Closing a document doesn't clear diagnostics; a check that stops reporting a file clears it.
6. **The extension:** `LanguageClient("lugha", "Lugha", { command: lugha.path, args: ["lsp"] }, { documentSelector: [{ language: "lugha", scheme: "file" }], synchronize: { configurationSection: "lugha" }, initializationOptions: { entries } })`. Its output channel is "Lugha" (`lugha.showOutput` opens it). If the server fails to start or exits at once — a missing `lughac`, or one from before milestone 70 whose `lughac lsp` is a usage error — one notification: `` `lughac lsp` didn't start (`<path>`): set `lugha.path` to a lughac from milestone 70 or later ``, with a "Set lugha.path" button. Highlighting and the editing configuration don't change.

### Risk
- **Medium:** a new long-running process and three crates. Bounded by: stdio only, offline, no writes, a check per pause on small programs, and panics contained.
- **Version skew:** extension 0.2.0 needs `lughac` ≥ m70; the notification says so.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The design above, test-first.
- **Spec v16:** §9 CLI table row `lughac lsp` and a paragraph (what it reports, when, offline, no writes, panics); §11 row 70; the changes summary.
- **Docs:** CLAUDE.md (STACK: the crates and why; COMMANDS; tree: `driver/lsp/`, `tests/lsp.rs`, the extension's new files), README, `editors/vscode/README.md`, TODO (hover and go-to-definition as the next PRPs), CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m70`.

### Must NOT Do
- **No hover, go-to-definition, completion, formatting or code actions** (later PRPs, or not planned).
- **No `git`, no network, no file writes** from the server — not even `lugha.lock`.
- **No workspace scan** for main files (decision 14).
- **No async runtime** (tokio) and no threads beyond what `lsp-server` uses for stdio.
- **No incremental checking or caching of checker results** — whole-program re-checks only.
- **No change to `lughac check`'s output** or to the JSON format.
- **No fallback** in the extension to the old on-save `lughac check`.

### New dependencies
- **Rust** (all MIT or MIT/Apache-2.0, GPL-3.0-compatible):
  - `lsp-server` 0.10 — the stdio message loop, framing and `initialize`/`shutdown` handshake; maintained by rust-analyzer.
  - `lsp-types` 0.97 — typed LSP structures, so protocol mistakes are compile errors.
  - `serde_json` 1 — reading request parameters and building results (`lsp-server` passes `serde_json::Value`).
  - Transitive: `serde`, `serde_derive`, `serde_repr`, `crossbeam-channel`, `url`, `bitflags`, `log` and their dependencies; listed in the PR with their licenses.
- **npm runtime:** `vscode-languageclient` 10.x — MIT — the official client (process lifecycle, restarts, diagnostics wiring). Dev dependencies unchanged.

## ERROR HANDLING REQUIREMENTS
- **Server:** a malformed message from the client is answered with the protocol's error (`lsp-server` reports `ParseError`/`InvalidRequest`) and the loop continues; an I/O error on stdin/stdout ends the server with exit 2 (the client restarts it). A typed `LspError` (thiserror) for those.
- **Checks never fail the server:** program errors are diagnostics; internal failures and panics become one diagnostic each (design 4).
- **Ignored on purpose:** `didChange` for a document not opened (logged), configuration that isn't a list of strings (logged; entries treated as empty).
- **Extension:** start failures → the one notification; later crashes → the client's restart policy (5 in 3 minutes), then its own error message.

## SECURITY CONSIDERATIONS
- **External input:** LSP messages (from the editor) and the files of the workspace. Document text goes only to the lexer, parser and checker, as `lughac check`'s does. `entries` keep PRP-099's validation: relative, `.la`, inside the workspace folder.
- **No network, no `git`, no writes:** offline mode always; lock writing disabled for the server; a test checks that no `lugha.lock` appears and a fake `git` never runs.
- **The executable** is still chosen only by the machine-scoped `lugha.path`.
- **New crates** are third-party code in lughac: three direct, from rust-analyzer and serde, all widely used; listed for review. **Needs the user's review** (CLAUDE.md: dependencies are attack surface).
- No `unsafe`, crypto or credentials. Logs carry file paths and compiler messages only.

## TESTS TO WRITE
- [ ] **`tests/lsp.rs`** (spawns `lughac lsp`, writes framed JSON-RPC, reads replies with a timeout):
  - `initialize` returns full text sync and UTF-16; `shutdown` + `exit` → exit 0; `exit` alone → exit 1;
  - `didOpen` of a program with a type error (and `é` before it) → `publishDiagnostics` with `E0403`, the UTF-16 range, `source: "lughac"`, the help line;
  - `didChange` fixing it → an empty list for that file;
  - **unsaved text wins:** the file on disk has an error, the open text doesn't → no diagnostics; and an unsaved change to an imported module affects the main file's errors;
  - a module with an error, opened after its main file was opened → checked through that main (no E0303, error in the module);
  - a module opened alone → its errors without E0303;
  - `entries` in `initializationOptions` and through `didChangeConfiguration`;
  - a git dependency not in the cache → E0325, no `git` run, no `lugha.lock` written;
  - an unknown request → `MethodNotFound`, and the server still answers afterwards; garbage between messages → the server survives or exits 2, never hangs (timeout).
- [ ] **Unit tests** in `driver/lsp/`: `has_main` (tokens, not comments or strings); UTF-16 positions; ownership merges and clears; a panicking check becomes one internal-error diagnostic; `<std>` files skipped; entries validation.
- [ ] **Extension:** `server.test.ts` (the command from `lugha.path`, args `["lsp"]`); grammar and spec-keyword tests unchanged; `npm run package` includes `vscode-languageclient`.
- [ ] **Manual check** (README), by the user: type an error and see it appear without saving; fix it; change a module without saving and see the main file update; a pre-m70 `lughac` gives the notification.
- [ ] **Regression:** `cargo test` unchanged otherwise; `lughac check` output byte-identical (existing CLI tests).

## ROLLBACK PLAN
- Branch `prp-100-lsp_diagnostics` from `main`; tag `m70`.
- To abandon before merge: delete the branch. After: revert the merge; the extension returns to 0.1.0 and its on-save checks.

## ACCEPTANCE CRITERIA
- [ ] All tests pass; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`, the extension's `npm test`; CI green.
- [ ] Spec at milestone 70; docs updated.
- [ ] No file over 500 lines; no `unsafe`; the new crates and their licenses listed in the PR.
- [ ] The manual check passes in VS Code.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test lsp
cd editors/vscode && npm ci && LUGHAC=../../target/debug/lughac npm test && npm run package
```
