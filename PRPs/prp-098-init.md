## FEATURE: `lughac init` — start a Lugha program or library in a folder, with the right files for each; milestone 69.

**Status:** approved 2026-10-15 ("approve PRP-098 lughac init, build it") — built in session 142; proposed in session 139.
**Milestone:** 69 (one PRP; tag `m69` at merge)
**Spec:** `Language v16 Draft.md` decisions 9–12; spec §6 (packages, `[package]`), §9 (the CLI table), §11 (row 69).

**Decisions:**
- builds on v15's package layout (MEMORY 98–99: `[package] name`, `source`, the implicit package for `examples/`);
- no open entries in DECISIONS.md apply.

## OBJECTIVE
```
$ lughac init oyster               $ lughac init shapes --lib
created oyster/lugha.toml          created shapes/lugha.toml
created oyster/src/main.la         created shapes/src/shapes.la
created oyster/.gitignore          created shapes/examples/hello.la
                                   created shapes/.gitignore
$ lughac run oyster/src/main.la    $ lughac run shapes/examples/hello.la
Hello from oyster!                 Hello, world, from shapes!
```

## CONTEXT
- **Starting state:** no `init`; the CLI has `build`, `run`, `check`, `update`, `spec` (`src/driver/mod.rs`, clap derive).
- **Ending state:** decisions 9–11.
- **Related existing code:**
  - `src/driver/mod.rs` (the CLI, exit codes); a new `src/driver/init.rs`;
  - `src/lexer` (keywords, for name checks);
  - `tests/cli.rs` (the binary's tests; 342 lines, so `init`'s tests go in a new `tests/init.rs`).
- **Open decisions that must be resolved first:** none.

### Design
1. **CLI:** `Command::Init { folder: Option<PathBuf>, lib: bool, name: Option<String> }`.
2. **Name:** `--name`, else the folder's last component (after resolving `.`). Valid means `[A-Za-z_][A-Za-z0-9_]*`, not a keyword (the lexer's list) and not `std`. Otherwise exit 2 with `error: `my-shell` isn't a valid package name` and `= help: try `lughac init my-shell --name my_shell`` (non-identifier characters replaced by `_`).
3. **Order of work:**
   - validate the name;
   - refuse if `<folder>/lugha.toml` exists (`error: <folder> already has a lugha.toml`, exit 2);
   - create the folders;
   - write each file only if it doesn't exist (`create_new`), printing `created <path>` or `kept    <path> (already exists)`.

   An I/O failure is exit 2 with the system's message, and files already written stay (reported).
4. **Contents:**
   - **`lugha.toml`:** `[package]`, `name = "<name>"`, `version = "0.1.0"`, `source = "src"`.
   - **`src/main.la` (a program):** a header comment, then `fun main() { println("Hello from <name>!"); }`.
   - **`src/<name>.la` (a library):** a header comment, then a documented `public fun greet(who: string): string = "Hello, {who}, from <name>!";`.
   - **`examples/hello.la` (a library):** `import <name>.<name>;` and a `main` printing `<name>.greet("world")`.
   - **`.gitignore`:** `/target` and the default build outputs (`/main` for a program, `/hello` for a library's example).
5. **Last line:** a next-step hint, e.g. `run it with: lughac run src/main.la`.

### Risk
- **Writing into a folder that has files:** guarded by `create_new` and the manifest check; a test checks an existing file is byte-for-byte unchanged.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- **Code:** the above.
- **Spec v16:**
  - §9 CLI table: `lughac init [folder] [--lib] [--name N]`, with the layouts, the rules and the exit codes;
  - §11 row 69; the changes summary.
- **Docs:** CLAUDE.md (COMMANDS, STACK, tree: `driver/init.rs`, `tests/init.rs`), README (getting started with `init`), TODO, CHANGELOG, MEMORY plan and built entries, CONTEXT; tag `m69`.

### Must NOT Do
- **No overwriting,** no `--force`, no `git`, no network, no prompts.
- **No new manifest keys.**

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **Exit 2 (bad usage, CLAUDE.md rule 6):** an existing `lugha.toml`, an invalid name, an I/O error. Messages name the path and suggest the fix.
- **Exit 0** on success, including when some files were `kept`.

## SECURITY CONSIDERATIONS
- **Writes only inside the given folder;** paths are built from the folder and fixed relative names, the package name being an identifier (no `/` or `..`).
- **No `unsafe`, no dependencies, no external commands.**

## TESTS TO WRITE
`tests/init.rs`:
- [ ] **Program:** `lughac init p` creates the three files; `lughac check p/src/main.la` passes; `lughac run p/src/main.la` prints `Hello from p!`.
- [ ] **Library:** `lughac init shapes --lib` creates the four files; `lughac run shapes/examples/hello.la` prints `Hello, world, from shapes!`.
- [ ] **Current folder:** `lughac init` takes the folder's name.
- [ ] **`--name` overrides** the folder's name.
- [ ] **Refusals:** an existing `lugha.toml` (exit 2, nothing written); `my-shell` (exit 2, hint `my_shell`); `fun` and `std` as names (exit 2).
- [ ] **Kept:** an existing `.gitignore` stays byte-for-byte, reported `kept`, exit 0.

## ROLLBACK PLAN
- Branch `prp-098-init`; tag `m69`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] All tests pass.
- [ ] Spec at milestone 69; docs updated.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] fmt, clippy and tests pass; CI green.

## VALIDATION
```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test
cargo test --test init
```
