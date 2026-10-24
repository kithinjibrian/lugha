## FEATURE: Executables built for any x86-64 CPU by default, with `--cpu` to choose a newer level or this machine's own; milestone 130 (v17), roadmap step A1.

**Status:** proposed 2026-10-24 (session 310), the first step of the portability roadmap (TODO.md, "Portability roadmap"). Discovery: (1) the default is the baseline `x86-64`, as Rust, Go, gcc and clang default to; (2) `--cpu` takes a fixed list, `x86-64`, `x86-64-v2`, `x86-64-v3`, `x86-64-v4` or `native`, anything else being exit 2; (3) it changes only LLVM's code for the program, so the runtime and `[link]` C files stay at `cc`'s default.

## OBJECTIVE
Today `host_machine` (`src/codegen/mod.rs`) builds for this machine's exact CPU and features (`get_host_cpu_name`, `get_host_cpu_features`). At `-O2` LLVM may then use instructions such as AVX-512, so a binary built here can die with an illegal instruction on an older x86-64 PC. After this change `lughac build app.la` produces code any x86-64 CPU runs, and `lughac build app.la --cpu=native` gives today's behaviour on request. The same applies to `run` and `test`.

## CONTEXT
- Starting state: `host_machine(opt)` in `src/codegen/mod.rs` (314 lines) is used by `emit_object` and `emit_object_modules`. The driver's shared `Options` (`src/driver/mod.rs`, 391 lines) holds `-O`. `lughac test` builds through `src/driver/test.rs` and `test_build.rs`.
- Ending state:
  - `codegen::Cpu`, a `pub` enum: `X86_64` (default), `V2`, `V3`, `V4`, `Native`. It maps to LLVM's CPU names `x86-64`, `x86-64-v2`, `x86-64-v3` and `x86-64-v4` with no extra features. `Native` keeps today's host name and features.
  - `emit_object` and `emit_object_modules` take a `Cpu`.
  - `--cpu` is a clap `value_enum` on `build`, `run` and `test`. An unknown value is clap's usage error (exit 2), which lists the accepted values. `check` and `lsp` don't take it, since they emit no code.
  - The target triple, data layout and relocation model are unchanged.
- Related existing code: `src/codegen/mod.rs`, `src/driver/mod.rs`, `src/driver/build.rs`, `src/driver/pipeline.rs`, `src/driver/test.rs`, `src/driver/test_build.rs`.
- Spec: §9's command table gets a `--cpu` row beside `-O0` / `-O2`, plus a sentence that executables run on any x86-64 CPU unless `--cpu` asks otherwise; §11 gets row 130; §0's v17 summary gets a line. Approving this PRP approves these spec edits.
- Open decisions that must be resolved first: none. The roadmap's later steps (A3 `--target`, other CPUs' levels) will extend `Cpu` per target. That is not decided here.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- The default build uses CPU `x86-64` and an empty feature string, for both `-O0` and `-O2`.
- `--cpu=native` produces exactly today's target machine.
- Thread the `Cpu` from the CLI to every `emit_object*` call, including `lughac test`'s builds.
- Document `Cpu` and each variant (`///`), and comment why the default is the baseline.
- Update the spec (as above), README's usage lines, CHANGELOG and CLAUDE.md's COMMANDS block (`[--cpu=…]`) and its v17 milestone list.

### Must NOT Do
- Don't pass `-march` to `cc` for the runtime or `[link]` C files (discovery answer 3). Their objects and cache entries stay as they are.
- Don't accept arbitrary LLVM CPU names. LLVM only warns on an unknown one and builds generic code, so a typo would pass unnoticed.
- Don't add `--target` or any non-x86-64 handling. That is roadmap step A3.
- Don't change `--emit=ir` output: the IR has no CPU attributes today, and the triple and data layout don't depend on the CPU.

### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- An unknown `--cpu` value is clap's error, exit 2 (architecture rule 6: bad CLI usage).
- No new `CodegenError` variants. A missing LLVM target is already `CodegenError::Emit`.

## SECURITY CONSIDERATIONS
- `--cpu` is validated by clap against a fixed list before it reaches LLVM. No new external input, no `unsafe`, no new process runs.

## TESTS TO WRITE
- [ ] `src/codegen/mod.rs` unit: the target machine for each `Cpu` has the expected CPU name (`x86-64` … `x86-64-v4`) and an empty feature string; `Native`'s CPU and features equal the host's.
- [ ] `tests/codegen.rs`: a program whose hot loop is `x.count_ones()` built at `-O2`. Its object holds no `popcnt` (bytes `F3 0F B8` or `F3 48 0F B8`) by default and holds one with `--cpu=x86-64-v2`. Both run and print the same result.
- [ ] `tests/cli.rs`: `build --cpu=native` and `run --cpu=x86-64-v3` succeed. (v3 only when the host supports it: the test reads `/proc/cpuinfo` for `avx2` and otherwise uses `x86-64-v2`.)
- [ ] `tests/cli.rs`: `build --cpu=zen4` is exit 2, its stderr naming the accepted values, and no file is written.
- [ ] `tests/test_runner.rs`: `lughac test --cpu=native` runs a passing test.

## ROLLBACK PLAN
- Branch to return to: `main`.
- State: host CPU and features as before. Revert the branch's commits.

## ACCEPTANCE CRITERIA
- [ ] A default `-O2` build contains no instruction beyond baseline x86-64 (shown by the `popcnt` test)
- [ ] `--cpu=native` matches today's behaviour
- [ ] `--cpu` with an unknown value is exit 2
- [ ] `cargo clippy --all-targets -- -D warnings` passes
- [ ] `cargo fmt --check` passes
- [ ] Every `pub` item has a doc comment
- [ ] CHANGELOG.md updated

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test --lib`, `cargo test --test codegen`, `--test cli`, `--test test_runner`, `--test programs` (codegen changed for every program), `--test spec`
