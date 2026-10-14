## FEATURE: `std.random` — a seeded or OS-seeded xoshiro256** `Rng` with `uniform`, `below`, `between`, `normal`, `chance`, `choice` and `shuffle`; milestone 54.

**Status:** approved 2026-10-14 — session 114, with the milestone 54 additions to `Language v15 Draft.md`, including `uniform(): f64`.
**Milestone:** 54 (one PRP; tag `m54` at merge)
**Spec:** `Language v15 Draft.md` (decisions 19–21, the milestone 54 choices, §10 M54, §11 row 54).

**Decisions:** MEMORY 102–103 (the std library, `$` built-ins, `Float`).

## OBJECTIVE
```
import std.random;

fun main() {
    let mut rng = random.seeded(42);
    println(rng.uniform());                 // the same value on every run
    println(rng.below(6) + 1);              // a die roll
    println(rng.normal(0.0, 1.0));
    println(rng.shuffle([1, 2, 3, 4, 5]));
    let mut fresh = random.from_os();       // different every run
    println(fresh.chance(0.5));
}
```
firefly then drops its own xorshift generator for `std.random`.

## CONTEXT

- **Starting state:**
  - `std` has `math` only;
  - Lugha has no random numbers, so firefly carries its own `src/random.la`.
- **Ending state:** decisions 19–21, implemented.
- **Related existing code:**
  - **Std modules:** `src/std/math.la`, `src/lib.rs` (`STD`).
  - **Built-ins:** `src/check/float_ops.rs`, `src/codegen/float_ops.rs` (`$float` built-ins).
  - **Runtime:** `runtime/`, `src/link.rs` (`RUNTIME_PARTS`, `RUNTIME_SOURCE`, `RUNTIME_SYMBOLS`).
- **Open decisions that must be resolved first:** none.

### Design
- **`src/std/random.la`,** a second `STD` entry, is written in Lugha:
  - `public struct Rng { s0, s1, s2, s3: u64 }` (private state);
  - `seeded(seed)` fills the state by splitmix64;
  - `from_os()` is `seeded($os.seed())`;
  - **`next_u64`** is xoshiro256\*\*: `rotate_left(s1 * 5, 7) * 9` with wrapping arithmetic;
  - **`uniform`** is `(next_u64() >> 11) as f64 * 2⁻⁵³`;
  - **`below(n)`** uses rejection sampling over a multiple of `n`, so it is unbiased; `between(lo, hi)` is `lo + below(hi - lo)`;
  - **`normal`** uses Box–Muller in `f64`, converted through `$float.from_f64`;
  - **`chance(p)`** is `uniform() < p`; **`choice`** is `below(len)`; **`shuffle`** is Fisher–Yates on a local copy, which it returns.
- **Built-ins** (in `check/float_ops.rs` and `codegen/float_ops.rs`, or a sibling `os_ops.rs` if cleaner):
  - **`$float.from_f64(x)`:** `x: f64`. The result is the expected type, which must be `f32`, `f64` or a `T: Float`. It lowers to a no-op or `fptrunc` per instance.
  - **`$os.seed()`:** a `u64`, lowered to a call of `lugha_rt_os_seed`.
- **Runtime:** a new fragment, `runtime/random.c`, holds `uint64_t lugha_rt_os_seed(void)`:
  - it calls `getrandom(buf, 8, 0)`, retrying on `EINTR`;
  - it falls back to reading 8 bytes of `/dev/urandom`;
  - it panics (`no randomness available from the operating system`) if both fail;
  - it is added to `RUNTIME_PARTS`, `RUNTIME_SOURCE` and `RUNTIME_SYMBOLS` (47 entries).
- **Programs that don't import `std.random`** give byte-identical IR.

### For your sign-off: one change to the approved API
`uniform<T: Float>()` has no arguments, so `T` can't be inferred: a bare `rng.uniform()` would be E0412, needing `let x: f64 = rng.uniform();` every time. I propose **`uniform(): f64`**, with `rng.uniform() as f32` for `f32`. `normal<T: Float>(mean, std)` keeps its generic form.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - the changes summary;
  - "The standard library" (a `std.random` section: the generator, seeding, each method, ranges, panics, reproducibility, "not cryptographically secure");
  - §9 (the runtime function and its table row);
  - the §10 program;
  - §11 row 54.
- **Docs:** CLAUDE.md (tree: `std/random.la`, `runtime/random.c`), README, TODO, CHANGELOG, MEMORY decision 104, CONTEXT. The tag `m54` is added at the merge.
- **firefly:**
  - replace `src/random.la`'s generator with `std.random`;
  - keep a `normals(rng, shape, scale)` helper;
  - re-run XOR and the gradient check.

### Must NOT Do
- **No global or hidden generator,** and no cryptographic claims or APIs.
- **No distributions beyond `normal`** (exponential, Poisson and weighted choice wait).
- **No change to `std.math`.**

### New dependencies
None (`getrandom` is in glibc since 2.25; the `/dev/urandom` fallback covers older systems).

## ERROR HANDLING REQUIREMENTS
- **Panics:**
  - `below(n)` with `n <= 0`, and `between(lo, hi)` with `lo >= hi` (`below: n must be positive, not 0`, …);
  - `from_os()` when the OS gives no randomness.
- **`choice([])`** is `None`.
- **E0101:** `$os` in a program.

## SECURITY CONSIDERATIONS
- **Not cryptographically secure,** and the spec says so prominently: never use it for keys, tokens or passwords. Only the seed of `from_os()` comes from the OS.
- **The new C function** reads 8 bytes into a local buffer and does nothing else. No `unsafe`, no new dependencies.

## TESTS TO WRITE

Unit tests:
- [ ] **Checker:** `$float.from_f64` typing against expected `f32`, `f64` and `T: Float`; `std.random` method types.
- [ ] **Codegen:** `$os.seed()` calls `lugha_rt_os_seed`.
- [ ] **Link:** `RUNTIME_SYMBOLS` lists it.

Acceptance — `tests/programs/m54/`:
- [ ] `random.la`: the §10 program with a fixed seed. The exact output is the reproducibility test; the first values are checked against a reference xoshiro256\*\* implementation.
- [ ] `more.la`:
  - `between` bounds over many draws;
  - `normal` at `f32`;
  - `chance(0.0)`/`chance(1.0)`;
  - `choice([])`;
  - `shuffle` keeps every element;
  - a `from_os()` generator's draws checked only for range.
- [ ] `below_panic.la`: `below(0)` panics.

Regression:
- [ ] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-083-std_random`, merged into `main` after CI passes; tag `m54`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [ ] The `m54/` programs pass at `-O0`, and at `-O2` with identical output.
- [ ] The spec is at milestone 54; docs updated.
- [ ] firefly uses `std.random`, and XOR and the gradient check still pass.
- [ ] No file over 500 lines; no new dependencies; no `unsafe`.
- [ ] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m54/random.la
```
