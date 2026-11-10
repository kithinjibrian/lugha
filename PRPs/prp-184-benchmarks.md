## FEATURE: A benchmark suite comparing Lugha with C, Rust, Go, Node and Python (tooling, not a language milestone)

## OBJECTIVE

Running one command builds six small programs in every installed language, checks they all print the same checksum, times them and prints a table:

```
$ python3 bench/run.py
benchmark    lugha -O2   lugha -O0   c        rust     go       node     python
fib          0.21s       0.64s       0.18s    0.18s    0.25s    0.60s    9.80s
nbody        …
binarytrees  …
peak memory (MB) …
```

```
python3 bench/run.py nbody fib              # only these benchmarks
python3 bench/run.py --lang lugha,c,go      # only these languages
python3 bench/run.py --runs 10              # timed runs per cell (default 5)
python3 bench/run.py --check                # build and compare checksums at small sizes, no timing
python3 bench/run.py --save                 # also append the table to bench/RESULTS.md
```

The suite gives decisions that need a measurement (for example TODO's "hold it to a printing benchmark") a standard harness to use, and it tracks how Lugha's speed changes across milestones.

## CONTEXT

- Read first: spec §6 (`args()`, `parse_i64`), §7 (memory model, Boehm GC), §12 (overflow always panics), `tests/programs.rs` (how end-to-end cases are found).
- Creates:
  - `bench/README.md`: how to run the suite, what each benchmark stresses, and the fairness rules.
  - `bench/run.py`: the harness (Python 3 stdlib only, ≤ 500 lines).
  - `bench/measure.c`: a tiny launcher that times one run and records its own child's peak RSS (built with gcc by the harness).
  - `bench/programs/<name>/<name>.{la,c,rs,go,js,py}` for `fib`, `nbody`, `binarytrees`, `fannkuch`, `mandelbrot`, `wordfreq`.
  - `bench/RESULTS.md`: tables saved with `--save` (starts with one run on the user's machine).
  - `tests/bench.rs`: compiles each `.la` benchmark and checks its checksum at a small size, so language changes can't silently break the suite.
- Changes: `CLAUDE.md` (COMMANDS: the bench commands), `CHANGELOG.md`.
- Spec: no section changes. No §11 row, since this is tooling and adds no language feature.
- Decisions: none open. No new crates.

## REQUIREMENTS

### Must Do

- **Six benchmarks**, each taking its size as the first argument and printing one checksum line:
  | Benchmark | Stresses | Default size |
  |---|---|---|
  | `fib` | calls, integer overflow checks | n = 35 |
  | `nbody` | float arithmetic, struct fields | 5,000,000 steps |
  | `binarytrees` | allocation, Boehm GC | depth 18 |
  | `fannkuch` | array indexing, bounds checks | n = 10 |
  | `mandelbrot` | tight float loops (prints the number of points in the set) | 2000 |
  | `wordfreq` | strings, maps, hashing (generated text from a fixed LCG seed, no input files) | 1,000,000 words |
  Timed sizes (in `<name>/size.txt`) are tuned so the C version runs for about 0.2–1 s: `fib` 40, `nbody` 5,000,000, `binarytrees` 16, `fannkuch` 10, `mandelbrot` 2000, `wordfreq` 2,000,000. Python runs only when `--lang` names it, since it takes minutes per cell at these sizes.
- **Fairness rules**, stated in `bench/README.md`:
  - The same algorithm and data structures in every language, written plainly. No SIMD intrinsics, no threads, no `unsafe` in Rust, no hand-unrolled loops.
  - `binarytrees` allocates every node on the heap: `malloc`/`free` in C, `Box` in Rust, pointers in Go, objects in JS and Python, and Lugha's GC.
  - Compiled languages build at `-O2` for the default CPU, with no `-march=native`: `lughac build -O2`, `gcc -O2`, `rustc -C opt-level=2`, and `go build` (Go has no levels). Lugha also gets an informational `-O0` column.
  - Each `.la` file's header comment notes what is specific to Lugha, such as overflow checks that are always on and C/Rust having none at `-O2`.
- **The harness:**
  - Builds into `bench/.build/` (added to `.gitignore`), and only rebuilds when the source is newer than the binary.
  - Builds Lugha with `cargo build --release` first and uses `target/release/lughac`.
  - Skips any language whose toolchain is missing, with one line saying so.
  - Runs each program once as a warm-up and checks that its checksum equals C's. A mismatch marks the cell `WRONG` and the command exits 1.
  - Times N runs by wall clock and reports the median, plus the minimum in `--save` output. It measures each run's time and peak RSS through `bench/measure.c`: Linux keeps the larger peak RSS across `exec`, so a child started straight from Python reports Python's ~18 MB, but one forked from the tiny launcher reports its own. A timeout kills the run's whole process group.
  - `--save` records the date, CPU model, git commit, and every compiler's version.
  - A program that crashes or exceeds a timeout (default 60 s) shows as `FAIL` and doesn't stop the rest.

### Must NOT Do

- Run timings in CI. CI machines are too noisy, so only `tests/bench.rs` (correctness at small sizes) runs there.
- Add Rust crates or Python packages. The harness uses the stdlib only.
- Change the compiler or runtime to win a benchmark. Speed-ups found this way become their own PRPs.
- Use `shell=True` or interpolate strings into commands. Every `subprocess` call takes an argument list, and benchmark names from the CLI are checked against the directory listing.

## DESIGN

- **Checksums:** integers are printed as is; floats use 9 decimal places (`nbody` energy) so every language prints identical text. `mandelbrot` counts the points in the set; `wordfreq` prints the number of distinct words and the count of the 10 most frequent, sorted by count and then word.
- **`wordfreq` text:** a 32-bit LCG (`x = x * 1103515245 + 12345 mod 2^31`) draws from `x >> 16` (the low bits cycle quickly) to pick words from a fixed 1,000-word vocabulary built from the same LCG, so no language reads files and all generate the same bytes. Lugha's version does the modulo through `i64` with no overflow.
- **`tests/bench.rs`:** for each `bench/programs/<name>/<name>.la`, it builds with `lughac`, runs at the small size on the first line of `bench/programs/<name>/check.txt`, and compares with the rest of the file (the exact stdout), and compares. `run.py --check` reads the same files for every language.
- **Security:** the harness runs only the compilers listed above and the binaries it built, from fixed paths; there's no external input beyond the CLI flags, which are validated. No `unsafe`, no new dependencies.

## TESTS

- [ ] `tests/bench.rs`: each of the six `.la` programs builds and prints its `check.txt` output (happy path).
- [ ] `tests/bench.rs`: a missing `check.txt` fails the test with the benchmark's name, so a new benchmark can't skip its check.
- [ ] `python3 bench/run.py --check` passes for every installed language (manual, recorded in the PR).
- [ ] `python3 bench/run.py --lang nope` exits 2 with the list of known languages; an unknown benchmark name exits 2 with the list of benchmarks.

## ACCEPTANCE

- [ ] `python3 bench/run.py` prints the full table on this machine (C, Rust, Go, Node, Python, Lugha -O2/-O0) and every checksum matches.
- [ ] `bench/RESULTS.md` holds the first saved run.
- [ ] `cargo test --test bench` passes; `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` are clean.
- [ ] Every file ≤ 500 lines; CHANGELOG and CLAUDE.md COMMANDS updated.
