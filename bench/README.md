# Benchmarks

Six small programs, each written in Lugha, C, Rust, Go, JavaScript (Node) and Python, timed side by side (PRP-184).

```bash
python3 bench/run.py                      # every benchmark; Lugha (-O2 and -O0), C, Rust, Go, Node
python3 bench/run.py nbody fib            # only these
python3 bench/run.py --lang lugha,c,go    # only these languages (add python to include it)
python3 bench/run.py --runs 10            # timed runs per cell (default 5)
python3 bench/run.py --check              # small sizes against check.txt, every language, no timing
python3 bench/run.py --save               # also append the tables to bench/RESULTS.md
```

The harness builds the release `lughac` first, compiles into `bench/.build/` (ignored by git) and rebuilds only what changed. Every program runs once as a warm-up, and its output must equal the C version's, or the cell shows `WRONG`. Then it's timed `--runs` times. The tables give the median wall time, the fastest run, and the median peak memory (RSS). Each run goes through `bench/measure.c`, a tiny launcher the harness builds with gcc, because a program started straight from Python would report Python's own memory: Linux keeps the larger peak across `exec`. A cell shows `-` when that language's source is missing, `BUILD`, `FAIL` or `TIMEOUT` when something broke (details go to stderr), and the command exits 1. A missing toolchain is skipped with a note. Python is left out unless `--lang` names it, because it takes minutes per cell at these sizes.

Timings are never run in CI, where machines are too noisy. `cargo test --test bench` checks only that the Lugha programs still build and print `check.txt`.

## The benchmarks

| Benchmark | What it stresses | Timed size |
|---|---|---|
| `fib` | function calls; integer overflow checks | n = 40 |
| `nbody` | float arithmetic, struct fields in an array | 5,000,000 steps |
| `binarytrees` | allocation and the garbage collector | depth 16 |
| `fannkuch` | array indexing, bounds checks | n = 10 |
| `mandelbrot` | tight float loops (prints the number of points in the set) | 2000 × 2000 |
| `wordfreq` | strings, splitting, maps and hashing | 2,000,000 words |

Each program takes its size as its only argument. `<name>/size.txt` holds the timed size. `<name>/check.txt` holds a small size on its first line, then exactly what every version prints at that size.

`wordfreq` makes its text with a 31-bit LCG (`x = (x * 1103515245 + 12345) mod 2^31`, draws from `x >> 16`), so every language builds the same bytes and nothing reads files. It joins the words with spaces, splits the text again, counts each word in the language's map (a hand-written FNV-1a table in C), and prints the number of distinct words and the ten most common.

## Fairness rules

The point is to compare compilers and runtimes, not cleverness:

- **The same algorithm and data structures in every language, written plainly.** No SIMD intrinsics, threads, `unsafe` Rust or hand-unrolled loops.
- **`binarytrees` allocates every node separately:** `malloc`/`free` in C, `Box` in Rust, a pointer in Go, an object in JS and Python, and a GC'd enum in Lugha. No pools or arenas.
- **`-O2` for the default CPU, never `-march=native`:** `lughac build -O2`, `gcc -O2`, `rustc -C opt-level=2`, `go build`. Lugha also gets an informational `-O0` column.
- **Lugha keeps its checks.** Integer overflow always panics in Lugha (spec §12), and every index is bounds-checked. C has neither check, and Rust has only bounds checks at `-O2`. Those costs are part of what the numbers show.
- Floats print with 9 decimals so every language's text matches. Go's `mandelbrot` writes `float64(2.0*zr*zi)` so Go can't fuse the operations into an FMA instruction, which would change the result.

## Adding a benchmark

Make `bench/programs/<name>/` with `<name>.la`, `<name>.c` and whichever of `.rs`, `.go`, `.js` and `.py` you write, then add `size.txt` and `check.txt`. The harness and `tests/bench.rs` find it by itself. Run `python3 bench/run.py --check <name>` before timing it.
