# DECISION-010 — Cost of array copies: measurements

Session 21, 2026-10-09. Measured at compiler commit `dde302d`.

## Question

Spec §12 leaves one question open. Should Lugha keep **eager deep copies** at the §4 copy sites, or move to **copy-on-write** (CoW)? CoW needs a reference count per array, which the Boehm collector doesn't provide. The spec says to measure real programs first.

## Method

- **Programs:** six programs in `decision-010/`, written the way the language invites. Each runs for 0.1–0.5 s at `-O2`:

  | Program | What it does |
  | --- | --- |
  | `life.la` | Game of Life on a 200×200 `bool[][]`, 100 steps, double-buffered with `let mut next = grid; … grid = next;` |
  | `particles.la` | 2000 `struct Particle { pos: f64[], vel: f64[], mass: f64 }`, 1000 steps, each updated through a local: `let mut p = ps[i]; … ps[i] = p;` |
  | `particles_inplace.la` | The same update written through the place chain: `ps[i].pos[d] += …` |
  | `sort.la` | Bottom-up merge sort of 200 000 `i64`, swapping buffers with `ys = buf;`, run 5 times |
  | `matmul.la` | 200×200 `f64[][]` multiply, 10 times; results are fresh values |
  | `histogram.la` | Counting bytes into a `[0; 256]` in place; no copies (a control) |

- **Instrumented compiler:** a throwaway copy of `lughac`, never committed, read an environment variable at build time:
  - `count`: every array copy calls a counter, and the copies and bytes copied (header plus elements) are printed at exit.
  - `nocopy`: every deep copy is skipped. This breaks value semantics, so outputs differ, but it is an upper bound on what any copy-avoiding scheme could save.
- **Timing:** best of 7 runs, wall clock, Intel i5-4308U, Ubuntu 26.04, LLVM 21.

## Results

| Program | Copies | Bytes copied | Eager (ms) | No copies (ms) | Upper bound | What CoW could save |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| `life` | 40 400 | 8.7 MB | 457 | 391 | 14% | ≤ 7%. Every row of `next` is written after `let mut next = grid`, so CoW copies it anyway. Only `grid = next` could be saved, and only if `next` is known dead. |
| `particles` | 8 004 000 | 256 MB | 253 | 26 | 89% | ≤ ~65%. `p.pos` is written after `let mut p = ps[i]` (copied anyway). `p.vel` and the copy back in `ps[i] = p` could be saved, the latter only if `p` is known dead. |
| `particles_inplace` | 4 000 | 0.1 MB | **25** | — | — | Same output as `particles`, 10× faster, with no copies after setup. |
| `sort` | 95 | 152 MB | 164 | 45 | 72% | ≈ 0%. After `ys = buf`, `buf` is written in the next pass, so CoW copies it anyway. |
| `matmul` | 2 400 | 3.9 MB | 139 | 133 | 4% | ≈ 0%. The copies are the repeat-literal rows, and every row is written. |
| `histogram` | 0 | 0 | 132 | 135 | — | — |

The spec §10 programs (primes, centroid) make **no copies at all**.

## Analysis

1. **Most copies are followed by a write.** CoW only saves a copy when neither side is written while the array is shared. In `life`, `sort` and `matmul`, the copied array is written right after, so CoW would pay the same copy later, plus a reference-count check on every element store. The "no copies" column overstates CoW's benefit.

2. **The copies CoW could save come from locals that are dead.** In `grid = next` and `ps[i] = p`, the source is never read again. CoW only saves those if counts are decremented when locals go out of scope, which is full reference counting:
   - an increment on every share;
   - a decrement at every scope exit, `return`, `break` and `continue`;
   - a uniqueness check on every element write.

   With Boehm, nothing else frees memory, so the counts would exist only for CoW. That cost is paid by every program, including those that never copy.

3. **The same savings are available statically.** A compiler optimization, *move on last use*, could store a local's array without copying it when that local is never read again. It is a liveness check at compile time, with no run-time cost and no change to what programs can observe (§4 already moves `return xs`). It would remove `grid = next` and `ps[i] = p` but not the `sort` or `matmul` copies, which CoW couldn't remove either.

4. **The worst case has a source-level fix today.** Writing through the place (`ps[i].pos[d] += …`) instead of through a local copy is 10× faster in `particles`.

5. **`sort` shows a real gap that neither scheme closes.** A buffer swap (`ys = buf` while `buf` stays in use) is a full copy. Only a swap or move operation in the language would avoid it, which is v1 language design (spec §11), not a copy strategy.

## Recommendation

- **Keep eager deep copies; do not implement copy-on-write.** Its realistic savings are small, or reachable more cheaply, and it would add reference counting that Boehm makes otherwise unnecessary.
- **Note, but don't schedule:**
  - a possible future optimization PRP, *move on last use for locals* (no spec change);
  - a v1 language question: a swap or move operation for buffers.
- **Close the spec §12 item** with the outcome, and point programmers to updating through place chains for arrays inside records.

## Reproducing

Build the programs with `lughac build -O2` and time them. The `count` and `nocopy` builds need a local patch to `codegen/copy.rs` and `runtime/lugha_rt.c` that adds the counter and the skip; it is described above and was deliberately not committed.
