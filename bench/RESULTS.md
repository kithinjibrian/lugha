# Benchmark results

Tables appended by `python3 bench/run.py --save` (see README.md). Compare runs only on the same machine.

## 2026-10-31 21:23, commit 5d5ab18

- CPU: Intel(R) Core(TM) i5-4308U CPU @ 2.80GHz
- lugha: lughac 0.1.0
- c: gcc (Ubuntu 15.2.0-16ubuntu1) 15.2.0
- rust: rustc 1.99.0 (b940084d7 2026-09-28)
- go: go version go1.26.8 linux/amd64
- node: v22.22.1

Median wall time of 5 runs:

| benchmark   | lugha  | lugha-O0 | c      | rust   | go     | node   |
|-------------|--------|----------|--------|--------|--------|--------|
| binarytrees | 1.114s | 1.290s   | 0.519s | 1.371s | 1.076s | 0.542s |
| fannkuch    | 0.251s | 1.015s   | 0.216s | 0.216s | 0.226s | 0.458s |
| fib         | 0.562s | 1.074s   | 0.229s | 0.328s | 0.563s | 1.747s |
| mandelbrot  | 0.364s | 0.807s   | 0.361s | 0.406s | 0.378s | 0.542s |
| nbody       | 0.650s | 2.762s   | 0.533s | 0.605s | 0.685s | 0.977s |
| wordfreq    | 0.557s | 1.111s   | 0.096s | 0.152s | 0.349s | 1.106s |

Fastest run:

| benchmark   | lugha  | lugha-O0 | c      | rust   | go     | node   |
|-------------|--------|----------|--------|--------|--------|--------|
| binarytrees | 1.079s | 1.265s   | 0.513s | 1.339s | 0.881s | 0.530s |
| fannkuch    | 0.250s | 1.012s   | 0.215s | 0.215s | 0.223s | 0.453s |
| fib         | 0.558s | 1.074s   | 0.229s | 0.326s | 0.560s | 1.743s |
| mandelbrot  | 0.363s | 0.805s   | 0.361s | 0.404s | 0.372s | 0.538s |
| nbody       | 0.647s | 2.736s   | 0.530s | 0.600s | 0.681s | 0.968s |
| wordfreq    | 0.535s | 1.096s   | 0.095s | 0.151s | 0.334s | 1.101s |

Peak memory (MB, median):

| benchmark   | lugha | lugha-O0 | c    | rust | go    | node  |
|-------------|-------|----------|------|------|-------|-------|
| binarytrees | 70.3  | 70.2     | 9.8  | 18.1 | 11.9  | 108.8 |
| fannkuch    | 2.2   | 2.2      | 1.7  | 2.1  | 2.0   | 63.3  |
| fib         | 2.2   | 2.2      | 1.7  | 2.2  | 1.8   | 62.8  |
| mandelbrot  | 2.2   | 2.2      | 1.7  | 2.2  | 1.9   | 65.3  |
| nbody       | 2.2   | 2.2      | 1.7  | 2.1  | 2.0   | 66.0  |
| wordfreq    | 83.4  | 100.4    | 16.2 | 47.2 | 103.2 | 234.2 |
