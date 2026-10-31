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

## 2026-10-31, commit 9d1e7f6, with Python

- CPU: Intel(R) Core(TM) i5-4308U CPU @ 2.80GHz
- lugha: lughac 0.1.0 (with `loop` and labels, milestone 152)
- c: gcc (Ubuntu 15.2.0-16ubuntu1) 15.2.0
- rust: rustc 1.99.0 (b940084d7 2026-09-28)
- go: go version go1.26.8 linux/amd64
- node: v22.22.1
- python: Python 3.14.4

Assembled from two runs of 3: `binarytrees`, `fannkuch`, `fib` and `mandelbrot` from a run of every language (`--lang lugha,lugha-O0,c,rust,go,node,python --runs 3 --timeout 300`), `nbody` and `wordfreq` from a rerun of those two (`--lang lugha,c,rust,go,node,python`). The first run's `nbody` and `wordfreq` came after many minutes of full load and every language was 1.5–4× slower in them (the CPU throttling), so they were measured again on a cool machine; the rerun had no `-O0` column (`—`).

Median wall time of 3 runs:

| benchmark   | lugha  | lugha-O0 | c      | rust   | go     | node   | python  |
|-------------|--------|----------|--------|--------|--------|--------|---------|
| binarytrees | 1.136s | 1.362s   | 0.559s | 1.496s | 0.857s | 0.595s | 4.818s  |
| fannkuch    | 0.272s | 1.204s   | 0.231s | 0.227s | 0.270s | 0.555s | 8.996s  |
| fib         | 0.674s | 1.217s   | 0.251s | 0.360s | 0.596s | 2.012s | 22.291s |
| mandelbrot  | 0.407s | 1.145s   | 0.367s | 0.439s | 0.411s | 0.669s | 27.672s |
| nbody       | 0.648s | —        | 0.535s | 0.603s | 0.682s | 1.012s | 28.382s |
| wordfreq    | 0.523s | —        | 0.095s | 0.152s | 0.353s | 1.138s | 1.012s  |

Fastest run:

| benchmark   | lugha  | lugha-O0 | c      | rust   | go     | node   | python  |
|-------------|--------|----------|--------|--------|--------|--------|---------|
| binarytrees | 1.103s | 1.288s   | 0.540s | 1.482s | 0.852s | 0.558s | 4.632s  |
| fannkuch    | 0.260s | 1.101s   | 0.225s | 0.227s | 0.268s | 0.532s | 8.852s  |
| fib         | 0.628s | 1.210s   | 0.246s | 0.359s | 0.586s | 1.970s | 22.172s |
| mandelbrot  | 0.391s | 0.888s   | 0.364s | 0.439s | 0.382s | 0.643s | 20.402s |
| nbody       | 0.648s | —        | 0.534s | 0.603s | 0.680s | 0.975s | 28.207s |
| wordfreq    | 0.518s | —        | 0.095s | 0.151s | 0.349s | 1.101s | 1.005s  |

Peak memory (MB, median):

| benchmark   | lugha | lugha-O0 | c    | rust | go    | node  | python |
|-------------|-------|----------|------|------|-------|-------|--------|
| binarytrees | 70.3  | 70.2     | 9.8  | 18.1 | 12.4  | 102.4 | 21.8   |
| fannkuch    | 2.2   | 2.1      | 1.7  | 2.2  | 2.0   | 58.2  | 9.7    |
| fib         | 2.1   | 2.2      | 1.7  | 2.1  | 2.0   | 57.6  | 9.3    |
| mandelbrot  | 2.2   | 2.2      | 1.7  | 2.0  | 2.1   | 60.3  | 10.1   |
| nbody       | 2.2   | —        | 1.7  | 2.1  | 2.0   | 61.2  | 10.3   |
| wordfreq    | 79.9  | —        | 16.2 | 47.3 | 108.1 | 229.4 | 158.5  |
