#!/usr/bin/env python3
"""Benchmarks Lugha against C, Rust, Go, Node and Python (PRP-184).

Builds every benchmark in bench/programs/ for each installed language,
checks that each prints what the C version prints, times the runs and
prints a markdown table. Python's standard library only. See README.md.
"""

import argparse
import datetime
import os
import platform
import shutil
import signal
import statistics
import subprocess
import sys
from pathlib import Path

BENCH = Path(__file__).resolve().parent
REPO = BENCH.parent
PROGRAMS = BENCH / "programs"
BUILD = BENCH / ".build"
RESULTS = BENCH / "RESULTS.md"
LUGHAC = REPO / "target" / "release" / "lughac"
MEASURE = BUILD / "measure"

# Languages in table order. `tool` must be on PATH (or, for Lugha, built)
# for the language to run; `build` makes `out` from `src`, or is None for an
# interpreter; `run` is the command that runs the program.
LANGS = {
    "lugha": {
        "ext": "la",
        "tool": "cargo",
        "build": lambda src, out: [str(LUGHAC), "build", str(src), "-O2", "-o", str(out)],
    },
    "lugha-O0": {
        "ext": "la",
        "tool": "cargo",
        "build": lambda src, out: [str(LUGHAC), "build", str(src), "-O0", "-o", str(out)],
    },
    "c": {
        "ext": "c",
        "tool": "gcc",
        "build": lambda src, out: ["gcc", "-O2", "-o", str(out), str(src), "-lm"],
    },
    "rust": {
        "ext": "rs",
        "tool": "rustc",
        "build": lambda src, out: ["rustc", "-C", "opt-level=2", "-o", str(out), str(src)],
    },
    "go": {
        "ext": "go",
        "tool": "go",
        "build": lambda src, out: ["go", "build", "-o", str(out), str(src)],
    },
    "node": {"ext": "js", "tool": "node", "build": None, "run": ["node"]},
    # -I keeps the interpreter from importing anything from the
    # benchmark's own directory.
    "python": {"ext": "py", "tool": "python3", "build": None, "run": ["python3", "-I"]},
}

# Python takes minutes per cell at the timed sizes, so it runs only when
# asked for with --lang.
DEFAULT_LANGS = [name for name in LANGS if name != "python"]

# The language every other one's output is compared with.
REFERENCE = "c"

VERSION_COMMANDS = {
    "lugha": [str(LUGHAC), "--version"],
    "c": ["gcc", "--version"],
    "rust": ["rustc", "--version"],
    "go": ["go", "version"],
    "node": ["node", "--version"],
    "python": ["python3", "--version"],
}


class Failure(Exception):
    """A build or run that failed, with the text shown in its cell."""

    def __init__(self, cell, detail):
        super().__init__(detail)
        self.cell = cell
        self.detail = detail


def benchmarks():
    """The benchmark names: every directory under bench/programs/."""
    return sorted(p.name for p in PROGRAMS.iterdir() if p.is_dir())


def read_check(name):
    """The small size and the expected output from <name>/check.txt."""
    path = PROGRAMS / name / "check.txt"
    first, _, expected = path.read_text().partition("\n")
    return int(first), expected


def read_size(name):
    """The timed size from <name>/size.txt."""
    return int((PROGRAMS / name / "size.txt").read_text().strip())


def available(lang):
    """Whether the language's toolchain is installed."""
    return shutil.which(LANGS[lang]["tool"]) is not None


def build_lughac():
    """Builds the release compiler that the Lugha columns use."""
    print("building lughac (release)…", file=sys.stderr)
    result = subprocess.run(
        ["cargo", "build", "--release", "-q"], cwd=REPO, capture_output=True, text=True
    )
    if result.returncode != 0:
        sys.exit(f"cargo build --release failed:\n{result.stderr}")


def program(name, lang):
    """Builds the program if needed, and returns the command that runs it."""
    spec = LANGS[lang]
    src = PROGRAMS / name / f"{name}.{spec['ext']}"
    if not src.exists():
        raise Failure("-", f"no {src.name}")
    if spec["build"] is None:
        return spec["run"] + [str(src)]
    out = BUILD / f"{name}-{lang}"
    # A rebuilt compiler can change Lugha's output, so its binary counts as
    # a source too.
    inputs = [src] + ([LUGHAC] if lang.startswith("lugha") else [])
    newest = max(p.stat().st_mtime for p in inputs)
    if not out.exists() or out.stat().st_mtime < newest:
        BUILD.mkdir(exist_ok=True)
        result = subprocess.run(spec["build"](src, out), capture_output=True, text=True)
        if result.returncode != 0:
            raise Failure("BUILD", result.stdout + result.stderr)
    return [str(out)]


def build_measure():
    """Builds bench/measure.c, the launcher that times each run."""
    src = BENCH / "measure.c"
    if not MEASURE.exists() or MEASURE.stat().st_mtime < src.stat().st_mtime:
        BUILD.mkdir(exist_ok=True)
        result = subprocess.run(
            ["gcc", "-O2", "-o", str(MEASURE), str(src)], capture_output=True, text=True
        )
        if result.returncode != 0:
            sys.exit(f"building measure.c failed:\n{result.stderr}")


def run_once(cmd, size, timeout):
    """Runs the program once through `measure`: (stdout, seconds, peak RSS in KB)."""
    out_path = BUILD / "stdout.txt"
    result_path = BUILD / "measure.txt"
    result_path.unlink(missing_ok=True)
    with open(out_path, "wb") as out:
        # A session of its own, so a timeout kills the benchmark as well as
        # `measure`; a leftover would steal CPU from the next runs.
        proc = subprocess.Popen(
            [str(MEASURE), str(result_path)] + cmd + [str(size)],
            stdout=out,
            stderr=subprocess.PIPE,
            start_new_session=True,
        )
        try:
            _, stderr = proc.communicate(timeout=timeout)
        except subprocess.TimeoutExpired:
            os.killpg(proc.pid, signal.SIGKILL)
            proc.communicate()
            raise Failure("TIMEOUT", f"still running after {timeout:g}s") from None
    if proc.returncode != 0:
        raise Failure("FAIL", stderr.decode(errors="replace") or f"exit {proc.returncode}")
    seconds, peak = result_path.read_text().split()
    return out_path.read_text(), float(seconds), int(peak)


def measure(name, lang, size, runs, timeout, expected):
    """Times one cell: (median seconds, min seconds, median peak KB)."""
    cmd = program(name, lang)
    output, _, _ = run_once(cmd, size, timeout)  # warm-up and check
    if expected is not None and output != expected:
        raise Failure("WRONG", f"printed:\n{output}")
    times, peaks = [], []
    for _ in range(runs):
        _, seconds, peak = run_once(cmd, size, timeout)
        times.append(seconds)
        peaks.append(peak)
    return statistics.median(times), min(times), statistics.median(peaks)


def table(header, rows):
    """A markdown table with aligned columns."""
    widths = [max(len(r[i]) for r in [header] + rows) for i in range(len(header))]
    line = lambda r: "| " + " | ".join(c.ljust(w) for c, w in zip(r, widths)) + " |"
    sep = "|" + "|".join("-" * (w + 2) for w in widths) + "|"
    return "\n".join([line(header), sep] + [line(r) for r in rows])


def time_all(names, langs, runs, timeout):
    """Runs every cell. Returns the tables and whether every cell passed."""
    ok = True
    times, mins, mems, problems = [], [], [], []
    for name in names:
        size = read_size(name)
        print(f"{name} ({size})…", file=sys.stderr)
        expected = None
        trow, mrow, prow = [name], [name], [name]
        # The reference runs first, so its output is what the rest must print.
        order = [REFERENCE] + [l for l in langs if l != REFERENCE] if REFERENCE in langs else langs
        cells = {}
        for lang in order:
            try:
                median, low, peak = measure(name, lang, size, runs, timeout, expected)
                if lang == REFERENCE:
                    expected = (BUILD / "stdout.txt").read_text()
                cells[lang] = (f"{median:.3f}s", f"{low:.3f}s", f"{peak / 1024:.1f}")
            except Failure as f:
                if f.cell != "-":
                    ok = False
                    problems.append(f"{name}/{lang}: {f.cell}\n{f.detail.strip()}")
                cells[lang] = (f.cell, f.cell, f.cell)
        for lang in langs:
            trow.append(cells[lang][0])
            prow.append(cells[lang][1])
            mrow.append(cells[lang][2])
        times.append(trow)
        mins.append(prow)
        mems.append(mrow)
    header = ["benchmark"] + langs
    text = (
        f"Median wall time of {runs} runs:\n\n{table(header, times)}\n\n"
        f"Fastest run:\n\n{table(header, mins)}\n\n"
        f"Peak memory (MB, median):\n\n{table(header, mems)}\n"
    )
    return text, ok, problems


def check_all(names, langs, timeout):
    """Runs every cell at its small size against check.txt."""
    ok = True
    for name in names:
        size, expected = read_check(name)
        for lang in langs:
            try:
                output, _, _ = run_once(program(name, lang), size, timeout)
                status = "ok" if output == expected else "WRONG"
            except Failure as f:
                status, output = f.cell, f.detail
            if status != "-":
                print(f"{status:7} {name} {lang}")
            if status not in ("ok", "-"):
                ok = False
                print(output.rstrip())
    return ok


def first_line(cmd):
    """The first line a version command prints, or 'not found'."""
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
    except (OSError, subprocess.TimeoutExpired):
        return "not found"
    return (result.stdout or result.stderr).strip().splitlines()[0]


def machine():
    """The CPU model, as /proc/cpuinfo names it."""
    try:
        for line in Path("/proc/cpuinfo").read_text().splitlines():
            if line.startswith("model name"):
                return line.split(":", 1)[1].strip()
    except OSError:
        pass  # Not Linux: the platform module's answer is good enough.
    return platform.processor() or platform.machine()


def save(text, langs):
    """Appends the tables to RESULTS.md with what produced them."""
    commit = first_line(["git", "-C", str(REPO), "rev-parse", "--short", "HEAD"])
    versions = []
    for lang in langs:
        key = "lugha" if lang.startswith("lugha") else lang
        if key in VERSION_COMMANDS and key not in [v[0] for v in versions]:
            versions.append((key, first_line(VERSION_COMMANDS[key])))
    stamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M")
    lines = [f"\n## {stamp}, commit {commit}\n", f"- CPU: {machine()}"]
    lines += [f"- {key}: {version}" for key, version in versions]
    with open(RESULTS, "a") as f:
        f.write("\n".join(lines) + "\n\n" + text)
    print(f"saved to {RESULTS.relative_to(REPO)}", file=sys.stderr)


def parse_args(argv):
    """The command line, with benchmark and language names validated."""
    known = benchmarks()
    p = argparse.ArgumentParser(description="Benchmark Lugha against other languages.")
    p.add_argument("names", nargs="*", metavar="benchmark", help=f"one of: {', '.join(known)}")
    p.add_argument("--lang", help=f"comma-separated, from: {', '.join(LANGS)}")
    p.add_argument("--runs", type=int, default=5, help="timed runs per cell (default 5)")
    p.add_argument("--timeout", type=float, default=60.0, help="seconds per run (default 60)")
    p.add_argument("--check", action="store_true", help="compare outputs at small sizes, no timing")
    p.add_argument("--save", action="store_true", help="append the tables to bench/RESULTS.md")
    args = p.parse_args(argv)
    for name in args.names:
        if name not in known:
            p.error(f"unknown benchmark {name!r}; known: {', '.join(known)}")
    args.names = args.names or known
    langs = args.lang.split(",") if args.lang else DEFAULT_LANGS
    for lang in langs:
        if lang not in LANGS:
            p.error(f"unknown language {lang!r}; known: {', '.join(LANGS)}")
    if args.runs < 1:
        p.error("--runs must be at least 1")
    args.langs = langs
    return args


def main(argv):
    """Runs the suite; exits 1 when any cell fails or prints the wrong thing."""
    args = parse_args(argv)
    langs = []
    for lang in args.langs:
        if available(lang):
            langs.append(lang)
        else:
            print(f"skipping {lang}: {LANGS[lang]['tool']} not found", file=sys.stderr)
    if any(lang.startswith("lugha") for lang in langs):
        build_lughac()
    build_measure()
    if args.check:
        return 0 if check_all(args.names, langs, args.timeout) else 1
    text, ok, problems = time_all(args.names, langs, args.runs, args.timeout)
    print(text)
    for problem in problems:
        print(problem, file=sys.stderr)
    if args.save:
        save(text, langs)
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
