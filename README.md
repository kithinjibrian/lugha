<p align="center">
  <img src="editors/vscode/icons/lugha.svg" alt="Lugha: a baobab before the sun" width="128">
</p>

<h1 align="center">Lugha</h1>

<p align="center">
  <b>A small, statically typed language that compiles to native code.</b><br>
  Plain values, green threads and actors, and C libraries without bindings.
</p>

<p align="center">
  <a href="#install">Install</a> ·
  <a href="#a-tour">Tour</a> ·
  <a href="docs/specs/Language%20v17%20Specification.md">Specification</a> ·
  <a href="#editors">Editors</a> ·
  <a href="#status">Status</a>
</p>

---

```lugha
enum Shape {
    Circle(f64),
    Rect(f64, f64),
}

fun area(s: Shape): f64 = match s {
    Circle(r) => 3.14159 * r * r,
    Rect(w, h) => w * h,
};

fun main() {
    let shapes = [Shape.Circle(1.0), Shape.Rect(2.0, 3.0), Shape.Circle(0.5)];
    let total = shapes.iter().map(area).sum();
    println("{shapes.len} shapes, total area {total:.2}");
}
```

```console
$ lughac run shapes.la
3 shapes, total area 9.93
$ lughac build shapes.la -O2 && ls -lh shapes
-rwxr-xr-x 1 you you 212K  shapes        # one file, needs only libc
```

*Lugha* is Swahili for "language". `lughac`, its compiler, is written in Rust. It checks a `.la`
program, compiles it through LLVM and links it with a small C runtime and a garbage collector into
one executable.

## Why Lugha

- **Everything is a value.** Assigning an array, struct or enum gives you your own copy, so nothing
  changes behind your back. Copies are cheap: arrays are shared until one side writes. When you do
  want sharing, `Ref<T>` says so in the type.
- **Mistakes stop the program, never corrupt it.** Integer overflow, out-of-bounds indexes and
  division by zero panic with a message and the line, at `-O2` too. `match` must cover every case,
  and there is no `null`: a missing value is `Option<T>`.
- **Concurrency without `async`.** `spawn f(x)` runs any call on a green thread, channels and
  `select` carry values between them, and actors keep state that needs no locks. Every function
  stays an ordinary function.
- **C is one declaration away.** `extern fun` calls C, `extern struct` passes structs exactly as a C
  compiler does, closures become C callbacks, and a `[link]` table in `lugha.toml` links any library,
  `pkg-config` included. GTK 4 apps are written in plain Lugha
  ([lugha-gtk](https://github.com/kithinjibrian/lugha-gtk)).
- **The tools come in the box.** One `lughac` binary builds, runs, tests, formats, explains every
  error code and serves editors through the Language Server Protocol.

## Install

Lugha builds on Linux. You need Rust (the toolchain is pinned, so [rustup](https://rustup.rs)
fetches the right one), LLVM 21 and a C compiler. On Ubuntu:

```sh
sudo apt install llvm-21-dev build-essential
git clone https://github.com/kithinjibrian/lugha && cd lugha
cargo install --path .
```

Then start a project:

```console
$ lughac init hello
created hello/lugha.toml
created hello/src/main.la
…
$ lughac run hello/src/main.la
Hello, world, from hello!
$ lughac test hello
test hello.test.greeting_test.greets ... ok

1 test: 1 passed, 0 failed
```

<details>
<summary>Optional extras</summary>

- **TLS and crypto** (`std.tls`, `std.http` over `https`, `std.crypto`): `sudo apt install libssl-dev`.
- **Git packages**: `git` on your `PATH`.
- **C libraries through `pkg-config`**: `pkg-config` or `pkgconf`, and the library's development files.
- **Another C compiler**: `LUGHA_CC=clang` or `LUGHA_CC="zig cc"`.
- **ARM64 Linux**: `sudo apt install gcc-aarch64-linux-gnu qemu-user`, then
  `lughac build app.la --target aarch64-unknown-linux-gnu` and run it with
  `qemu-aarch64 -L /usr/aarch64-linux-gnu ./app`, or copy it to an ARM machine.
- **This machine's CPU only**: `--cpu=native` (by default, executables run on any x86-64).

</details>

## A tour

### Errors are values

`Result<T, E>` holds a value or an error, and `?` passes the error up.

```lugha
fun parse_age(s: string): Result<i64, string> {
    let n = parse_i64(s.trim()).ok_or("not a number: {s}")?;
    if n < 0 {
        return Err("negative: {n}");
    }
    Ok(n)
}

fun main() {
    for s of ["42", " 7 ", "-1", "abc"] {
        match parse_age(s) {
            Ok(n) => println("age {n}"),
            Err(e) => eprintln("error: {e}"),
        }
    }
}
```

### Actors and green threads

An actor handles one message at a time, so its state needs no locks. `c.add(n)` is queued and
`c.get()` waits for the answer.

```lugha
actor Counter {
    count: i64,
    fun add(mut self, n: i64) {
        self.count += n;
    }
    fun get(self): i64 = self.count;
}

fun work(c: Counter, n: i64) {
    c.add(n);
}

fun main() {
    let c = spawn Counter { count: 0 };
    let tasks = [spawn work(c, 1), spawn work(c, 2), spawn work(c, 3)];
    for t of tasks {
        t.join();
    }
    println("count = {c.get()}");  // count = 6
}
```

Channels (`Chan<T>`), `select` with timeouts, and a panic that ends only the actor it happens in
are covered in the spec. For a step-by-step look at the scheduler, open
[`docs/learn/green-threads/index.html`](docs/learn/green-threads/index.html) in a browser.

### Errors that say what to do

```console
$ lughac check push.la
error[E0501]: cannot call `mut self` method `push` on `xs`: it is not mutable
  --> push.la:3:5
  |
2 |     let xs = [1, 2, 3];
  |         -- declared here
3 |     xs.push(4);
  |     ^^
  |
  = help: make it mutable: `let mut xs`

For more about E0501, run `lughac explain E0501`.
```

Every code has a page with an example and its fix (`lughac explain E0501`). With
`--diagnostics=json`, tools get the same diagnostics as one JSON object per line.

### And the rest

- **Types:** integers from `i8` to `u64`, `f32` and `f64`, `char`, UTF-8 strings, tuples, structs,
  enums, growable arrays, `Map` and `Set` (`#{"a": 1}`), and global constants computed by pure code.
- **Abstraction:** generics with inferred type arguments, interfaces met by any type with the right
  methods, interface values, default methods, operator overloading, user-defined `==` and order,
  and `derive` blocks that say once how every struct meets an interface (JSON, hashing, printing).
- **Control:** `match` with or-patterns, guards and type patterns; `if let`, `while let` and
  `let … else`; `loop` and labelled `break`; `defer`; closures; lazy iterators (`map`, `filter`,
  `zip`, `take_while`, `collect`, …).
- **Standard library**, each module loaded only when imported: `std.math`, `std.random`, `std.fs`
  (files, folders and streams), `std.env`, `std.process` (never through a shell), `std.time`,
  `std.json`, `std.toml`, `std.net`, `std.http`, `std.tls`, `std.crypto` and `std.task`.
- **Programs and packages:** modules across files, `private` and `public`, packages from a folder or
  a git tag with a lock file, and `include_str`/`include_bytes` for files built into the program.

The full language, with every rule and example program, is the
[specification](docs/specs/Language%20v17%20Specification.md). `lughac spec` prints the copy that
matches your compiler, ready to read or to paste into an AI assistant.

## The `lughac` command

| Command | What it does |
| --- | --- |
| `lughac init [folder] [--lib]` | Start a program, or a library with an example and a test |
| `lughac run prog.la [-- args…]` | Build and run; the exit code is the program's |
| `lughac build prog.la [-o out] [-O2]` | Build an executable |
| `lughac check prog.la` | Type-check only |
| `lughac test [paths…] [--filter T]` | Run `@test` functions, each in its own process, in parallel |
| `lughac fmt [paths…] [--check]` | Format `.la` files, keeping every comment |
| `lughac explain [CODE]` | Explain an error code, or list them all |
| `lughac update prog.la` | Re-resolve git packages and rewrite `lugha.lock` |
| `lughac lsp` | Serve the Language Server Protocol on stdin and stdout |
| `lughac spec` | Print the language specification |

`lughac` exits 0 on success, 1 when the program has errors, and 2 for bad usage, an internal error
or output it couldn't write.

## Editors

**VS Code:** the extension in [`editors/vscode/`](editors/vscode) shows errors as you type, plus
hover, go to definition, find references, rename, completion, signature help, quick fixes,
formatting, snippets, and a ▶ beside every test.

```sh
cd editors/vscode && npm ci && npm run package
code --install-extension lugha-*.vsix
```

**Neovim 0.11 and later** need no plugin; `lughac lsp` colours the code itself:

```lua
vim.filetype.add({ extension = { la = "lugha" } })
vim.lsp.config("lugha", {
  cmd = { "lughac", "lsp" },
  filetypes = { "lugha" },
  root_markers = { "lugha.toml", ".git" },
})
vim.lsp.enable("lugha")
```

Helix, Zed and any other editor with LSP support start `lughac lsp` the same way, for `.la` files.

## Performance

Lugha checks every integer operation and index, and uses a garbage collector. On numeric code it
runs close to C; on code that allocates many small objects it is slower. A sample from
`python3 bench/run.py` (median wall time, `-O2`, Intel i5-4308U):

| Benchmark | Lugha | C | Rust | Go | Node |
| --- | --- | --- | --- | --- | --- |
| mandelbrot | 0.36 s | 0.36 s | 0.41 s | 0.38 s | 0.54 s |
| nbody | 0.65 s | 0.53 s | 0.61 s | 0.69 s | 0.98 s |
| fannkuch | 0.25 s | 0.22 s | 0.22 s | 0.23 s | 0.46 s |
| binarytrees | 1.11 s | 0.52 s | 1.37 s | 1.08 s | 0.54 s |
| wordfreq | 0.56 s | 0.10 s | 0.15 s | 0.35 s | 1.11 s |

All six benchmarks, peak memory and how to run them are in [`bench/`](bench).

## Status

Lugha is young and maintained by one person. It runs real programs, GTK apps among them, but the
language still changes: version 17 is in progress, and a release may break code written for the
last one. Linux on x86-64 is the main platform; ARM64 Linux builds and runs.

## Building and testing lughac

```sh
cargo build                                  # target/debug/lughac
cargo test                                   # everything; needs llvm-21-dev, cc and libssl-dev
cargo fmt --check && cargo clippy --all-targets -- -D warnings
```

End-to-end tests are programs in [`tests/programs/`](tests/programs), grouped by feature: `name.la`
with its expected `name.stdout` and `name.exit`, or `name.stderr` for a program that must be
rejected. Add a test by adding files; no runner changes.

## Contributing

Each feature starts as a short proposal in [`PRPs/`](PRPs), and every design decision and its reason
is in [`DECISIONS.md`](DECISIONS.md). Code follows [`docs/CODE_STYLE.md`](docs/CODE_STYLE.md), and
tests come first. [`TODO.md`](TODO.md) lists what's next and [`CHANGELOG.md`](CHANGELOG.md) what
shipped. Issues and pull requests are welcome.

## License

[GPL-3.0](LICENSE).
