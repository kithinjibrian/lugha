# Lugha

Lugha (Swahili for "language") is a small, statically typed, compiled language with Rust-flavored structure and TypeScript-flavored declarations. `lughac` compiles a `.la` file through LLVM to a native executable; heap memory is managed by the Boehm garbage collector.

```
fun fib(n: i64): i64 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    println(fib(30));
    0
}
```

```console
$ lughac run fib.la
832040
```

v0 has integers (`i32`, `i64`, `u8`), `f64`, `bool`, immutable strings, fixed-length arrays, structs, `if` expressions, loops, `for … of`, `extern fun` for calling C, and checked arithmetic and bounds that panic instead of misbehaving. Everything is a value: assigning an array or struct copies it. The full definition is [`docs/specs/Language v0 Specification.md`](docs/specs/Language%20v0%20Specification.md).

## Prerequisites

- **Rust** via [rustup](https://rustup.rs). The toolchain version is pinned in `rust-toolchain.toml`, and rustup installs it on first build.
- **LLVM 21** development files. It must be major version 21, and it is linked dynamically.
- **Boehm GC** development files and a **C compiler** (`cc`). `lughac` links every program with them.

On Ubuntu 26.04:

```sh
sudo apt install llvm-21-dev libgc-dev build-essential
```

## Build and install

```sh
cargo build --release          # target/release/lughac
cargo install --path .         # or put lughac on your PATH
```

## Usage

| Command | Effect |
| --- | --- |
| `lughac build prog.la [-o prog]` | Compile and link an executable |
| `lughac run prog.la` | Build to a temporary file and run it; the exit code is the program's |
| `lughac check prog.la` | Lex, parse and type-check only |
| `lughac spec` | Print the language spec bundled with this compiler |
| `lughac build --emit=tokens\|ast\|ir prog.la` | Print an intermediate stage and stop |

`build`, `run` and `check` take `--diagnostics=human|json` (human is the default). `build` and `run` take `-O0` (default) or `-O2`.

Exit codes:
- **0** — success.
- **1** — the program has errors, reported with codes such as `E0401`.
- **2** — bad usage, or an internal compiler error.

`lughac spec` prints the specification matching your compiler, ready to paste into an AI assistant's prompt.

## Tests

```sh
cargo test
```

- **Example programs:** every program in spec §10 is extracted from the spec and run (`tests/spec.rs`).
- **Acceptance tests:** these live in `tests/programs/`. Add one by adding files, without editing any runner:
  - a program: `name.la`;
  - its expected output: `name.stdout` and `name.exit`;
  - for a program that must be rejected: `name.stderr`.

CI runs `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` on every push and pull request.

## Contributing

The project is developed with AI coding assistants under a context system:
- [`CLAUDE.md`](CLAUDE.md) holds the rules.
- [`PRPs/`](PRPs) holds one brief per feature.
- [`MEMORY.md`](MEMORY.md) and [`DECISIONS.md`](DECISIONS.md) hold settled and open decisions.
- [`CONTEXT.md`](CONTEXT.md) holds the session log.
- [`docs/setup.md`](docs/setup.md) explains the system.

Human contributors follow the same flow: a PRP before code, and tests first.

## License

GPL-3.0 — see [LICENSE](LICENSE).
