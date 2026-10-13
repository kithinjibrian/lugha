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

Lugha v2 and v3 are complete (milestones 9–16), and v4, input and output, is complete (milestones 17–19), with v4.1 allowing string literals inside interpolations. v5, interface values, is complete (milestones 20–22), and v6, generic interfaces, iterators, `Self` and converting between interfaces, is complete (milestones 23–26). Today it has everything in v1.2:
- **Basic types:** integers (`i32`, `i64`, `u8`), `f64`, `bool` and immutable strings.
- **Compound types:** growable arrays (`push`, `pop`, `clear`), structs, and enums with exhaustive `match`, including `Option<T>` and recursive enums.
- **Behaviour:** methods in `impl` blocks, `if` expressions, loops and `for … of`, and `swap(a, b)` to exchange two places without copying.
- **C interop:** `extern fun` for calling C.
- **Safety:** checked arithmetic and bounds that panic instead of misbehaving.
- **New in v2 so far (milestone 9):** `==` on every type (deep, structural), bytewise string order (`"ab" < "abc"`), and nested `match` patterns with `if` guards (`Some(Circle(r)) if r > 1.0 =>`).
- **Modules (milestone 10):** a program can span several files: `import geometry;`, then `geometry.origin()`, `geometry.Vec2 { … }`. Everything is visible to the whole program unless marked `private`, which keeps an item, method or field inside its file.
- **Generics (milestone 11):** generic functions, structs, enums and methods (`fun max<T: Ord>(a: T, b: T): T`, `struct Stack<T>`), with `Eq`/`Ord` bounds and type arguments that are always inferred; each instance is compiled separately.
- **Interfaces (milestone 12):** `interface Shape { fun area(self): f64; }` as a bound (`fun total<T: Shape>(xs: T[])`); any type with matching methods satisfies it, with no declaration, and each call compiles to the concrete method.
- **Input and interpolation (v3, milestone 13):** `args()` and `parse_i64`/`parse_f64` read the command line, and strings interpolate: `"{name} is {age}"` (since v4.1 with string literals inside, `"{parts.join("-")}"`). Pass arguments with `lughac run prog.la -- 21`.
- **Functions as values (v3, milestone 14):** `fun(i64): i64` is a type; named functions can be stored, passed and returned (`let fs = [double, triple]; apply(fs[0], 3)`), and any function value can be called.
- **Closures (v3, milestone 15):** `let add = fun(x) = x + k;` captures `k` by copy; parameter types come from the expected function type, and `fun adder(k: i64): fun(i64): i64 = fun(x) = x + k;` returns one.
- **Array helpers (v3, milestone 16):** `map`, `filter`, `fold`, `sort_by` (stable), `any` and `all`, each returning a new value: `people.filter(fun(p) = p.age >= 18).map(fun(p) = p.name)`.
- **Results (v4, milestone 17):** `Result<T, E>` with `Ok`/`Err`, and `?` to return a failure early: `let h = half(n)?;`.
- **Text helpers (v4, milestone 18):** `s.lines()`, `s.split(",")`, `s.trim()`, `s.contains(x)`, `s.slice(a, b)`, `parts.join("-")`, and `unwrap_or`, `ok_or`, `map` on `Option`/`Result`.
- **Input and output (v4, milestone 19):** `read_line()` for stdin, `read_file`/`write_file`/`append_file` returning `Result`, `eprint`/`eprintln` for stderr, and `exit(code)`.
- **Interface values (v5, milestone 20):** an interface is a type, so one array holds different shapes: `let shapes: Shape[] = [Circle { r: 1.0 }, Square { side: 2.0 }];`, and `s.area()` runs each one's own method. Interface values are values: copies are independent.
- **Type patterns (v5, milestone 21):** `match s { c: Circle => c.r, _: Square => 1.0, _ => 0.0 }` gets the concrete value back out of an interface value.
- **Default methods and error conversion (v5, milestone 22):** `fun report(self): string = "error: {self.message()}";` inside an interface, and `?` turning a `ParseError` into an `Error` in a function returning `Result<T, Error>`.
- **Generic interfaces (v6, milestone 23):** `interface Source<T> { fun next(mut self): Option<T>; }` and `fun collect<T, S: Source<T>>(src: S): T[]`, where `collect(Count { … })` works out `T = i64` from `Count.next`; generic interfaces are types too, `let s: Source<i64> = Count { … };`.
- **Iterators (v6, milestone 24):** `for n of c { … }` loops over anything with `fun next(mut self): Option<T>`, such as a struct, a type parameter `I: Iter<T>` or an `Iter<i64>` value, and works on a copy, so `c` is unchanged afterwards.
- **`Self` (v6, milestone 25):** `interface Ordered { fun less(self, other: Self): bool; }` is satisfied by `Money` with `fun less(self, other: Money): bool`, and `fun largest<T: Ordered>(xs: T[]): T` works on it; such an interface is a bound only (E0427 as a type).
- **Converting between interfaces (v6, milestone 26):** a `Pet` value becomes a `Named` where one is expected when `Pet` declares everything `Named` does; the dog inside is still a `Dog` to a type pattern, and `?` converts interface errors the same way.

Everything is a value: assigning an array, struct or enum gives you your own copy. The full definition is [`docs/specs/Language v6 Specification.md`](docs/specs/Language%20v6%20Specification.md) (what `lughac spec` prints); the approved plan it is built from is [`Language v6 Draft.md`](docs/specs/Language%20v6%20Draft.md), and v5 is frozen in [`Language v5 Specification.md`](docs/specs/Language%20v5%20Specification.md).

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
| `lughac run prog.la [-- args…]` | Build to a temporary file and run it, passing `args` to it; the exit code is the program's |
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
