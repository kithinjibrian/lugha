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

Lugha v2 and v3 are complete (milestones 9–16), and v4, input and output, is complete (milestones 17–19), with v4.1 allowing string literals inside interpolations. v5, interface values, is complete (milestones 20–22), and v6, generic interfaces, iterators, `Self` and converting between interfaces, is complete (milestones 23–26). v7, numbers and bits, is complete (milestones 27–29: `u32`, `u64`, `f32`, the bit operators and integer methods), v8, packages, is complete (milestone 30), v9, `match` conveniences, is complete (milestones 31–33), v10, `Self` everywhere, more integers and iterator helpers, is complete (milestones 34–36), v11, package versions and re-exports, is complete (milestones 37–38), v12, printing any value, is complete (milestones 39–40), v13, `Self` in declarations, `for … of mut`, more iterator helpers and format specifiers, is complete (milestones 41–44), and v14 is in progress (milestones 45–46 done). Today it has everything in v1.2:
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
- **More numbers (v7, milestone 27):** `u32`, `u64` and `f32`, as in `let h: u64 = 14695981039346656037;` and `-1 as u32`; an `f32` prints its own shortest form (`0.1`).
- **Bits (v7, milestone 28):** `& | ^ ~ << >>` with Rust's precedence (`flags & MASK == 0`), `&=` and friends, `0b1010` literals; a shift by the width or more panics.
- **Integer methods (v7, milestone 29):** `h.wrapping_mul(1099511628211)` and friends never panic; `x.count_ones()`, `leading_zeros()`, `trailing_zeros()` give an `i64`.
- **Packages (v8, milestone 30):** a `lugha.toml` next to `main.la` lists package folders (`[packages]` / `geo = "libs/geo"`), and `import geo.shapes;` loads a package's module; a package can list its own packages, and other packages see only what it marks `public`.
- **Better patterns (v9, milestone 31):** `Circle(r) | Square(r) => r * 2.0`, `"q" | "quit" => …`, and `Some(mut n) => { n += 1; … }` with the arm's own copy.
- **Unwrapping (v9, milestone 32):** `parse_i64(s).unwrap()` and `.expect("need a number")` panic at the call on `None`/`Err`; `x %= 10`.
- **Conditional patterns (v9, milestone 33):** `if let Some(n) = parse_i64(w) { … } else { … }`, `while let Some(x) = stack.pop() { … }`, and `let Some(n) = o else { return; };`.
- **`Self` everywhere (v10, milestone 34):** `fun origin(): Self = Self { x: 0, y: 0 };` and `Self.Red` inside `impl` blocks; an interface's default method called straight on a type, `p.describe()`.
- **More integers (v10, milestone 35):** `i8`, `i16` and `u16`; `a.checked_add(1)` gives `None` instead of panicking, `saturating_mul` clamps, `rotate_left(n)` rotates; `i16.MAX`; `parse_u64(s)`.
- **Iterator helpers (v10, milestone 36):** `xs.iter().filter(even).map(f).collect()`, `range(1, 101).sum()`, `s.bytes().enumerate()` and `for b of s`, lazy so `take(3)` stops early.
- **Package versions (v11, milestone 37):** a package's `lugha.toml` says `[package]` `version = "1.4.0"`, and a program requires `geo = { path = "libs/geo", version = "1.2" }`; an incompatible folder is E0324.
- **Re-exports (v11, milestone 38):** `import mathx.vec { Vec2, add };` brings items into a file, and `public import mathx.vec { Vec2 };` lets users of your package write `shapes.Vec2` without listing `mathx`.
- **Printing values (v12, milestone 39):** `println(Point { x: 1, y: 2 })` prints `Point { x: 1, y: 2 }`; arrays, enums and `Option` print too, and a type's own `fun show(self): string` overrides the form.
- **Printing anything (v12, milestone 40):** interface values print as their contents, function values as `<fun>`, and `r.unwrap()` on an `Err` panics with `unwrap on Err: <the error>`.
- **`Self` in declarations (v13, milestone 41):** `struct Tree<T> { value: T, kids: Self[] }` and `enum List { Cons(i64, Self), Nil }`.
- **`for … of mut` (v13, milestone 42):** `for x of mut xs { x *= 2; }` changes the array, and `for w of mut words { … break; }` leaves the iterator where it stopped.
- **More iterator helpers (v13, milestone 43):** `a.zip(b)` (yielding `Pair { first, second }`, tuples since v14), `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max` and `position`.
- **Format specifiers (v13, milestone 44):** `"{pi:.2}"`, `"{n:>6}"`, `"{n:05}"`, `"{n:#x}"`, `"{name:*^9}"`.
- **Bytes and indexes (v14, milestone 45):** `read_bytes`/`write_bytes`/`append_bytes`/`read_stdin_bytes`, `s.to_bytes()`, `string_from_utf8(bytes)` (checked), and `xs[i]` with `i` of any integer type.
- **Unicode text (v14, milestone 46):** a `char` type with `'é'` literals, `s.chars()`, `s.char_count()`, `c as u32`, `char_from(n)`, and `is_alphabetic`/`to_upper`-style methods for ASCII and Latin-1.
- **Tuples (v14, milestone 47):** `fun divmod(a: i64, b: i64): (i64, i64)`, `let (q, r) = divmod(7, 2);`, `t.0`, `for (i, w) of xs.iter().zip(ws.iter())`, tuple patterns in `match`, `==` and `<`, printed `(1, "a")`.
- **`format` (v14, milestone 48):** `format("{} has {:>4} items", name, n)` returns a `string`; the template is a literal checked when compiling.
- **Import aliases and re-exports (v14, milestone 49):** `import a.util as u1;`, `import shapes.vec { Vec2 as V };`, and `public import shapes.vec;` so that importers write `shapes.vec.Vec2`.
- **Git packages (v14, milestone 50):** `geo = { git = "https://example.org/geo.git", version = "1.4" }` in `lugha.toml` fetches the newest matching `vX.Y.Z` tag into a per-user cache and pins its commit in `lugha.lock`; `lughac update` refreshes it and `--offline` never fetches.
- **Package layout (v15, milestone 51):** a library's `lugha.toml` can say `[package] name = "glowworm"` and `source = "src"`, so `src/array.la` is `glowworm.array`; a program inside the package (`examples/basics.la`) imports it by name with no manifest of its own.
- **Operator overloading (v15, milestone 52):** give a struct `fun add(self, o: Vec2): Vec2` and `a + b` calls it (likewise `sub`, `mul`, `div`, `rem`, `neg`); `fun add<T: Add>(a: T, b: T): T = a + b;` works for numbers, strings and your types; `impl Grid<T: Num> { … }` does element arithmetic.
- **The standard library (v15, milestone 53):** `import std.math;` then `math.sqrt(2.0)`, `math.pow`, `math.sin`, `math.round`, `math.abs(-3)`, `math.max(2, 7)`, `math.clamp`, `math.pi()`; the float functions work for any `T: Float` (`f32`, `f64`). Std modules ship inside `lughac` and cost nothing unless imported.
- **Copy-on-write arrays (v15, milestone 55):** `let g = grads[i];` no longer copies the array; it is shared until either side writes, and the writer copies it first. Programs print exactly what they did before, and read-heavy code such as firefly's autograd trains 13–35% faster.
- **Numbers as `T` (v15, milestone 58):** generic numeric code can write `s / xs.len as T`, `i as T`, `x as f64`, `x * 2` and (for `T: Float`) `x * 0.5`; a literal too big for one instance (`300` as a `u8`) is reported at that literal.
- **Nested `mut self` calls (v15, milestone 57):** `g.matmul(g.tanh(g.add(a, b)), w)` works: arguments may call methods on the receiver or read numbers from it, and run before the call. Only real aliasing, like `v.add(v)`, is still rejected.
- **Owning arrays once per loop (v15, milestone 56):** a loop that writes into arrays it can't share (`counts[c] += 1`, `out[i][j] += …`) makes them unique once before it starts, so copy-on-write costs nothing inside it.
- **Random numbers (v15, milestone 54):** `import std.random;`, then `let mut rng = random.seeded(42);` (or `random.from_os()`), and `rng.uniform()`, `rng.below(6)`, `rng.between(lo, hi)`, `rng.normal(0.0, 1.0)`, `rng.chance(p)`, `rng.choice(xs)`, `rng.shuffle(xs)`. xoshiro256\*\*, repeatable for a seed; not cryptographically secure.

Everything is a value: assigning an array, struct or enum gives you your own copy. The full definition is [`docs/specs/Language v15 Specification.md`](docs/specs/Language%20v15%20Specification.md) (what `lughac spec` prints, in progress); the approved plan it is built from is [`Language v15 Draft.md`](docs/specs/Language%20v15%20Draft.md), and v14 is frozen in [`Language v14 Specification.md`](docs/specs/Language%20v14%20Specification.md).

## Prerequisites

- **Rust** via [rustup](https://rustup.rs). The toolchain version is pinned in `rust-toolchain.toml`, and rustup installs it on first build.
- **LLVM 21** development files. It must be major version 21, and it is linked dynamically.
- **Boehm GC** development files and a **C compiler** (`cc`). `lughac` links every program with them.
- **git**, only for programs whose `lugha.toml` lists git packages (v14).

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
| `lughac update prog.la` | Fetch each git package's newest matching tag and rewrite `lugha.lock` |
| `--offline` | On `build`, `run` and `check`: never run `git`; git packages must be locked and cached |
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
