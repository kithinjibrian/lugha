# Lugha v15 Specification

Oct 13, 2026 · @kithinji

**Status:** in progress (milestones 51–66 of v15). It is built from the approved plan, `Language v15 Draft.md`. This file describes exactly what `lughac` implements, and is what `lughac spec` prints. `Language v14 Specification.md` keeps v14 frozen.

**v14.** This is v13 plus binary I/O, `char`, tuples, `format`, import aliases and whole-module re-exports, and git packages (milestones 45–50).

**Changes since v14** (milestones 51–66):
- A package's `[package]` table may give its `name`, checked against the key a manifest lists it under (E0327), and its `source`, the folder its modules start in: with `source = "src"`, `src/array.la` of package `glowworm` is `glowworm.array` (section 6).
- A program with no `lugha.toml` beside its main file uses the nearest one above it, looking no further than a folder with `.git` or the home folder; when that manifest names a package, the program imports it by that name, so `glowworm/examples/basics.la` writes `import glowworm.array;` with no manifest of its own (section 6).
- A file imported as two modules is E0328 (section 6).
- A manifest path such as `".."` resolves when the main file is given with a folder (`lughac run example/main.la`); v14 reported E0322.
- Operator overloading: `a + b` on a struct, enum or interface value calls `a.add(b)`, and likewise `-` `sub`, `*` `mul`, `/` `div`, `%` `rem` and unary `-` `neg`, for a method of the operator's shape; `a += b` is `a = a.add(b)`. The prelude's `Add`, `Sub`, `Mul`, `Div`, `Rem`, `Neg` and `Num` are bounds that the numbers (and `string`, for `Add`) satisfy, so `fun add<T: Add>(a: T, b: T): T = a + b;` works for numbers, strings and your own types; `0` and `1` are values of a `T: Num`. An `impl` block may bound its parameters, `impl Grid<T: Num> { … }` (sections 3–5).
- A standard library: `import std.math;` loads a module bundled with `lughac`, only when imported. `std.math` has `sqrt`, `pow`, `exp`, `ln`, `log10`, `sin`, `cos`, `tan`, `atan2`, `floor`, `ceil` and `round` for any `T: Float` (`f32` or `f64`, a new prelude bound), `abs`, `min`, `max` and `clamp`, and `pi()` and `e()`. `std` is a reserved package name (sections 4, 6, "The standard library").
- Arrays are copy-on-write: a copy shares the array until either side writes to it, so copying costs a constant time and reading a copy never copies. Every copy still behaves as an independent value, and no program's output changes (sections 4, 5, 7, 9, 12). A loop makes the arrays it writes, and can't share, unique once before it starts, so its writes cost no more than before copy-on-write (milestone 56; section 9).
- An argument of a `mut self` call may call methods on the receiver, `g.tanh(g.add(a, b))`, or read numbers and strings from it; only an argument that can be the receiver's own storage (`v.add(v)`, `v.add(v.inner)`) is E0508. The arguments run before the receiver is reached, and a change they make to a struct containing the receiver is no longer E0510 unless an array index lies between (milestone 57; sections 4–5).
- Numbers as `T`: `n as T` and `x as f64` cast to and from a type parameter bounded by `Num` or `Float`, and any integer literal is a `T: Num` and any float literal a `T: Float` (`x * 2`, `x * 0.5`); a literal that doesn't fit an instance's type is E0402, naming the instance (milestone 58; section 4).
- User-defined order: a struct or enum with `fun cmp(self, other: Self): Ordering`, the prelude's new `enum Ordering { Less, Equal, Greater }` (written `Ordering.Less`), is `Ord`: `<`, `<=`, `>` and `>=` call `cmp`, tuples compare through it, and `math.min`, `it.min()` and the new `xs.sort()` accept it. `==` stays structural (milestone 59; section 4).
- An imported std module's functions and methods are compiled only when the program reaches them, as the prelude's are; every one is still checked. No program's behaviour changes, only the size of its IR (milestone 60; sections 6, 9).
- `it.sum()` goes through one generic prelude helper for every numeric type; nothing visible changes (milestone 61; section 9).
- `parse_i64`, `parse_u64` and `string_from_utf8` are written in Lugha in the prelude, and `string_from_utf8` and `s.chars()` share one UTF-8 decoder; results and messages are unchanged (milestone 62; section 9).
- A format width counts characters, not bytes (`{"Zoë":<6}` pads with three spaces, not two), and any character may fill (`{n:é^9}`); text without non-ASCII characters prints as before (milestone 63; section 5).
- `std.env`: `env.get(name)` reads an environment variable as an `Option<string>`, and `env.current_dir()` gives the working directory; nothing sets either (milestone 64; "The standard library").
- `std.fs`: `fs.list_dir`, `make_dir`, `make_dirs`, `remove_file`, `remove_dir` (empty folders only), `exists`, `is_dir` and `is_file` (milestone 65; "The standard library").
- `std.process`: `process.run`, `run_with` and `call` run another program with an argument list, never through a shell, and give its exit code and output (milestone 66; "The standard library").
- `std.random`: `random.seeded(42)` or `random.from_os()` makes a generator (xoshiro256\*\*, not cryptographically secure) with `uniform`, `below`, `between`, `normal`, `chance`, `choice` and `shuffle` ("The standard library").

Every valid v14 program and package is a valid v15 one: a module's own type or interface named `Add`, `Sub`, `Mul`, `Div`, `Rem`, `Neg`, `Num` or `Ordering` still means its own, as for `Iter`.

**Changes in v14 since v13** (milestones 45–50):

**v13.** This is v12 plus `Self` in declarations, `for … of mut`, more iterator helpers and format specifiers (milestones 41–44).

- Binary I/O: `read_bytes`, `write_bytes`, `append_bytes`, `read_stdin_bytes`; `s.to_bytes()`; `string_from_utf8(bytes)`, which rejects invalid UTF-8. An index may be of any integer type (sections 4–5).
- `char`, a Unicode scalar value: `'a'`, `'é'`, `'\u{1F600}'`; `s.chars()` and `s.char_count()`; `c as u32`, `n as char`, `char_from(n)`; classification and case for ASCII and Latin-1 (sections 2–5).
- Tuples: `(A, B)` types and `(a, b)` values, `t.0`, patterns `(p, q)` in `match`, `if let`, `let (a, b) = t;` and `for (i, w) of …`; element-wise `==`, lexicographic `<`, printed `(1, "a")`. `zip` yields tuples, and `Pair` is gone (sections 3–5).
- `format("{} has {:>4} items", name, n)` returns a `string`: a literal template whose holes `{}` and `{:spec}` take the further arguments in order (E0432; sections 2, 5).
- Imports take `as`: `import a.util as u1;` binds the module as `u1`, and `import a.b { X as Y };` binds the item as `Y`. `public import a.b [as v];` re-exports the whole module, which files importing this one reach as `m.v.X` (sections 3, 6).
- Git packages: a manifest entry `name = { git = "https://…", version = "1.2" }` fetches the newest matching `vX.Y.Z` tag with the system `git` into a per-user cache, and `lugha.lock` pins its commit; `lughac update` re-resolves, and `--offline` never runs `git` (E0325, E0326; sections 6, 9).

A valid v13 program is a valid v14 program unless it uses `Pair` or `zip`'s `Pair` fields (`zip` yields tuples now: `p.first` is `p.0`), uses `char` as a name (a type keyword now), or declares a function named `format`, `read_bytes`, `write_bytes`, `append_bytes`, `read_stdin_bytes` or `string_from_utf8`, which are now intrinsics.

**v12.** This is v11 plus printing any value: the prelude's `Display` and `show`, built-in forms, and `unwrap on Err: <error>` (milestones 39–40).

**Changes in v13 since v12** (milestones 41–44):
- In a struct's fields and an enum's payloads, `Self` is the type being declared: `struct Tree<T> { value: T, kids: Self[] }`, `enum List { Cons(i64, Self), Nil }` (section 4).
- `for x of mut xs` makes `x` each element itself, so `x *= 2` changes `xs`; `for x of mut it` advances the iterator `it` itself, so a later loop resumes where a `break` left it (sections 3, 5).
- Iterators gain `zip` (yielding the prelude's `Pair { first, second }` in v13; a tuple since v14), `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max` and `position` (section 4).
- An interpolation may end in a format specifier: `"{pi:.2}"`, `"{n:>6}"`, `"{n:05}"`, `"{n:#x}"`, `"{name:*^9}"`; a bad one is E0431 (sections 3, 5).

Every valid v12 program was a valid v13 program.

**v11.** This is v10 plus package versions and named imports and re-exports (milestones 37–38).

**Changes in v12 since v11** (milestones 39–40):
- `print`, `println`, `eprint`, `eprintln`, `to_string` and `"{x}"` accept structs, enums, arrays, `Option`, `Result` and type parameters. A type with its own `fun show(self): string` (the prelude's `Display`) prints through it; any other prints in a built-in form, `Point { x: 1, y: 2 }`, `Circle(1.5)`, `[Some(1), None]`, with the strings inside it quoted (sections 4–6).
- Interface values print as the value inside them, function values as `<fun>`, and `unwrap()` on an `Err` panics with `unwrap on Err: ` followed by the error (sections 4–5).

Every valid v11 program was a valid v12 program; only the `unwrap on Err` panic message is longer.

**v10.** This is v9 plus `Self` in `impl` blocks, default methods on concrete types, `i8`/`i16`/`u16` with checked, saturating and rotating methods, and iterator helpers (milestones 34–36).

**Changes in v11 since v10** (milestones 37–38):
- A package's `lugha.toml` may declare its version, `[package]` then `version = "1.4.0"`, and a manifest may require a compatible one, `geo = { path = "libs/geo", version = "1.2" }`; a version that doesn't meet it is E0324 (section 6).
- `import a.b { X, y };` brings items into a file by name, and `public import a.b { X, y };` re-exports them as items of the module, so a program can use `shapes.Vec2` from a package it doesn't list (sections 3, 6).

Every valid v10 program and manifest was valid in v11.

**v9.** This is v8 plus or-patterns, `mut` bindings, `unwrap`/`expect`, `%=`, and `if let`, `while let` and `let … else` (milestones 31–33).

**Changes in v10 since v9** (milestones 34–36):
- Inside `impl T`, `Self` is `T` (with its parameters, `Stack<T>`), as a type, in struct literals (`Self { x: 0 }`), associated calls (`Self.new()`) and variants (`Self.Red`) (section 4).
- A default method can be called on a struct or enum that lacks its own, `c.describe()`, when exactly one visible interface the type satisfies supplies it; two are E0430 (section 4).
- `i8`, `i16` and `u16` are integer types like the others. Every integer type has `checked_add/sub/mul/div` (an `Option`), `saturating_add/sub/mul` and `rotate_left/right`, and the constants `I.MIN` and `I.MAX` (`i16.MAX`); `parse_u64(s)` parses a `u64` (sections 2–5, 8).
- Iterator helpers: `xs.iter()`, `s.bytes()` and `range(a, b)` make iterators; `it.map(f)`, `filter`, `take`, `skip` and `enumerate` wrap one lazily, and `collect`, `count`, `fold`, `any`, `all`, `find` and `sum` consume it. `for b of s` loops over a string's bytes (sections 4–5).

A valid v9 program was a valid v10 program unless it uses `i8`, `i16` or `u16` as a name, which are type keywords now, or declares a function named `parse_u64`, which is now an intrinsic.

**v8.** This is v7 plus packages: a `lugha.toml` manifest, package imports, and `public` / package / `private` visibility (milestone 30).

**Changes in v9 since v8** (milestones 31–33):
- A pattern may be alternatives, `Circle(r) | Square(r) => r * 2.0` or `"q" | "quit" => …`, nested anywhere; every alternative binds the same names at the same types (E0428). `mut name` makes a binding the arm's own copy, which the arm may change (sections 3–4).
- `o.unwrap()` and `o.expect(msg)` on `Option` and `Result` give the value inside or panic at the call (`unwrap on None`, `unwrap on Err`, or `msg`); `x %= y` is the compound remainder (sections 2–5).
- `if let p = e { … } else { … }`, `while let p = e { … }` and `let p = e else { … };` test one pattern without a full `match`; a `let … else` block must leave (E0429) (sections 3, 5).

Every valid v8 program was a valid v9 program.

**v7.** This is v6 plus `u32`, `u64` and `f32`, bit operators, and wrapping and bit-counting methods on integers (milestones 27–29).

**Changes in v8 since v7** (milestone 30):
- A `lugha.toml` next to the main file lists packages, `name = "path"` under `[packages]`; `import geo.shapes;` then loads `shapes.la` from package `geo`'s folder, and a package may list its own packages in its own `lugha.toml` (section 6).
- `public` is allowed (it was reserved, E0317): an item without a keyword is now visible within its own package, `public` makes it visible to other packages, and `private` still limits it to its module. A `public` declaration may mention only `public` types (sections 3, 6).

Every valid v7 program was a valid v8 program: one without a `lugha.toml` is a single package, where the default visibility still reaches every module.

**v6.** This is v5 plus generic interfaces, iterators, `Self` in interfaces and converting between interfaces (milestones 23–26).

**Changes in v7 since v6** (milestones 27–29):
- `u32`, `u64` and `f32` are numeric types: literals take them from context, arithmetic and overflow panics work at their widths, every numeric type casts to every other, and they print, interpolate and cross the C boundary (sections 2, 4, 5, 7–9).
- Bit operators `&`, `|`, `^`, `~`, `<<` and `>>` on integers, with Rust's precedence (`flags & MASK == 0` is `(flags & MASK) == 0`), the compound forms `&=` `|=` `^=` `<<=` `>>=`, binary literals (`0b1010`), and a panic for a shift that is negative or at least the type's width (sections 2–5, 9).
- Every integer type has `wrapping_add`, `wrapping_sub` and `wrapping_mul`, which never panic, and `count_ones`, `leading_zeros` and `trailing_zeros`, which give an `i64` (sections 4, 9).

A valid v6 program was a valid v7 program unless it used `u32`, `u64` or `f32` as a name: they are type keywords now, like `i32`.

**v5.** This is v4 plus interface values, type patterns, default methods and `?` converting into an interface error (milestones 20–22).

**Changes in v6 since v5** (milestones 23–26):
- An interface may take type parameters, and a bound passes it arguments: `interface Source<T> { fun next(mut self): Option<T>; }`, `fun collect<T, S: Source<T>>(src: S): T[]`. A parameter that appears only in a bound's arguments is read off the concrete type's methods. A generic interface is a type too: `let s: Source<i64> = Count { n: 0, end: 3 };` (sections 3–4, 9).
- The prelude declares `interface Iter<T> { fun next(mut self): Option<T>; }`, and `for x of e` loops over anything with that `next`, working on a copy of `e` (sections 4–5, 9).
- Inside an interface, `Self` is the type that satisfies it: `fun less(self, other: Self): bool`. Such an interface is a bound only; as a type it is E0427 (sections 4, 9).
- A value of one interface type converts to another where that one is expected, when its interface declares every item of the other: a `Pet` becomes a `Named`, and `?` converts interface errors the same way (sections 4, 7, 9).

Every valid v5 program was a valid v6 program.

**v4.** This is v3 plus `Result` and `?`, string and `Option`/`Result` helpers, and input and output (milestones 17–19); v4.1 allows string literals inside interpolations.

**Changes in v5 since v4** (milestones 20–22):
- An interface whose items are all methods is a type: `let shapes: Shape[] = [c, sq];`. A value converts to it where an interface is expected, and a call on it runs the concrete type's method (sections 4, 7, 9).
- A type pattern, `c: Circle => …` in a `match` on an interface value, matches when the value holds that type and binds it (sections 3–4).
- An interface method may have a default body, which a type gets unless it has the method itself (sections 3–4, 9).
- `?` converts an error into an interface error: in a function returning `Result<U, Error>`, `?` on a `Result<T, ParseError>` wraps the `ParseError` as an `Error` (section 4).

Every valid v4 program was a valid v5 program.

**v3.** This is v2 plus command-line arguments and number parsing, string interpolation, functions as values, closures, and array helpers (milestones 13–16).

**Changes in v4 since v3** (milestones 17–19):
- `Result<T, E>` is predeclared, with `Ok(x)` and `Err(e)` written without a prefix (section 4).
- The postfix `?` gives a `Result`'s `Ok` value or an `Option`'s `Some` value, or returns the failure from the function at once (sections 2–4).
- Strings have `lines`, `split`, `trim`, `contains`, `starts_with`, `ends_with` and `slice`, `string[]` has `join`, and `Option` and `Result` have `unwrap_or`, `ok_or`, `ok`, `map`, `map_err` and `is_*` (section 4).
- Programs read stdin with `read_line`, read and write text files with `read_file`, `write_file` and `append_file`, write to stderr with `eprint`/`eprintln`, and stop with `exit(code)` (section 5).

A valid v3 program was a valid v4 program unless it defines something named `Result`, `Ok` or `Err`, or a function named `read_line`, `read_file`, `write_file`, `append_file`, `eprint`, `eprintln` or `exit`.

## 1. Overview

Lugha (Swahili for "language") is a small, statically typed, compiled language with Rust-flavored structure and TypeScript-flavored declarations. It compiles through LLVM to native executables and manages heap memory with the Boehm garbage collector. Source files use the `.la` extension.

v0 is a learning target: every feature in this spec should be implementable by one person in a weekend or two, and every milestone ends in a program that runs.

**Goals**

- A complete pipeline from source text to native binary: lexer, parser, type checker, LLVM codegen, linker.
- Static types with one implicit conversion, to an interface where one is expected (v5), so type errors are caught before codegen.
- Memory safety for ordinary code: no null, no dangling pointers, bounds-checked indexing.
- Clear error messages with source locations.

**Non-goals for v0**

- Generics (other than the built-in `Option<T>`), modules and closures. See section 11.
- Performance tuning beyond what LLVM's optimizer gives for free.
- A self-hosted compiler or a package manager.

**Design principles**

- One way to do each thing. Fewer features, fully specified.
- Immutable by default. A binding can only be reassigned if it is declared `let mut`.
- Every construct maps directly onto LLVM IR without a runtime type system.
- Where Rust and TypeScript disagree, pick whichever is simpler to implement and parse.

**Syntax at a glance**

```
struct Point { x: f64, y: f64 }

fun dist2(a: Point, b: Point): f64 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    dx * dx + dy * dy
}

fun main() {
    let mut total = 0;
    for i in 0..10 {
        total += i;
    }
    println(total);
}
```

## 2. Lexical structure

Source files are UTF-8. The lexer produces a flat list of tokens, each carrying a span (start and end byte offsets) for error reporting.

**Whitespace and comments.** Spaces, tabs, carriage returns and newlines separate tokens and are otherwise ignored. A line comment starts with `//` and runs to the end of the line. v0 has no block comments.

**Identifiers.** `[A-Za-z_][A-Za-z0-9_]*`, excluding keywords. Identifiers are case-sensitive. In the sources bundled with `lughac` (the prelude and `std`), an identifier may also start with `$`, naming a compiler built-in such as `$float.sqrt` (v15); in a program `$` is E0101, so programs can never name one.

**Keywords.**

```
fun  extern  struct  enum  impl  let  mut  if  else  while  for  in  of  match
import  private  public  interface
return  break  continue  true  false  as  self
i32  i64  u8  f64  bool  string  u32  u64  f32  i8  i16  u16  char
```

Built-in type names are keywords, so they can't be shadowed by user structs. `Option`, `Some` and `None` are not keywords but built-in names, reserved like the intrinsics (section 6).

**Literals**

| Kind | Examples | Notes |
| --- | --- | --- |
| Integer | `42`, `1_000_000`, `0xFF`, `0b1010` | Decimal, hex or binary (v7); `_` separators allowed between digits |
| Float | `3.14`, `2.0e-3` | Must have digits on both sides of the `.`; optional exponent |
| String | `"hello\n"` | Double-quoted; escapes `\n` `\t` `\r` `\\` `\"` `\0` |
| Boolean | `true`, `false` | Keywords |

**Number details.**

- `_` may appear only between two digits, and only in integers. `1__0`, `1_`, `0x_FF` and `1_0.5` are errors.
- The hex prefix is lowercase `0x` followed by at least one hex digit, in either case. `0XFF` and `0x` are errors. The binary prefix (v7) is lowercase `0b` followed by at least one `0` or `1`: `0B1`, `0b` and `0b102` are errors.
- Leading zeros are allowed and mean nothing special: `007` is 7.
- A literal without a `.` is an integer. `2e5` is an error; write `2.0e5`.
- An exponent is `e` or `E`, an optional sign, then at least one digit.
- A `.` not followed by a digit ends an integer: `1..10` is `1` `..` `10`.
- A letter directly after a number is an error (`123abc`, `1.5x`).
- The lexer accepts integers up to 2⁶⁴−1. Whether a literal fits its actual type is checked later (section 4).
- A float literal that rounds to infinity is an error. One that underflows rounds to zero.

**String details.** A string may not contain a raw line break: write `\n`. An unclosed string is reported at the end of its line, and lexing continues on the next one. Any other UTF-8 may appear inside strings and comments. Outside them, only ASCII is allowed.

**String interpolation** (v3). Inside a string literal, `{` starts an interpolation that runs to its matching `}`: `"{name} is {age * 2}"`. Braces inside the expression nest, so `"{if c { 1 } else { 2 }}"` works. The expression may contain string literals, which may interpolate in turn (v4.1): `"<{parts.join("-")}>"`, `"{if ok { "yes" } else { "no" }}"`, `"{name}: {"{a}/{b}"}"`. `{{` and `}}` are literal braces. The lexer emits such a string as its literal parts (`STRING_START`, `STRING_MID`, `STRING_END`) with the expression's tokens between them, a nested string's tokens among them; escapes apply in every part. Errors (E0111): a lone `}`, a `{` never closed, or an empty `{}`. Strings stay on one line: a line break ends a string that began inside an interpolation with E0111 at that interpolation's `{`, and otherwise ends the string (E0102).

**`format` templates** (v14). A string literal directly after `format` `(` (the name not after a `.`) is a template: one `STRING` token whose escapes are decoded but whose braces are kept as written, so nothing in it interpolates. Section 5 says what its braces mean.

**Lexical errors.** The lexer reports every error it finds, not just the first.

| Code | Error | Example |
| --- | --- | --- |
| E0101 | Unexpected character | `@`, `#`, `é` or `$` outside a string |
| E0102 | Unterminated string | `"abc` at end of line or file |
| E0103 | Unknown escape sequence | `"\q"` |
| E0104 | Integer literal too large | `18446744073709551616` |
| E0105 | Misplaced `_` in a number | `1__0`, `1_`, `0x_FF`, `1_0.5` |
| E0106 | Number is missing digits | `0x`, `2.0e`, `2.0e+` |
| E0107 | Float needs a fractional part | `2e5` (help: write `2.0e5`) |
| E0108 | Invalid character in a number | `123abc`, `0XFF`, `0xFG`, `0b102` |
| E0109 | Float literal out of range | `1.0e999` |
| E0110 | Source is not valid UTF-8 | A Latin-1 `é` byte; reported at the first bad byte |
| E0111 | Bad interpolation in a string (v3) | `"a } b"`, `"{x"`, `"{}"` — write `{{`/`}}` for literal braces |
| E0112 | A bad `char` literal (v14) | `''`, `'ab'`, `'\q'`, `'\u{D800}'`, `'\u{110000}'`, an unclosed `'a` |

**Operators and punctuation.** The lexer uses longest match, so `<=` is one token, not `<` then `=`.

```
+  -  *  /  %  ==  !=  <  <=  >  >=  &&  ||  !
&  |  ^  ~  <<                                  (v7)
=  +=  -=  *=  /=  %=  &=  |=  ^=  <<=          (v7: the bit forms; v9: `%=`)
(  )  {  }  [  ]  ,  ;  :  .  ..  =>  ?
```

`>>` and `>>=` (v7) are not tokens: the lexer gives two `>` (or `>` and `>=`), so `Option<Option<i64>>` closes both type-argument lists, and an expression reads `>` immediately followed by `>`, with nothing between, as a right shift (`>>=` likewise in an assignment). With a space between (`a > > b`), they stay two `>`, a parse error.

`::` is lexed as a token only so the parser can reject it helpfully (E0207): Lugha writes `Shape.Circle`, not `Shape::Circle`.

## 3. Grammar

The grammar is LL(1) apart from expressions, which use Pratt parsing with the precedence table below. Parentheses around the condition of `if` and `while`, and around a `for` header, are optional: `if x > 0 {` and `if (x > 0) {` are both valid.

```ebnf
program      = { import } { item } EOF ;
import       = [ "public" ] "import" IDENT { "." IDENT } [ "as" IDENT ]
               [ "{" import_item { "," import_item } [ "," ] "}" ] ";" ;   (* names and `public`: v11; `as`: v14 *)
import_item  = IDENT [ "as" IDENT ] ;
item         = [ visibility ] ( fun_decl | extern_decl | struct_decl | enum_decl | interface_decl ) | impl_decl ;
visibility   = "private" | "public" ;                (* `public`: v8, packages *)

fun_decl     = "fun" IDENT [ type_params ] "(" [ params ] ")" [ ":" type ] fun_body ;
fun_body     = block | "=" expr ";" ;          (* "= e;" means "{ e }" *)
extern_decl  = "extern" "fun" IDENT "(" [ params ] ")" [ ":" type ] ";" ;
struct_decl  = "struct" IDENT [ type_params ] "{" [ field { "," field } [ "," ] ] "}" ;
field        = [ visibility ] IDENT ":" type ;
enum_decl    = "enum" IDENT [ type_params ] "{" [ variant { "," variant } [ "," ] ] "}" ;
impl_decl    = "impl" IDENT [ type_params ] "{" { [ visibility ] method } "}" ;   (* bounds: v15 *)
method       = "fun" IDENT [ type_params ] "(" [ receiver [ "," params ] | params ] ")" [ ":" type ] fun_body ;
type_params  = "<" type_param { "," type_param } [ "," ] ">" ;
type_param   = IDENT [ ":" bound { "+" bound } ] ;
bound        = "Eq" | "Ord" | IDENT [ "." IDENT [ "." IDENT ] ] [ "<" type { "," type } [ "," ] ">" ] ;
                                          (* an interface, `m.Shape` from module `m`, `Source<T>` (v6) *)
interface_decl = "interface" IDENT [ type_params ] "{" { iface_item } "}" ;
iface_item   = "fun" IDENT "(" [ receiver [ "," params ] | params ] ")" [ ":" type ]
               ( ";" | "=" expr ";" | block ) ;      (* a default body (v5): methods only *)
receiver     = [ "mut" ] "self" ;
variant      = IDENT [ "(" type { "," type } [ "," ] ")" ] ;
params       = param { "," param } [ "," ] ;
param        = IDENT ":" type ;

type         = base_type { "[" "]" } ;
base_type    = "i32" | "i64" | "u8" | "f64" | "bool" | "string"
             | "u32" | "u64" | "f32"                (* v7 *)
             | "i8" | "i16" | "u16"                 (* v10 *)
             | "char"                               (* v14 *)
             | "(" type "," type { "," type } [ "," ] ")"   (* a tuple (v14) *)
             | IDENT [ "." IDENT [ "." IDENT ] ] [ "<" type { "," type } [ "," ] ">" ]
             | "fun" "(" [ type { "," type } [ "," ] ] ")" [ ":" type ]   (* a function type (v3) *)
             | "(" type ")" ;                  (* `(fun(i64): i64)[]` is an array of functions *)
                                           (* `m.T`, `m.v.T` (v14), `Stack<i64>`; `Option<T>` is one such *)

block        = "{" { stmt } [ expr ] "}" ;   (* trailing expr = tail value *)
stmt         = var_decl | while_stmt | for_stmt
             | "return" [ expr ] ";"
             | "break" ";" | "continue" ";"
             | block_like                     (* no ";" needed *)
             | expr [ assign_op expr ] ";" ;
block_like   = if_expr | match_expr | block ;
var_decl     = "let" [ "mut" ] IDENT [ ":" type ] "=" expr ";"
             | "let" pattern "=" expr [ "else" block ] ";" ; (* let-else (v9); without else, irrefutable (v14) *)
assign_op    = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" ;   (* bit forms: v7; `%=`: v9 *)

if_expr      = "if" ( cond | "let" pattern "=" cond ) block [ "else" ( if_expr | block ) ] ;   (* if let (v9) *)
while_stmt   = "while" ( cond | "let" pattern "=" cond ) block ;                            (* while let (v9) *)
for_stmt     = "for" ( for_head | "(" for_head ")" ) block ;
for_head     = ( IDENT | "(" pattern "," pattern { "," pattern } ")" ) ( "in" cond ".." cond | "of" [ "mut" ] cond ) ;   (* `of mut`: v13; a tuple pattern: v14 *)
cond         = expr ;            (* parsed in no-struct-literal mode *)

match_expr   = "match" cond "{" [ arm { arm_sep arm } [ "," ] ] "}" ;
arm          = pattern [ "if" cond ] "=>" expr ;
arm_sep      = "," | (* nothing, after an arm whose expr is a block *) ;
pattern      = alternative { "|" alternative } ;      (* `A | B` (v9) *)
alternative  = "_"
             | IDENT [ "(" pattern { "," pattern } [ "," ] ")" ]
             | [ "-" ] INT | STRING | "true" | "false"
             | ( IDENT | "_" ) ":" type               (* `c: Circle` (v5) *)
             | "mut" IDENT [ ":" type ]               (* a mutable binding (v9) *)
             | "(" pattern ")"
             | "(" pattern "," pattern { "," pattern } [ "," ] ")" ;   (* a tuple (v14) *)

expr         = prefix { infix } ;            (* Pratt loop, see table *)
prefix       = ( "-" | "!" ) prefix | postfix ;   (* binds tighter than infix *)
postfix      = primary { call | index | member | "?" } ;   (* `read(p)?` (v4) *)
call         = "(" [ expr { "," expr } [ "," ] ] ")" ;
index        = "[" expr "]" ;
member       = "." ( IDENT | INT ) ;          (* `t.0` (v14) *)
primary      = INT | FLOAT | CHAR | STRING | interpolated | closure | "true" | "false"
             | "(" expr "," expr { "," expr } [ "," ] ")"   (* a tuple (v14) *)
             | IDENT [ "." IDENT ] [ struct_init ]  (* `m.T { … }` *)
             | "self"
             | int_type "." IDENT                 (* `i16.MAX` (v10) *)
             | "(" expr ")"
             | "[" [ expr ( ";" expr | { "," expr } [ "," ] ) ] "]"
             | block_like ;
interpolated = STRING_START expr [ FMT_SPEC ] { STRING_MID expr [ FMT_SPEC ] } STRING_END ;   (* "a {x} b {y:>5} c" *)
closure      = "fun" "(" [ cparam { "," cparam } [ "," ] ] ")" [ ":" type ] ( block | "=" expr ) ;   (* v3 *)
cparam       = IDENT [ ":" type ] ;
struct_init  = "{" [ IDENT ":" expr { "," IDENT ":" expr } [ "," ] ] "}" ;
infix        = binop expr | "as" type ;
```

**Operator precedence**, from lowest to highest. All binary operators are left-associative, except equality and comparison (levels 3 and 4), which are non-associative: `a < b < c` and `a == b == c` are parse errors. Write `(a == b) == c` if that is really meant.

| Level | Operators | Kind |
| --- | --- | --- |
| 1 | `\|\|` | Logical or, short-circuit |
| 2 | `&&` | Logical and, short-circuit |
| 3 | `==` `!=` | Equality |
| 4 | `<` `<=` `>` `>=` | Comparison |
| 5 | `\|` | Bitwise or (v7) |
| 6 | `^` | Bitwise exclusive or (v7) |
| 7 | `&` | Bitwise and (v7) |
| 8 | `<<` `>>` | Shifts (v7) |
| 9 | `+` `-` | Additive |
| 10 | `*` `/` `%` | Multiplicative |
| 11 | `as` | Cast |
| 12 | `-` `!` `~` | Prefix unary |
| 13 | call, `[]`, `.` | Postfix |

The bit operators sit between comparisons and `+ -`, as in Rust, so `flags & MASK == 0` means `(flags & MASK) == 0` and `a + b << 2` means `(a + b) << 2`.

**Assignment is a statement**, not an expression, so `a = b = c` and `if (x = 1)` are parse errors. The left side of an assignment must be a place expression: a variable, a field access, an index, or a chain of these (`pts[i].x`). The parser accepts any expression there; the type checker rejects non-places.

**Struct literals in conditions.** Without required parentheses, `if p == Point { x: 0.0, y: 0.0 } { ... }` is ambiguous, because the parser can't tell where the condition ends. So every `cond` is parsed in *no-struct-literal* mode, where `IDENT {` ends the expression instead of starting a literal. The restriction lifts inside every bracket pair — parentheses, brackets, call arguments and blocks — so `if p == (Point { x: 0.0, y: 0.0 }) { ... }` and `if ok { Point { x: 1.0, y: 2.0 } } else { ... }` both work. Rust uses the same rule.

**Enum values and `match`.** `Shape.Circle(2.0)` and `Shape.Empty` need no grammar of their own: they parse as a member access, followed by a call when there are payloads, and the checker decides they name a variant (section 4). A `match` scrutinee is a `cond`, parsed in no-struct-literal mode like an `if` condition. At the top level a bare `IDENT` pattern must name a variant; there are no top-level binding patterns, so use `_`. Inside a payload slot, a bare `IDENT` is a variant if the slot's type is an enum with a variant of that name (`Some(Empty)`), and otherwise a new binding (`Some(x)`). A guard after `if` is parsed like a `cond`.

**Generic headers.** Type parameters follow the name, like Rust and TypeScript: `fun max<T: Ord>(a: T, b: T): T`. An `impl` names its type's parameters without bounds, `impl Stack<T> { … }`; the bounds come from the type's declaration. Type arguments are never written at a use (section 4), so `<` after a name in an expression is always a comparison; `Stack::<i64>` is E0207.

**Block-like statements.** A statement that starts with `if`, `match` or `{` is parsed as `block_like` and needs no trailing `;`. It ends at its closing brace, so `if c { a } - 1` is a statement followed by the expression `-1`, not a subtraction. If a block's final element is an expression with no `;` after it, including a block-like one, that expression is the block's tail value (section 5). A `;` directly after a block-like statement is allowed and makes it an ordinary statement: `if c { f(); };`. A `;` on its own, where a statement should start, is an error.

**Parse errors.** The parser reports as many syntax errors as it can, skipping to the next statement, or to the next item at top level, after each one.

| Code | Error | Example |
| --- | --- | --- |
| E0201 | Unexpected token | `let x = 1 let y = 2;`, `let x = );`, a stray `;`, `let` at top level; `private import a;` (v11) |
| E0202 | Unclosed delimiter | `(`, `[` or `{` still open at end of file |
| E0203 | Chained comparison or equality | `a < b < c`, `a == b != c` |
| E0204 | Assignment used as an expression | `a = b = c;`, `if (x = 1) {}`, `f(x = 1)` |
| E0205 | Struct literal in a condition without parentheses | `if p == Point { x: 0.0 } { }` |
| E0206 | Nesting too deep | More than 256 levels of nested expressions and blocks |
| E0208 | `import` after an item | `fun f() {}` then `import geometry;` (v2) |
| E0209 | A closure parameter that isn't a plain name | `fun(self) = 1`, `fun(mut x) = x` (v3) |
| E0207 | `::` used as a path separator | `Shape::Circle(1.0)` — help: write `Shape.Circle`; parsing continues as if `.` were written |

## 4. Type system

Lugha is statically typed with one implicit conversion: a value becomes an interface value where an interface type is expected (v5, Interface values below). Every expression has exactly one type, computed bottom-up from its parts, with one exception: numeric literals take their type from context (see Inference).

**Types**

| Type | Description | LLVM lowering |
| --- | --- | --- |
| `i32` | 32-bit signed integer | `i32` |
| `i64` | 64-bit signed integer | `i64` |
| `u8` | 8-bit unsigned integer (a byte) | `i8` |
| `f64` | 64-bit IEEE 754 float | `double` |
| `u32` | 32-bit unsigned integer (v7) | `i32` |
| `u64` | 64-bit unsigned integer (v7) | `i64` |
| `f32` | 32-bit IEEE 754 float (v7) | `float` |
| `i8` | 8-bit signed integer, −128 to 127 (v10) | `i8` |
| `i16` | 16-bit signed integer, −32768 to 32767 (v10) | `i16` |
| `u16` | 16-bit unsigned integer, 0 to 65535 (v10) | `i16` |
| `char` | A Unicode scalar value, U+0000–U+D7FF and U+E000–U+10FFFF (v14) | `i32` |
| `bool` | `true` or `false` | `i1` (stored as `i8` in memory) |
| `string` | Immutable UTF-8 bytes | `ptr` to `{ i64 len, bytes, NUL }` on the GC heap (literals: constant globals) |
| `T[]` | Fixed-length array of `T` | `ptr` to `{ i64 len, T... }` on the GC heap |
| `S` (struct) | User-declared record | LLVM named struct `%S` |
| `E` (enum) | One of the declared variants, with its payloads | LLVM named struct `%E` = `{ i32 tag, payload area }` (section 7) |
| `Option<T>` | `Some(T)` or `None` | As an enum with those two variants |
| `void` | Return type only; written by omitting `: type` | `void` |

**Value semantics.** Every Lugha type behaves as a value. Assigning or storing a value gives the new place its own independent copy, so changing one variable never changes another. Strings are immutable, so the compiler shares them freely and no program can tell. Arrays live on the heap but are copied whenever sharing could be observed (see Array copies below). Enum payloads can't be changed in place, so enum values share them freely too, and no program can tell.

```
let a = Point { x: 1.0, y: 2.0 };
let mut b = a;    // copy
b.x = 9.0;        // a.x is still 1.0

let xs = [1, 2, 3];
let mut ys = xs;  // copy
ys[0] = 9;        // xs[0] is still 1
```

There is no null. Every string and array variable holds a valid object.

**void is not a value.** `void` can't be written as a type, and no variable, parameter, field or array element can hold one. `let x = println(1);` is a type error. A `void` expression can only be used as a statement, as the tail of a `void` block or function, or as a branch of an `if` whose type is `void`.

**Structs.** Declared at top level with named, typed fields. A struct literal must initialize every field exactly once, in any order. Structs may contain other structs and arrays, but a struct may not contain itself directly or through other structs (it would have infinite size). Containing itself through an array is fine, because an array is a pointer: `struct Node { value: i64, kids: Node[] }` builds trees. Other recursive data, such as linked lists, uses an enum or `Option`: a struct may contain itself through either, as in `struct Item { value: i64, next: Option<Item> }`. Only a cycle through structs alone is an error (E0307).

**Enums.** Declared at top level, in any order, as a list of variants that each carry zero or more positional payloads: `enum Shape { Circle(f64), Rect(f64, f64), Empty }`. A variant name is unique within its enum (E0309). A value is built through the enum's name with `.`: `Shape.Circle(2.0)`, or `Shape.Empty` for a variant without payloads. Each payload expects its declared type, and the count must match (E0416); an unknown variant is E0415. `==` compares two enum values by variant and then payload by payload; there is no other operator on enums (E0404). A payload can never be changed in place — there is no syntax for it, and `match` bindings are immutable — so copying an enum shares its payloads, and the copies that matter happen when a payload is stored: building a variant from a place copies it (`Job.Batch(xs)`), and storing a `match` binding copies it (section 4, Array copies). 

**Recursive data.** An enum may contain itself, directly or through structs, other enums or `Option`: `enum List { Nil, Cons(i64, List) }` builds a linked list, and `enum Tree { Leaf, Node(Tree, i64, Tree) }` a binary tree. Nothing has to be annotated and there is no `Box` (writing `Box<List>` is E0305 with a hint). The compiler stores each payload slot that leads back to its enum behind a hidden pointer (section 7), so the type has a finite size, values like `List.Nil` cost nothing, and — since payloads are shared — copying a list of any length takes constant time. A program can't observe the pointer: in `Cons(x, rest)`, `rest` is an ordinary `List` value.

**`Option`.** `Option<T>` is a generic enum predeclared in every module as `enum Option<T> { Some(T), None }`. Its variants are written without a prefix. `Some(x)` has type `Option<T>` where `x : T`, or the expected `Option` type when there is one. `None` needs an expected type, exactly like `[]`: `let x: Option<i64> = None;` — otherwise it is E0412.

**`match`.** `match e { pattern => value, … }` is an expression. A pattern is `_`, an integer, `bool` or string literal, or a variant written without the enum's name. A variant's payload slots are patterns too, so they nest: `Circle(r)`, `Rect(w, _)`, `Empty`, `Some(Circle(r))`, `Pair(1, Some("a"))`. Variant patterns need an enum or `Option` value and literal patterns a value of their type, at any depth (E0403); a variant needs one slot per payload (E0416). An arm may have a guard, `pattern if cond => value`: the guard runs after the pattern matches, with its bindings in scope, and must be `bool` (E0403); if it is false, matching continues with the next arm. The arms' values follow the rules of `if`/`else`: they must agree (E0403), and an arm that never finishes fits any type.

- **Exhaustiveness.** A `match` must cover every value, counting only arms without guards. Otherwise it is E0417, which lists up to three missing cases as patterns (`Some(Rect(_, _))`, `None`) and then "and N more"; a missing variant with any payloads shows as its bare name at the top level (`Rect`). A `bool` is covered by `true` and `false`. Integers and strings, at the top level or inside a variant, always need a `_` or a binding.
- **Type patterns** (v5). `name: T` matches when an interface value holds a `T`, and binds the value inside, as a `T`, to `name` (immutable, and copied when stored, like any `match` binding); `_: T` matches without binding. Guards and nesting work as for every pattern: `c: Circle if c.r > 1.0 =>`, `Some(c: Circle) =>`. `T` is a concrete type that satisfies the matched interface, written with its type arguments (`Stack<i64>`) and module (`geo.Crate`). A type pattern on a value that isn't an interface value, with an interface or a type parameter as `T`, with a generic `T` written without its arguments, or with a `T` that doesn't satisfy the interface (so the arm could never match) is E0426. The set of types is open, so a `match` on an interface value needs a `_` arm (E0417, with `_` as the missing case); a later arm that an earlier unguarded one covers is W0102. In a `match` scrutinee, `{ name:` begins the arms, never a struct literal.
- **Unreachable arms.** An arm that no value can reach past the earlier unguarded arms is warning W0102: after `_`, after the same pattern, or after arms that together cover it (`Some(_)` and `None` before `Some(Empty)`).
- **Bindings.** Payload names are immutable and borrowed, like `for … of` variables: storing or returning one copies it. Each is in scope in its own arm (guard and value) only. A binding that its arm never uses is warning W0103, which also catches a misspelled variant name that became a binding.
- **Or-patterns** (v9). `p | q | …` matches when any alternative does, tried left to right, and may appear wherever a pattern may (`Some(1 | 2)`); a guard after it covers the whole arm (`A | B if g =>`). Every alternative must bind the same names, each at the same type: a name missing from an alternative, or bound at another type, is E0428. For exhaustiveness an or-pattern counts as each of its alternatives (`Red | Green` leaves `Blue` for E0417), and an alternative that earlier arms, or earlier alternatives of the same arm, already cover is W0102.
- **`mut` bindings** (v9). `mut name` (or `mut name: T` in a type pattern) binds a mutable local holding the arm's own copy of what it matched, made at the copy sites, so assigning to it or calling `mut self` methods on it never changes the scrutinee: `Some(mut n) => { n += 1; … }`. At the top of an arm, `mut x` binds the whole scrutinee. `mut` before a name that is a variant of the matched type (`mut None`) is E0428. Bindings without `mut` stay read-only (E0501).

**Arrays.** `T[]` has a length, read with `.len` (type `i64`), that changes only through the built-in methods below. Elements are read and written with `xs[i]`, where `i` is of any integer type (v14; before, `i64` only), and an unannotated literal index is an `i64`. Every index is bounds-checked as its value: below 0, or at least `.len`, panics (`the length is 3 but the index is -1`); a `u64` above `i64.MAX` is out of bounds too. Arrays are created two ways:

- List literal `[a, b, c]`. All elements must have the same type. An empty literal `[]` needs an expected type from context, such as `let xs: i64[] = [];`.
- Repeat literal `[value; count]`, where `count` is a runtime `i64`. Every element is a copy of `value`. `value` is evaluated once, before `count`. A negative `count` panics with `negative array length`; a `count` of 0 gives an empty array.

**Array copies.** The compiler copies an array only when it is read from an existing place and stored into another one. Fresh arrays and function arguments are never copied. Since v15 a copy is *shared until written* (copy-on-write): the copy site only marks the array shared, in constant time, and whichever side next writes to it copies it first. A program can't tell the difference: in the table, "copied" means the new place behaves as its own array.

| Situation | Copied? | Why |
| --- | --- | --- |
| `let ys = xs;` or `ys = xs;` | Yes | `ys` gets its own array |
| A place used as a struct-literal field or array-literal element (`Wrap { data: xs }`) | Yes | The new struct or array gets its own copy |
| `return p;` where `p` is a parameter, a `for … of` variable, or part of one | Yes | Otherwise the caller's argument, or the iterated array, would be shared |
| `return xs;` where `xs` is a local or part of one | No | The local is about to go away, so the array simply moves |
| Passing an argument, `f(xs)` | No | Parameters are immutable, and nothing else runs during the call |
| Calling a method, `x.m()` | No | `self` is passed like any argument; a `mut self` method edits `x` itself, in place |
| `swap(a, b)` | No | The two places exchange their contents (section 5) |
| A fresh value: literal, call result, string concatenation | No | Nothing else refers to it |

A function's tail value follows the same rules as `return`: a body ending in `p` copies, and one ending in `xs` moves. "From a place" looks through `if` and block tails: `let ys = if c { xs } else { [0] };` copies. When the value comes from a local that is never used again, the compiler may move it instead of copying; no program can tell the difference.

Copies are deep. Copying an `i64[][]` copies every inner array, and copying a struct copies the arrays inside it. Strings inside are shared, since they are immutable. With copy-on-write (v15) a deep copy happens a level at a time: writing `g[0][1]` after `let h = g;` copies `g`'s outer array and the one inner array written, and leaves the other inner arrays shared.

**Strings.** Immutable. `.len` gives the length in bytes as `i64`. `s[i]` reads one byte as `u8`, bounds-checked, with `i` of any integer type (v14). `s.to_bytes()` (v14) gives the bytes as a new `u8[]`. `+` concatenates two strings into a new one. `==` and `!=` compare contents. `<`, `<=`, `>` and `>=` compare bytewise: the first differing byte decides, compared as an unsigned value, and a proper prefix is smaller (`"ab" < "abc"`). This is the order of C's `memcmp` and needs no Unicode tables.

**Inference.** Local inference only.

1. A `let` without an annotation takes the type of its initializer.
2. Function parameters, return types and struct fields are always annotated.
3. Numeric literals use the expected type when there is one: the variable's annotation, the parameter type at a call, the declared return type, the other operand of a binary operator, the other bound of a range, or the target of an assignment.
4. With no expected type, integer literals are `i64` and float literals are `f64`.
5. An integer literal that doesn't fit its type is a compile error. An integer literal never becomes a float, or vice versa.
6. A unary `-` applied directly to a numeric literal is folded into the literal before the range check, so `-2147483648` is a valid `i32` and `-9223372036854775808` a valid `i64`. A negated `u8`, `u32` or `u64` literal is an error (E0402), and so is `-x` on a value of an unsigned type (E0404). A float literal takes `f32` where `f32` is expected (v7), and is then the `f32` nearest its value.

```
let a = 5;            // i64
let b: i32 = 5;       // i32
let c = b + 1;        // i32: 1 takes its type from b
let d = 2.5;          // f64
let e: f64 = 5;       // error: integer literal where f64 expected
```

**Operator typing**

| Operators | Operands | Result |
| --- | --- | --- |
| `+` `-` `*` `/` `%` | Two numbers of the same type | That type |
| `+` | Two strings | `string` |
| `<` `<=` `>` `>=` | Two numbers of the same type, or two strings | `bool` |
| `==` `!=` | Two values of the same type | `bool` |
| `&&` `\|\|` | Two `bool` | `bool` |
| `!` | `bool` | `bool` |
| unary `-` | `i32`, `i64`, `f32` or `f64` | Same type |
| `&` `\|` `^` | Two integers of the same type (v7) | That type |
| `<<` `>>` | An integer, and an amount of the same type (v7) | That type |
| `~` | An integer (v7) | Same type |

**Equality on every type.** `==` and `!=` compare deeply:
- **Primitives and strings:** as before.
- **Structs:** field by field.
- **Enums:** same variant, then payload by payload.
- **Arrays:** same `.len`, then element by element.
- **Option:** `None == None`, and `Some(a) == Some(b)` when `a == b`.

Equality costs time proportional to the values compared. `f64` keeps IEEE semantics, so a `NaN` anywhere makes the values unequal, even a value compared with itself.

**Operand types.** In `==`, `!=` and the comparisons, an operand that can't be typed on its own (a number literal, `None`, `[]`, or a list or `Some` around one) takes its type from the other operand, whichever side it is on. So `o == None`, `None == o` and `xs == []` work. If neither side can be typed alone (`None == None`), it is E0412. A number literal never takes a non-numeric type this way: `"a" == 1` is E0404.

**Characters** (v14). A `char` literal is one character between single quotes, `'a'`, `'é'`, `'😀'`, or an escape: `'\n'`, `'\t'`, `'\r'`, `'\0'`, `'\\'`, `'\''`, `'\"'`, `'\u{1F600}'` (one to six hex digits naming a scalar value). Anything else is E0112.
- **Comparison:** `==` and `<` compare code points, so `char` is `Ord`. There is no arithmetic (E0404).
- **Casts:** `c as` any integer type gives its code point (truncated as casts do); any integer `as char` gives that character, or U+FFFD when the value is not a scalar value, since casts never fail. `char_from(n: u32): Option<char>` checks instead. `char` casts to nothing else (E0408).
- **Strings:** `s.chars()` iterates a string's characters (a `Chars` iterator), decoding UTF-8; each invalid sequence gives U+FFFD and moves one byte on, so it never fails. `s.char_count()` counts them. `for b of s` still loops over bytes.
- **Methods:** `c.is_alphabetic()`, `is_digit()` (`0`–`9`), `is_whitespace()`, `is_upper()`, `is_lower()`, `to_upper()` and `to_lower()` cover ASCII and Latin-1 (U+0000–U+00FF) only: other characters are neither letters nor cased, and `ß`, `ÿ` and `µ` keep their case.
- **Names:** `char_from` and `Chars` are the prelude's, visible like `range` unless a module declares its own.

**Casts.** `expr as T` converts between numeric types (`i32`, `i64`, `u8`, `u32`, `u64`, `f32`, `f64`; every pair, v7). `f64` to `f32` rounds to nearest, and `f32` to `f64` is exact. Integer narrowing truncates. Integer to float rounds to nearest. Float to integer truncates toward zero and saturates at the target's range; NaN becomes 0. `bool` cannot be cast; write `if b { 1 } else { 0 }` instead. A type parameter bounded by `Num` or `Float` casts like a number, to and from any numeric type or other such parameter (v15): `n as T`, `x as f64`; each instance casts by these rules at its own types. An unbounded `T` is E0408.

**Names of types.** A local — a `let`, a parameter, a `for` variable or a `match` binding — may not be named like a struct, an enum or `Option` (E0311). So a name before `.` is always unambiguous: a type, meaning a variant or associated function follows, or a local, meaning a field or method follows.

**Methods.** An `impl T { … }` block attaches functions to a struct or enum `T` declared anywhere in the file; a type may have several `impl` blocks, and no `impl` may target a built-in type (E0305). Method names are unique per type and may not repeat a field or variant name (E0310).

- **`self` methods.** A method whose first parameter is `self` is called on a value: `v.len_sq()`. `self` is an immutable, borrowed parameter of type `T`: storing or returning it copies it, like any parameter.
- **`mut self` methods.** A method whose first parameter is `mut self` changes the caller's value in place: `v.scale(2.0)`. The receiver must be a place whose root is `let mut` (E0501), not inside a string (E0506), and not an array being iterated (E0507); it may be a variable, a field or an element (`r.min.shift(1.0)`, `ps[1].scale(2.0)`). Inside, `self`, its fields and its elements can be assigned, and `self` can be replaced whole.
- **Associated functions.** A function in an `impl` without `self` is called on the type: `Vec2.new(1.0, 2.0)`.
- **No aliasing.** An argument of a `mut self` call may not be the receiver's own storage (E0508): a place rooted at the receiver's root variable whose type is passed by pointer (a struct, enum, `Option`, array, interface value or type parameter), written directly or as the tail of an `if`, block or `match`. In `v.add(v)` the method would see its own edits through `o`; `v.add(v.inner)`, `g.set(g.values)` and `v.add(if c { v } else { w })` are rejected the same way. Copy first: `let w = v; v.add(w);`. Any other mention is allowed (v15): calls on the receiver, `g.tanh(g.add(a, b))`, whose results are fresh values; operators; and reads of numbers, `bool`, `char` and strings, `g.scale(x, g.lr)`, which are passed by value.

Calling a method that doesn't exist, a `self` method on a type or an associated function on a value is E0415. Strings and `Option` have no methods; arrays have only the built-ins below.

**Growable arrays.** Every array has three built-in methods:

| Method | Effect |
| --- | --- |
| `xs.push(v)` | Appends `v`; `xs.len` grows by one |
| `xs.pop()` | Removes and returns the last element as `Some(x)`, or `None` if `xs` is empty |
| `xs.clear()` | Removes every element; `xs.len` becomes 0 |

They follow the `mut self` rules: `xs` must be a place whose root is `let mut` (E0501), not inside a string (E0506), and not an array being iterated by `for … of` (E0507). `push` stores its value like an assignment does — copied from a place, moved on last use — and reads it before the array grows, so `xs.push(xs[0])` simply appends a copy of the first element (E0508 doesn't apply). `pop` moves the element out. Indexing is still bounds-checked against the current `.len`, and copying an array copies its current elements.

**`Result`** (v4). `Result<T, E>` is predeclared in every module as `enum Result<T, E> { Ok(T), Err(E) }`, an ordinary generic enum.
- **Construction:** `Ok(x)` and `Err(e)` are written without a prefix, like `Some`. The type argument the payload doesn't give comes from the expected type (`let r: Result<i64, string> = Ok(1);`); `Ok(1)` alone is E0412.
- **Everything else** is as for any enum: `match` with `Ok(x)` and `Err(e)` patterns (exhaustiveness, E0417), `==` when `T` and `E` are `Eq`, copies and layout.
- **Names:** `Result`, `Ok` and `Err` are built-in names: declaring one is E0302, and a local named `Result` is E0311. A local named `Ok` or `Err` shadows the constructor, as one named `Some` does. Messages print the type as `Result<i64, string>`.

**The `?` operator** (v4). `e?` evaluates `e` once.
- **On `Result<T, E>`:** its type is `T`. If `e` is `Err(x)`, the enclosing function returns `Err(x)` at once. The function must return `Result<U, E>` with the same `E`, or `Result<U, F>` where `F` is an interface that `E`, a concrete type, satisfies, or an interface that `E`, an interface type, converts to (v6, section 4): then the error is converted into an `F` (v5).
- **On `Option<T>`:** its type is `T`. If `e` is `None`, the function returns `None`. The function must return an `Option`.
- **In a closure**, it returns from the closure, whose return type must be written or expected (E0412 otherwise, as for `return`).
- **Misuse is E0424**, with help: `?` on any other type, in `main`, in a function returning something else, on an `Option` in a function returning `Result`, or with error types that neither match nor convert.
- **Early return:** on failure nothing after the `?` runs, and a value it was part of is never stored, exactly as for `return`.

**String, `Option` and `Result` helpers** (v4). These methods each take `self`, never change it, and return a new value.

| Receiver | Method | Result |
| --- | --- | --- |
| `string` | `s.lines()` | `string[]`: split at `\n`, one trailing `\r` removed from each line, and no empty last line after a final `\n` (`"a\nb\n"` gives `["a", "b"]`, `""` gives `[]`) |
| `string` | `s.split(sep)` | `string[]`: the parts between occurrences of `sep`, empty parts kept (`"a,,b"` gives `["a", "", "b"]`); an empty `sep` panics with `split with an empty separator` |
| `string` | `s.trim()` | `string`: without leading and trailing ASCII whitespace (space, `\t`, `\n`, `\r`) |
| `string` | `s.contains(sub)`, `s.starts_with(p)`, `s.ends_with(p)` | `bool`; true for an empty argument |
| `string` | `s.slice(from, to)` | `string`: the bytes `from..to`; panics (at the call) unless `0 <= from <= to <= s.len` |
| `string` | `s.to_bytes()` (v14) | `u8[]`: a new array of the string's bytes |
| `string` | `s.chars()`, `s.char_count()` (v14) | the characters (`Chars`, yielding `char`) and their count |
| `string[]` | `xs.join(sep)` | `string`: the elements with `sep` between them; `""` for `[]` |
| `Option<T>` | `o.unwrap_or(d)` | `T`: the value, or `d` |
| `Option<T>` | `o.ok_or(e)` | `Result<T, E>`: `Ok` with the value, or `Err(e)` |
| `Option<T>` | `o.map(f)` | `Option<U>` |
| `Option<T>` | `o.is_some()`, `o.is_none()` | `bool` |
| `Result<T, E>` | `r.unwrap_or(d)` | `T`: the `Ok` value, or `d` |
| `Result<T, E>` | `r.ok()` | `Option<T>` |
| `Result<T, E>` | `r.map(f)`, `r.map_err(f)` | `Result<U, E>`, `Result<T, F>` |
| `Result<T, E>` | `r.is_ok()`, `r.is_err()` | `bool` |
| `Option<T>`, `Result<T, E>` | `x.unwrap()` (v9) | `T`: the `Some` or `Ok` value; otherwise panics (at the method name) with `unwrap on None`, or `unwrap on Err: ` and the error printed (v12) |
| `Option<T>`, `Result<T, E>` | `x.expect(msg)` (v9) | `T`, as `unwrap`, but the panic's message is `msg`, a `string` evaluated even when it isn't needed |

Strings are bytes: every position is a byte offset, and `slice` can cut a multi-byte character, giving a string of exactly those bytes. A closure argument is typed as for the array helpers (`o.map(fun(x) = "{x}")` is an `Option<string>`). `map` means the array, `Option` or `Result` helper according to the receiver. `parse_i64(s).ok_or("bad number")?` turns a missing value into an error and returns it. There is no `unwrap()`: handle a failure with `match`, `?` or `unwrap_or`.

**Array helpers** (v3). Every array `xs: T[]` also has these methods. Each takes `self`: the receiver never changes, and every array it returns is new.

| Method | Arguments | Result |
| --- | --- | --- |
| `xs.map(f)` | `f: fun(T): U` | `U[]`: `f` applied to each element, in order |
| `xs.filter(f)` | `f: fun(T): bool` | `T[]`: the elements for which `f` is true, in order |
| `xs.fold(init, f)` | `init: A`, `f: fun(A, T): A` | `A`: `f(…f(f(init, xs[0]), xs[1])…)`, left to right |
| `xs.sort_by(less)` | `less: fun(T, T): bool` | `T[]`: sorted so that `less` holds between neighbours or neither way, stably (equal elements keep their order) |
| `xs.sort()` (v15) | — | `T[]` for a `T: Ord`: ascending, stably, through `<` (E0419 otherwise) |
| `xs.any(f)` / `xs.all(f)` | `f: fun(T): bool` | `bool`, stopping at the first element that decides |

`T` comes from the receiver, and the other type arguments (`U`, `A`) are inferred as for any generic call, so a closure argument needs no written types: `xs.map(fun(x) = "{x}")` is a `string[]`, its result type coming from the closure's body. Arity and argument types are checked at the call (E0405, E0403). `f` runs once per element it is given, in order; a panic inside it stops the program as usual. If `less` is inconsistent, `sort_by` still returns every element exactly once, in some order. The helpers chain: `people.filter(fun(p) = p.age >= 18).map(fun(p) = p.name)`. Functions named `map`, `filter` and so on in a program are unaffected: the helpers exist only as array methods.

**Generic declarations.** `fun`, `struct`, `enum` and methods can take type parameters.
- **Scope:** a type parameter is a type inside its declaration (fields, payloads, signature and body), and a method also sees its `impl`'s parameters. It may not be named like a type, a module or another parameter of the same declaration, nor a method's like its `impl`'s (E0316).
- **Bounds:** `T: Eq`, `T: Ord`, interfaces (below), or several (`T: Shape + Eq`). Anything else is E0315. `Ord` doesn't imply `Eq`; write both if both are used.
- **`Float`** (v15, milestone 53): a prelude bound satisfied by `f32` and `f64` only (E0419 otherwise, with help to write `2.0` or convert), allowing everything `Num` does. `std.math`'s float functions take a `T: Float`.
- **Operator bounds** (v15): the prelude's interfaces `Add`, `Sub`, `Mul`, `Div`, `Rem` (each `fun op(self, other: Self): Self;`), `Neg` (`fun neg(self): Self;`) and `Num` (all six). `T: Add` allows `a + b` and `a.add(b)` on `T`, and so on; `T: Num` allows every arithmetic operator, `==` and `<`, and any integer literal as a value of `T` (`let mut s: T = 0;`, `x * 2`; v15, milestone 58), as `T: Float` also allows any float literal (`x * 0.5`); a float literal where `T` is only `Num` is E0401. Each instance gives the literal its own type, and a literal that doesn't fit one is E0402 at the literal, naming the instance (`300` in `big<u8>`). The numbers satisfy all of them as built-ins and `string` satisfies `Add`; other types satisfy the single-operator ones by having the method, and only the numbers satisfy `Num` (E0419). They are visible in every module unless the module declares the name, like `Iter`. Through a bound, each instance uses the built-in operator at a number or string type, panics included, and calls the method otherwise.
- **What a bare `T` allows:** being stored, moved, copied, passed, returned and put in arrays, structs and enums. `==` needs `T: Eq`, `<` needs `T: Ord`, and a method call needs an interface bound that declares it. Using `==` or `<` without its bound is E0404, with help suggesting the bound.
- **What `T` never allows:** arithmetic without an operator bound (v15), casts, `.len`, or indexing. Those need a concrete type or a bound. A `T` prints (v12), each instance by its concrete type's rules.
- **Checked once:** a generic body is checked once, with its parameters abstract, so every instance compiles.

**Using a generic.**
- **Type arguments:** a generic type is written with all its arguments in type positions: `Stack<i64>`, `Option<Vec2>`, `Pair<string, f64>`. A wrong count, or a bare `Stack` in a type position, is E0418.
- **Inference:** at a call, a struct literal, a variant or an associated function, the type arguments are inferred, never written. They come first from the expected type, then from the arguments that can be typed alone, then from the ones that can't (number literals, `None`, `[]`), so `max(x, 3)` with `x: u8` makes `3` a `u8`. A method's `impl` parameters come from its receiver: `s.push(1)` on a `Stack<string>` is E0401.
- **Can't infer:** a parameter still undecided is E0412, with help to annotate: `let s: Stack<i64> = Stack.new();`.
- **Bounds at a use:** a type argument that doesn't satisfy its bound is E0419. For example, `largest([[1]])` fails because `i64[]` is not `Ord`. Every type is `Eq`, so only `Ord` can fail: it holds for the numbers, `string`, `char`, tuples of `Ord` elements, type parameters declared `Ord`, and (v15) a struct or enum with `cmp`, below.
- **User-defined order** (v15, milestone 59): a struct or enum whose `impl` has exactly `fun cmp(self, other: Self): Ordering`, returning the prelude's `enum Ordering { Less, Equal, Greater }`, is `Ord`. `a < b` is `a.cmp(b) == Ordering.Less`, `a <= b` is `!= Greater`, `a > b` is `== Greater` and `a >= b` is `!= Less`; a generic type's `cmp` counts where its `impl`'s bounds hold. A `cmp` of another shape (a `mut self`, another parameter or result type) doesn't make a type `Ord`: `<` is E0404 and a bound E0419, each with help naming the shape. `==` stays structural and never calls `cmp`, and `Ord` still doesn't imply `Eq`. The numbers, `string` and `char` keep their built-in order and have no `cmp` method. `Ordering`'s variants are written `Ordering.Less`, like any enum's; a module's own `Ordering` means its own, and the prelude's prints as `$prelude.Ordering` in messages.

**Interfaces.**
- **Declaring one:** `interface Shape { fun area(self): f64; fun name(self): string; }` lists methods (with a `self` or `mut self` receiver) and associated functions (without one), each ending in `;`. Their signatures use concrete types, the interface's own type parameters (below) and `Self` (v6, below); an item has no type parameters of its own. An item name appears once (E0310).
- **Generic interfaces** (v6): `interface Source<T> { fun next(mut self): Option<T>; }` takes type parameters as a struct does, with bounds (`interface Sorted<T: Ord>`); a parameter named like a type, twice, or `Self` is E0316. It is always written with its arguments, `Source<i64>`; a bare `Source` or the wrong number is E0418, and an argument that misses its parameter's bound is E0419. A type satisfies `Source<X>` when it satisfies the interface with `X` substituted, and a call through `S: Source<X>` is checked against the substituted signature. A generic interface is a type wherever a plain one is, written with its arguments (`Source<i64>`, `Source<T>[]`), and its values follow every rule of interface values below at those arguments: a value converts to `Source<i64>` when it satisfies `Source<i64>`, `s.next()` is checked against the substituted signature, and a `Source<i64>` is not a `Source<string>` (E0403).
- **Iterators** (v6): the prelude declares `interface Iter<T> { fun next(mut self): Option<T>; }`. `Iter` is visible in every module, like `Result`, unless the module declares something named `Iter`, which it then means there. `for x of e` loops over any `e` that isn't an array and whose type has `next` with exactly that shape: a struct or enum with its own method, a type parameter whose bounds declare it, or an interface value (`Iter<i64>`, or a program's own `Source<i64>`). This is satisfaction of the prelude's `Iter<T>` by structure, so a loop works whether or not the program names `Iter`. Anything else is E0411 (section 5).
- **`Self`** (v6): in an interface's signatures and default bodies, `Self` is the type that satisfies the interface, and can be written wherever a type can: `other: Self`, `: Self`, `Self[]`, `Option<Self>`. It means that even in a module that declares a type named `Self`; outside interfaces, `impl` blocks (v10) and struct and enum declarations (v13, below), a written `Self` is an unknown type (E0305), and an interface parameter can't be named `Self` (E0316). A type `C` satisfies an item that mentions `Self` when its method has `C` in those places (`Money` meets `fun less(self, other: Self): bool` with `fun less(self, other: Money): bool`; for a generic type, `Self` is it at its arguments, `Stack<i64>`), and a mismatch is E0422, showing the signature with `Self` replaced. Through a bound `T: Ordered`, `Self` is `T`: `a.less(b)` needs `b: T`, `a.max(b)` is a `T`, and an associated `fun zero(): Self` is called as `T.zero()`. An interface any of whose methods mentions `Self` outside its receiver is a bound only: written as a type (`Ordered`, `Ordered[]`, a field) it is E0427, naming the method, because two `Ordered` values could hold different types (E0425 comes first for an interface with an associated function). An interface value satisfies such an interface, as a type argument, when its own items match with `Self` replaced by its interface type.
- **Bounds with arguments** (v6): a bound's arguments may name the declaration's other type parameters, `fun collect<T, S: Source<T>>(src: S): T[]`. A parameter that no argument or expected type fixes is read off the methods of the type a bound parameter has: `collect(Count { … })` gives `S = Count`, then `T = i64` from `Count.next`, and this repeats until nothing new is learnt (`T` from `S`, `S` from `H: Holder<S>`). A parameter still unknown is E0412; one that the arguments fix differently fails the bound (E0422). The same holds for a parameter declared as an interface value: `fun count<T>(s: Source<T>): i64` called as `count(Count { … })` reads `T = i64` off `Count.next`, then converts the argument to `Source<i64>`.
- **`Self` in `impl` blocks** (v10): inside `impl T { … }`, in signatures and bodies, `Self` is `T` as written there, with the `impl`'s parameters: `Point`, or `Stack<T>` in `impl Stack<T>`. It can be written wherever `T` can: as a type (`other: Self`, `: Self`, `Self[]`, `Option<Self>`), as a struct literal's name (`Self { x: 0, y: 0 }`), and before `.` for an associated call or a variant (`Self.origin()`, `Self.Red`). In a struct's field types and an enum's payload types (v13), `Self` is likewise the type being declared, with its parameters: `struct Tree<T> { value: T, kids: Self[] }` means `kids: Tree<T>[]`, and `enum List { Cons(i64, Self), Nil }` is recursive as if `List` were written; a struct holding `Self` directly is E0307, as for its own name. In a function outside these, `Self` is an unknown type (E0305).
- **Default methods** (v5): a method may end in `= expr;` or a block instead of `;`. Its body is checked once, with `self` of a hidden type that has only the interface's items, so it can call them on `self` and nothing else (no fields, E0410). `Self` names that hidden type (v6), so a default can take and return it: `fun max(self, other: Self): Self = if self.less(other) { other } else { self };`. A type satisfies the interface without the defaulted method; a method of its own by that name must match the signature (E0422), and is used instead of the default. A default is called through an interface value, a bound, or (v10) directly on a struct or enum without its own method by that name: `c.describe()` uses the default of the one interface visible at the call (declared in the calling module, in a module it imports, or in the prelude, and not hidden by its visibility) that supplies `describe` and that `c`'s type satisfies, a generic interface at the arguments the type's methods give it. None is E0415, with help naming an interface; two or more is E0430, naming two of them. Numbers, strings, arrays and `Option` have no `impl` blocks, so a default is never called on them directly. Only methods take a default body; an associated function with one is E0201.
- **Satisfying one:** a type satisfies `Shape` if, for every item, it has a method or associated function with the same name, the same receiver kind, and the same parameter and return types, visible where the bound is checked. No declaration is needed. A generic type's methods count with its type arguments substituted (`Stack<i64>` meets `fun size(self): i64` through `impl Stack<T>`); a method with type parameters of its own never matches. A type parameter satisfies an interface when its own bounds declare every item the same way. Primitives, strings, arrays and `Option` have no methods of their own, so they satisfy only an interface with no items, except the prelude's operator bounds, which the numbers (and `string`, for `Add`) satisfy as built-ins (v15).
- **Bounds on `impl` blocks** (v15): `impl Grid<T: Num> { … }` writes bounds on the block's parameters with a function's syntax, and every method of the block has them, added to the type's own. A type may have several `impl` blocks with different bounds, such as `impl Grid<T: Num>` for arithmetic and `impl Grid<T>` for the rest; a method defined in two blocks is E0310 as before. A bounded method used on an instance that misses the bound is E0419: `Grid { data: ["a"] }.total()`.
- **Using one as a bound:** `fun total<T: Shape>(xs: T[]): f64`, combined freely with `Eq`, `Ord` and other interfaces (`T: Shape + Eq`), or from another module (`T: shapes.Shape`). In the body, `x.area()` and, for an associated item, `T.make(…)` are allowed when some bound declares them, and are checked against its signature. Calling something no bound declares is E0421, with help suggesting a bound. Two bounds of one parameter may declare the same name only with the same signature; otherwise the bound list is E0423.
- **At a use:** a type argument that doesn't satisfy an interface bound is E0422, which names each missing or mismatched item.
- **Names and visibility:** interfaces share their module's item namespace (E0302) and follow the visibility rules of section 6. Through a bound, each instance calls the concrete type's own method, so there is no dispatch at run time.

**Interface values** (v5).
- **The type:** an interface whose items are all methods is a type, written by its name (`Shape`, `shapes.Shape`) wherever a type can be: `Shape[]`, `Option<Shape>`, fields, payloads, parameters, return types, function types and type arguments. A generic interface takes its arguments (v6, above); a plain one takes none (E0418). An interface with an associated function can still be a bound, but used as a type it is E0425, naming the associated function.
- **Conversion,** the one implicit conversion: where the expected type is exactly an interface `I`, a value of a type that satisfies `I` (checked where the conversion happens, with that place's visibility) becomes an `I`; a type that doesn't is E0422, naming each missing or mismatched item. An expected type reaches a value through `let` annotations, arguments, struct-literal fields, array elements, payloads (`Some(c)`, `Ok(c)`), assignments, `return` and tail values, and the branches of `if` and `match`. A numeric literal takes its default type (`i64`, `f64`) and converts.
  - **Not inside other types:** a `Circle[]` is not a `Shape[]`, and an `Option<Circle>` is not an `Option<Shape>` (E0403).
  - **Between interfaces** (v6): a value of interface type `J` converts to another interface `I` when `J` declares every item of `I`, defaulted ones included, with the same name, receiver and signature (each at its arguments): a `Pet` becomes a `Named`. Otherwise it is E0422, naming what `J` lacks. The value inside keeps its concrete type, so a type pattern on the `I` still matches it (`d: Dog`), and each method behaves as it did through `J`, including a default `J` supplied. The same interface at other arguments doesn't convert (`Source<i64>` to `Source<string>`, E0403). In a generic instance whose type argument is an interface value, converting that `T` to `I` works the same way.
  - **Type parameters:** `T` converts to `I` when its bounds satisfy `I`; each instance converts its concrete type.
  - **No expected type, no conversion:** `let s = c;` keeps `c`'s type, and `[c, sq]` with no expected type is E0403.
- **Calls:** `s.area()` calls the concrete type's method, checked against the interface's signature. A `mut self` method needs a mutable place (E0501, E0502, E0508). The value inside has no reachable fields (E0410).
- **Copies:** an interface value holds its own copy of the value inside. Assigning, passing or storing one copies that value deeply, as for an array, so a `mut self` call through one interface value never changes another; move on last use applies.
- **Equality, order and printing:** none. `==` is E0404, so an interface type, and a struct, enum, array or `Option` holding one, doesn't satisfy `Eq` or `Ord` (E0419). Interface values print as the value inside them (v12).
- **As a type argument:** `I` satisfies the interface `I` itself, so `total_area<Shape>(shapes)` works and calls through the values. It satisfies another interface only if `I` declares each of that interface's items the same way.

**Function types** (v3). `fun(A, B): R` is the type of a function value taking `A` and `B` and returning `R`; `fun(A)` returns nothing. Two function types are the same when their parameter and return types are. A return type takes any `[]` after it, so `fun(i64): i64[]` returns an array, and an array of functions is written `(fun(i64): i64)[]`; messages print types the same way.
- **Equality:** function values have no `==` (E0404), and neither does a struct, enum, array or `Option` that holds one anywhere. So a function type, or a type holding one, doesn't satisfy `Eq` (E0419).
- **Order, printing and casts:** function values have no `<`, print as `<fun>` (v12), and can't be cast (E0408).
- **Copying:** a function value is immutable, so a copy is the same value.
- **Generics and interfaces:** a function type can be a type argument (`apply<T>(f: fun(T): T, x: T)`), a field, an element or a payload. It has no methods, so it satisfies only an interface with no items.

**Functions as values** (v3).
- **Named functions:** a function used without a call is a value of its function type: `let f = double;`, `apply(double, 3)`, `m.f` from an imported module (visibility applies, E0313).
- **Associated functions** are values too: `let make = Point.at;`, `Circle.make`.
- **Generic functions** need an expected function type to fix their type arguments: `let m: fun(i64, i64): i64 = max;`. Otherwise it is E0412.
- **Not values:** methods with a receiver (call them on a value), externs and the intrinsics. Using one as a value is E0406, with help.

**Calls** (v3). `e(args)` calls any expression `e` of function type: a local, a parameter, an element (`fs[0](3)`), a call's result (`make()(3)`). `s.f(x)` calls method `f` if the type has one, otherwise field `f` if it holds a function; a method and a field never share a name (E0310), so this is never ambiguous. Arity (E0405) and argument types (E0403) are checked against the function type. Calling a value that isn't a function is E0406. A local of function type shadows a function of the same name, as any local does.

**Closures** (v3). `fun(x) = x + k` and `fun(x: i64): i64 { … }` are function values written inline: a function without a name. An `= e` body takes the longest expression, so `sort(xs, fun(a, b) = a > b)` ends at the `)`.
- **Parameter types** are written, or come from an expected function type in the same position (`let g: fun(i64): i64 = fun(x) = x + 1;`, or a parameter of a called function). Otherwise it is E0412, with help to write the type. In a generic call, a closure whose parameter types aren't written is typed last, so `apply(fun(x) = x + 1, 41)` makes `T` an `i64` first.
- **The return type** is written, or the expected one, or the body's type. A closure that uses `return` needs its return type written or expected (E0412).
- **Its own function:** `return` leaves the closure, and `break` and `continue` reach only loops inside it (E0504 otherwise).
- **Captures:**
  - A closure captures every local, parameter or `self` of an enclosing function that its body mentions.
  - Each is copied when the closure is created, in order of first mention, by the copy rules above; arrays are always deep-copied.
  - The closure reads its copies and never changes them: assigning a captured variable, one of its fields or elements, calling a `mut self` method or `push`/`pop`/`clear` on it, or `swap`ping it is E0511, with help to return a value instead.
  - So changing the outer variable later doesn't change the closure, and calling the closure never changes the caller's variables.
  - A closure's own `let mut` locals are ordinary.
- **No recursion:** a closure has no name, so it can't call itself; use a named function.

**Generic recursion.** A generic struct or enum may contain itself only with its own parameters unchanged: `enum List<T> { Nil, Cons(T, List<T>) }` is fine, and the indirect-slot rule (section 7) applies to each instance. Instantiation must end: a program whose instances nest more than 64 deep, such as `f<T>` calling `f<T[]>` or a `struct S<T>` holding `S<T[]>`, is E0420. Recursion at the same arguments (`count<T>` calling `count<T>`) never counts. Generic code copies exactly where concrete code would, because each instance is concrete (Array copies, above).

**Mutability.** Bindings are immutable by default. Assigning to any place requires the variable at its root to be declared `let mut`. That covers the variable itself, a field (`p.x = 1.0`), an element (`xs[0] = 1`) and any chain of these (`shapes[i].pts[j].x = 2.0`), and both places of a `swap` (section 5). Because every variable owns its data, a variable declared without `mut` never changes after it is initialized.

## 5. Expressions and statements

Evaluation is strictly left to right: operands, call arguments and struct-literal fields are evaluated in source order. The only exceptions are `&&` and `||`, which skip their right operand when the left decides the result.

**Assignment order.** `place = value` and `place op= value` first evaluate the place's index expressions, root outward, bounds-checking each; then the value (for `op=`, the place's current value, then the right operand); then the write. So in `xs[i] = e`, an `e` that copies `xs` keeps the array as it was before the write (v15 spells this out; it is what every version did). `push`'s argument is likewise evaluated before the array is changed.

**Operator overloading** (v15). `a op b` where `a` is a struct, enum or interface value calls a method of `a`'s type: `+` `add`, `-` `sub`, `*` `mul`, `/` `div`, `%` `rem`, and unary `-a` `neg`.
- **Shape:** the method qualifies when it has a plain `self` receiver (not `mut self`), exactly one parameter (none for `neg`) and a result. No declaration is needed, as for interfaces. A method of that name with another shape doesn't make an operator, so a set's `fun add(mut self, x: i64)` leaves `set + 1` an error.
- **Types:** `b` is checked against the method's parameter (E0403; a literal of the wrong kind, E0401) and the result has the method's return type, so `fun mul(self, k: f64): Vec2` makes `v * 2.0` a `Vec2`. Each type has one method per operator; the left operand decides, so `2.0 * v` is E0404.
- **Compound assignment:** `a += b` is `a = a.add(b)`, the place evaluated once; the method's result must have the place's type (E0403).
- **Missing:** an operator on a type without a fitting method is E0404, with help naming the shape when a method of that name exists. Numbers and strings keep the built-in operators, and have no methods to call: `5.add(3)` is E0415.
- **Unchanged:** `==`, `!=`, `<` and the others, `&&`/`||`, the bit operators and indexing are not overloadable.

**Arithmetic.** Integer `+`, `-`, `*` and unary `-` panic on overflow: if the true result doesn't fit the type, the program stops with `integer overflow` instead of continuing with a wrong value. Integer `/` and `%` panic when the divisor is zero, and also for `MIN / -1`, which overflows. `%` takes the sign of the dividend, like C and Rust. Compound assignments such as `+=` are checked the same way, including `%=` (v9), which panics on a zero divisor as `%` does. Shifts (v7) lose the bits shifted out and never panic for overflow; `>>` copies the sign bit for `i32` and `i64` and fills with zeros for the unsigned types; a shift amount that is negative, or at least the type's width (8, 32 or 64), panics with `shift out of range` at the operator. `&=`, `|=`, `^=`, `<<=` and `>>=` mean `x = x op y` with the place evaluated once, as `+=` does, and need an integer.

**Tuples** (v14). `(A, B, …)` is a tuple type of two or more elements, and `(a, b, …)` a tuple value; `(x)` is just `x`, and `(a,)` is E0201. A tuple is a value like a struct whose fields are named `0`, `1`, …: `t.0` reads an element and `t.0 = v` changes one in a `let mut` tuple (an index past the end is E0410), and `t.1.0` reaches into a nested tuple. Assigning, passing and returning copy it by the copy rules. `==` compares element-wise; `<`, `<=`, `>` and `>=` compare lexicographically, the first unequal element deciding, and need every element `Ord` (E0404 otherwise), so a tuple of `Ord` elements is `Ord`. Functions can return several values: `fun divmod(a: i64, b: i64): (i64, i64) = (a / b, a % b);`. A tuple prints as `(1, "a")`, its strings quoted. `Pair` (v13) is gone: `zip` yields tuples.
- **Patterns:** `(p, q, …)` matches a tuple element by element, nesting like any pattern; bare names inside it bind. A tuple has one constructor, so `match` checks exhaustiveness through its elements (E0417, W0102).
- **Destructuring:** `let (q, r) = divmod(7, 2);` and `for (i, w) of xs.iter().zip(ws.iter()) { … }` bind each element. Their patterns must match every value (bindings, `_`, `mut`, and tuples of those); anything that could fail is E0433, with help to use `let … else` or `match`. `let mut (x, y)` is not a thing: write `let (mut x, mut y)`.


**Iterator sources and helpers** (v10). These make and use iterators, the values `for … of` steps through (`Iter<T>`, above):

| Expression | Gives | Yields |
| --- | --- | --- |
| `xs.iter()` | `ArrayIter<T>` | the elements of a copy of `xs`, in order |
| `s.bytes()` | `Bytes` | the bytes of `s`, as `u8` |
| `range(a, b)` | `Range` | `a`, `a + 1`, … up to `b − 1` (`i64`s; nothing if `a >= b`) |

Every iterator `it` that yields `T` (a value whose type has `fun next(mut self): Option<T>`, an `Iter<T>` value, or a type parameter bound by `Iter<T>`) has these helpers, unless its type declares a method or item by that name, which wins:

| Helper | Gives | Meaning |
| --- | --- | --- |
| `it.map(f)`, `f: fun(T): U` | `Map<T, U>`, yielding `U` | `f` of each value |
| `it.filter(f)`, `f: fun(T): bool` | `Filter<T>` | the values `f` is true for |
| `it.take(n)` | `Take<T>` | the first `n` values (`n` an `i64`); after them it pulls nothing more |
| `it.skip(n)` | `Skip<T>` | every value after the first `n` |
| `it.enumerate()` | `Enumerate<T>`, yielding `Indexed<T>` | each value with its position: `p.i` (from 0) and `p.value` |
| `it.collect()` | `T[]` | every value, in order |
| `it.count()` | `i64` | how many values |
| `it.fold(init, f)`, `f: fun(A, T): A` | `A` | the values folded from `init` |
| `it.any(f)`, `it.all(f)` | `bool` | whether some or every value passes `f`, stopping at the first that decides |
| `it.find(f)` | `Option<T>` | the first value `f` is true for |
| `it.sum()` | `T` | the total, for a number type `T` only (E0415 otherwise) |
| `a.zip(b)` (v13) | `Zip<A, B>`, yielding `(A, B)` (a `Pair<A, B>` before v14) | `a`'s and `b`'s values side by side, ending with the shorter |
| `a.chain(b)` (v13) | `Chain<T>` | `a`'s values, then `b`'s (`b` must yield `T`, E0422 otherwise) |
| `it.take_while(f)` (v13) | `TakeWhile<T>` | values while `f` holds; after the first that fails, nothing more is pulled |
| `it.skip_while(f)` (v13) | `SkipWhile<T>` | every value from the first `f` fails for |
| `it.flat_map(f)`, `f: fun(T): I` (v13) | `FlatMap<T, U, I>`, yielding `U` | each value's iterator `I` (yielding `U`) in turn; for an array, return `xs.iter()` |
| `it.min()`, `it.max()` (v13) | `Option<T>` | the first smallest or largest value, `None` when empty; `T` must be `Ord` (E0419) |
| `it.position(f)` (v13) | `Option<i64>` | the index, from 0, of the first value `f` holds for |

- **Lazy:** the five adapters (`map` to `enumerate`) only wrap their iterator; nothing runs until a loop or a consumer pulls values, so they work on endless iterators: `range(10, 1000000000).filter(f).take(3)` tests only as many numbers as it needs.
- **Copies:** a helper takes its iterator by copy, as `for … of` does, so `let m = it.map(f);` leaves `it` unchanged; `xs.iter()` holds its own copy of `xs`.
- **Arguments:** closures are typed from the receiver as for the array helpers, and `take`/`skip` take an `i64`, a negative one taking or skipping nothing. Arrays keep their eager helpers: `xs.map(f)` is an array, `xs.iter().map(f)` an iterator.
- **Panics:** `sum` uses `+`, so it panics on overflow, located in the prelude.
- **Names:** these are Lugha in the prelude. `range` and the types `ArrayIter`, `Bytes`, `Range`, `Map`, `Filter`, `Take`, `Skip`, `Enumerate` and `Indexed`, and (v13) `Zip`, `Chain`, `TakeWhile`, `SkipWhile` and `FlatMap`, are visible in every module, like `Result`, unless the module declares the name; they print with their module, `$prelude.Map<i64, string>`.

**Methods on integers** (v7, v10). Every integer type (`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`) has these built-in methods:

| Method | Gives | Meaning |
| --- | --- | --- |
| `a.wrapping_add(b)`, `a.wrapping_sub(b)`, `a.wrapping_mul(b)` | `a`'s type | the result modulo 2^width; never panics |
| `a.count_ones()` | `i64` | how many bits are 1 |
| `a.leading_zeros()` | `i64` | zero bits above the highest 1 (the width for 0) |
| `a.trailing_zeros()` | `i64` | zero bits below the lowest 1 (the width for 0) |
| `a.checked_add(b)`, `a.checked_sub(b)`, `a.checked_mul(b)`, `a.checked_div(b)` | `Option` of `a`'s type | `None` where the operator would panic: on overflow, a zero divisor, or `MIN / -1` (v10) |
| `a.saturating_add(b)`, `a.saturating_sub(b)`, `a.saturating_mul(b)` | `a`'s type | the result clamped to `MIN … MAX` (v10) |
| `a.rotate_left(n)`, `a.rotate_right(n)` | `a`'s type | the bits rotated by `n`, an `i64`, modulo the width: a negative `n` rotates the other way (v10) |

`b` has `a`'s type, and a literal `b` takes it (E0403 for another type, E0405 for the wrong count).

**Integer limits** (v10). `I.MIN` and `I.MAX` are the smallest and largest value of integer type `I`, of type `I`: `i8.MIN` is −128, `u64.MAX` is 18446744073709551615. Only these two names follow an integer type keyword (E0415 otherwise); other type keywords take none. Any other method on an integer, and these names on a float or `bool`, is E0415. A literal receiver with no expected type is `i64`: `(5).count_ones()` is `2`. The wrapping methods are the explicit way to wrap; `+`, `-` and `*` still panic on overflow.

Float arithmetic follows IEEE 754: overflow gives an infinity and division by zero gives an infinity or NaN, never a panic. Explicit casts are the one place values wrap: narrowing with `as` truncates (section 4).

**Method calls.** `x.m(a, b)` evaluates `x`, then `a`, then `b`; `xs.push(v)` evaluates the place `xs`, then `v`. For a `mut self` call the place `x` runs like an assignment's place (v15): its index expressions first, bounds-checked, then the arguments, then the place itself is reached and the method runs. Calls on the same receiver inside the arguments finish first, and the outer call sees their effects.

**`match` evaluation.** A `match` evaluates its scrutinee once, then tests the arms from top to bottom and runs the first that matches, outside in for nested patterns; a guard runs only once its pattern has matched. A variant's payloads are evaluated left to right when it is built.

**Compound assignment.** `x op= e` means `x = x op e`, except that the place `x` is evaluated once. So `xs[next()] += 1` calls `next()` once.

**Evaluation of `==`.** Both operands are evaluated left to right, then compared. Structs and arrays are compared in field or element order, stopping at the first difference. A comparison never panics.

**Changing what a statement writes.** An assignment, a `swap` and a `mut self` call each fix their place before the rest of the statement runs: the value, the arguments, and the place's own indexes. The rest may not change anything that contains that place (E0510), because growing, shrinking or replacing an array would leave the write aimed at an array that is no longer there. `xs[0] = { xs.push(2); 9 };`, `xs[{ xs.clear(); 0 }] = 1;` and `g[0][0] = { g[1] = [5]; 1 };` are rejected (any two indexes might be equal). For a `mut self` receiver (v15), which is reached again after the arguments, a change counts only when an array index lies between the changed place and the receiver: `gs[0].f(gs.clear())` and `self.layers[i].f(self.reset())` are rejected, while `self.g.f(self.norm(x))` and `a.b.f(a.reset())` are valid. Changing the place itself, a place at the same depth, or a sibling is fine, and reading is always fine: `xs[0] = { xs[1] = 5; 9 };`, `p.items[0] = { p.n += 1; 7 };`, `xs[i] = xs[i] + 1;` and `xs = { xs.push(1); xs };` are valid. Make the change in its own statement instead.

**Variable declarations.** Every variable must be initialized. A variable is in scope from the end of its declaration to the end of its enclosing block, so `let x = x + 1;` refers to an outer `x`. Shadowing is allowed, both in nested blocks and in the same block.

**if / else.** `if` is an expression. The condition must be `bool`. With an `else`, both branches must have the same type, which becomes the type of the whole `if`. Without an `else`, the type is `void`. An expected type flows into both branches, so `let x: i32 = if c { 1 } else { 2 };` is valid.

```
let sign = if x < 0 { -1 } else if x == 0 { 0 } else { 1 };
```

**Blocks and tail values.** A block `{ stmts tail }` runs its statements, then evaluates `tail` and yields it as the block's value. A block with no tail has type `void`. Variables declared inside a block go out of scope at its closing brace. A block-like statement in the middle of a block must have type `void`, because a value there would be silently thrown away; the checker reports it as an error.

**while.** Evaluates the condition before each iteration; stops when it is `false`.

**Conditional patterns** (v9). Each tests one pattern, typed as a `match` arm's (or-patterns and `mut` included), with no exhaustiveness check; the scrutinee is a condition, so a struct literal needs parentheses.
- **`if let p = e { a } [else b]`** evaluates `e` once; if `p` matches it runs `a` with `p`'s bindings in scope, otherwise `b` (or nothing). It is an `if`: with `else` both branches agree on a type and it is a value; without, it is `void`. `else if let` chains. An irrefutable pattern (`if let x = e`) is allowed.
- **`while let p = e { body }`** evaluates `e` and tests `p` before each iteration, running `body` with the bindings while it matches; `break` ends the loop and `continue` goes to the next test.
- **`let p = e else { … };`** evaluates `e`; if `p` matches, its bindings are in scope for the rest of the block. Otherwise the `else` block runs, and it must not finish normally: it ends in `return`, `break`, `continue`, `panic(…)` or `exit(…)`, or is otherwise of type `never` (E0429). `let x = e else { … };` reads `x` as a pattern, so at the top it is a variant, as in a `match` arm. `mut` goes inside the pattern: `let Some(mut n) = o else { return; };`.

**for over a range.** `for i in a..b { body }` runs with `i` = `a`, `a+1`, …, `b-1`. The range is half-open, and empty when `a >= b`. `a` and `b` must have the same integer type, which becomes the type of `i`. Both bounds are evaluated once, before the first iteration. `i` is a fresh immutable binding on each iteration. It desugars to:

```
{
    let __end = b;
    let mut __i = a;
    while __i < __end {
        let i = __i;
        body
        __i += 1;   // also run on continue
    }
}
```

**for over an array.** `for x of xs { body }` binds each element in order. `xs` is evaluated once and is not copied. `x` is an immutable binding to the element, so assigning to `x` is an error; write `xs[i]` with a range loop to modify elements. The body may not assign to `xs` or any part of it, which is checked at compile time, so the loop always sees the array as it was when the loop started. Strings are not iterable in v0.

**for over `mut`** (v13). `for x of mut e { body }` loops over `e` itself rather than a copy. `e` must be a variable, field or element (E0502) rooted in a `let mut` local or `mut self` (E0501); a string is E0506.
- **Over an array,** `x` is each element in turn, not a copy: `x = v`, `x += 1`, `x.field = v` and `mut self` calls on `x` change the array. `x` is still a borrowed value, so storing or returning it copies. The body may copy `e` (`let c = xs;`): later writes to `x` change `e` only, never the copy.
- **Over an iterator,** `next` is called on `e` itself, so after a `break` the iterator stands where the loop left it and a later loop resumes it. `x` is an immutable value, as without `mut`.
- **Guard:** in either case the body may not assign to `e`, grow or shrink it, or call its `mut self` methods (E0507), since the loop is using it; it may read it.

**for over a string** (v10). `for b of s { body }`, where `s` is a `string`, binds each byte of `s` in order, as a `u8`, like `for b of s.bytes()`; strings never change, so nothing in the body can disturb it.

**for over an iterator** (v6). `for x of e { body }`, where `e` isn't an array but has `fun next(mut self): Option<T>` (section 4), evaluates `e` once into a hidden mutable copy (moved instead when `e` is a local's last use), then calls `next` on that copy before each iteration: `Some(v)` runs the body with `x` bound to `v`, and `None` ends the loop. `x` is immutable and holds its own value. `break` ends the loop and `continue` goes to the next call of `next`. Because the loop works on a copy, `e` itself never changes, and the body may assign to it; looping twice over the same variable starts from the beginning both times. As in every `for` header, a struct literal needs parentheses: `for n of (Count { n: 0, end: 3 }) { … }`. It desugars to:

```
{
    let mut $it = e;
    while true {
        match $it.next() {
            Some(x) => { body }
            None => { break; }
        }
    }
}
```

**break and continue.** Apply to the innermost enclosing loop. Using either outside a loop is a compile error.

**Expression statements.** Any expression followed by `;` is a statement, and its value is discarded. Discarding the result of a non-`void` call is allowed. Adding `;` after a would-be tail expression turns it into a statement, so `{ x * x; }` is a `void` block. When a value was expected, the checker reports this as "remove this semicolon" and points at it.

**Panics.** A panic prints `panic: <message> at <file>:<line>:<col>` to stderr and exits with code 101. Panics come from integer overflow (message `integer overflow`, including `MIN / -1`), integer division by zero (`division by zero`), a shift out of range (`shift out of range`; v7), out-of-bounds indexing (`index out of bounds: the length is N but the index is I`, located at the `[`), an out-of-range `slice` (`slice out of bounds: from F to T of a string of length L`, at the method name; v4), `split` with an empty separator (located in the prelude, `<prelude>:…`; v4), a negative repeat-literal count, too-deep recursion (`stack overflow`), `unwrap` on a `None` or an `Err` (`unwrap on None`, `unwrap on Err: <error>` with the error printed, v12) and `expect(msg)` likewise (message `msg`), each located at the method name (v9), and the `panic(msg)` intrinsic. An arithmetic panic's location is its operator: the `+` in `x + 1`, the `+=` of a compound assignment, the `-` of a negation. A stack overflow is the one panic without a location — it prints just `panic: stack overflow` — because the runtime can't tell which call went too deep. They cannot be caught.

**Intrinsics.** These names are built into the compiler, not declared in source. They accept arguments that ordinary functions can't, which is why they are special.

| Intrinsic | Accepts | Effect |
| --- | --- | --- |
| `print(x)` | Any value (v12) | Writes `x`'s text (below) to stdout with no newline |
| `println(x)` | The same, or nothing | Writes `x`'s text and a newline |
| `panic(msg)` | `string` | Panics with `msg` |
| `to_string(x)` | The same as `print` (v12) | Returns the text `print` would write; a `string` is itself |
| `swap(a, b)` | Two places of the same type | Exchanges their contents |
| `args()` | Nothing | Returns the program's command-line arguments as a new `string[]`; `args()[0]` is the program's path (v3) |
| `parse_i64(s)` | `string` | `Some(n)` if `s` is an optional `-` followed by decimal digits and fits in `i64`, else `None`; no `+`, spaces, `_` or hex (v3) |
| `parse_u64(s)` | `string` | `Some(n)` if `s` is decimal digits only and fits in `u64`, else `None`; no sign (v10) |
| `parse_f64(s)` | `string` | `Some(x)` if `s` is an optional `-`, digits, an optional `.` and digits, and an optional exponent (`e`/`E`, optional sign, digits), else `None`; `"3"` gives `3.0`, while `".5"`, `"5."`, `"inf"` and `"nan"` give `None` (v3) |
| `read_line()` | Nothing | `Option<string>`: the next line of stdin without its `\n` (and a `\r` before it); a last line without `\n` is still returned; `None` at the end of input (or on a read error). Stdout is flushed first, so a prompt shows (v4) |
| `read_file(path)` | `string` | `Result<string, string>`: the whole file, or `Err` with the system's message, such as `"No such file or directory"` (v4) |
| `write_file(path, text)` | `string`, `string` | `Result<i64, string>`: replaces the file's contents; `Ok` with the bytes written (v4) |
| `append_file(path, text)` | `string`, `string` | `Result<i64, string>`: adds to the end, creating the file if needed (v4) |
| `read_bytes(path)` | `string` | `Result<u8[], string>`: the whole file as bytes, or `Err` with the system's message (v14) |
| `write_bytes(path, data)`, `append_bytes(path, data)` | `string`, `u8[]` | `Result<i64, string>`: like `write_file` and `append_file`, writing exactly `data` (v14) |
| `read_stdin_bytes()` | Nothing | `u8[]`: everything left on stdin (v14) |
| `string_from_utf8(data)` | `u8[]` | `Result<string, string>`: a string of exactly those bytes if they are valid UTF-8 (no overlong forms, surrogates, code points above U+10FFFF or truncated sequences), else `Err("invalid UTF-8 at byte N")` with the first bad sequence's offset; the only way from bytes to a `string` (v14) |
| `eprint(x)`, `eprintln(x)` | Like `print`, `println` | Writes to stderr, formatted as `print` does, after flushing stdout so a terminal shows output in program order (v4) |
| `exit(code)` | `i32` | Flushes stdout and stops the program with exit code `code`; never returns, so code after it is unreachable (W0101) (v4) |

**Files** (v4). Paths are relative to the current directory, exactly as given; a path containing a NUL byte is an `Err`. Files are read and written as bytes, with no newline translation, and a whole file is read into memory. Every failure is a value (`Err` or `None`), never a panic.

**How values print** (v12). `print`, `println`, `eprint`, `eprintln`, `to_string` and interpolation share one text for each value:

| Value | Prints as |
| --- | --- |
| a number or `bool` | as below |
| a `string` | its bytes |
| a `char` | its character, as UTF-8 (v14) |
| a struct or enum whose own method is `fun show(self): string` | what `show` returns |
| any other struct | `Point { x: 1, y: 2 }`, or `Point {}` without fields |
| any other enum value, `Option`, `Result` | `Circle(1.5)`, `Empty`, `Some(5)`, `None`, `Ok(1)`, `Err("no")` |
| an array | `[1, 2, 3]`, or `[]` |
| an interface value | as the value inside it |
| a function value | `<fun>` |

- **Parts:** the fields, payloads and elements inside a built-in form print by the same table, except that a `string` there is quoted: `"`, `\`, newline, tab and carriage return are written `\"`, `\\`, `\n`, `\t`, `\r`, other bytes below 0x20 as `\u{XX}`, and every other byte as itself. A `show`'s text is never quoted. A `char` there is quoted too (v14): `'a'`, `'\n'`, `'\''`.
- **Names:** a struct prints its bare name, without its module path or type arguments (`Stack { items: [3] }`), and an enum value only its variant.
- **`show`:** the prelude declares `interface Display { fun show(self): string; }`, visible in every module like `Iter` unless the module declares `Display`. A struct or enum with its own method of exactly that shape prints through it, whatever the method's visibility, since printing is the type's own behaviour; a method `show` of any other shape is ignored, and the built-in form is used. Like any interface, `Display` works as a bound (`T: Display` gives `x.show()`) and as a type.
- **Depth:** a recursive enum prints to its full depth. Values are trees, so printing always ends.

**Interpolated strings** (v3). Each `{expr}` is evaluated left to right and formatted exactly as `print` would write it (floats as below); the parts are joined into one new `string`. Any value interpolates (v12); only a `void` expression can't (E0407).

**Format specifiers** (v13). An interpolation may end in `:` and a specifier, `{expr:spec}`, which changes how its value is written. The lexer splits the specifier off at the first `:` outside every `(`, `[` and `{` of the expression and outside strings, and takes everything up to the closing `}`; an expression that needs such a `:` goes in parentheses. A specifier is `[[fill]align][#][0][width][.precision][kind]`:
- **`align`:** `<` left, `>` right, `^` centre (an odd gap's extra character goes right). Without one, numbers align right and everything else left. **`fill`**, written before an alignment, is any character other than `{` and `}` (v15; ASCII only before); the default is a space.
- **`width`:** at most 1024, counted in characters (Unicode scalar values) since v15 (milestone 63), in bytes before: `{"Zoë":<6}` adds three spaces. Shorter text is padded to it; longer text is never cut. A character counts as one whatever its displayed width; there are no grapheme clusters or East Asian widths.
- **`#`:** with a kind, prefixes `0x`, `0b` or `0o`. **`0`:** pads a number with zeros after its sign and prefix (`{-42:05}` is `-0042`, `{255:#06x}` is `0x00ff`), and is ignored when an alignment is given.
- **`.precision`:** floats only, at most 1024: that many decimals, as C's `%.*f` (`{3.14159:.2}` is `3.14`). Without it a float prints as `print` writes it.
- **`kind`:** integers only. `x` hex, `X` upper-case hex, `b` binary, `o` octal; a negative value prints its two's complement at its type's width (`{-1 as i8:b}` is `11111111`).
- **Other values** take only fill, alignment and width, applied to the text `print` would write: `{p:>20}` right-aligns a struct.
- **Errors:** an empty specifier, an unknown kind, trailing text, a width or precision over 1024, a brace fill, precision on a non-float, a kind on a non-integer, `#` without a kind, or `0` on a non-number is E0431 at the specifier.

**`format`** (v14). `format(template, a, b, …)` returns a `string`: the template's text with each hole replaced by the next argument, written as an interpolation would write it. The template must be a string literal, read raw (section 2). In it, `{}` is a hole, `{:spec}` a hole with a format specifier as above, and `{{` and `}}` are literal braces. `format("{} has {:>4} items", "box", 12)` is `"box has   12 items"`. Any value that prints may be an argument; `format` is the only function that takes any number of them. Errors (E0432): a template that isn't a literal, `{x}` or any other text inside a hole, a `{` never closed, a lone `}`, or a number of holes unlike the number of further arguments. A bad specifier is E0431, as in an interpolation.

`swap(a, b)` exchanges what two places hold, like `let t = a; a = b; b = t;` but without copying anything: swapping two arrays exchanges them in constant time, which makes double buffering cheap (`swap(ys, buf)`). Both arguments must be places of the same type (E0502, E0403), each following the assignment rules: the root is `let mut` (E0501), not inside a string (E0506), and not an array being iterated (E0507). The places are evaluated left to right, and elements are bounds-checked, before either is written. A place can't be swapped with a part of itself (E0509): `swap(n, n.kids[0])` would store `n` inside itself. Places that might be the same are fine, since swapping a place with itself changes nothing: `swap(xs[i], xs[j])`, `swap(p.a, p.b)` and `swap(a, a)` are all valid.

Floats print as the shortest decimal that round-trips, in Lugha float-literal syntax: always a `.` with at least one digit on each side, so `2.0` prints as `2.0`, not `2`. Values with magnitude at or above `1e16`, or below `1e-5` and nonzero, use an exponent: `1.0e16`, `2.5e-7`. Infinities and NaN print as `inf`, `-inf` and `NaN`; `-0.0` prints as `-0.0`. `bool` prints as `true` or `false`. `u8` prints as a number, not a character. `u32` and `u64` print as unsigned decimals (v7). An `f32` prints by the same rules as the shortest decimal that reads back as the same `f32`, so `0.1` prints `0.1`, not the digits of its `f64` value.

## 6. Functions and program structure

A v0 program is a single `.la` file containing top-level items: functions, extern declarations and structs. There are no top-level variables or statements.

**Item order doesn't matter.** A function can call a function declared later in the file, and a struct or enum can use a struct or enum declared later. The type checker runs in two passes to support this:

1. Collect every struct name and function signature into a global table.
2. Resolve struct field and enum payload types, check for structs that contain themselves through structs alone (E0307), and mark the enum payload slots that lead back to their enum (indirect slots, section 7).
3. Check each function body against the global table.

**Modules (loading).** A program can span several files (v2, milestone 10; being built).
- **Files are modules.** A file is a module named by its path from the main file's directory, without `.la`, with `/` written as `.`: `shapes/circle.la` is `shapes.circle`.
- **Loading.** `import shapes.circle;` loads `<root>/shapes/circle.la`, where the root is the main file's directory. Each file is loaded once, however many files import it, so import cycles are fine.
- **Errors.** A missing file is E0312. These are E0314: importing the same module twice in one file, two imports whose last names clash, or an import named like one of the file's own items. Importing the main file is E0319.
- **Each file is checked for its own errors.** Invalid UTF-8 (E0110) and lexical and syntax errors in any module are reported against that module's file.
- **Order and cycles.** The passes above run over all modules together, so an item may be used before its declaration across files too.

**Packages** (v8). A package is a folder of modules that a program, or another package, lists in its manifest.
- **The manifest** is `lugha.toml`, in the main file's folder for the program and in a package's folder for that package; both are optional. lughac reads a small subset of TOML: blank lines, comments from `#` to the end of a line, a `[packages]` table and (v11) a `[package]` table, each at most once and in either order, and their lines:
  - under `[packages]`, `name = "path"` or (v11) `name = { path = "path", version = "R" }`, where `name` is a Lugha identifier, `path` a double-quoted string without `"` or `\`, relative to the manifest's folder with `/`, and the inline table's keys come in either order, `path` required and `version` optional;
  - under `[package]` (v11), `version = "MAJOR.MINOR.PATCH"`, and (v15) `name = "ident"` and `source = "path"`, each at most once. A `name` is a Lugha identifier; a `source` is a relative path with `/`, not empty, with no `..`, so a package's modules never come from outside it.

  Anything else, a table twice, a name listed twice, an empty path, or a malformed version or requirement is E0321, at its line of `lugha.toml`; a path whose folder doesn't exist is E0322.
  ```
  # lugha.toml
  [packages]
  geo = { path = "libs/geo", version = "1.2" }
  ```
- **Versions** (v11). A version is `MAJOR.MINOR.PATCH`: three decimal numbers without leading zeros (`0` itself is fine) and without suffixes. A requirement is `MAJOR`, `MAJOR.MINOR` or `MAJOR.MINOR.PATCH`, the missing parts 0, and it accepts the compatible versions: those at or above it that keep every part up to its first non-zero written part (its last written part when all are 0). So `"1.2"` accepts 1.2.0 up to, but not including, 2.0.0; `"0.3"` accepts 0.3.x; `"0.0.4"` accepts only 0.0.4; `"0"` accepts 0.x.y. Once every module is loaded, each requirement in every manifest read is checked against the listed package's declared version; a package that no module imports has its manifest read for this. A version that doesn't meet a requirement, or a package that declares none, is E0324 at the requirement's line, naming the range and the version found. The program's own `[package]` version is allowed and unchecked. A git entry's version is its tag's, so its requirement isn't checked against the package's `[package]` version.
- **Git packages** (v14). An entry `name = { git = "URL", version = "R" }` names a package in a git repository; `path` and `git` are exclusive, and `git` needs `version`.
  - **URLs:** only `https://host/…` and `file:///…`. A URL with a user name or password, whitespace or control characters, or another scheme (`ssh://`, `git@host:…`, `git://`, `ext::…`) is E0321: credentials belong in git's own credential helper, never in a manifest.
  - **Resolving:** lughac runs `git ls-remote --tags` and takes the highest tag `vMAJOR.MINOR.PATCH` that `R` accepts, clones it (`--depth 1`, no submodules) into a scratch folder of the cache, and moves it into place as `<cache>/git/<escaped URL>/<commit>`, without its `.git`. A package's own git entries are resolved the same way. Packages from git otherwise behave like folder packages.
  - **The cache** is `$LUGHA_CACHE`, else `$XDG_CACHE_HOME/lugha`, else `$HOME/.cache/lugha`, each used only when it is an absolute path; with none, lughac stops with an internal error. A folder in place is used as is.
  - **`lugha.lock`**, next to the main file, records each git package's URL, version and commit in the manifest's subset (a `[locked]` table of `name = { git = "…", version = "…", commit = "<40 hex digits>" }`). lughac writes it when a build resolves something new, through a temporary file and a rename; commit it with the program. A locked entry whose requirement still accepts its version is used exactly: from the cache, or cloned again at its tag, whose `HEAD` must be the locked commit (E0326 otherwise, with help to run `lughac update`). A malformed lock line is E0321.
  - **Commands:** `lughac update prog.la` ignores the lock, re-resolves every git package, and rewrites the lock. `--offline` on `build`, `run` and `check` runs no `git`: a package not locked and cached is E0325.
  - **Failures:** no `git`, a failing `git` command (with the first lines of its message), no matching tag, or a malformed commit is E0325 at the entry.
  - **How `git` runs:** from an argument list, never a shell, with `--` before every URL and folder, and with `GIT_TERMINAL_PROMPT=0` (no password prompts), `GIT_ALLOW_PROTOCOL=https:file`, a low-speed timeout, hooks off (`core.hooksPath=/dev/null`), `core.fsmonitor=false`, `submodule.recurse=false` and `core.symlinks=false`, so a symlink in a package is checked out as a plain file holding its target's name and a package can't make lughac read files outside it. The tag lughac clones is rebuilt from the parsed version. The user's own git configuration (proxies, credential helpers) still applies.
  - **What runs:** lughac only reads a package's `.la` files and `lugha.toml`; nothing in a package runs while building. A built program runs a package's code, `extern` calls included, like any code. lughac trusts git's commit ids and does no hashing of its own, and it can't protect against a package that is malicious when run, a compromised host, or a repository too large for the disk.
- **Package layout** (v15).
  - **`source`:** a package's modules start in its folder joined with `source` (its folder without one). With `source = "src"`, `src/array.la` of package `glowworm` is `glowworm.array`; module paths and symbols are as if the file sat at the package's top. A `source` folder that doesn't exist is E0322.
  - **`name`:** a manifest that lists a package with a declared name under another key is E0327 at the listing, with help to list it by its name and to use an import alias (`import glowworm.array as gw;`) for a short one. A package without `name` is listed under any key, as before.
  - **The program's manifest:** the `lugha.toml` beside the main file, as before; when there is none, the nearest one in a parent folder. The lookup stops, finding nothing, at a folder that holds `.git` or is the home folder (`$HOME`), after checking that folder itself; so a repository's root manifest is found, and one in a shared folder above a repository or the home folder (`/tmp`, `/`) never is. A found manifest's `[packages]` are the program's, with paths relative to its folder.
  - **The implicit package:** when the program's manifest declares `name = N`, the program may import that package as `N` (`import N.array;`), with its `source`, as if listed. So a library's `examples/` and `tests/` programs need no manifest of their own. `N` may not also be a folder or `.la` file beside the main file (E0320).
  - **One file, one module:** a file reached under two module paths, such as `src/cli.la` as `cli` from `src/main.la` and as `tool.cli` through the implicit package, is E0328 at the second import.
  - **Run:** `lughac run glowworm/examples/basics.la` works from any folder; the file argument is required.
- **The standard library** (v15, milestone 53). `import std.<module>;` loads a module bundled with `lughac`, written in Lugha, the same in every build of a given compiler version. It is read and checked only when a file imports it, so a program that imports nothing from `std` is unchanged, and of an imported module only the functions and methods the program reaches (by calls, bounds, interface values or as function values) are compiled (milestone 60); there is no way to opt out, and none is needed. Std modules follow the normal rules: their `public` items, `import std.math as m;`, `import std.math { sqrt };`. `std` is reserved: a manifest entry or `[package] name` called `std` is E0321, a folder `std/` or file `std.la` beside an importing module is E0320, and an unknown module (`import std.maths;`) is E0312, with help listing the std modules. Diagnostics inside a std module name its file `<std>/math.la`. The modules are listed under "The standard library" below.
- **Imports.** In `import a.b.c;` written in a module of package P (the program itself is the root package): if P's manifest lists `a`, the import loads `b/c.la` from `a`'s folder, and the module is `a.b.c`; otherwise it loads `a/b/c.la` from P's own folder, as before. So inside package `geo`, `import util.names;` is geo's own `geo.util.names`, and a program's `util.names` is a different module. `import geo;` naming only a package is E0312, with help to name a module.
- **Not transitive.** A package's manifest is seen only by that package's modules: a program that wants `mathx` lists it itself, even if a package it uses lists `mathx` too.
- **One name, one folder.** Across all the manifests of a build, a package name must mean one folder (compared after resolving `.` and `..`), or it is E0323 at the later entry. Because the root package's modules have no package prefix, no package name may also be a folder or `.la` file in the main file's folder, and inside a package a listed name may not also be one of that package's own folders or files: either is E0320.
- **Loading.** Every file is read once, so packages may import each other. A package has no main file; `main` in one is E0304, and importing the program's main file is E0319, as for any module.
- **Visibility.** Another package sees only what is `public` (below).

**The standard library** (v15). Version 15 has `std.math`, `std.random`, `std.env`, `std.fs` and `std.process`.

`std.math` (milestone 53). The float functions take and return any `T: Float` (`f32` or `f64`) and follow IEEE 754: they never panic, `sqrt(-1.0)` is `NaN`, `ln(0.0)` is `-inf` and `pow(0.0, -1.0)` is `inf`.

| Function | Result |
| --- | --- |
| `pi(): f64`, `e(): f64` | π and e (top-level constants are a later feature) |
| `sqrt(x)`, `exp(x)`, `ln(x)`, `log10(x)` | square root, eˣ, natural and base-10 logarithm |
| `pow(x, y)` | xʸ |
| `sin(x)`, `cos(x)`, `tan(x)` | in radians |
| `atan2(y, x)` | the angle of the point (x, y), in −π..π |
| `floor(x)`, `ceil(x)` | rounded down, up |
| `round(x)` | to the nearest integer value, halves away from zero (`round(-2.5)` is `-3.0`) |
| `abs<T: Num>(x)` | `-x` for a negative `x`; the minimum integer panics with `integer overflow`, as `-x` does |
| `min<T: Ord>(a, b)`, `max<T: Ord>(a, b)` | the smaller, larger; `a` when they are equal |
| `clamp<T: Ord>(x, lo, hi)` | `x` limited to `lo..=hi`; panics (`clamp: the lower bound is above the upper bound`) when `lo > hi` |

An integer goes to a float function through a cast: `math.sqrt(n as f64)`; `math.sqrt(2)` is E0419.

`std.random` (milestone 54). A `random.Rng` is a pseudo-random generator, xoshiro256\*\* (Blackman and Vigna) seeded through splitmix64. It is fast and statistically strong but **not cryptographically secure**: never use it for keys, tokens, passwords or anything an attacker must not predict. There is no global generator: a program makes one and passes it where it's needed; every method takes `mut self`, so the generator must be `let mut`.

| Function or method | Result |
| --- | --- |
| `seeded(seed: u64): Rng` | a generator whose numbers depend only on `seed`: one seed gives one sequence on every platform, at `-O0` and `-O2`, and in every v15 compiler |
| `from_os(): Rng` | a generator seeded with 64 bits from the operating system (`getrandom`, else `/dev/urandom`), different on every run; panics with `no randomness available from the operating system` if both fail |
| `r.next_u64(): u64` | the next 64 random bits |
| `r.uniform(): f64` | uniform in [0, 1), with 53 random bits; for `f32`, `r.uniform() as f32` |
| `r.below(n: i64): i64` | uniform in `0..n`, without bias; panics unless `n > 0` |
| `r.between(lo: i64, hi: i64): i64` | uniform in `lo..hi`, half-open like `for i in lo..hi`; panics unless `lo < hi` |
| `r.normal<T: Float>(mean: T, std: T): T` | a normal (Gaussian) value, by Box–Muller, in the arguments' type |
| `r.chance(p: f64): bool` | `true` with probability `p` |
| `r.choice<T>(xs: T[]): Option<T>` | a uniformly chosen element, `None` for an empty array |
| `r.shuffle<T>(xs: T[]): T[]` | a copy of `xs` in a uniformly random order (Fisher–Yates); `xs` is unchanged |

`std.env` (milestone 64). The program's environment, read-only: nothing sets variables or changes the working directory.

| Function | Result |
| --- | --- |
| `get(name: string): Option<string>` | the variable's value, or `None` when it is unset, isn't valid UTF-8, or `name` contains a NUL byte |
| `current_dir(): Result<string, string>` | the absolute path of the working directory, or `Err` with the system's message (e.g. when it was deleted) |

`std.fs` (milestone 65). Folders and what exists; a file's contents stay with the intrinsics (`read_file`, `write_file`, …). Errors are the system's message, as for `read_file`; a path holding a NUL byte is `Err("path contains a NUL byte")` (or `false`). Nothing deletes recursively.

| Function | Result |
| --- | --- |
| `list_dir(path): Result<string[], string>` | the entries' names sorted by their bytes, without `.` and `..`; a name that isn't UTF-8 is `Err("a name in <path> is not valid UTF-8")` |
| `make_dir(path): Result<i64, string>` | `Ok(0)` after making one folder (mode 0777 less the umask); its parent must exist |
| `make_dirs(path): Result<i64, string>` | `Ok(0)` after making the folder and any missing parents; already existing is fine |
| `remove_file(path): Result<i64, string>` | `Ok(0)` after removing a file or symbolic link; a folder is `Err` |
| `remove_dir(path): Result<i64, string>` | `Ok(0)` after removing an empty folder; a non-empty one is `Err` |
| `exists(path)`, `is_dir(path)`, `is_file(path)`: `bool` | whether something, a folder or a regular file is at `path`, following symbolic links; never an error |

`std.process` (milestone 66). Runs another program and waits for it. **There is never a shell**: the command and each argument reach the program exactly as given (`posix_spawnp`), so `;`, `|`, `$( )`, quotes and globs in them mean nothing. A command without `/` is looked up on `PATH`, one with `/` is a path; whoever controls `PATH` chooses which program a bare name runs, so use a path containing `/` when that matters. The program inherits this one's environment unchanged and gets only stdin, stdout and stderr: every other descriptor is closed on exec. A NUL byte in the command or an argument is `Err` before anything starts. There is no timeout and no limit on output size.

| Function | Result |
| --- | --- |
| `run(cmd: string, args: string[]): Result<Output, string>` | runs `cmd` with an empty stdin and captures its output |
| `run_with(cmd: string, args: string[], input: string): Result<Output, string>` | the same, writing `input` to its stdin |
| `call(cmd: string, args: string[]): Result<i32, string>` | runs `cmd` sharing this program's stdin, stdout and stderr (what this program printed comes first), and gives its exit code |

`Output` is `{ code: i32, stdout: string, stderr: string }`. `code` is the exit status, or `128 + n` when signal `n` ended the program, as shells report it. Any exit code is `Ok`; `Err` means the program couldn't start (`"tool: No such file or directory"`), an argument held a NUL byte, or its output isn't valid UTF-8 (`"stdout is not valid UTF-8"`). stdin, stdout and stderr are served together, so a program that fills one pipe while another is being read can't deadlock.


**Using another module.** `import shapes.circle;` binds the name `circle` in that file only.
- **Calls:** `circle.area(c)` calls function `area` of the module.
- **Types:** `circle.Circle` names its type in annotations; `circle.Circle { r: 1.0 }` builds it; `circle.Circle.new()` calls an associated function; `circle.Shape.Round(c)` and `circle.Shape.Flat` build variants.
- **Methods:** a method call on a value from another module (`c.area()`) works like any method call.
- **Errors:** an unknown module name is E0301; an unknown function or type in a known module is E0301 or E0305.

**Named imports and re-exports** (v11). An import may list items after its path:
- **`import a.b { X, y };`** imports module `a.b` as before (binding `b`), and binds each listed name in this file to that module's item: a function, extern, struct, enum or interface, written without a prefix (`X { … }`, `y(1)`, `X.f()`, `X.V`, `T: X`). Each must be an item of the module (E0301) visible here (E0313). A listed name the file also declares is E0302; one listed twice, or named like an imported module, is E0314.
- **`public import a.b { X, y };`** does the same, and makes `X` and `y` items of this module, visible to every package: a module that imports this one writes `m.X` and `m.y`, and may re-export them again. A re-exported item must itself be `public` (E0318). `private import` is E0201.
- **Same items:** a named import is the item itself, not a copy: `shapes.Vec2` and `vec.Vec2` are one type, calls reach one function, and types print with the declaring module's path (`mathx.vec.Vec2`). A name imported without `public` is the file's own: another module writing `m.X` for it is E0313. Re-exports don't change manifests: a program uses `shapes.Vec2` without listing `mathx`, and still can't `import mathx.vec;` unless it lists `mathx` itself.

**Aliases and whole-module re-exports** (v14).
- **`import a.b as c;`** binds the module as `c` instead of `b`; `b` is not bound. Aliases clash like any import name (E0314), so `import a.util as u1; import b.util as u2;` is how two `util`s share a file.
- **`import a.b { X as Y };`** binds the item as `Y` only. With `public`, the item is re-exported as `Y`.
- **`public import a.b;`** (or `public import a.b as v;`) makes the module a member of this one, visible to every package. A file that imports this module as `m` names `v`'s items `m.v.X`: calls `m.v.f()`, types `m.v.T`, literals `m.v.T { … }`, associated calls `m.v.T.f()`, variants `m.v.E.V`, bounds `T: m.v.I`, and function values `m.v.f`. Only `public` items are reached this way (E0313). One re-export is followed: a module re-exported by a re-exported module is reached through its own import.
- **Same items:** as with named imports, `m.v.X` is the item itself, so keys, types and symbols are the declaring module's.

**Names.** Each module has its own namespace for functions, structs, enums, interfaces and externs, so two modules may each declare `Point`; duplicates within a module are a compile error. Intrinsics and the built-in names `Option`, `Some`, `None`, `Eq` and `Ord` are visible in every module, and no program may declare them (E0302). Methods live in their type's namespace, so a function `dist` and a method `Point.dist` can coexist. No local may be named like a type or an imported module (E0311, section 4). Local variables may shadow function names. Names resolve from the innermost scope outward: block scopes, then function parameters, then the module's own items and named imports (v11), then imported module names. In messages a type from another module is shown with its full module path (`shapes.circle.Circle`); a main-file type keeps its bare name. `main` belongs in the main file (E0304 elsewhere).

**Visibility.** Each function, extern, struct, enum, interface, method, associated function and struct field has one of three levels (v8):

| Written | Visible in |
| --- | --- |
| `private` | its own module only |
| nothing | every module of its own package |
| `public` | every package that imports its module |

A program without a `lugha.toml` is one package, so there "nothing" means the whole program, as before v8. The prelude's items are visible everywhere.
- **Using what isn't visible** (E0313): naming such an item, calling such a method, or reading or assigning such a field (including through `swap` and `mut self`) is an error. Its message says why: "`helper` is private to module `range`", or "`helper` is not `public`: it is only visible inside package `geo`". A method another package can't see doesn't satisfy that package's interface bounds (E0422).
- **Struct literals:** a literal outside the struct's module needs every field to be visible, so a struct with a `private` field is built through one of its functions (`range.Range.new(1, 5)`). This is how a type protects an invariant such as `lo <= hi`. `==`, copies and passing values are unaffected: they never name a field.
- **Fields and variants:** a field can be no more visible than its struct; variants share their enum's visibility.
- **No leaking** (E0318): a declaration may not mention a type less visible than itself, since the code that can use it couldn't name that type. A declaration that isn't `private` may not mention a `private` type in its signature (parameters, return type, a field's type, a variant's payloads, an interface item), looking inside arrays, `Option` and function types. A `public` declaration (v8) may mention only `public` structs, enums and interfaces, looking also inside type arguments (`Box<Inner>`), interface types and a generic function's bounds.

**Functions.** Declared with `fun`, typed parameters, and an optional `: type` return annotation. Omitting the annotation means `void`.

**Generic functions.** A generic function is declared once and compiled once for each distinct list of type arguments the program uses (section 9). One that nothing calls is checked but not compiled. An `extern fun` can't be generic, which the grammar enforces.

**Expression bodies.** A function whose body is a single expression can be written Kotlin-style, with `=` and a semicolon instead of braces. `fun f(...): T = e;` means exactly `fun f(...): T { e }`, so every rule for blocks and tail values applies. The return type follows the same rule as block bodies: it must be written, and omitting it means `void`. This keeps signatures readable without looking at the body, and lets the checker collect every signature before it checks any body.

```
fun square(x: i64): i64 = x * x;

fun is_even(n: i64): bool = n % 2 == 0;

fun max(a: i64, b: i64): i64 = if a > b { a } else { b };

fun greet(name: string) = println("hello, " + name);

fun clamp(x: i64, lo: i64, hi: i64): i64 {
    let low = max(x, lo);
    if low > hi { hi } else { low }
}
```

- Parameters are immutable bindings. To modify one, shadow it with a mutable copy: `let mut n = n;`.
- Arguments are never copied at the call. Structs and arrays are passed by pointer, which is safe because the callee can't modify them and nothing else runs during the call. A copy happens only if the callee stores or returns the parameter (section 4).
- Recursion, including mutual recursion, is allowed.
- There are no default parameters, overloading, variadics or nested functions.

**Return checking.** A non-`void` function must produce a value on every path. Its body passes if its tail expression has the return type, or if the body *definitely returns* by these rules:

- `return e;` definitely returns.
- A block definitely returns if any statement in it does. Statements after it are unreachable and reported as a warning.
- `if/else` definitely returns if both branches do. `if` without `else` never does.
- Loops never do, even `while true`.

A `panic(...);` statement also counts as definitely returning, because it never returns. A block that definitely returns can stand in for a value of any type, so `if x < 0 { return 0; } else { x }` has type `i64`.

This rejects a few correct programs, such as a function whose only exit is a `return` inside `while true`. Add a final `panic("unreachable");` in that case. `return` stays available for early exits. In a `void` function, `return;` is allowed and falling off the end is an implicit return.

**Entry point.** Every program must define `main` as either `fun main()` or `fun main(): i32`, without type parameters (E0304). The process exit code is 0 for the first form and `main`'s return value for the second. The user's `main` is emitted as `lugha_fn_main`, like every Lugha function, and the compiler emits a real C `main` that initializes the runtime and calls it (see section 9). Command-line arguments are not accessible in v0.

## 7. Memory model

Value types live on the stack; strings and arrays live on a garbage-collected heap managed by the Boehm-Demers-Weiser conservative collector. Programs never free memory explicitly.

**Stack.** Every local variable and parameter gets an `alloca` in its function's entry block, including struct values. LLVM's `mem2reg` pass promotes these to registers. Struct parameters are passed as a pointer to the caller's storage (section 9); struct return values are returned by value in LLVM IR, and LLVM handles the platform calling convention.

**Enums.** An enum value is a stack value `{ i32 tag; payload area }`. Variants are numbered in declaration order (`None` is 0 and `Some` is 1). The payload area has room for the largest variant's payloads laid out as a struct with natural alignment, and the whole value is aligned to the larger of 4 and that alignment. Enum parameters are passed like structs: as a pointer to the caller's storage. Copying an enum copies its tag and payload bytes; arrays in its payloads are shared, never mutated.

**Indirect slots.** A payload slot whose type leads back to its enum through values held inline — struct fields, enum payloads or `Option`'s payload, but not arrays — is an 8-byte pointer to a copy of the value on the GC heap, allocated with `lugha_rt_alloc` when the variant is built and never mutated afterwards. Every other slot, and the enum value itself, stays inline. `List` is therefore `{ i32 tag; i64 head; ptr rest }`.

**Function values** (v3) are a pair `{ ptr code, ptr env }`, 16 bytes with alignment 8, passed and stored by value. A named function's value has a null environment, and its code is the function's `lugha_ref_` wrapper (section 9).

**Interface values** (v5) are a pair `{ ptr table, ptr box }`, 16 bytes with alignment 8, passed and stored by value. The box is a GC object allocated with `lugha_rt_alloc` that holds one value of the concrete type, primitives included. The table is a constant per (concrete type, interface) pair: the type's identity, a function copying a box, then one pointer per method in the interface's declaration order. Copying an interface value calls the table's copy function, which allocates a new box holding a deep copy. Converting one interface value to another (v6) copies the box as any conversion does and pairs it with the other interface's table for the same concrete type.

**Closure environments** (v3). A closure's environment is one GC object, allocated with `lugha_rt_alloc` when the closure is created and laid out as a struct of its captures. It is never written afterwards, so copies of the closure share it safely. A closure that captures nothing has a null environment.

**Generic instances** lay out exactly like the concrete types they stand for: `Stack<i64>` is laid out like a struct with one `i64[]` field, and `List<bool>` gets its own indirect slots.

**Recursion depth.** Function calls use the native stack, which is finite. A recursion that goes too deep — for example walking a list of a million nodes recursively — panics with `stack overflow` (exit 101). The runtime detects it when the stack's guard area is hit, so ordinary calls cost nothing. Walk very long lists with a loop.

**Heap objects.** All heap memory comes from one runtime function, `lugha_rt_alloc(size)`, which calls `GC_malloc`. Memory from `GC_malloc` is zeroed. Every heap object starts with a length header:

| Object | Layout | Pointer held by Lugha code |
| --- | --- | --- |
| `string` | `{ i64 len; u8 bytes[len]; u8 nul }` | Points at `len` |
| `T[]` | `{ i64 len; i64 cap; T elems[cap] }` | Points at `len` |

Elements are laid out with LLVM's natural alignment for `T`. An array's capacity is never visible to programs: literals, repeat literals and copies are full (`cap == len`), and `push` on a full array allocates a new one with double the capacity (at least 4), copies the elements, and stores the new pointer in the array's place — its only owner, so no other pointer sees the old object. String data is always followed by a NUL byte, so it can be passed to C without copying. String literals are emitted as private constant globals with the same layout, and are never freed.

**Copy-on-write** (v15). The top bit of an array's `cap` word is its *shared* flag, so capacities use 63 bits and the header stays 16 bytes. A copy site sets the flag and shares the pointer. Before any write into an array — an assignment through it, `push`, `pop`, `clear`, `swap`, a `mut self` call or `for … of mut` — the array is made unique: if the flag is set, it is cloned one level (its elements copied, and any array held in them marked shared in turn) and the clone, unshared and full, replaces it in the place that held it. The flag is sticky: an array once shared stays marked, so its other holders clone it if they ever write. No reference counts are kept, which Boehm doesn't provide. A clone that turns out to be the last holder costs one extra copy, never a wrong result. A loop may make arrays unique once, before it starts, instead of at each write (section 9, milestone 56); programs can't tell the difference.

So, as far as programs can tell, every array is owned by exactly one variable, field or element. Only unwritten arrays and strings, which are immutable, are actually shared.

**Collection.** Boehm scans the stack, registers and the GC heap for anything that looks like a pointer into the heap. Objects with no such pointer are reclaimed. Two consequences matter for v0:

- **No compiler support is needed.** The compiler doesn't emit stack maps or GC metadata. It only has to route every allocation through `lugha_rt_alloc`.
- **Pointers must stay visible.** Boehm can't see memory allocated by C's `malloc`. If an extern C function stores a Lugha string or array somewhere Boehm doesn't scan, the object may be freed while still in use. In v0, extern functions must not keep references to Lugha objects after they return.

Boehm is conservative, so an integer that happens to look like a heap address keeps that object alive. This can waste some memory but never causes incorrect behavior.

**Safety.** Ordinary v0 code cannot produce a dangling pointer, read uninitialized memory, or index out of bounds. The only route to undefined behavior is calling extern C code.

## 8. C interop

Lugha calls C functions through `extern fun` declarations, which give a C function's name and signature without a body. The compiler emits an LLVM function declaration, and the system linker resolves it.

```
extern fun puts(s: string): i32;
extern fun abs(x: i32): i32;
extern fun sqrt(x: f64): f64;
```

**Allowed types.** Extern signatures may use only the types below. Structs, arrays, enums, `Option`, function values and interface values (v5), generic or not, cannot cross the boundary: C can't call a Lugha function. So C never sees an array, and copy-on-write (v15) needs nothing at the boundary; strings are immutable.

| Lugha type | C type | Passed as |
| --- | --- | --- |
| `i32` | `int32_t` | Value |
| `i64` | `int64_t` | Value |
| `u8` | `uint8_t` | Value |
| `f64` | `double` | Value |
| `u32` | `uint32_t` | Value (v7) |
| `u64` | `uint64_t` | Value (v7) |
| `f32` | `float` | Value (v7) |
| `i8`, `i16` | `int8_t`, `int16_t` | Value, sign-extended (v10) |
| `u16` | `uint16_t` | Value, zero-extended (v10) |
| `bool` | `bool` (C99) | Value, `i1` zero-extended |
| `string` (parameter only) | `const char *` | Pointer to the first data byte, 8 bytes past the header |
| return type omitted | `void` | — |

The compiler adjusts the string pointer at each call site. C code receives an ordinary NUL-terminated string. A `string` may not be an extern return type, because C strings aren't allocated by the GC and have no length header.

**Rules**

- Extern functions are called exactly like Lugha functions.
- Variadic C functions such as `printf` cannot be declared in v0. Use the `print` and `println` intrinsics instead.
- The extern name is used as the C symbol name unchanged. Lugha functions themselves are emitted with a `lugha_fn_` prefix, so they never collide with C symbols. Methods and associated functions are `lugha_fn_<Type>.<name>` (for example `lugha_fn_Point.dist`); `.` can't appear in a Lugha name, so these can't collide either.
- A package's modules are named by paths that start with the package name (v8), so their symbols do too: function `area` of `geo`'s module `shapes` is `lugha_fn_geo/shapes/area`. Diagnostics in a `lugha.toml` name that file relative to the main file's folder (`lugha.toml`, `libs/geo/lugha.toml`).
- Named imports and re-exports (v11) are resolved by the checker to the declaring module's items, so they add no symbols: `shapes.add` calls `lugha_fn_mathx/vec/add`.
- Items of a module other than the main file put the module's path before their name, joined with `/`: function `area` of module `shapes.circle` is `lugha_fn_shapes/circle/area`, and its method `Circle.area` is `lugha_fn_shapes/circle/Circle.area`. A name contains neither `/` nor `.`, so a module function (`lugha_fn_a/b/c`) can't collide with a method (`lugha_fn_a/b.c`, method `c` of type `b` in module `a`).
- A generic instance adds its type arguments as they print, with module paths joined by `/`: `lugha_fn_max<i64>`, `lugha_fn_a/b/max<a/b/Point>`. A method's go on its type, then its own: `lugha_fn_Stack<string>.push`, `lugha_fn_Stack<string>.tag<f64>`. `<` can't appear in a Lugha name, so instances can't collide with anything else.
- A function used as a value (v3) is called through `lugha_ref_<symbol>`, a wrapper that takes an environment pointer first and calls `lugha_fn_<symbol>`: `lugha_ref_double`, `lugha_ref_max<i64>`, `lugha_ref_Point.at`. It is emitted once, when first needed.
- An interface value's table (v5) is `lugha_vt_<Type>.<Interface>`, with names printed as in other symbols (`lugha_vt_Circle.Shape`, `lugha_vt_Stack<i64>.geo/Sized`), emitted once per pair a program converts. Its identity slot points at `lugha_type_<Type>`, its copy slot at `lugha_copy_<mangled type>_box` (`lugha_copy_6Circle_box`), and each method slot at the method itself: methods take their receiver as a pointer, so a call passes the box. Copying an interface value goes through `lugha_copy_<mangled interface>` (`lugha_copy_5Shape`). A generic interface (v6) prints with its arguments: `lugha_vt_Count.Source<i64>`, `lugha_copy_11Source<i64>`. A type pattern compares the table's identity slot with `lugha_type_<T>`.
- A default method (v5) compiles once per type that uses it, through a table or a bound: `lugha_fn_<Interface>.<method><Type>` (`lugha_fn_Error.report<ParseError>`); that type's table slot, bound calls and (v10) calls on the type itself use it. A generic interface's default (v6) compiles once per type and interface arguments, the arguments printed on the interface: `lugha_fn_Source<i64>.skip<Count>`. A converted `?` error is boxed on the failure branch, like any conversion. A program without interface values has none of these.
- Printing a value that isn't a number, `bool` or `string` (v12) calls `lugha_show_<type>(value) -> string`, named like the copy helpers (`lugha_show_5Point`, `lugha_show_i64_arr`) and emitted once, only when used: it calls the type's `show`, or builds the built-in form with `lugha_rt_str_concat` and quotes strings with `lugha_rt_str_quote`. A generic type's `show` is listed as an instance wherever such a value is printed, inside other values and through type parameters too. An interface type's helper, built at the end of lowering, compares the value's identity slot with each concrete type the program has a table of for that interface and prints the box's value by its rules; dispatch tables are unchanged. A function value prints the literal `<fun>`. `unwrap()` on a `Result` builds its panic message in the failing branch from the error's text. Programs printing only numbers, `bool` and strings, and calling no `unwrap()` on a `Result`, give byte-identical IR.
- `Self` in `impl` blocks and in struct and enum declarations (v10, v13) is replaced by the parser with the type it stands for, so the checker and codegen see the type's own name.
- `for x of mut e` (v13) works on `e`'s place: over an array, `x` names the element itself (an `i1` occupies one byte, so `bool` elements alias too); over an iterator, `next` is called on `e`'s own storage instead of a hidden copy. Since v15 the element (or iterator) is reached afresh through `e`'s place, made unique, at each use, with the index expressions of `e` evaluated once before the loop.
- **Copy-on-write places** (v15): a write lowers in two phases. The plan evaluates the place's index expressions and bounds-checks them; the commit walks from the root slot, loading each array on the path, testing its shared flag (`cap < 0`) and, if set, cloning it and storing the clone back into the slot, then forms the element address. Assignment plans, evaluates the value, then commits; `swap` plans both places before committing either.
- **Owning arrays once per loop** (v15, milestone 56): before any loop, the compiler scans its condition and body. A variable from outside the loop qualifies when the loop mentions it only as the root of a written place whose new value holds no array (`=`, `op=`, `push`, `pop`, `clear`, a method on a value without arrays), or in reads whose value holds no array (numbers, `bool`, `char`, strings, `.len`, structs without arrays). Any other mention — a copy, an argument or return, a capture, a method call on a value holding arrays, `swap`, `for … of`, `match` — disqualifies it. For a qualifying variable, each array the loop writes through whose path is fixed (every index a literal or a variable the loop never assigns or binds, with no calls; a `for … of mut` variable is never fixed) is made unique before the loop. An index then out of bounds skips that step instead of panicking. Writes inside the loop to such a path load the array without testing its flag: nothing in the loop can share it again.
- The v13 iterator helpers are Lugha in the prelude's third file, compiled only when reached like the others. `flat_map` keeps its current inner iterator in a 0- or 1-element array, so `next` steps it in place.
- A format specifier (v13) is parsed once by the checker (E0431) and again by codegen, which passes its parts to `lugha_rt_fmt_int`, `lugha_rt_fmt_float` or `lugha_rt_pad`, from a runtime fragment `format.c`. Programs without specifiers give byte-identical IR.
- `format` (v14) splits its template into text and holes in the checker and again in codegen, then lowers like an interpolated string with the arguments as its parts. Programs without `format` give byte-identical IR.
- `std.process` (v15) is Lugha code over one built-in, `$os.spawn(cmd, args, input, mode): Result<(i32, u8[], u8[]), string>` (`lugha_rt_spawn`: `posix_spawnp`, `pipe2(O_CLOEXEC)` pipes, a `poll` loop, `SIGPIPE` blocked while writing, `waitpid`); the outputs become strings through `string_from_utf8`. The runtime opens its own files close-on-exec (`fopen` mode `"e"`, `O_CLOEXEC`). Changes to `runtime/process.c` need the user's security review, like `modules/git.rs`.
- `std.fs` (v15) is Lugha code over `$os.list_dir` (names as bytes, sorted in Lugha), `$os.make_dir`, `$os.remove_file`, `$os.remove_dir` (`mkdir`, `unlink`, `rmdir`) and `$os.stat_kind` (`stat`: 0 none, 1 file, 2 folder, 3 other); `make_dirs` is Lugha over `make_dir` and `is_dir`.
- `std.env` (v15) is Lugha code over `$os.env(name): Option<u8[]>` (`lugha_rt_env`, `getenv`, a copy) and `$os.cwd(): Result<u8[], string>` (`lugha_rt_cwd`, `getcwd` with a growing buffer); the bytes become strings through `string_from_utf8`, so C never decides what UTF-8 is.
- `std.random` (v15) is Lugha code except `$os.seed()`, which calls the runtime's `lugha_rt_os_seed() -> u64`, and `$float.from_f64(x)`, which is a no-op at `f64` and `fptrunc` at `f32`; both built-ins can be named only in bundled sources.
- A std module (v15) is embedded in `lughac` (`lugha::STD`) and joins the program's offset space like a file when imported; its items' symbols carry its path, `lugha_fn_std/math/sqrt<f64>`. A `$float.<op>` built-in at `f32` or `f64` lowers to LLVM's intrinsic at that width (`llvm.sqrt.f64`, `llvm.log` for `ln`, `llvm.round`, `llvm.atan2`, …), so `std.math` makes no call into libm. Programs that import nothing from `std` give byte-identical IR.
- An overloaded operator (v15) lowers to a call of the method, `lugha_fn_Vec2.add(self, other)` with a pointer to a copy of the left operand as for any `self` method, or through the interface's table for an interface value. A generic body's operator on `T` is lowered per instance: at a number or string type it is the built-in operator, so `add<i64>` is a plain checked `add`; at a struct or enum type, the method call. A `0` or `1` of type `T` is the instance's constant (`0.0` at `f64`). Programs without overloaded operators give byte-identical IR.
- An index (v14) is evaluated at its own integer type and sign- or zero-extended to `i64` before the bounds check, so an `i64` index lowers exactly as before.
- A `char` (v14) is an LLVM `i32`, compared unsigned. An integer `as char` selects U+FFFD unless the value, widened to `i64`, is a scalar value. `chars()`, `char_count()`, `char_from` and the classification and case methods are Lugha in the prelude's fourth file.
- A tuple type (v14) is the generic struct `$tuple<n>` with fields `0`, `1`, …, so it lowers as a struct: an LLVM named struct spelt as the type prints, `%"(i64, string)"`, with struct copy, equality and print helpers, and symbols such as `lugha_fn_max<(i64, i64)>`. `<` calls one `lugha_cmp_<type>(a, b) -> i32` helper per tuple type. A tuple pattern tests each field's address, with no tag. `for (i, w) of e` becomes a loop over a hidden variable whose body starts `let (i, w) = …;`.
- Converting a `J` value to `I` (v6) calls `lugha_up_<J>.<I>(table) -> table`, which compares the value's table with each `J` table the program has and returns the matching `I` table; its body is built after everything else, when every table is known. That `I` table is derived from the table the value was built with: `lugha_vt_<C>.<J>.<I>` has `C`'s identity and copy and, for each `I` item, the function `C`'s `J` table has, so methods behave as through `J`; derived tables keep the original interface (`<C>.<J>.<K>` for a chain) and converting back to it gives the original table. A program that never converts between interfaces has no `lugha_up_` functions and no derived tables. (The approved draft put one slot per target interface in each table instead; tables are emitted while lowering, before every conversion is known, so PRP-054 uses lookup functions.)
- The v7 operators and methods: `& | ^` are `and`/`or`/`xor`; `<<` is `shl` and `>>` is `ashr` for `i32`/`i64` and `lshr` for the unsigned types, each after an `icmp uge` of the amount against the width that panics (`shift out of range`); `~x` is `xor x, -1`; the wrapping methods are plain `add`/`sub`/`mul`; the counting methods are `llvm.ctpop`, `llvm.ctlz` and `llvm.cttz` (zero not poison), widened to `i64`. A program that uses none of them has none of this code.
- The v10 integer methods: `checked_add/sub/mul` read the overflow flag of `llvm.{s,u}{add,sub,mul}.with.overflow` and select `None` or `Some`; `checked_div` divides by 1 instead when the divisor is 0 (or the operands are `MIN` and −1) and selects `None` then, so it never traps; `saturating_add/sub` are `llvm.{s,u}{add,sub}.sat`, and `saturating_mul` selects `MIN` or `MAX` on overflow; rotations are `llvm.fshl`/`llvm.fshr` of the value with itself, the `i64` count truncated to the width; `I.MIN`/`I.MAX` are constants.
- `if let`, `while let` and `let … else` (v9) test their pattern as an arm of the arm chain does, jumping on a mismatch to the `else` branch, the loop's exit, or the `else` block; `let … else` declares its bindings in the enclosing block's scope.
- `unwrap` and `expect` (v9) are built in, not prelude functions, so their panic points at the caller: the value's tag is compared with `Some` or `Ok`, and a mismatch calls `lugha_rt_panic` with the message and the method name's location.
- An or-pattern (v9) lowers in the arm chain: each alternative that fails tries the next, and every alternative stores a name's value into the arm's one local for it; a `match` with an or-pattern or a `mut` binding never uses the flat `switch`. A `mut` binding is a local of its own, deep-copied when its type is.
- An iterator loop (v6) stores the iterator in a hidden local and calls its `next` with that local's address: directly for a struct or enum (its own method, or a bound default's instance in a generic instance), and through the table for an interface value.
- A closure's body (v3) is `lugha_closure_<enclosing>.<n>`: the enclosing function's symbol without its prefix, then the closure's number in that function from 0 (`lugha_closure_main.0`, `lugha_closure_main.0.0` for a closure inside it, `lugha_closure_max<i64>.0` in an instance). Its environment comes first, as for every function value's code.
- The array helpers (v3) are generic functions of a prelude written in Lugha, bundled with the compiler and checked with every program, under the module path `$prelude`, which no program can name: `lugha_fn_$prelude/map<Person, string>`. Each is compiled only at the types a program uses.
- The iterator helpers and sources (v10) are Lugha in the prelude too: `it.map(f)` calls the prelude's `iter_map` with `it` converted to `Iter<T>` (its type's table, as for any conversion), `xs.iter()` calls `array_iter`, and `it.sum()` calls the `iter_sum_<T>` of its element type. Like every prelude item, each function, instance and method (`lugha_fn_$prelude/Range.next`) is compiled only when the program reaches it. `for b of s` is the array loop over the string's bytes, 8 bytes past the string pointer.
- An extern follows the visibility rules of section 6, and the same C function may be declared in several modules only with identical signatures (E0306); it is one symbol.
- Runtime library functions use the `lugha_rt_` prefix. Neither prefix is a prefix of the other, so a Lugha function can never collide with a runtime function, whatever its name.
- Extern names starting with `lugha_` are reserved and rejected with an E03xx error, so an extern declaration can't collide with either.
- Extern C code must not keep a Lugha string after the call returns, and must not write through the pointer (section 7).
- C's `libc` and `libm` are always linked, so their functions can be declared directly.

## 9. Compilation model

The compiler, `lughac`, is written in Rust with inkwell. It turns one `.la` file into one native executable, in six stages. Each stage stops the pipeline if it reports an error.

1. **Lex.** Source text → tokens with spans.
2. **Parse.** Tokens → AST. Recursive descent for items and statements, Pratt parsing for expressions.
3. **Check.** Resolve names, compute a type for every expression, check every rule in sections 4–6. Output: the AST plus a side table mapping each expression to its type.
4. **Lower.** Typed AST → LLVM IR via inkwell, one LLVM function per Lugha function. Run `module.verify()` afterwards; a verifier failure is a compiler bug, not a user error.
5. **Optimize and emit.** Run LLVM's pass pipeline (`default<O0>` or `default<O2>`), then write an object file with `TargetMachine`.
6. **Link.** Invoke the system C compiler, which also compiles the runtime: `cc prog.o lugha_rt.c -lgc -lm -o prog`. The runtime source is embedded in `lughac` and written to a private temporary file for each build.

**Lowering notes.** These are the places where Lugha semantics need specific IR:

- **Overflow checks:** lower `+`, `-` and `*` to the `llvm.sadd.with.overflow`, `llvm.ssub.with.overflow` and `llvm.smul.with.overflow` intrinsics (the `u` versions for `u8`). Each returns the result plus an overflow flag; branch to a panic call when the flag is set. Unary `-x` is a checked `0 - x`. At `-O2`, LLVM removes checks it can prove never fire.
- **Division:** compare the divisor to zero (and check `MIN / -1`) before `sdiv`/`srem`; branch to a panic call on failure.
- **Bounds checks:** load the length header, compare with `icmp ult` (an unsigned compare catches negative indexes too), branch to a panic call on failure.
- **Short-circuit `&&` and `||`:** separate basic blocks joined by a `phi`.
- **if expressions:** lower each branch to its own basic block, as for statements, and merge a non-`void` result with a `phi` in the join block. A branch that definitely returns contributes no incoming edge.
- **Float-to-int casts:** the `llvm.fptosi.sat` intrinsic gives the saturating behavior in section 4.
- **Opaque pointers:** every `load`, `store` and `getelementptr` needs its element type, taken from the checker's type table.
- **Array copies:** generate one deep-copy function per type that contains arrays (for example `lugha_copy_i64_arr_arr`, or `lugha_copy_5Point_arr` — struct names are length-prefixed so they can't be confused with `_arr` or primitive names; the `lugha_copy_` prefix is disjoint from `lugha_fn_` and `lugha_rt_`), and call it to clone a shared array before a write (v15). A copy site itself only marks arrays shared: `or` of the flag into `cap`, through the struct, interface box and closure copies that reach them. For arrays of plain values, a clone is one `lugha_rt_alloc` plus `llvm.memcpy`; for arrays of arrays, the helper copies the outer array and marks the inner ones.
- **Deep equality:** generate one `lugha_eq_<type>(T, T) -> i1` per struct, enum, `Option` or array type compared, with the copy helpers' type names (`lugha_eq_1L_opt` for `Option<L>`). Numbers and `bool` compare inline, and strings through `lugha_rt_str_eq`.
- **Struct, enum and array arguments:** pass a pointer to the caller's storage rather than the value itself, since the callee can't modify it.
- **`?`** (v4): the operand goes in a slot and its tag picks the branch. The failing branch builds the function's own `Err(e)` (or `None`) from the payload and returns it; the passing branch loads the `Ok`/`Some` payload.
- **Text and option helpers** (v4) are prelude functions (`str_trim`, `option_map`, `result_map_err`, …) chosen by the receiver's type; `slice` is built in and calls `lugha_rt_str_slice`. A prelude function without type parameters is compiled only when user code reaches it, as generic helpers are.
- **`Result`** (v4) is declared in the prelude, so it is the generic-enum machinery throughout; its LLVM type and helpers are named as it prints (`%"Result<i64, string>"`).
- **Array helpers** (v3): `xs.map(f)` calls the prelude's `map` instance with `xs` first. The prelude is Lugha source like any other (`sort_by` is a bottom-up merge sort between two buffers), so it needs no special lowering; `--emit=tokens|ast` leaves it out, and an error inside it is reported as an internal compiler error.
- **User-defined order** (v15): `<` on a struct or enum with `cmp` calls `lugha_fn_<Type>.cmp` (a generic type's instance) with a pointer to a copy of the left operand, and compares the returned `Ordering`'s tag with `Less` and `Greater` by variant; a tuple's `lugha_cmp_<type>` helper does the same for such elements, as −1, 0 or 1. Through `T: Ord`, each instance resolves `cmp` for its type.
- **Compiled when reached** (v15, milestone 60): the prelude's and std's non-generic functions and methods are listed in the instance list like generic ones, from the uses in the program's bodies and in other listed bodies, and only listed ones are declared and defined. A function used as a value counts as a use, and so does converting a value to an interface for each method its table holds. A program's own modules and its packages are compiled whole.
- **`it.sum()`** (v15, milestone 61) calls the prelude's `iter_sum<T: Num>` instance for the element type (`lugha_fn_$prelude/iter_sum<i64>`), one generic body instead of one per number type.
- **Calling a function value** (v3): extract its code and environment, and call the code indirectly with the environment as a hidden first argument, then the arguments as for any call (aggregates by pointer).
- **Monomorphization:** after checking, the checker lists every generic function and method instance reachable from the non-generic code, at concrete type arguments, and every concrete type those instances use (E0420 when they nest too deep). Codegen compiles each instance from its one body by substituting the type arguments into the types the checker recorded; it still never infers a type. Generic structs and enums get one LLVM type per instance, named as they print (`%"Stack<i64>"`), and their own copy and equality helpers (`lugha_copy_10Stack<i64>`).
- **Methods:** ordinary functions whose first parameter is the receiver. A `self` receiver is passed like a struct argument; a `mut self` receiver is the address of the caller's place itself, so the method's writes land there.
- **`match`:** an LLVM `switch` on the tag for enums and on the value for integers and `bool`s; a chain of `lugha_rt_str_eq` tests for strings. Payload bindings load from the payload area at constant offsets. An exhaustive `match` without `_` ends in `unreachable`. A `match` with a nested pattern or a guard is lowered arm by arm instead: each arm tests its pattern outside in (loading indirect payloads through their pointer), binds, runs its guard, and on any failure branches to the next arm.

**Runtime library.** A small C file, `lugha_rt.c`, is compiled once and linked into every program. The compiler calls only these functions:

| Function | Purpose |
| --- | --- |
| `lugha_rt_init(argc, argv)` | Keeps `argc`/`argv` for `args()`, runs `GC_INIT()`, then installs the stack-overflow handler (an alternate signal stack and a `SIGSEGV` handler that panics only for faults at the stack's end); the C `main` calls it first, since a C macro can't be called from IR |
| `lugha_rt_alloc(size)` | Allocate zeroed GC memory |
| `lugha_rt_panic(msg, file, line, col)` | Print the panic message and `exit(101)` |
| `lugha_rt_str_concat(a, b)` | Implement string `+` |
| `lugha_rt_panic_bounds(len, index, file, line, col)` | The out-of-bounds panic, with the length and index |
| `lugha_rt_str_eq(a, b)` | Implement string `==` and `!=`; returns 1 or 0 as `i32` |
| `lugha_rt_str_cmp(a, b)` | Implement string `<`, `<=`, `>`, `>=`; returns −1, 0 or 1 as `i32` (v2) |
| `lugha_rt_array_grow(array, elem_size)` | A copy of a full array with double the capacity (at least 4), for `push`; the copy is unshared |
| `lugha_rt_print_*` / `lugha_rt_to_string_*` | One per primitive type (plus `lugha_rt_print_str`), for the intrinsics; `bool` and `u8` arguments are passed zero-extended to 32 bits; `u32` goes to the `u64` functions zero-extended (v7), as does `u16`, and `i8`/`i16` go to the `i64` ones sign-extended (v10) |
| `lugha_rt_str_quote(s)` | `s` quoted and escaped, for strings inside a printed value (v12) |
| `lugha_rt_os_seed()` | 64 bits of OS entropy for `std.random`'s `from_os()` (v15): `getrandom`, else `/dev/urandom`, else a panic |
| `lugha_rt_env(name, out)` / `lugha_rt_cwd(out, error)` | `std.env` (v15): a variable's bytes, or 0 when unset or the name holds a NUL; the working directory's bytes, or 0 and the system's message |
| `lugha_rt_list_dir(path, out, error)` / `lugha_rt_make_dir` / `lugha_rt_remove_file` / `lugha_rt_remove_dir(path, error)` / `lugha_rt_stat_kind(path)` | `std.fs` (v15): a folder's names as bytes, one folder made or removed, a file removed, each with the system's message on failure; the kind of thing at a path |
| `lugha_rt_spawn(cmd, args, input, mode, code, out, err, error)` | `std.process` (v15): runs a program without a shell (mode 0 shares stdio, 1 captures with an empty stdin, 2 also writes `input`), and gives its code and output bytes, or 0 and why it couldn't start |
| `lugha_rt_fmt_int(v, bits, signed, kind, alt, fill, align, width, zero)` / `lugha_rt_fmt_float(v, f32, precision, fill, align, width, zero)` / `lugha_rt_pad(text, fill, align, width)` | Format specifiers (v13): an integer (sign- or zero-extended to `i64`), a float (as `f64`), or any other value's text, padded; all arguments are plain numbers |
| `lugha_rt_print_newline()` | The newline `println` adds |
| `lugha_rt_args()` | A new `string[]` of the program's arguments (v3) |
| `lugha_rt_parse_f64(s, out)` | Parse `s` into `*out`; return 1, or 0 for `None` (v3). `parse_i64` and `parse_u64` are prelude functions since v15 (milestone 62) |
| `lugha_rt_str_slice(s, from, to, file, line, col)` | `s.slice(from, to)`: a new string of those bytes, or the bounds panic (v4) |
| `lugha_rt_read_bytes` / `lugha_rt_write_bytes` / `lugha_rt_read_stdin_bytes` / `lugha_rt_str_to_bytes` / `lugha_rt_str_from_bytes` | Bytes (v14): file and stdin bytes, a string's bytes, failing ones by flag and out-pointers as the v4 file functions do; and (v15) checked bytes as a new string, behind the prelude's `$string.from_bytes`. The UTF-8 check behind `string_from_utf8` and `s.chars()` is one prelude decoder, `utf8_decode` (milestone 62) |
| `lugha_rt_print_char` / `lugha_rt_to_string_char` / `lugha_rt_char_quote` | A `char` written as UTF-8, as a string, and quoted for a built-in form (v14) |
| `lugha_rt_read_line(out)` | `read_line()`: 1 and the line, or 0 at the end of input (v4) |
| `lugha_rt_read_file(path, out, error)` / `lugha_rt_write_file(path, text, append, count, error)` | The file intrinsics: 1 and the value, or 0 and the C library's `strerror` message (v4) |
| `lugha_rt_eprint(text, newline)` / `lugha_rt_exit(code)` | `eprint`/`eprintln` (values formatted first) and `exit`, each flushing stdout first (v4) |

The runtime writes through C's `stdout` stream, so output from `print`/`println` and from libc functions such as `puts` appears in program order.

The C `main(argc, argv)` emitted by the compiler calls `lugha_rt_init(argc, argv)` (which runs `GC_INIT()`), then `lugha_fn_main()`, and returns its exit code. An interpolated string lowers to the `lugha_rt_to_string_*` functions and `lugha_rt_str_concat`.

**Command-line interface**

| Command | Effect |
| --- | --- |
| `lughac build prog.la [-o prog]` | Compile and link an executable |
| `lughac run prog.la [-- args…]` | Build to a temporary file and run it, passing `args` to the program (v3) |
| `lughac check prog.la` | Lex, parse and type-check only; no codegen or linking (type checking from milestone 3) |
| `lughac spec` | Print the LLM-ready spec bundled with this compiler version |
| `lughac update prog.la` | Re-resolve every git package to its newest matching tag and rewrite `lugha.lock` (v14) |
| `--offline` | On `build`, `run` and `check`: run no `git`; every git package must be locked and cached (v14) |
| `lughac build --emit=tokens\|ast\|ir prog.la` | Print an intermediate stage and stop; with several modules, `tokens` and `ast` print each after a `; module <name>` line (`main` for the main file) |
| `--diagnostics=human\|json` | Diagnostic format for `build`, `run` and `check`; `human` is the default |
| `-O0` / `-O2` | Optimization level; `-O0` is the default |

Exit codes: 0 on success, 1 when the program has errors, 2 for bad command-line usage or an internal compiler error.

**Diagnostics.** Errors print the file, line and column, the offending source line, a caret under the span with its label, and any secondary labels, in a rustc-like layout rendered by the `codespan-reporting` crate. Each diagnostic is followed by a blank line, and lines carry no trailing whitespace. The checker reports as many errors as it can find instead of stopping at the first one.

Internal errors — an unreadable file, a failed link — have no code and exit with 2. They print as `error: <message>`, with a source snippet when they have a location. A crash inside the compiler itself is reported the same way, as `error: internal compiler error: <message>` with a note asking for a bug report, and also exits with 2.

**Error codes.** Every diagnostic has a stable code, grouped by stage. A code is never reused or renumbered once released; its message wording may improve.

| Range | Stage |
| --- | --- |
| E01xx | Lexing: bad characters, unterminated strings, invalid escapes, out-of-range literals |
| E02xx | Parsing: unexpected tokens, missing delimiters |
| E03xx | Names and scopes: undefined or duplicate names, recursive structs |
| E04xx | Types: mismatches, invalid operators, bad casts, wrong argument counts |
| E05xx | Mutability and control flow: assignment to immutable bindings, missing returns, `break` outside a loop |
| W01xx | Warnings: unreachable code |

Name and type errors, reported by the checker:

| Code | Error | Example |
| --- | --- | --- |
| E0301 | Undefined name | `count + 1` with no `count` in scope; a call to an undefined function |
| E0302 | Name defined more than once, or a function named like an intrinsic | two `fun area`; `fun println()`; `import vec { add };` in a file declaring `add` (v11) |
| E0303 | No `main` function | |
| E0304 | `main` with the wrong signature, generic, or outside the main file | `fun main(argc: i32): i32`; `fun main<T>()`; a `main` in a module (v2) |
| E0305 | Unknown type name | `let p: Point = 1;` with no `Point` |
| E0306 | Reserved extern name, or one C function declared with two signatures | `extern fun lugha_rt_alloc(size: i64): i64;` (§8) |
| E0307 | Struct contains itself | `struct A { b: B }` with `struct B { a: A }` — hold it in an array instead (`A[]`) |
| E0308 | Field declared twice | `struct P { x: i64, x: f64 }` |
| E0309 | Variant declared twice | `enum E { A, B, A }` |
| E0310 | Method declared twice, or named like a field or variant | `impl P { fun x(self) {} }` where `P` has a field `x` |
| E0311 | A local named like a type or an imported module | `let Shape = 3;`, `fun f(Point: i64)` |
| E0312 | Module not found | `import shapes.cirlce;` with no `shapes/cirlce.la` (v2); `import std.maths;` (v15) |
| E0313 | An item, method or field not visible here | another module's `private` item: `range.helper()`, `r.lo`, `range.Range { lo: 1, hi: 5 }` (v2); another package's item without `public` (v8); `shapes.Vec2` when `shapes` imported `Vec2` without `public` (v11); `shapes.vec.helper()` through a re-exported module when `helper` isn't `public` (v14) |
| E0314 | Import clash | `import a.util;` and `import b.util;` in one file; an import named like an item (v2); `import vec { add, add };` (v11); `import a as u; import b as u;` (v14) |
| E0315 | Unknown bound | `fun f<T: Hash>(x: T)`; a struct as a bound (v2) |
| E0316 | Bad type parameter name | `fun f<T, T>()`, a parameter named like a struct (v2) |
| E0317 | Retired in v8 (was: `public` before packages existed); never reported, never reused | |
| E0318 | A declaration exposing a less visible type | `private struct S {}` then `fun make(): S` (v2); `public fun a(): Inner` where `Inner` isn't `public` (v8); `public import util { helper };` where `helper` isn't `public` (v11) |
| E0319 | The main file imported | a module writing `import main;` (v2) |
| E0320 | A package name that is also a local folder or file (v8) | `geo` in `lugha.toml` while `geo/` sits next to `main.la`; `import std.math;` beside a `std.la` (v15) |
| E0321 | A malformed `lugha.toml` (v8, v11; `name`/`source`, v15) or `lugha.lock` (v14) | `geo = libs/geo` (no quotes); `[deps]`; `geo` listed twice; `std = "lib"` or `name = "std"` (v15); `{ git = "ssh://h/g", version = "1" }`; `{ git = "https://me:pw@h/g", version = "1" }`; `git` without `version`; a lock commit that isn't 40 hex digits |
| E0322 | A manifest path that doesn't exist (v8) | `geo = "libs/goe"` with no such folder |
| E0323 | One package name for two folders (v8) | two manifests list `mathx` with different folders |
| E0325 | A git package that can't be fetched (v14) | no `git`; a URL that fails; no tag meeting `version`; `--offline` with the package not locked and cached |
| E0326 | A git package that doesn't match `lugha.lock` (v14) | the locked tag now gives another commit |
| E0327 | A package listed under another name than it declares (v15) | `gw = { path = "../glowworm" }` when glowworm's `[package]` says `name = "glowworm"` |
| E0328 | One file imported as two modules (v15) | `src/main.la` importing `cli` and `tool.cli` when `tool`'s `source` is `src` |
| E0324 | A package version that doesn't meet a requirement, or a requirement on a package without a version (v11) | `geo = { path = "libs/geo", version = "1.2" }` when geo's `lugha.toml` says `version = "2.0.0"`, or nothing |
| E0401 | Numeric literal of the wrong kind for its expected type | `x + 2.5` where `x` is `i32` |
| E0402 | Integer literal out of range for its type | `let b: u8 = 256;`; `x + 300` in a `fun big<T: Num>` called at `u8` (v15) |
| E0403 | Type mismatch | `let n: i32 = ok;` where `ok` is `bool`; `if`/`else` of different types |
| E0404 | Operator applied to invalid operand types | `a < b` on a struct without a `cmp` of the right shape (v15); `true + 1`, `-u` where `u` is `u8`, `u16`, `u32` or `u64`; `a == b` on a `T` without `Eq` (with help to declare it); `s + 1` where `s`'s type has no `add` of the operator shape, `a + a` on a `T` without `Add` or `Num` (v15) |
| E0405 | Wrong number of arguments | `add(1)` for `fun add(a: i64, b: i64)` |
| E0406 | Not a function, or something that can't be a value | `let step = 2; step(1);`; `let a = Circle.area;` (a method), `let p = println;`, an extern (v3) |
| E0407 | `void` used as a value | `let x = log();` where `log` returns nothing |
| E0410 | No such field | `s.size` on a `string`; `(5).len`; `p.z` or `Point { z: 1.0 }` on a `Point` without `z` |
| E0411 | Not indexable or not iterable | `n[0]` where `n` is an `i64`; `for c of 5`; a `for … of` over a type whose `next` isn't `fun next(mut self): Option<T>` (v6) |
| E0412 | Cannot infer an array's element type, `None`'s type, or a type argument | `let xs = [];`, `let x = None;`, `let s = Stack.new();`, `let m = max;` — annotate it: `let xs: i64[] = [];` |
| E0413 | Struct literal missing fields | `Point { x: 1.0 }` |
| E0414 | Struct literal field given twice | `Point { x: 1.0, x: 2.0, y: 0.0 }` |
| E0415 | No such variant or method | `Shape.Circel(1.0)`, a `Square` pattern on a `Shape`, `p.size()`, `xs.push(1)` |
| E0416 | Wrong number of payloads | `Shape.Rect(1.0)`, `Rect(w) => …` |
| E0417 | Non-exhaustive `match` | `match s { Circle(r) => r }` with more variants; an integer `match` without `_` |
| E0418 | Wrong number of type arguments | `Stack<i64, i64>`, a bare `Stack` as a parameter type (v2) |
| E0419 | Type argument doesn't satisfy its bound | `largest([[1]])`: `i64[]` is not `Ord` (v2); `sum(["a"])` with `T: Num`, `math.sqrt(2)` (`i64` isn't `Float`), or a method of an `impl Grid<T: Num>` on a `Grid<string>` (v15) |
| E0420 | Generic instantiation too deep | `fun f<T>(x: T) { f([x]); }`; `struct S<T> { xs: S<T[]>[] }` (v2) |
| E0421 | A method no bound declares | `s.perimeter()` in `fun f<T: Shape>(s: T)` (v2) |
| E0422 | A type argument, or a converted value, doesn't satisfy an interface | `total_area([1, 2])`: `i64` has no method `area` (v2); `let s: Shape = 1;` (v5); a `Named` value where a `Pet` is expected, without `sound` (v6) |
| E0423 | Two bounds declare one item differently | `T: Sized + Measured` where both declare `size` with different types (v2) |
| E0424 | `?` where it can't return | `?` on an `i64`; in `main`; on an `Option` in a function returning `Result`; error types that neither match nor convert (v4, v5) |
| E0425 | An interface with an associated function used as a type | `fun f(x: Factory)` where `Factory` declares `fun make(): Factory;` (v5) |
| E0426 | A type pattern that can't apply | `c: Circle` on an `i64`; `s: Shape` (an interface); `d: Dot` when `Dot` doesn't satisfy the interface (v5) |
| E0427 | An interface that uses `Self` used as a type | `let xs: Ordered[] = …;` where `Ordered` declares `fun less(self, other: Self): bool;` (v6) |
| E0429 | A `let … else` whose `else` block can finish (v9) | `let Some(n) = o else { println("no"); };` |
| E0433 | A pattern that must match every value but might not (v14) | `let (1, x) = t;`, `let Some(n) = o;` without `else`, `for (Some(a), b) of …` |
| E0428 | Or-pattern alternatives that bind differently, or `mut` on a variant | `Some(x) \| None`; `Count(n) \| Circle(n)` with `n` an `i64` and an `f64`; `mut None` (v9) |
| E0430 | A default method that two visible interfaces supply (v10) | `c.describe()` where `Named` and `Shown` both default `describe` and `Circle` satisfies both |
| E0431 | A bad format specifier (v13) | `"{n:.2}"` with `n: i64`; `"{x:q}"`; `"{f:x}"` with `f: f64`; `"{n:#}"`; `"{s:05}"` with `s: string`; `"{n:2000}"` |
| E0432 | A bad `format` template (v14) | `format(t, 1)` with `t` a variable; `format("{} and {}", 1)`; `format("{x}", 1)`; `format("a {")` |
| E0409 | Type not allowed in an extern signature | `extern fun getenv(key: string): string;` — no `string` returns; no array, struct, enum, `Option`, function or interface parameters or returns (§8) |
| E0408 | Invalid cast | `ready as i32` where `ready` is `bool`; `x as bool`; `n as T` for a `T` without `Num` or `Float` (v15) |
| E0501 | Assignment to an immutable binding | `let total = 0; total = 5;`; assigning to a parameter or loop variable; `xs[0] = 1;` where `xs` isn't `let mut`; `for x of mut xs` where `xs` isn't (v13) |
| E0502 | Assignment to something that isn't a place | `(a + b) = 3;`; `for x of mut [1, 2]` (v13) |
| E0503 | Missing return | a non-void function whose body can end without a value (§6); a stray `;` after the result gets a "remove this semicolon" label |
| E0504 | `break` or `continue` outside a loop | |
| E0505 | Block-like statement with a discarded value | `if big { 100 } else { 1 }` followed by more statements |
| E0506 | Assignment into a string | `s[0] = 104;`, `s.len = 2;`, `for b of mut s` (v13) — strings are immutable (§4) |
| E0507 | Assignment to the array being iterated | `for x of xs { xs[0] = x; }`, or a `mut self` call or `swap` on it; `it.next()` inside `for n of mut it` (v13) |
| E0508 | Argument aliases a `mut self` receiver | `v.add(v)` where `add` takes `mut self`; `v.add(v.inner)`, `g.set(if c { g.vals } else { h })` (v15) |
| E0509 | A place swapped with a part of itself | `swap(n, n.kids[0])` |
| E0510 | A statement changes what contains the place it writes | `xs[0] = { xs.push(2); 9 };`; for a `mut self` receiver only across an index, `gs[0].f(gs.clear())` (§5) |
| E0511 | Changing a variable a closure captured | `let inc = fun() { count += 1; };` (v3) |
| W0101 | Unreachable code (warning) | statements after `return`, `break` or `continue`; reported once per block |
| W0102 | Unreachable `match` arm (warning) | an arm after `_`, or a variant already covered |
| W0103 | Unused `match` binding (warning) | `Some(Emtpy) => 0`, a misspelled variant that became a binding |

**JSON diagnostics.** With `--diagnostics=json`, the compiler writes one JSON object per line to stderr, one per diagnostic, and nothing else. Lines and columns are 1-based; columns and offsets count UTF-8 bytes. `label` is the text shown under the primary span, or `null`. `labels` holds secondary spans, and `help` is an optional suggestion. Internal errors have `"code":null`, and `"span":null` when they have no location. `"file"` is the file of the primary span: the main file as given on the command line, or a module's path relative to the main file's directory (`shapes/circle.la`); lines, columns and offsets are within that file. A label in a different file carries its own `"file"` key, which appears only then.

```json
{"severity":"error","code":"E0401","message":"float literal where i32 expected","file":"main.la","span":{"start":{"line":3,"col":17,"offset":49},"end":{"line":3,"col":20,"offset":52}},"label":"expected i32","labels":[{"span":{"start":{"line":3,"col":13,"offset":45},"end":{"line":3,"col":14,"offset":46}},"message":"this operand is i32"}],"help":null}
```

Both formats come from the same internal diagnostic records, so they always report the same errors.

**LLM-ready spec.** No model has seen Lugha in training, so the compiler ships a single Markdown file containing this spec and the section 10 programs, compact enough to paste into a prompt. `lughac spec` prints the copy matching the installed compiler, so an assistant or harness always gets the rules for the version it is targeting.

## 10. Example programs

These programs are the v0 acceptance tests: a conforming compiler builds and runs each one, and its stdout and exit code must match exactly. Every expected output ends with a newline.

**Hello world** (milestone 4). Prints `Hello, world!`, exits with 0.

```
fun main() {
    println("Hello, world!");
}
```

**Recursion** (milestone 4). Prints `832040`, exits with 0.

```
fun fib(n: i64): i64 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    println(fib(30));
    0
}
```

**Arrays, repeat literals and mutability** (milestone 5). Prints `25`, exits with 0. `is_composite` needs `mut` because its elements are assigned, just as `count` and `j` need it to be reassigned.

```
fun count_primes(limit: i64): i64 {
    let mut is_composite = [false; limit + 1];
    let mut count = 0;
    for i in 2..limit + 1 {
        if !is_composite[i] {
            count += 1;
            let mut j = i * i;
            while j <= limit {
                is_composite[j] = true;
                j += i;
            }
        }
    }
    count
}

fun main() {
    println(count_primes(100));
}
```

**Structs, for-of and casts** (milestone 5). Prints `centroid: 2.0, 1.0`, exits with 0.

```
struct Point { x: f64, y: f64 }

fun centroid(pts: Point[]): Point {
    let mut sx = 0.0;
    let mut sy = 0.0;
    for p of pts {
        sx += p.x;
        sy += p.y;
    }
    let n = pts.len as f64;
    Point { x: sx / n, y: sy / n }
}

fun main() {
    let pts = [
        Point { x: 0.0, y: 0.0 },
        Point { x: 4.0, y: 0.0 },
        Point { x: 2.0, y: 3.0 },
    ];
    let c = centroid(pts);
    println("centroid: " + to_string(c.x) + ", " + to_string(c.y));
}
```

**Calling C** (milestone 4). Prints `hello from libc` then `1.4142135623730951` on two lines, exits with 0.

```
extern fun puts(s: string): i32;
extern fun sqrt(x: f64): f64;

fun main() {
    puts("hello from libc");
    println(sqrt(2.0));
}
```

**Enums and match** (milestone 6). Prints `12.56636` then `6.0` then `0.0` on three lines, exits with 0.

```
enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Empty,
}

fun area(s: Shape): f64 = match s {
    Circle(r) => 3.14159 * r * r,
    Rect(w, h) => w * h,
    Empty => 0.0,
};

fun main() {
    let shapes = [Shape.Circle(2.0), Shape.Rect(2.0, 3.0), Shape.Empty];
    for s of shapes {
        println(area(s));
    }
}
```

**A linked list** (milestone 6). Prints `6`, exits with 0.

```
enum List {
    Nil,
    Cons(i64, List),
}

fun sum(l: List): i64 = match l {
    Nil => 0,
    Cons(x, rest) => x + sum(rest),
};

fun main() {
    let l = List.Cons(1, List.Cons(2, List.Cons(3, List.Nil)));
    println(sum(l));
}
```

**Methods** (milestone 7). Prints `5.0` then `6.0` on two lines, exits with 0.

```
struct Vec2 { x: f64, y: f64 }

impl Vec2 {
    fun new(x: f64, y: f64): Vec2 = Vec2 { x: x, y: y };
    fun len_sq(self): f64 = self.x * self.x + self.y * self.y;
    fun scale(mut self, k: f64) {
        self.x *= k;
        self.y *= k;
    }
}

fun main() {
    let mut v = Vec2.new(1.0, 2.0);
    println(v.len_sq());
    v.scale(2.0);
    println(v.x + v.y);
}
```

**A stack** (milestone 8). Prints `3` then `2` then `empty` on three lines, exits with 0.

```
fun main() {
    let mut stack: i64[] = [];
    stack.push(1);
    stack.push(2);
    stack.push(3);
    match stack.pop() {
        Some(x) => println(x),
        None => println("empty"),
    }
    println(stack.len);
    stack.clear();
    match stack.pop() {
        Some(x) => println(x),
        None => println("empty"),
    }
}
```

**A rejected enum program** (milestone 6). The checker reports the missing variant, and `lughac` exits with 1:

```
enum Shape { Circle(f64), Empty }

fun area(s: Shape): f64 = match s {
    Circle(r) => r * r,
};

fun main() {}
```

```
error[E0417]: non-exhaustive match: `Empty` not covered
  --> main.la:3:27
  |
3 | fun area(s: Shape): f64 = match s {
  |                           ^^^^^^^
  |
  = help: add the missing arms, or `_ => …`
```

**A rejected program** (milestone 3). The checker must report this error with its location, and `lughac` exits with 1:

```
fun main() {
    let x: i32 = 5;
    let y = x + 2.5;
}
```

```
error[E0401]: float literal where i32 expected
  --> main.la:3:17
  |
3 |     let y = x + 2.5;
  |             -   ^^^ expected i32
  |             |
  |             this operand is i32
```

With `--diagnostics=json`, the same error is the JSON line shown in section 9.

**Patterns and equality** (milestone 9). Prints `big circle` then `square` then `nothing` then `true` then `true` then `true` on six lines, exits with 0.

```
enum Shape { Circle(f64), Rect(f64, f64), Empty }

fun describe(s: Option<Shape>): string = match s {
    Some(Circle(r)) if r > 1.0 => "big circle",
    Some(Circle(_)) => "small circle",
    Some(Rect(w, h)) if w == h => "square",
    Some(Rect(_, _)) => "rect",
    Some(Empty) => "empty",
    None => "nothing",
};

fun main() {
    println(describe(Some(Shape.Circle(2.0))));
    println(describe(Some(Shape.Rect(3.0, 3.0))));
    println(describe(None));
    println([1, 2] == [1, 2]);
    println(Shape.Rect(1.0, 2.0) == Shape.Rect(1.0, 2.0));
    println("apple" < "banana");
}
```

**Two modules** (milestone 10). Prints `3.5`, exits with 0. The two files sit in one directory, and `lughac run main.la` builds both.

```
// geometry.la
struct Vec2 { x: f64, y: f64 }

impl Vec2 {
    fun add(self, o: Vec2): Vec2 = Vec2 { x: self.x + o.x, y: self.y + o.y };
}

fun origin(): Vec2 = Vec2 { x: 0.0, y: 0.0 };
```

```
// main.la
import geometry;

fun main() {
    let mut p = geometry.origin();
    p = p.add(geometry.Vec2 { x: 1.5, y: 2.0 });
    println(p.x + p.y);
}
```

**Generics** (milestone 11). Prints `b` then `true` then `plum` on three lines, exits with 0.

```
struct Stack<T> { items: T[] }

impl Stack<T> {
    fun new(): Stack<T> = Stack { items: [] };
    fun push(mut self, x: T) { self.items.push(x); }
    fun pop(mut self): Option<T> = self.items.pop();
}

fun contains<T: Eq>(xs: T[], x: T): bool {
    for y of xs {
        if y == x { return true; }
    }
    false
}

fun largest<T: Ord>(xs: T[]): Option<T> {
    if xs.len == 0 { return None; }
    let mut best = xs[0];
    for x of xs {
        if x > best { best = x; }
    }
    Some(best)
}

fun main() {
    let mut s: Stack<string> = Stack.new();
    s.push("a");
    s.push("b");
    match s.pop() {
        Some(x) => println(x),
        None => println("empty"),
    }
    println(contains([1, 2, 3], 2));
    match largest(["pear", "apple", "plum"]) {
        Some(w) => println(w),
        None => println("none"),
    }
}
```

**Interfaces** (milestone 12). Prints `15.0` then `9.0` then `square` on three lines, exits with 0.

```
interface Shape {
    fun area(self): f64;
    fun name(self): string;
}

struct Circle { r: f64 }

impl Circle {
    fun area(self): f64 = 3.0 * self.r * self.r;
    fun name(self): string = "circle";
}

struct Square { side: f64 }

impl Square {
    fun area(self): f64 = self.side * self.side;
    fun name(self): string = "square";
}

fun total_area<T: Shape>(shapes: T[]): f64 {
    let mut sum = 0.0;
    for s of shapes {
        sum += s.area();
    }
    sum
}

fun describe<T: Shape>(s: T): string = s.name();

fun main() {
    println(total_area([Circle { r: 1.0 }, Circle { r: 2.0 }]));
    println(total_area([Square { side: 3.0 }]));
    println(describe(Square { side: 1.0 }));
}
```

**Arguments and interpolation** (milestone 13). Run as `prog 21`. Prints `21 doubled is 42`, exits with 0.

```
fun main() {
    let a = args();
    if a.len < 2 {
        println("usage: prog <n>");
        return;
    }
    match parse_i64(a[1]) {
        Some(n) => println("{n} doubled is {n * 2}"),
        None => println("usage: prog <n>"),
    }
}
```

**Functions as values** (milestone 14). Prints `6` then `9` on two lines, exits with 0.

```
fun double(x: i64): i64 = x * 2;
fun triple(x: i64): i64 = x * 3;

fun apply(f: fun(i64): i64, x: i64): i64 = f(x);

fun main() {
    let fs = [double, triple];
    println(apply(fs[0], 3));
    println(apply(fs[1], 3));
}
```

**Closures** (milestone 15). Prints `13` then `1` then `20` on three lines, exits with 0.

```
fun adder(k: i64): fun(i64): i64 = fun(x) = x + k;

fun main() {
    let add10 = adder(10);
    println(add10(3));
    let mut count = 0;
    let show = fun(): i64 = count + 1;
    count = 5;
    println(show());
    let twice = fun(f: fun(i64): i64, x: i64): i64 = f(f(x));
    println(twice(fun(y) = y + 5, 10));
}
```

**Array helpers** (milestone 16). Prints `ann, cy` then `61` then `cy` on three lines, exits with 0.

```
struct Person { name: string, age: i64 }

fun main() {
    let people = [Person { name: "ann", age: 30 }, Person { name: "bo", age: 12 }, Person { name: "cy", age: 31 }];
    let adults = people.filter(fun(p) = p.age >= 18);
    let names = adults.map(fun(p) = p.name);
    println(names.fold("", fun(acc, n) = if acc == "" { n } else { "{acc}, {n}" }));
    println(adults.fold(0, fun(sum, p) = sum + p.age));
    println(people.sort_by(fun(a, b) = a.age > b.age)[0].name);
}
```

**Results and `?`** (milestone 17). Prints `3` then `3 is odd` on two lines, exits with 0.

```
fun half(n: i64): Result<i64, string> = if n % 2 == 0 { Ok(n / 2) } else { Err("{n} is odd") };

fun quarter(n: i64): Result<i64, string> {
    let h = half(n)?;
    half(h)
}

fun main() {
    match quarter(12) {
        Ok(q) => println(q),
        Err(e) => println(e),
    }
    match quarter(6) {
        Ok(q) => println(q),
        Err(e) => println(e),
    }
}
```

**Text helpers** (milestone 18). Prints `42` then `a-b-c` then `2` on three lines, exits with 0.

```
fun main() {
    let text = "name, age\nann, 30\n\nbo, 12\n";
    let mut total = 0;
    for line of text.lines() {
        let parts = line.split(",").map(fun(p) = p.trim());
        if parts.len < 2 || parts[0] == "name" {
            continue;
        }
        total += parse_i64(parts[1]).unwrap_or(0);
    }
    println(total);
    println(["a", "b", "c"].join("-"));
    println("  hi  ".trim().len);
}
```

**Reading input** (milestone 19). Run with `5`, `7` and `x` on three lines of stdin. Prints `12` then `skipped 1` on two lines, exits with 0.

```
fun main() {
    let mut total = 0;
    let mut skipped = 0;
    while true {
        match read_line() {
            Some(line) => match parse_i64(line.trim()) {
                Some(n) => { total += n; }
                None => { skipped += 1; }
            },
            None => { break; }
        }
    }
    println(total);
    println("skipped {skipped}");
}
```

**Interface values** (milestone 20). Prints `16.0` then `1.0` then `circle` on three lines, exits with 0.

```
interface Shape {
    fun area(self): f64;
    fun name(self): string;
    fun grow(mut self, k: f64);
}

struct Circle { r: f64 }

impl Circle {
    fun area(self): f64 = 3.0 * self.r * self.r;
    fun name(self): string = "circle";
    fun grow(mut self, k: f64) { self.r *= k; }
}

struct Square { side: f64 }

impl Square {
    fun area(self): f64 = self.side * self.side;
    fun name(self): string = "square";
    fun grow(mut self, k: f64) { self.side *= k; }
}

fun total(shapes: Shape[]): f64 {
    let mut sum = 0.0;
    for s of shapes {
        sum += s.area();
    }
    sum
}

fun main() {
    let c = Circle { r: 1.0 };
    let mut shapes: Shape[] = [c, Square { side: 2.0 }];
    shapes[0].grow(2.0);
    println(total(shapes));
    println(c.r);
    let first = shapes[0];
    println(first.name());
}
```

**Type patterns** (milestone 21). Prints `big circle` then `circle of radius 1.5` then `a square` on three lines, exits with 0.

```
interface Shape {
    fun area(self): f64;
}

struct Circle { r: f64 }

impl Circle {
    fun area(self): f64 = 3.0 * self.r * self.r;
}

struct Square { side: f64 }

impl Square {
    fun area(self): f64 = self.side * self.side;
}

fun describe(s: Shape): string = match s {
    c: Circle if c.r > 10.0 => "big circle",
    c: Circle => "circle of radius {c.r}",
    _: Square => "a square",
    _ => "some shape",
};

fun main() {
    let shapes: Shape[] = [Circle { r: 20.0 }, Circle { r: 1.5 }, Square { side: 2.0 }];
    for s of shapes {
        println(describe(s));
    }
}
```

**Errors as interfaces** (milestone 22). Prints `42` then `error: not a number: x` then `range error: 500 is out of range` on three lines, exits with 0.

```
interface Error {
    fun message(self): string;
    fun report(self): string = "error: {self.message()}";
}

struct ParseError { text: string }

impl ParseError {
    fun message(self): string = "not a number: {self.text}";
}

struct RangeError { n: i64 }

impl RangeError {
    fun message(self): string = "{self.n} is out of range";
    fun report(self): string = "range error: {self.message()}";
}

fun parse(s: string): Result<i64, ParseError> = parse_i64(s).ok_or(ParseError { text: s });

fun check(n: i64): Result<i64, RangeError> =
    if n < 100 { Ok(n) } else { Err(RangeError { n: n }) };

fun read(s: string): Result<i64, Error> {
    let n = parse(s)?;
    let m = check(n)?;
    Ok(m * 2)
}

fun main() {
    for s of ["21", "x", "500"] {
        match read(s) {
            Ok(n) => println(n),
            Err(e) => println(e.report()),
        }
    }
}
```

**Generic interfaces** (milestone 23). Prints `3` then `a-b` then `11` on three lines, exits with 0.

```
interface Source<T> {
    fun next(mut self): Option<T>;
}

struct Count { n: i64, end: i64 }

impl Count {
    fun next(mut self): Option<i64> {
        if self.n == self.end {
            return None;
        }
        self.n += 1;
        Some(self.n)
    }
}

struct Words { xs: string[], at: i64 }

impl Words {
    fun next(mut self): Option<string> {
        if self.at == self.xs.len {
            return None;
        }
        self.at += 1;
        Some(self.xs[self.at - 1])
    }
}

fun collect<T, S: Source<T>>(src: S): T[] {
    let mut s = src;
    let mut out: T[] = [];
    let mut more = true;
    while more {
        match s.next() {
            Some(x) => { out.push(x); }
            None => { more = false; }
        }
    }
    out
}

fun main() {
    println(collect(Count { n: 0, end: 3 }).len);
    println(collect(Words { xs: ["a", "b"], at: 0 }).join("-"));
    let mut s: Source<i64> = Count { n: 10, end: 12 };
    println(s.next().unwrap_or(0));
}
```

**Iterators** (milestone 24). Prints `1`, `2`, `3`, `0`, `88` on five lines, exits with 0.

```
struct Count { n: i64, end: i64 }

impl Count {
    fun next(mut self): Option<i64> {
        if self.n == self.end {
            return None;
        }
        self.n += 1;
        Some(self.n)
    }
}

struct Fib { a: i64, b: i64, left: i64 }

impl Fib {
    fun next(mut self): Option<i64> {
        if self.left == 0 {
            return None;
        }
        self.left -= 1;
        let a = self.a;
        self.a = self.b;
        self.b += a;
        Some(a)
    }
}

fun total(it: Iter<i64>): i64 {
    let mut sum = 0;
    for n of it {
        sum += n;
    }
    sum
}

fun main() {
    let c = Count { n: 0, end: 3 };
    for n of c {
        println(n);
    }
    println(c.n);
    println(total(Fib { a: 0, b: 1, left: 10 }));
}
```

**`Self`** (milestone 25). Prints `1999` then `2.0` on two lines, exits with 0.

```
interface Ordered {
    fun less(self, other: Self): bool;
    fun max(self, other: Self): Self = if self.less(other) { other } else { self };
}

struct Money { cents: i64 }

impl Money {
    fun less(self, other: Money): bool = self.cents < other.cents;
}

struct Version { major: i64, minor: i64 }

impl Version {
    fun less(self, other: Version): bool =
        self.major < other.major || (self.major == other.major && self.minor < other.minor);
}

fun largest<T: Ordered>(xs: T[]): T {
    let mut best = xs[0];
    for x of xs {
        best = best.max(x);
    }
    best
}

fun main() {
    println(largest([Money { cents: 250 }, Money { cents: 1999 }, Money { cents: 5 }]).cents);
    let v = largest([Version { major: 1, minor: 9 }, Version { major: 2, minor: 0 }, Version { major: 1, minor: 10 }]);
    println("{v.major}.{v.minor}");
}
```

**Converting between interfaces** (milestone 26). Prints `good dog, rex` then `woof` then `hello, tom` then `meow` on four lines, exits with 0.

```
interface Named {
    fun name(self): string;
}

interface Pet {
    fun name(self): string;
    fun sound(self): string;
}

struct Dog { called: string }

impl Dog {
    fun name(self): string = self.called;
    fun sound(self): string = "woof";
}

struct Cat { called: string }

impl Cat {
    fun name(self): string = self.called;
    fun sound(self): string = "meow";
}

fun greet(n: Named): string = match n {
    d: Dog => "good dog, {d.called}",
    _ => "hello, {n.name()}",
};

fun main() {
    let pets: Pet[] = [Dog { called: "rex" }, Cat { called: "tom" }];
    for p of pets {
        println(greet(p));
        println(p.sound());
    }
}
```

**More numbers** (milestone 27). Prints `18446744073709551615` then `4000000001` then `0.1` then `true` then `4294967295` on five lines, exits with 0.

```
fun main() {
    let big: u64 = 18446744073709551615;
    println(big);
    let n: u32 = 4000000000;
    println(n + 1);
    let x: f32 = 0.1;
    println(x);
    println(x as f64 > 0.1);
    println(-1 as u32);
}
```

**Bits** (milestone 28). Prints `true`, `11`, `5`, `5`, `1024`, `-4`, `65280` on seven lines, exits with 0.

```
fun main() {
    let flags: u32 = 0b1010;
    println(flags & 0b0010 == 0b0010);
    println(flags | 1);
    println(flags ^ 0xf);
    println(~flags & 0xf);
    println(1 << 10);
    println(-8 >> 1);
    let mut m: u64 = 0xff;
    m <<= 8;
    println(m);
}
```

**Hashing** (milestone 29). Prints `12638187200555641996`, `14695981039346656037`, `3`, `3`, `25`, `0` on six lines, exits with 0.

```
fun hash(s: string): u64 {
    let mut h: u64 = 14695981039346656037;
    for i in 0..s.len {
        h = (h ^ (s[i] as u64)).wrapping_mul(1099511628211);
    }
    h
}

fun main() {
    println(hash("a"));
    println(hash(""));
    let x: u32 = 0b1011000;
    println(x.count_ones());
    println(x.trailing_zeros());
    println(x.leading_zeros());
    let m: u8 = 255;
    println(m.wrapping_add(1));
}
```

**Packages** (milestone 30). Prints `12.0`, exits with 0. The files sit as their first lines say, and `lughac run main.la` builds them all.

```
# lugha.toml
[packages]
geo = "libs/geo"
```

```
// libs/geo/shapes.la
public struct Circle { r: f64 }

impl Circle {
    public fun area(self): f64 = 3.0 * self.r * self.r;
    fun diameter(self): f64 = 2.0 * self.r;
}

public fun circle(r: f64): Circle = Circle { r: r };
```

```
// main.la
import geo.shapes;

fun main() {
    let c = shapes.circle(2.0);
    println(c.area());
}
```

In the same layout, a `main.la` that calls `c.diameter()` or writes `shapes.Circle { r: 1.0 }` is rejected with E0313: `diameter` and the field `r` have no `public`, so only package `geo` may use them.

**Or-patterns** (milestone 31). Prints `3.0`, `3.0`, `stop`, `run`, `42` on five lines, exits with 0.

```
enum Shape { Circle(f64), Square(f64), Rect(f64, f64) }

fun size(s: Shape): f64 = match s {
    Circle(r) | Square(r) => r * 2.0,
    Rect(w, h) => w + h,
};

fun kind(cmd: string): string = match cmd {
    "q" | "quit" | "exit" => "stop",
    "h" | "help" => "help",
    _ => "run",
};

fun main() {
    println(size(Shape.Square(1.5)));
    println(size(Shape.Rect(1.0, 2.0)));
    println(kind("quit"));
    println(kind("ls"));
    match Some(41) {
        Some(mut n) => {
            n += 1;
            println(n);
        }
        None => {}
    }
}
```

**Unwrapping** (milestone 32). Prints `9` then `5` on two lines, exits with 101 (the last `expect` panics with its message).

```
fun main() {
    let n = parse_i64("42").unwrap();
    let m = parse_i64("7").expect("need a number");
    let mut x = n + m;
    x %= 10;
    println(x);
    let r: Result<i64, string> = Ok(5);
    println(r.unwrap());
    println(parse_i64("x").expect("not a number"));
}
```

**Conditional patterns** (milestone 33). Prints `skipped x`, `7`, `321`, `42`, `-1` on five lines, exits with 0.

```
fun double(s: string): i64 {
    let Some(n) = parse_i64(s) else {
        return -1;
    };
    n * 2
}

fun main() {
    let mut total = 0;
    for w of ["3", "x", "4"] {
        if let Some(n) = parse_i64(w) {
            total += n;
        } else {
            println("skipped {w}");
        }
    }
    println(total);
    let mut stack = [1, 2, 3];
    while let Some(top) = stack.pop() {
        print(top);
    }
    println("");
    println(double("21"));
    println(double("x"));
}
```

**`Self` in `impl` blocks** (milestone 34). Prints `(3, 4)`, `this is (3, 4)`, `green` on three lines, exits with 0.

```
interface Describe {
    fun name(self): string;
    fun describe(self): string = "this is {self.name()}";
}

struct Point { x: i64, y: i64 }

impl Point {
    fun origin(): Self = Self { x: 0, y: 0 };
    fun plus(self, other: Self): Self = Self { x: self.x + other.x, y: self.y + other.y };
    fun unit(): Self = Self.origin().plus(Self { x: 1, y: 1 });
    fun name(self): string = "({self.x}, {self.y})";
}

enum Light { Red, Green }

impl Light {
    fun toggle(self): Self = match self {
        Red => Self.Green,
        Green => Self.Red,
    };
}

fun main() {
    let p = Point.unit().plus(Point { x: 2, y: 3 });
    println(p.name());
    println(p.describe());
    match Light.Red.toggle() {
        Green => println("green"),
        Red => println("red"),
    }
}
```

**More integers** (milestone 35). Prints `32767`, `-1`, `32767`, `255`, `3`, `true`, `18446744073709551615` on seven lines, exits with 0.

```
fun main() {
    let a: i16 = i16.MAX;
    println(a);
    println(a.checked_add(1).unwrap_or(-1));
    println(a.saturating_add(100));
    let b: u8 = 200;
    println(b.saturating_mul(2));
    let c: u16 = 0b1000000000000001;
    println(c.rotate_left(1));
    let d: i8 = i8.MIN;
    println(d.checked_div(-1).is_none());
    println(parse_u64("18446744073709551615").unwrap());
}
```

**Iterator helpers** (milestone 36). Prints `16,4,36`, `5050`, `63`, `0=104;1=101;2=121;`, `3`, `true`, `5`, `31` on eight lines, exits with 0.

```
fun even(n: i64): bool = n % 2 == 0;

fun main() {
    let xs = [3, 1, 4, 1, 5, 9, 2, 6];
    let words = xs.iter().filter(even).map(fun(n) = "{n * n}").collect();
    println(words.join(","));
    println(range(1, 101).sum());
    println(range(10, 1000000000).filter(fun(n) = n % 7 == 0).take(3).sum());
    for p of "hey".bytes().enumerate() {
        print("{p.i}={p.value};");
    }
    println("");
    let mut vowels = 0;
    for b of "banana" {
        if b == 97 {
            vowels += 1;
        }
    }
    println(vowels);
    println(xs.iter().any(fun(n) = n > 8));
    println(xs.iter().find(fun(n) = n > 4).unwrap());
    println(xs.iter().fold(0, fun(a, n) = a + n));
}
```

**Package versions** (milestone 37). Prints `12`, exits with 0. If geo's manifest said `version = "2.0.0"`, or declared no version, the program would be rejected with E0324 at the requirement.

```
# lugha.toml
[packages]
geo = { path = "libs/geo", version = "1.2" }
```

```
# libs/geo/lugha.toml
[package]
version = "1.4.0"
```

```
// libs/geo/shapes.la
public fun area(w: i64, h: i64): i64 = w * h;
```

```
// main.la
import geo.shapes;

fun main() {
    println(shapes.area(3, 4));
}
```

**Re-exports** (milestone 38). Prints `8`, exits with 0. The program lists only `geo`, and uses `mathx`'s `Vec2` and `add` through `geo.shapes`.

```
# lugha.toml
[packages]
geo = "libs/geo"
```

```
# libs/geo/lugha.toml
[packages]
mathx = "../mathx"
```

```
// libs/mathx/vec.la
public struct Vec2 { public x: i64, public y: i64 }

public fun add(a: Vec2, b: Vec2): Vec2 = Vec2 { x: a.x + b.x, y: a.y + b.y };
```

```
// libs/geo/shapes.la
public import mathx.vec { Vec2, add };

public fun centre(a: Vec2, b: Vec2): Vec2 = add(a, b);
```

```
// main.la
import geo.shapes;

fun main() {
    let c = shapes.centre(shapes.Vec2 { x: 1, y: 2 }, shapes.Vec2 { x: 3, y: 4 });
    println(shapes.add(c, c).x);
}
```

**Printing values** (milestone 39). Prints `Point { x: 1, y: 2 }`, `Circle(1.5)`, `[Some(1), None]`, `Team { name: "red", members: ["ann", "bo"] }`, `$5.25`, `wallet: Wallet { cash: $5.25 }`, `Stack { items: [3] }` on seven lines, exits with 0.

```
struct Point { x: i64, y: i64 }

enum Shape { Circle(f64), Empty }

struct Team { name: string, members: string[] }

struct Money { cents: i64 }

impl Money {
    fun show(self): string = "${self.cents / 100}.{self.cents % 100}";
}

struct Wallet { cash: Money }

struct Stack<T> { items: T[] }

fun dump<T>(x: T) {
    println(x);
}

fun main() {
    println(Point { x: 1, y: 2 });
    println(Shape.Circle(1.5));
    println([Some(1), None]);
    println(Team { name: "red", members: ["ann", "bo"] });
    let m = Money { cents: 525 };
    println(m);
    println("wallet: {Wallet { cash: m }}");
    dump(Stack { items: [3] });
}
```

**Printing anything** (milestone 40). Prints `Circle { r: 2.0 }`, `square 3`, `<fun>` on three lines, exits with 101 (the last line panics, with the message unwrap on Err: Bad { code: 7 }).

```
interface Shape { fun area(self): f64; }

struct Circle { r: f64 }
impl Circle { fun area(self): f64 = 3.0 * self.r * self.r; }

struct Square { side: i64 }
impl Square {
    fun area(self): f64 = (self.side * self.side) as f64;
    fun show(self): string = "square {self.side}";
}

struct Bad { code: i64 }

fun main() {
    let shapes: Shape[] = [Circle { r: 2.0 }, Square { side: 3 }];
    for s of shapes {
        println(s);
    }
    println(fun(x: i64): i64 = x);
    let r: Result<i64, Bad> = Err(Bad { code: 7 });
    println(r.unwrap());
}
```

**`Self` in declarations** (milestone 41). Prints `3` then `Cons(1, Cons(2, Nil))` on two lines, exits with 0.

```
struct Tree<T> { value: T, kids: Self[] }

impl Tree<T> {
    fun size(self): i64 {
        let mut n = 1;
        for k of self.kids {
            n += k.size();
        }
        n
    }
}

enum List { Cons(i64, Self), Nil }

fun main() {
    let t = Tree { value: 1, kids: [Tree { value: 2, kids: [] }, Tree { value: 3, kids: [] }] };
    println(t.size());
    println(List.Cons(1, List.Cons(2, List.Nil)));
}
```

**`for … of mut`** (milestone 42). Prints `[2, 4, 6]`, `a b`, `c d` on three lines, exits with 0.

```
fun main() {
    let mut xs = [1, 2, 3];
    for x of mut xs {
        x *= 2;
    }
    println(xs);
    let mut words = ["a", "b", "--", "c", "d"].iter();
    let mut first = "";
    for w of mut words {
        if w == "--" {
            break;
        }
        first = first + w + " ";
    }
    println(first.trim());
    let mut rest = "";
    for w of mut words {
        rest = rest + w + " ";
    }
    println(rest.trim());
}
```

**More iterator helpers** (milestone 43). Prints `[(1, "a"), (2, "b")]`, `[1, 2, 3, 4]`, `[1, 2]`, `[3, 1]`, `[0, 0, 1, 0, 1, 2]`, `Some(1)`, `Some(9)`, `Some(2)` on eight lines, exits with 0.

```
fun main() {
    let xs = [1, 2, 3, 1, 9];
    println(xs.iter().zip(["a", "b"].iter()).collect());
    println([1, 2].iter().chain([3, 4].iter()).collect());
    println(xs.iter().take_while(fun(n) = n < 3).collect());
    println(xs.iter().skip_while(fun(n) = n < 3).take(2).collect());
    println(range(1, 4).flat_map(fun(n) = range(0, n)).collect());
    println(xs.iter().min());
    println(xs.iter().max());
    println(xs.iter().position(fun(n) = n == 3));
}
```

**Format specifiers** (milestone 44). Prints `3.14`, `|    42|`, `|42    |`, `-0042`, `0xff`, `***ann***`, `11111111` on seven lines, exits with 0.

```
fun main() {
    let pi = 3.14159;
    let n = 42;
    println("{pi:.2}");
    println("|{n:6}|");
    println("|{n:<6}|");
    println("{-42:05}");
    println("{255:#x}");
    println("{"ann":*^9}");
    println("{-1 as i8:b}");
}
```

**Bytes and indexes** (milestone 45). Run in an empty folder. Prints `wrote 4`, `[104, 105, 0, 255]`, `6`, `Ok("héllo")`, `Err("invalid UTF-8 at byte 1")`, `195 169 104` on six lines, exits with 0.

```
fun main() {
    let data: u8[] = [104, 105, 0, 255];
    match write_bytes("out.bin", data) {
        Ok(n) => println("wrote {n}"),
        Err(e) => println("error {e}"),
    }
    match read_bytes("out.bin") {
        Ok(b) => println(b),
        Err(e) => println(e),
    }
    let text = "héllo";
    let raw = text.to_bytes();
    println(raw.len);
    println(string_from_utf8(raw));
    let bad: u8[] = [104, 195, 40];
    println(string_from_utf8(bad));
    let i: u8 = 1;
    let j: u32 = 2;
    let k: i32 = 0;
    println("{raw[i]} {raw[j]} {raw[k]}");
}
```

**Unicode text** (milestone 46). Prints `10 chars, 12 bytes`, `NAÏVE CAFÉ`, `9`, `['é', 'a', '\n']`, `233`, `None` on six lines, exits with 0.

```
fun shout(s: string): string {
    let mut out = "";
    for c of s.chars() {
        out = out + to_string(c.to_upper());
    }
    out
}

fun main() {
    let s = "naïve café";
    println("{s.char_count()} chars, {s.len} bytes");
    println(shout(s));
    println(s.chars().filter(fun(c) = c.is_alphabetic()).count());
    println(['é', 'a', '\n']);
    println('é' as u32);
    println(char_from(55296));
}
```

**Tuples** (milestone 47). Prints `3 2`, `0: ann`, `1: bo`, `(1, "a", 2.5)`, `a`, `big two`, `true`, `Some((1, 9))`, `(1, [2]) (1, [9])`, `true`, `7` on 11 lines, exits with 0.

```
fun divmod(a: i64, b: i64): (i64, i64) = (a / b, a % b);

fun main() {
    let (q, r) = divmod(17, 5);
    println("{q} {r}");
    let names = ["ann", "bo"];
    for (i, w) of range(0, 2).zip(names.iter()) {
        println("{i}: {w}");
    }
    let t = (1, "a", 2.5);
    println(t);
    println(t.1);
    let label = match (q > 2, r) {
        (true, 2) => "big two",
        (true, _) => "big",
        (false, _) => "small",
    };
    println(label);
    println((1, "b") < (1, "c"));
    println([(2, 1), (1, 9)].iter().min());
    let mut p = (1, [2]);
    let copy = p;
    p.1[0] = 9;
    println("{copy} {p}");
    println((1, 2) == (1, 2));
    if let (Some(a), b) = (Some(3), 4) {
        println(a + b);
    }
}
```

**Format** (milestone 48). Prints `box has   12 items`, `crate has  340 items`, `3.14 | 0xff | left  |`, `{literal} and (1, 'a')`, `total: 00352` on five lines, exits with 0.

```
// `format` (v14): a template and the values for its holes.
struct Item { name: string, n: i64 }

fun line(it: Item): string = format("{} has {:>4} items", it.name, it.n);

fun main() {
    let items = [Item { name: "box", n: 12 }, Item { name: "crate", n: 340 }];
    for it of items {
        println(line(it));
    }
    println(format("{:.2} | {:#x} | {:<6}|", 3.14159, 255, "left"));
    println(format("{{literal}} and {}", (1, 'a')));
    let total = items.iter().map(fun(it: Item): i64 = it.n).sum();
    println("total: {format("{:05}", total)}");
}
```

**Import aliases** (milestone 49). Prints `11 22`, `11`, `12`, `27`, `up`, `Vec2 { x: 2, y: 4 }` on six lines, exits with 0. `shapes` re-exports `shapes.vec` whole and `shapes.circle` as `round`.

```
// shapes/vec.la
public struct Vec2 { public x: i64, public y: i64 }

impl Vec2 {
    public fun zero(): Vec2 = Vec2 { x: 0, y: 0 };
}

public fun add(a: Vec2, b: Vec2): Vec2 = Vec2 { x: a.x + b.x, y: a.y + b.y };

public enum Dir { Up, Down }

fun hidden(): i64 = 1;
```

```
// shapes/circle.la
public fun area(r: i64): i64 = r * r * 3;
```

```
// shapes.la
public import shapes.vec;
public import shapes.circle as round;
```

```
// main.la
// Import aliases and whole-module re-exports (v14).
import shapes;
import shapes.circle as c;
import shapes.vec { Vec2 as V, add };

fun main() {
    let a = shapes.vec.Vec2 { x: 1, y: 2 };
    let b: shapes.vec.Vec2 = shapes.vec.add(a, V { x: 10, y: 20 });
    println("{b.x} {b.y}");
    println(add(b, shapes.vec.Vec2.zero()).x);
    println(c.area(2));
    println(shapes.round.area(3));
    let d = shapes.vec.Dir.Up;
    match d {
        Up => println("up"),
        Down => println("down"),
    }
    let f = shapes.vec.add;
    println(f(a, a));
}
```

**Operator overloading** (milestone 52). Prints `Vec2 { x: 4.0, y: 6.0 }`, `Vec2 { x: 3.0, y: 8.0 }`, `15`, `glowworm`, `Vec2 { x: 4.0, y: 6.0 }`, `6`, `0.75` on seven lines, exits with 0.

```
// Operator overloading (v15): `+` calls `add`, `*` calls `mul`, and
// generic code uses `+` through the prelude's `Add` and `Num` bounds.
struct Vec2 { x: f64, y: f64 }

impl Vec2 {
    fun add(self, o: Vec2): Vec2 = Vec2 { x: self.x + o.x, y: self.y + o.y };
    fun mul(self, o: Vec2): Vec2 = Vec2 { x: self.x * o.x, y: self.y * o.y };
}

fun add<T: Add>(a: T, b: T): T = a + b;

fun sum<T: Num>(xs: T[]): T {
    let mut s: T = 0;
    for x of xs {
        s += x;
    }
    s
}

fun main() {
    let a = Vec2 { x: 1.0, y: 2.0 };
    let b = Vec2 { x: 3.0, y: 4.0 };
    println(a + b);
    println(a * b);
    println(add(5, 10));
    println(add("glow", "worm"));
    println(add(a, b));
    println(sum([1, 2, 3]));
    println(sum([0.5, 0.25]));
}
```

**The standard library** (milestone 53). Prints `1.4142135623730951`, `5.0`, `7`, `3`, `3.0`, `3.141592653589793`, `1024.0`, `-2.0`, `10` on nine lines, exits with 0.

```
// The std library (v15): `import std.math;`, with float functions generic
// over `T: Float` and `abs`, `min`, `max` and `clamp` over any number.
import std.math;

fun hypot<T: Float>(a: T, b: T): T = math.sqrt(a * a + b * b);

fun main() {
    println(math.sqrt(2.0));
    println(hypot(3.0, 4.0));
    println(math.max(2, 7));
    println(math.abs(-3));
    println(math.round(2.5));
    println(math.pi());
    println(math.pow(2.0, 10.0));
    println(math.floor(-1.5));
    println(math.clamp(15, 0, 10));
}
```

**Random numbers** (milestone 54). Prints `1546998764402558742`, `0.3789802506626686`, `6`, `13`, `0.3834617912676943`, `false`, `Some("red")`, `[1, 3, 5, 2, 4]` on eight lines, exits with 0. The first two values match the reference xoshiro256\*\* with splitmix64 seeding.

```
// std.random (v15): a seeded generator gives the same numbers on every
// run and platform, which this program's expected output pins down.
import std.random;

fun main() {
    let mut rng = random.seeded(42);
    println(rng.next_u64());
    println(rng.uniform());
    println(rng.below(6) + 1);
    println(rng.between(10, 20));
    println(rng.normal(0.0, 1.0));
    println(rng.chance(0.5));
    println(rng.choice(["red", "green", "blue"]));
    println(rng.shuffle([1, 2, 3, 4, 5]));
}
```

**Copy-on-write** (milestone 55). Prints `[10, 2, 3] [1, 2, 3]`, `[[0, 0], [7, 0], [9]] [[0, 0], [0, 0]]`, `[0.5, 2.5] [0.5, 1.5]`, `[1.0, 3.0] [0.5, 1.5]` on four lines, exits with 0. Every copy keeps its values, exactly as with v14's eager copies.

```
// Copy-on-write (v15): copies share an array until one side writes, and
// every write leaves the other copies as they were.
struct Layer { weights: f64[] }

fun scaled(xs: f64[], k: f64): f64[] {
    let mut ys = xs;
    for y of mut ys { y *= k; }
    ys
}

fun main() {
    let mut a = [1, 2, 3];
    let b = a;
    a[0] = 10;
    println("{a} {b}");

    let mut grid = [[0; 2]; 2];
    let saved = grid;
    grid[1][0] = 7;
    grid.push([9]);
    println("{grid} {saved}");

    let mut net = [Layer { weights: [0.5, 1.5] }];
    let w = net[0].weights;
    net[0].weights[1] += 1.0;
    println("{net[0].weights} {w}");
    println("{scaled(w, 2.0)} {w}");
}
```

**Nested calls** (milestone 57). Prints `24.0`, `5`, `[3.0, 1.0, 4.0, 6.0, 24.0]` on three lines, exits with 0: the same as with one `let` per call.

```
// Nested `mut self` calls (v15): an argument may call methods on the
// receiver, and runs before the receiver is reached. Only an argument that
// could be the receiver's own storage is E0508.
struct Tape { vals: f64[] }

impl Tape {
    fun put(mut self, x: f64): i64 {
        self.vals.push(x);
        self.vals.len - 1
    }
    fun add(mut self, a: i64, b: i64): i64 = self.put(self.vals[a] + self.vals[b]);
    fun mul(mut self, a: i64, b: i64): i64 = self.put(self.vals[a] * self.vals[b]);
}

fun main() {
    let mut t = Tape { vals: [] };
    let x = t.put(3.0);
    let y = t.mul(t.add(x, t.put(1.0)), t.add(x, x));
    println(t.vals[y]);
    println(t.vals.len);
    println(t.vals);
}
```

**Numbers as T** (milestone 58). Prints `3`, `3.0`, `13`, `1.5`, `3` on five lines, exits with 0.

```
// Numbers as `T` (v15): `as` converts to and from a `T: Num`, and any
// integer literal is a `T: Num` and any float literal a `T: Float`.
fun mean<T: Num>(xs: T[]): T {
    let mut s: T = 0;
    for x of xs {
        s += x;
    }
    s / xs.len as T
}

fun scale<T: Num>(x: T): T = x * 3 - 2;
fun half<T: Float>(x: T): T = x * 0.5;

fun main() {
    println(mean([1, 2, 6]));
    println(mean([1.0, 2.0, 6.0]));
    println(scale(5 as u8));
    println(half(3.0 as f32));
    println(mean([2.5, 3.5]) as i64);
}
```

**Ordering** (milestone 59). Prints `gpt`, `gpt mlp bigram`, `true`, `true` on four lines, exits with 0.

```
// User-defined order (v15): `cmp` makes a type `Ord`, so `<`, `sort`,
// `min` and tuples work on it. `==` stays structural.
struct Run { loss: f64, name: string }

impl Run {
    fun cmp(self, o: Run): Ordering =
        if self.loss < o.loss {
            Ordering.Less
        } else if self.loss > o.loss {
            Ordering.Greater
        } else {
            Ordering.Equal
        };
}

fun main() {
    let runs = [Run { loss: 2.24, name: "gpt" }, Run { loss: 2.46, name: "bigram" }, Run { loss: 2.40, name: "mlp" }];
    let best = runs.iter().min().unwrap();
    println(best.name);
    println(runs.sort().map(fun(r: Run): string = r.name).join(" "));
    println(runs[0] < runs[1]);
    println((runs[1], 0) < (runs[1], 1));
}
```

**Environment** (milestone 64). Prints `true`, `default`, `true` on three lines, exits with 0.

```
// std.env (v15): read-only environment variables and the current
// directory.
import std.env;

fun main() {
    println(env.get("LUGHA_SURELY_UNSET").is_none());
    let home = env.get("LUGHA_SURELY_UNSET").unwrap_or("default");
    println(home);
    println(env.current_dir().is_ok());
}
```

**Directories** (milestone 65). Prints `["a.log", "b.log"]`, `true true false`, `Directory not empty`, `false` on four lines, exits with 0, and leaves nothing behind.

```
// std.fs (v15): folders and what exists, cleaned up afterwards.
import std.fs;

fun main() {
    let made = fs.make_dirs("runs/2026");
    let a = write_file("runs/2026/b.log", "b");
    let b = write_file("runs/2026/a.log", "a");
    match fs.list_dir("runs/2026") {
        Ok(names) => println(names),
        Err(e) => println(e),
    }
    println("{fs.is_dir("runs")} {fs.is_file("runs/2026/a.log")} {fs.exists("runs/x")}");
    match fs.remove_dir("runs/2026") {
        Ok(_) => println("removed"),
        Err(e) => println(e),
    }
    for name of fs.list_dir("runs/2026").unwrap_or([]) {
        let gone = fs.remove_file("runs/2026/{name}");
    }
    let x = fs.remove_dir("runs/2026");
    let y = fs.remove_dir("runs");
    println(fs.exists("runs"));
}
```

**Processes** (milestone 66). Prints `0: hello ; not a command`, `["apple", "pear", ""]`, `7`, `true` on four lines, exits with 0.

```
// std.process (v15): run a program with an argument list (never a shell),
// wait, and read its exit code and output.
import std.process;

fun main() {
    match process.run("echo", ["hello", "; not a command"]) {
        Ok(out) => println("{out.code}: {out.stdout.trim()}"),
        Err(e) => println(e),
    }
    match process.run_with("sort", [], "pear\napple\n") {
        Ok(out) => println(out.stdout.split("\n")),
        Err(e) => println(e),
    }
    match process.run("sh", ["-c", "exit 7"]) {
        Ok(out) => println(out.code),
        Err(e) => println(e),
    }
    println(process.run("surely-not-a-program", []).is_err());
}
```

## 11. Milestones and scope

The spec is implemented in five milestones, each ending in a program that runs. Build them in order; each one only adds to the last.

| # | Milestone | Spec subset | Done when |
| --- | --- | --- | --- |
| 1 | Expressions to a binary | Integer literals and arithmetic in `fun main(): i32`; lexer, parser, codegen, linking | `fun main(): i32 { 2 + 3 * 4 }` exits with 14 |
| 2 | Variables, control flow, functions | `let`/`let mut`, assignment, blocks with tail values, `if` expressions, `while`, `for` over ranges, optional condition parentheses, `=` function bodies, calls, recursion; integers only, all treated as `i64` | The program below exits with 55 |
| 3 | Type checker and diagnostics | All primitive types, literal inference, casts, mutability checks, return checking; error codes, `--diagnostics=json`, `lughac check`, "remove this semicolon" | The rejected program in section 10 reports E0401 in both formats |
| 4 | Runtime and C | `lugha_rt.c`, intrinsics, `extern fun`, string literals, integer overflow and division checks (panics) | Hello world and the libc example run |
| 5 | Heap data | Boehm GC, strings, arrays, structs, array copies, `for`-`of`, bounds checks, panics; `lughac spec` | Every program in section 10 passes |
| 6 | Enums (v1) | `enum`, construction, `match` with flat patterns, exhaustiveness, `Option<T>`, recursive enums | The enum programs in section 10 pass (done) |
| 7 | Methods (v1) | `impl`, `self`/`mut self`, associated functions, method calls | The methods program in section 10 passes (done) |
| 8 | Growable arrays (v1) | `push`, `pop`, `clear` on `T[]` | The stack program in section 10 passes (done: v1 complete) |
| 9 | Patterns and equality (v2) | deep `==` on every type, string order, nested patterns, guards, W0103 | The patterns program in section 10 passes (done) |
| 10 | Modules (v2) | `import`, per-module names, `m.f`/`m.T`, `private` (E0313), `public` reserved (E0317), no leaking (E0318), module symbols | The two-module program in section 10 passes (done) |
| 11 | Generics (v2) | type parameters, `Eq`/`Ord` bounds, inference, monomorphization, instance symbols, E0420; `Option<T>` as a predeclared generic enum | The generics program in section 10 passes (done) |
| 12 | Interfaces (v2) | `interface` declarations, interfaces as bounds, structural satisfaction (E0421, E0422, E0423) | The interfaces program in section 10 passes (done: v2 complete) |
| 13 | Input and interpolation (v3) | `args()`, `parse_i64`, `parse_f64`, string interpolation (E0111), `lughac run … -- args` | The arguments program in section 10 passes (done) |
| 14 | Functions as values (v3) | function types, named and associated functions as values, calling values, no `==` on functions, `lugha_ref_*` | The functions-as-values program in section 10 passes (done) |
| 15 | Closures (v3) | closure literals, capture by copy (E0511), parameter inference, `lugha_closure_*`, closures in generic code (E0209) | The closures program in section 10 passes (done) |
| 16 | Array helpers (v3) | `map`, `filter`, `fold`, `sort_by`, `any`, `all`, from a Lugha prelude | The array-helpers program in section 10 passes (done: v3 complete) |
| 17 | Results and `?` (v4) | `Result<T, E>` (in the prelude), `Ok`/`Err`, the `?` operator (E0424) | The results program in section 10 passes (done) |
| 18 | Text and option helpers (v4) | string helpers, `join`, `Option`/`Result` helpers, built-in `slice` | The text-helpers program in section 10 passes (done) |
| 19 | Input and output (v4) | `read_line`, `read_file`, `write_file`, `append_file`, `eprint`/`eprintln`, `exit` | The reading-input program in section 10 passes (done: v4 complete) |
| 20 | Interface values (v5) | interfaces as types, conversion, dynamic calls, copies, `lugha_vt_*` (E0425) | The interface-values program in section 10 passes (done) |
| 21 | Type patterns (v5) | `name: T` and `_: T` in `match`, E0426 | The type-patterns program in section 10 passes (done) |
| 22 | Default methods and error conversion (v5) | interface methods with bodies, `lugha_fn_<I>.<m><T>`; `?` converting into an interface error | The errors-as-interfaces program in section 10 passes (done: v5 complete) |
| 23 | Generic interfaces (v6) | `interface I<T>`, bounds with arguments, inference through bounds, generic interface values | The generic-interfaces program in section 10 passes (done) |
| 24 | Iterators (v6) | `Iter<T>` in the prelude, `for … of` over iterators | The iterators program in section 10 passes (done) |
| 25 | `Self` (v6) | `Self` in interfaces, bounds-only interfaces (E0427) | The `Self` program in section 10 passes (done) |
| 26 | Converting between interfaces (v6) | one interface value to another, and through `?` | The converting-between-interfaces program in section 10 passes (done: v6 complete) |
| 27 | More numbers (v7) | `u32`, `u64`, `f32`: literals, arithmetic, casts, printing, externs | The more-numbers program in section 10 passes (done) |
| 28 | Bits (v7) | `& \| ^ ~ << >>`, the compound forms, `0b` literals, the shift panic | The bits program in section 10 passes (done) |
| 29 | Integer methods (v7) | `wrapping_add`/`sub`/`mul`, `count_ones`, `leading_zeros`, `trailing_zeros` | The hashing program in section 10 passes (done: v7 complete) |
| 30 | Packages (v8) | `lugha.toml`, package imports, per-package manifests, `public` and package visibility, E0320–E0323 | The packages program in section 10 passes (done: v8 complete) |
| 31 | Better patterns (v9) | or-patterns (E0428), `mut` bindings | The or-patterns program in section 10 passes (done) |
| 32 | Unwrapping (v9) | `unwrap`, `expect`, `%=` | The unwrapping program in section 10 passes (done) |
| 33 | Conditional patterns (v9) | `if let`, `while let`, `let … else` (E0429) | The conditional-patterns program in section 10 passes (done: v9 complete) |
| 34 | `Self` everywhere (v10) | `Self` in `impl` blocks; default methods on concrete types (E0430) | The `Self`-in-`impl` program in section 10 passes (done) |
| 35 | More integers (v10) | `i8`, `i16`, `u16`; `checked_*`, `saturating_*`, rotations, `MIN`/`MAX`; `parse_u64` | The more-integers program in section 10 passes (done) |
| 36 | Iterator helpers (v10) | `iter`, `bytes`, `range`, `for b of s`; lazy adapters and consumers | The iterator-helpers program in section 10 passes (done: v10 complete) |
| 37 | Package versions (v11) | `[package]` versions, inline entries with requirements, E0324 | The package-versions program in section 10 passes (done) |
| 38 | Re-exports (v11) | `import m { X }`, `public import m { X }` | The re-exports program in section 10 passes (done: v11 complete) |
| 39 | Printing values (v12) | `Display` and `show`, built-in forms, quoting, printing type parameters | The printing-values program in section 10 passes (done) |
| 40 | Printing anything (v12) | interface and function values, `unwrap on Err: <error>` | The printing-anything program in section 10 passes (done: v12 complete) |
| 41 | `Self` in declarations (v13) | `Self` in struct fields and enum payloads | The `Self`-in-declarations program in section 10 passes (done) |
| 42 | `for … of mut` (v13) | iterators advanced in place; array elements as mutable aliases | The `for … of mut` program in section 10 passes (done) |
| 43 | More iterator helpers (v13) | `zip` (`Pair`; tuples since v14), `chain`, `take_while`, `skip_while`, `flat_map`, `min`, `max`, `position` | The more-helpers program in section 10 passes (done) |
| 44 | Format specifiers (v13) | `"{x:spec}"`, E0431 | The format-specifiers program in section 10 passes (done: v13 complete) |
| 45 | Bytes and indexes (v14) | binary file I/O, `to_bytes`, `string_from_utf8`, `read_stdin_bytes`, any-integer indexes | The bytes-and-indexes program in section 10 passes (done) |
| 46 | Unicode text (v14) | `char`, `'a'`, `chars()`, `char_count()`, casts, `char_from`, classification and case (E0112) | The Unicode-text program in section 10 passes (done) |
| 47 | Tuples (v14) | tuple types and values, `t.0`, patterns, `let`/`for` destructuring (E0433), `zip` → tuples, `Pair` removed | The tuples program in section 10 passes (done) |
| 48 | `format` (v14) | template `format(…)` (E0432) | The format program in section 10 passes (done) |
| 49 | Import aliases and re-exports (v14) | `as` on modules and items, `public import m [as v];`, `m.v.X` | The import-aliases program in section 10 passes (done) |
| 50 | Git packages (v14) | git entries, the cache, `lugha.lock`, `lughac update`, `--offline` (E0325, E0326) | A program using a `file://` repository is fetched, locked, built `--offline`, and a moved tag is caught (`tests/git_packages.rs`; done: v14 complete) |
| 51 | Package layout (v15) | `[package] name` (E0327) and `source`, the program's manifest from a parent folder and its implicit package, E0328, the `..` path fix | A library with `source = "src"` and an `examples/` program run from any folder (`tests/package_layout.rs`; done) |
| 52 | Operator overloading (v15) | `+ - * / %` and unary `-` through `add`/`sub`/`mul`/`div`/`rem`/`neg`, compound assignment, the prelude's `Add`…`Neg` and `Num` bounds, `0`/`1` as `T: Num`, bounds on `impl` blocks | The operator-overloading program in section 10 passes (done) |
| 53 | The std library and `std.math` (v15) | bundled `std.<module>` modules, `std` reserved, `$` built-ins in bundled sources, the `Float` bound, `std.math` | The standard-library program in section 10 passes (done) |
| 54 | `std.random` (v15) | a xoshiro256\*\* `Rng`: `seeded`, `from_os`, `uniform`, `below`, `between`, `normal`, `chance`, `choice`, `shuffle`; runtime `lugha_rt_os_seed` | The random-numbers program in section 10 passes (done) |
| 55 | Copy-on-write arrays (v15) | the shared flag in `cap`, marking at copy sites, cloning before writes, two-phase places, `for … of mut` through the place | The copy-on-write program in section 10 passes; every earlier program prints the same (done) |
| 56 | Owning arrays once per loop (v15) | the loop scan, fixed paths, guarded hoists before every loop kind; writes inside skip the flag test | Every program prints the same; `tests/programs/m56/` (done) |
| 57 | Nested `mut self` calls (v15) | E0508 only for arguments that can be the receiver's storage, `mut self` receivers reached after the arguments, E0510 for receivers only across an index | The nested-calls program in section 10 passes (done) |
| 58 | Numbers as `T` (v15) | `as` with `T: Num`/`Float`, any literal as `T`, E0402 per instance | The numbers-as-T program in section 10 passes (done) |
| 59 | User-defined `Ord` (v15) | `cmp(self, other: Self): Ordering`, the prelude's `Ordering`, `<` through `cmp`, tuples, `xs.sort()` | The ordering program in section 10 passes (done) |
| 60 | Lazy std modules (v15) | std functions and methods compiled only when reached, as the prelude's | Every program prints the same; `tests/lazy_std.rs` (done) |
| 61 | One `iter_sum` (v15) | `it.sum()` through `iter_sum<T: Num>` | Every program prints the same (done) |
| 62 | C → Lugha (v15) | `parse_i64`/`parse_u64` and one UTF-8 decoder in the prelude, `$string.from_bytes` | Every program prints the same; `m62/decode.la` matches the C implementation (done) |
| 63 | Widths in characters (v15) | format widths count characters, any character fills | `m63/widths.la` (done) |
| 64 | `std.env` (v15) | `get`, `current_dir`, read-only; `$os.env`, `$os.cwd` | The environment program in section 10 passes (done) |
| 65 | `std.fs` (v15) | folders and what exists; no recursive delete; `$os.list_dir` … `$os.stat_kind` | The directories program in section 10 passes (done) |
| 66 | `std.process` (v15) | `run`, `run_with`, `call`; no shell, `PATH` lookup, inherited environment, fds 0–2 only; `$os.spawn` | The processes program in section 10 passes (done: the v15 TODO complete) |

In milestones 1 and 2, every value can be treated as `i64`, so codegen can start before the type checker exists. Until milestone 4 provides `lugha_rt_panic`, integer `+`, `-` and `*` wrap, and `/` and `%` stop the program with a trap on a zero divisor or `MIN / -1`. Milestone 3 then replaces that assumption with real types. The process exit code is the low 8 bits of `main`'s result, so milestone tests use results below 256.

The milestone 2 test program. It uses `i32` throughout so it stays valid once milestone 3 adds real types:

```
fun fib(n: i32): i32 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    let n: i32 = 3;
    let mut total: i32 = 0;
    for i in 0..n {
        total += i;
    }
    fib(10) + total - n
}
```

**Out of scope for v15 so far** (for later, roughly in order; `std.fs` (directories), `std.process`, `std.env` and Unicode follow-ups are planned for v15):
- a default entry point (`lughac run` without a file), workspaces of several packages, and requiring `name`;
- a separate debug form, parsing values back from text, one-element tuples;
- a package registry, `ssh` URLs, branches or commits as requirements; two versions of one package in a build; full version ranges and pre-release versions;
- re-export chains (`m.v.w.X`), glob imports, and a `--package` flag;
- directories, streams and processes;
- shared references and closures that change their captures;
- explicit type arguments and `.len()`.

## 12. Open questions

These choices were made provisionally to keep the spec complete. Each is easy to change before implementation starts.

- [x] **Return type syntax.** `fun f(): i32` (TypeScript) was chosen over `fun f() -> i32` (Rust) so that `:` always introduces a type.
- [x] **Cost of array copies.** Decided in v15: copy-on-write with a sticky shared flag (milestone 55), reversing v0's eager deep copies. v0 judged that most copied arrays are written right away and that copy-on-write needs reference counts. Real programs proved otherwise: firefly's autograd copies arrays to read them (`let g = self.grads[i];`), and a sticky flag needs no counts, at the price of one extra copy when an array's last holder writes it. Measured: firefly's MLP and GPT training run 13–35% faster (1 m 43 s to 1 m 30 s for 6000 GPT steps; 20.1 s to 13.8 s for 1000). Each write tests a flag on every array along its path, which slowed loops that only write (a histogram count loop, a matrix multiply) by up to about 50%; since milestone 56 a loop makes such arrays unique once before it starts, which brings them back to within about 10% of eager copies (matmul 131 ms eager, 173 ms with milestone 55, 133 ms with 56). Writing through the place (`ps[i].pos[0] += 1.0`) is still cheapest.
- [x] **Default integer type.** Decided: unannotated integer literals are `i64`. This matches `.len` and array indexes, so loop counters need no casts, and it pushes the overflow trap out to about 9.2 quintillion. Use `i32` explicitly where memory in large arrays matters.
- [x] **Integer overflow.** Decided: always trap, like Swift, rather than wrapping or Rust's debug-only checks. One behavior in every build, at a cost of a few percent. Explicit wrapping operations can come later.
- [x] **Semicolons.** Required, which keeps the parser simple. TypeScript-style automatic semicolon insertion is possible later but adds edge cases.
- [ ] **Literal patterns on `f64`.** Excluded from v1: float equality in patterns invites mistakes. Revisit if needed.
- [x] **Swapping buffers.** Decided (v1.1): the `swap(a, b)` intrinsic. Double buffering with `ys = buf;` copies the whole buffer each time, because `buf` is still in use; copy-on-write and move on last use can't avoid that copy (DECISION-010), but `swap(ys, buf)` exchanges the two arrays without copying.
- [x] **Array growth factor.** Decided: capacity doubles, starting at 4. It was adequate on the stack program and the DECISION-010 programs; revisit only with measurements.
- [x] **Instantiation depth.** Decided (v2): 64, large enough for real code and small enough to fail fast (E0420).
- [ ] **`mut` bindings in `match`.** Bindings are borrowed and immutable; no program has needed `mut` ones yet.
- [x] **Expression-bodied functions.** Decided: adopted (section 6). `fun square(x: i64): i64 = x * x;` drops the braces from one-liners. It is a small parser addition and a second way to write the same function, accepted because one-liners are common and it is pure sugar for `{ e }`.
- [ ] **Boxing primitives** (v5). An `i64` inside an interface value is boxed like a struct, so every interface value has one layout. Small values could be stored inline later, an optimisation that changes nothing visible.
- [x] **Default methods on concrete types** (v5). Decided (v10): `c.describe()` works when exactly one visible interface the type satisfies supplies the default; two are E0430.
- [ ] **Iterating a copy** (v6). `for n of c` leaves `c` unchanged, which suits values but means a half-used iterator can't be resumed after a `break`. A later version could add `for n of mut c`.
- [ ] **One argument per type** (v6). A type satisfies `Source<X>` for at most one `X`, since it has one `next`. That suits iterators but rules out an interface such as `From<T>` implemented once per `T`, which would need methods overloaded by type.
- [ ] **`%=`** (v7). The compound assignments still lack `%=`, which v0 left out. Adding it is one line of grammar if programs want it.
- [x] **Unsigned indexes** (v7). Decided (v14): any integer type indexes; below 0 or past the end panics.
- [ ] **Re-exports** (v8). A package can't yet expose another package's items as its own (`public import`); users import both.
- [ ] **A `--package` flag** (v8). Pointing at a package without editing `lugha.toml`, for a one-off build, could come later as a command-line override.
- [x] **Printing errors** (v9). Decided (v12): `unwrap on Err: <error>` prints the error.
- [ ] **Irrefutable `if let`** (v9). `if let x = e` is allowed silently; a warning could come later if it proves a common mistake.
- [ ] **`xs.iter()` copies the array** (v10). It follows the copy rules, so iterating a variable still used later costs a copy; move on last use avoids it otherwise. A borrowing iterator would need shared references.
- [ ] **Unicode text** (v10). `bytes()` is the only way to iterate a string; iterating characters waits for a decision on text encoding.
- [ ] **Checking the program's own version** (v11). The root package may declare a version that nothing reads. A later `lughac` could print it, or check it against a minimum `lughac` version.
- [ ] **Re-exports and visibility levels** (v11). A re-export is always `public`. A package-level re-export (visible to the package's other modules only) could come later if packages need internal façades.
- [ ] **A `show` of another shape** (v12) is ignored silently, so v11 programs stay valid. A later warning could point out `fun show(self): i64`.
- [ ] **A default entry point** (v15). `[package] main = "src/main.la"` and `lughac run` with no file could come later.
- [ ] **Requiring `name`** (v15). A later version could require it once packages have adopted it.
- [ ] **Git packages and `extern`** (v14). A downloaded package may declare `extern` C functions, which a built program calls like any code. A later version could list a package's externs at `lughac update`, so users see what a package can call.
- [ ] **Module paths in printed names** (v12). A type prints with its bare name, so two modules' `Point`s look the same. A debug form could add the path.
- [ ] **Width counts bytes** (v13). Text with multi-byte characters pads as if it were longer; this waits for a decision on Unicode text.
- [ ] **Format specifiers at run time** (v13). A spec must be written in the string. v14's `format` keeps that rule: its template is a literal, checked when compiling.
