# Lugha v4 — Draft (delta against v3)

**Status:** approved 2026-10-13 — session 52
**Base:** `Language v3 Specification.md`, complete (milestones 13–16). It stays the source of truth, and what `lughac spec` prints, until v4's first milestone starts `Language v4 Specification.md`. That file then grows milestone by milestone, and v3 is frozen, as earlier versions were.

This draft lists only what changes, in the order of the v3 sections. Everything not mentioned keeps its v3 meaning. Every valid v3 program stays valid in v4 unless it:
- defines something named `Result`, `Ok` or `Err`;
- defines a function named `read_line`, `read_file`, `write_file`, `append_file`, `eprint`, `eprintln` or `exit` (all E0302).

## Decisions behind this draft (discovery, session 52)

1. **Headline: input and output.** Programs can read stdin line by line, read and write text files, and write to stderr. Interface values, packages, bitwise operators, more integer types and the `match` additions wait for a later version.
2. **Errors are values.** v4 adds a built-in `Result<T, E>`, an ordinary generic enum like `Option`, with `Ok` and `Err` written without a prefix. I/O functions return `Result<T, string>`, whose `Err` says what went wrong. Programs can use `Result` for their own errors.
3. **The `?` operator** returns early:
   - `e?` on a `Result<T, E>` gives the `T`, or returns the `Err(E)` from the enclosing function;
   - on an `Option<T>` it gives the `T`, or returns `None`;
   - in a closure it returns from the closure.
4. **Helpers, written in the prelude** like the array helpers:
   - for strings: `lines`, `split`, `trim`, `contains`, `starts_with`, `ends_with`, `slice`, and `join` on `string[]`;
   - for `Option` and `Result`: `unwrap_or`, `ok_or`, `ok`, `map`, `map_err`, `is_some`/`is_none`, `is_ok`/`is_err`.
5. **The I/O surface:** `read_line`, `read_file`, `write_file`, `append_file`, `eprint`/`eprintln` and `exit`. It is text only: no binary I/O, directories or streams.

## §2 Lexical structure — changes

- **`?`** is a new token, the postfix operator of §3. It was an unexpected character (E0101) before, so no valid program changes.

## §3 Grammar — changes

```ebnf
postfix      = primary { call | index | member | "?" } ;   (* `read_file(p)?` *)
```

- **Binding:** `?` binds like a call or an index, tighter than any prefix or infix operator, so `-x?` is `-(x?)` and `a + b?` is `a + (b?)`.
- **Chaining:** `f()?.len` and `parse(s)?.field` read left to right.

## §4 Type system — changes

**`Result<T, E>`** is predeclared in every module as `enum Result<T, E> { Ok(T), Err(E) }`.
- **Names:** `Result`, `Ok` and `Err` join the built-in names (E0302 to declare them, E0311 for a local).
- **Construction:**
  - `Ok(x)` and `Err(e)` are written without a prefix, like `Some`;
  - the missing type argument comes from the expected type, as for any generic enum: `let r: Result<i64, string> = Ok(1);`;
  - `Ok(1)` alone is E0412.
- **Patterns:** `Ok(x)` and `Err(e)`, nested freely, with exhaustiveness as for any enum.
- **Equality:** `Result<T, E>` is `Eq` when `T` and `E` are.
- **Layout and copies:** like any enum (§7).

**The `?` operator.**
- **On `Result<T, E>`:** the type is `T`. The enclosing function, or closure, must return `Result<U, E>` with the same `E`; there are no conversions.
- **On `Option<T>`:** the type is `T`. The enclosing function must return `Option<U>`.
- **Misuse is E0424**, with help:
  - `?` on any other type;
  - `?` in a function whose return type doesn't match (for example `?` on an `Option` in a function returning `Result`, with help to write `.ok_or(e)?`);
  - `?` in `main` (help: handle it with `match`, or call a function returning `Result`);
  - `?` in a closure whose return type is unknown (as for `return`, the E0412 rule of v3).
- **Evaluation:** `e?` evaluates `e` once. On `Err(x)` or `None`, it returns at once, as `return Err(x);` or `return None;` would: values about to be stored aren't stored, and nothing after it runs.

**Helpers** (prelude methods, like the array helpers; each takes `self` and never changes it).

| Receiver | Method | Result |
| --- | --- | --- |
| `string` | `s.lines()` | `string[]`: split at `\n`, a trailing `\r` removed from each line, no final empty line after a trailing `\n` |
| `string` | `s.split(sep)` | `string[]`: the parts between occurrences of `sep`; an empty `sep` panics |
| `string` | `s.trim()` | `string`: without leading and trailing ASCII whitespace (space, `\t`, `\n`, `\r`) |
| `string` | `s.contains(sub)`, `s.starts_with(p)`, `s.ends_with(p)` | `bool` |
| `string` | `s.slice(from, to)` | `string`: the bytes `from..to`; out of range or `from > to` panics like indexing |
| `string[]` | `xs.join(sep)` | `string`: the elements with `sep` between them |
| `Option<T>` | `o.unwrap_or(d)` | `T` |
| `Option<T>` | `o.ok_or(e)` | `Result<T, E>` |
| `Option<T>` | `o.map(f)` | `Option<U>` |
| `Option<T>` | `o.is_some()`, `o.is_none()` | `bool` |
| `Result<T, E>` | `r.unwrap_or(d)` | `T` |
| `Result<T, E>` | `r.ok()` | `Option<T>` |
| `Result<T, E>` | `r.map(f)`, `r.map_err(f)` | `Result<U, E>`, `Result<T, F>` |
| `Result<T, E>` | `r.is_ok()`, `r.is_err()` | `bool` |

- **Not included:** `unwrap()`, which would crash a program on an error instead of handling it.
- **Bytes, not characters:** helpers work on bytes, and strings are UTF-8, so `slice` can cut inside a character. The result is still a valid Lugha string of those bytes. Printing it prints those bytes.

## §5 Expressions and statements — changes

**Intrinsics** join the built-in names (E0302 to declare one).

| Intrinsic | Type | Effect |
| --- | --- | --- |
| `read_line()` | `Option<string>` | The next line of stdin without its `\n` (or `\r\n`); `None` at the end of input. A last line without `\n` is still returned. |
| `read_file(path)` | `Result<string, string>` | The whole file as text; `Err` with the system's message (`"No such file or directory"`) |
| `write_file(path, text)` | `Result<i64, string>` | Replaces the file's contents; `Ok` with the bytes written |
| `append_file(path, text)` | `Result<i64, string>` | Adds to the end, creating the file if needed |
| `eprint(x)`, `eprintln(x)` | `void` | Like `print`/`println`, to stderr. Stdout is flushed first, so output appears in program order on a terminal. |
| `exit(code)` | `never` | Flushes stdout and stops the program with exit code `code` (`i32`) |

- **Paths** are relative to the current directory, as given to the program.
- **Text only:** `read_file` reads bytes into a string; a file with a NUL byte or invalid UTF-8 is still read as bytes.

## §7 Memory model — changes

- **`Result<T, E>`** lays out like any two-variant enum. Its payloads follow the indirect-slot rule.
- **Strings returned by I/O** are GC strings like any other, allocated through `lugha_rt_alloc`.

## §8 C interop — changes

- `Result` can't cross the C boundary (E0409), like every enum.

## §9 Compilation model — changes

- **`?`** lowers to a branch on the tag. The `Err`/`None` path builds the enclosing function's return value and returns. The value it would have produced is only computed on the `Ok`/`Some` path.
- **The prelude** gains the §4 helpers. Making a string from a slice of another is the one thing Lugha can't express, and `slice` is a built-in method lowered to `lugha_rt_str_slice` (amended by PRP-043: a built-in method rather than a hidden intrinsic, so its panic points at the caller); the other helpers are prelude code using it.
- **Runtime:** it gains `lugha_rt_read_line`, `lugha_rt_read_file`, `lugha_rt_write_file` (with an append flag), `lugha_rt_eprint_*`, `lugha_rt_exit` and `lugha_rt_str_slice`. Failures return the C library's `strerror` text.
- **Tests:**
  - the runner gains an optional `<name>.stdin` beside a run case, fed to the program;
  - file-writing tests run in a scratch directory;
  - the §10 extractor understands "Run with … on stdin".

**New error code.** The number is proposed and becomes final when its milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0424 | `?` where it can't return | `?` on an `i64`; on an `Option` in a function returning `Result`; in `main` |

**Reused codes:**
- E0302 and E0311: `Result`, `Ok`, `Err`, and the new intrinsics.
- E0412: `Ok(1)` with no expected type, and `?` in a closure whose return type is unknown.
- E0403: argument types of the helpers and intrinsics.
- E0417: a non-exhaustive `match` on a `Result`.

## §10 Example programs — additions (v4 acceptance tests)

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

**Reading input** (milestone 19). Run with `5`, `7` and `x` on three lines of stdin. Prints `12` then `skipped 1` on two lines, exits with 0. (Corrected by PRP-044: assignment and `break` are statements, so these arms need braces.)

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

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 17 | Results and `?` | `Result<T, E>`, `Ok`/`Err`, the `?` operator (E0424) | The results program passes |
| 18 | Text and option helpers | the string helpers, `join`, the `Option`/`Result` helpers, internal intrinsics for the prelude | The text-helpers program passes |
| 19 | Input and output | `read_line`, `read_file`, `write_file`, `append_file`, `eprint`/`eprintln`, `exit`; stdin and scratch directories in the test runner | The reading-input program passes, and the v4 spec is complete |

**Order:**
- **M17 first:** I/O returns `Result`, and `?` makes using it short.
- **M18 next:** processing input needs `lines`, `split`, `trim` and `unwrap_or`.
- **M19 last:** with those in place, the I/O functions are small.

**Out of scope for v4** (for v5, roughly in order):
- interface values and dynamic dispatch, `Self`, generic interfaces, default methods;
- packages with `public`;
- bitwise operators, `u32`/`u64`/`f32`, wrapping arithmetic;
- or-patterns and `mut` bindings in `match`;
- binary I/O, directories, streams and processes;
- `unwrap()`;
- shared references and closures that change their captures;
- explicit type arguments and `.len()`.

## §12 Open questions — additions

- [x] **Error conversion with `?`:** v4 requires the same `E`. Resolved (PRP-042): kept until interfaces can describe conversions.
- [x] **`lines()` and `\r`:** a trailing `\r` is stripped from each line, so Windows files read naturally. Resolved (PRP-043): yes.
- [x] **Very large files:** `read_file` reads the whole file into memory. Resolved (PRP-044): fine for v4; streaming waits.
- [x] **`exit` and buffered output:** stdout is flushed, and stderr is unbuffered. Resolved (PRP-044): yes.
