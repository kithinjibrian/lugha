# Lugha v9 — Draft (delta against v8)

**Status:** approved 2026-10-14 — session 76
**Base:** `Language v8 Specification.md`, complete (milestone 30). It stays the source of truth, and what `lughac spec` prints, until v9's first milestone starts `Language v9 Specification.md`. That file then grows milestone by milestone, and v8 is frozen.

This draft lists only what changes, in the order of the v8 sections. Everything not mentioned keeps its v8 meaning. Every valid v8 program stays valid in v9, because everything v9 adds was an error before:
- `|` in a pattern, and `mut` before a binding, were parse errors;
- `%=` was a lexing error;
- `if let`, `while let` and `let … else` were parse errors;
- `unwrap` and `expect` on `Option` and `Result` were E0415.

## Decisions behind this draft (discovery, session 76)

1. **Headline: `match` and everyday conveniences.** Iterator helpers, `Self` in `impl` blocks and more integer types wait (§11).
2. **Or-patterns bind names** when every alternative binds the same names with the same types: `Circle(r) | Square(r) => r * 2.0`. Otherwise it is a new error, E0428.
3. **`mut` bindings give the arm its own copy:** `Some(mut n) => { n += 1; … }` changes `n` only, never the matched value.
4. **`unwrap()` and `expect(msg)`** on `Option` and `Result` panic with fixed messages, `unwrap on None`, `unwrap on Err` or `expect`'s own message, located at the method name.
5. **Extras:** `%=`, `if let`, `while let` and `let … else`.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **Or-patterns nest** anywhere a pattern may appear (`Some(1 | 2)`), and a guard applies to the whole arm (`A | B if g =>`).
- **`mut` binds wherever a name does:** in `match` arms, nested payloads, type patterns (`mut c: Circle`), and in `if let`, `while let` and `let … else`. A `mut` binding that the arm never changes is no error.
- **`if let` takes one pattern, with an optional guard-free `else`.** Its pattern may be refutable or not, and an irrefutable one (`if let x = e`) is allowed silently. With `else` it is an expression, as `if` is, and `else if let` chains.
- **The `else` of `let … else`** must not finish normally: it must end in `return`, `break`, `continue`, `panic(…)` or `exit(…)`, or be of type `never`. Otherwise it is a new error, E0429. The bindings are in scope after the statement. `let mut Some(n) = …` is not a thing; write `let Some(mut n) = … else { … };`.
- **`unwrap` and `expect` are built in,** not prelude functions, so a panic points at the caller's method name rather than into the prelude. `expect`'s message is a `string`.

Later (the out-of-scope list in §11): iterator helpers, `Self` outside interfaces, more integer types, package versions and re-exports.

## §2 Lexical structure — changes

- **New token:** `%=`.

## §3 Grammar — changes

```ebnf
assign_op    = … | "%=" ;
pattern      = alternative { "|" alternative } ;
alternative  = "_"
             | [ "mut" ] IDENT [ "(" pattern { "," pattern } [ "," ] ")" ]
             | [ "-" ] INT | STRING | "true" | "false"
             | ( [ "mut" ] IDENT | "_" ) ":" type
             | "(" pattern ")" ;
if_expr      = "if" ( cond | "let" pattern "=" cond ) block [ "else" ( if_expr | block ) ] ;
while_stmt   = "while" ( cond | "let" pattern "=" cond ) block ;
var_decl     = … | "let" pattern "=" expr "else" block ";" ;
```

- **`mut` before a name** that is a variant (`mut None`) is an error, E0428.
- **`let pattern = e else { … };`** is told apart from an ordinary `let` by the `else`. An ordinary `let` keeps its v0 form (`let [mut] name [: type] = e;`).

## §4 Type system — changes

**Or-patterns.**
- **Matching:** `p | q` matches when `p` or `q` does, trying them left to right.
- **Bindings:** every alternative must bind the same set of names, each at the same type. A name missing from an alternative, or bound at another type, is E0428, naming it.
- **Exhaustiveness and reachability:** an or-pattern counts as each of its alternatives. `Red | Green | Blue` covers those variants (E0417 for the rest), and an alternative that earlier arms cover is W0102, as an arm would be.

**`mut` bindings.** `mut name` binds a mutable local holding its own copy of the matched part, made at the copy sites, so assigning to it or calling `mut self` methods on it never changes the scrutinee. A plain binding stays read-only (E0501).

**`unwrap` and `expect`.**

| Method | On | Gives | Panics with |
| --- | --- | --- | --- |
| `o.unwrap()` | `Option<T>` | `T` | `unwrap on None` |
| `r.unwrap()` | `Result<T, E>` | `T` | `unwrap on Err` |
| `x.expect(msg)` | `Option<T>` or `Result<T, E>` | `T` | `msg`, a `string` |

The panic is located at the method name (`unwrap`, `expect`), and exits with 101 like every panic.

## §5 Expressions and statements — changes

- **`x %= y`** means `x = x % y`, with the place evaluated once. It panics on a zero divisor, as `%` does.
- **`if let p = e { a } else { b }`** evaluates `e` once. If `p` matches, it runs `a` with `p`'s bindings in scope; otherwise it runs `b` (or nothing). It is an expression under `if`'s rules: both branches agree on a type, and without `else` the type is `void`.
- **`while let p = e { body }`** evaluates `e` before each iteration and runs `body` while `p` matches. `break` and `continue` work as in `while`.
- **`let p = e else { … };`** evaluates `e`. If `p` matches, its bindings are in scope for the rest of the block. Otherwise the `else` block runs, and it must leave (E0429).
- **Scrutinees** of `if let` and `while let` are conditions, parsed without struct literals, as for `if`.

## §9 Compilation model — changes

- **Or-patterns** lower to the arm chain, with each alternative tested in turn. A binding is stored from whichever alternative matched.
- **`if let`, `while let` and `let … else`** lower as one-arm matches, with the same tests and bindings as `match`.
- **`unwrap` and `expect`** test the tag and call `lugha_rt_panic` with the message and the method's location.
- **The runtime** needs nothing new.
- **Programs using none of this** give byte-identical IR.

**New error codes.** The numbers are proposed, and become final when their milestone's PRP is approved.

| Code | Error | Example |
| --- | --- | --- |
| E0428 | Or-pattern alternatives that bind differently, or `mut` on a variant | `Some(x) \| None`; `A(x) \| B(y)`; `A(x) \| B(x)` with `x` an `i64` and an `f64`; `mut None` |
| E0429 | A `let … else` whose `else` block can finish | `let Some(n) = o else { println("no"); };` |

**Reused codes:**
- **E0415:** `unwrap` and `expect` on anything but `Option` and `Result`.
- **E0403:** `expect` with a message that isn't a `string`.
- **E0501:** assigning to a plain (non-`mut`) binding.
- **E0417:** a non-exhaustive `match`, including its or-patterns.
- **W0102:** an unreachable alternative.

## §10 Example programs — additions (v9 acceptance tests)

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

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 31 | Better patterns | or-patterns (E0428), `mut` bindings | The or-patterns program passes |
| 32 | Unwrapping | `unwrap`, `expect`, `%=` | The unwrapping program passes |
| 33 | Conditional patterns | `if let`, `while let`, `let … else` (E0429) | The conditional-patterns program passes, and the v9 spec is complete |

**Order:**
- **M31 first:** the pattern forms that `if let` and the others reuse.
- **M32:** small and independent.
- **M33 last:** builds on M31's patterns.

**Out of scope for v9** (for later, roughly in order):
- iterator helpers, strings as iterators, `for n of mut c`;
- `Self` outside interfaces, and default methods called on concrete types;
- `i8`, `i16`, `u16`, rotations, and checked and saturating methods; `parse_u64`; unsigned indexes;
- package versions, downloading, re-exports and a `--package` flag;
- binary I/O, directories, streams and processes;
- shared references and closures that change their captures;
- explicit type arguments and `.len()`.

## §12 Open questions — additions

- [ ] **Printing errors:** `unwrap on Err` can't show the error, because values other than numbers, `bool` and strings have no text form. A later `Display`-like interface could add it.
- [ ] **Irrefutable `if let`:** `if let x = e` is allowed silently. A warning could come later if it proves a common mistake.
