# Lugha v7 — Draft (delta against v6)

**Status:** approved 2026-10-13 — session 69
**Base:** `Language v6 Specification.md`, complete (milestones 23–26). It stays the source of truth, and what `lughac spec` prints, until v7's first milestone starts `Language v7 Specification.md`. That file then grows milestone by milestone, and v6 is frozen, as earlier versions were.

This draft lists only what changes, in the order of the v6 sections. Everything not mentioned keeps its v6 meaning. Every valid v6 program stays valid in v7, because everything v7 adds was an error before:
- `u32`, `u64` and `f32` were unknown types (E0305);
- `&`, `|`, `^`, `~`, `<<`, `>>` and `0b` literals were lexing or parsing errors;
- methods on integers were E0415.

## Decisions behind this draft (discovery, session 69)

1. **Headline: numbers and bits.** Bitwise operators, the types `u32`, `u64` and `f32`, and explicit wrapping arithmetic, so programs can write hash functions, checksums and bit flags. Packages, iterator helpers and the `match` additions wait (§11).
2. **Wrapping arithmetic is a method:** `a.wrapping_add(b)`, `wrapping_sub`, `wrapping_mul` on every integer type. Plain `+ - *` still panic on overflow in every build.
3. **An out-of-range shift panics:** `x << n` and `x >> n` with `n` negative or at least the type's width is a panic, like overflow. `>>` on a signed type copies the sign bit.
4. **Extras:** binary literals (`0b1010`), and the methods `count_ones()`, `leading_zeros()` and `trailing_zeros()` on every integer type. Hex literals and `_` separators already exist (v0).
5. **Precedence follows Rust:** `* / %` > `+ -` > `<< >>` > `&` > `^` > `|` > comparisons > `&&` > `||`. So `flags & MASK == 0` means `(flags & MASK) == 0`. Bitwise not is a prefix `~`, separate from the logical `!`.

**Choices made while writing this draft** (each settles a gap the answers left open):
- **Both operands of a bit operator have the same integer type**, as for `+`. A shift's amount has the same type as the value shifted: `x << 3` takes `3` as `x`'s type, and `h << (i as u64)` casts. There are no implicit conversions (§4).
- **Bit operators work on integers only.** On `bool`, `f32` or `f64` they are E0404; `bool` keeps `&&`, `||` and `!`.
- **Compound assignment** gains `&=`, `|=`, `^=`, `<<=` and `>>=`, matching the existing `+=` family.
- **The counting methods return `i64`,** like `.len`, so their results index and compare without casts.
- **`f32` prints as the shortest decimal that reads back as the same `f32`.** So `0.1` prints `0.1`, not the `f64` digits of its value.
- **`parse_i64` and `parse_f64` stay as they are.** The new types are reached with `as`.

Later (the out-of-scope list in §11): `i8`, `i16` and `u16`; rotations; checked and saturating methods; `parse_u64`.

## §2 Lexical structure — changes

- **New operators:** `&`, `|`, `^`, `~`, `<<`, `>>`, `&=`, `|=`, `^=`, `<<=`, `>>=`. A lone `&` or `|` is no longer E0101.
- **Binary integer literals:** a lowercase `0b` followed by at least one `0` or `1`, with `_` separators allowed between digits, as for hex. `0b`, `0b_1` and `0B1` are errors (E0106, E0105 and E0108). Any other digit is E0108 (`0b102`).
- **New type keywords:** `u32`, `u64` and `f32`.

## §3 Grammar — changes

```ebnf
base_type    = "i32" | "i64" | "u8" | "u32" | "u64" | "f32" | "f64" | "bool" | "string" | … ;
assign_op    = "=" | "+=" | "-=" | "*=" | "/=" | "&=" | "|=" | "^=" | "<<=" | ">>=" ;
```

The precedence table, lowest first:

| Level | Operators | Meaning |
| --- | --- | --- |
| 1 | `\|\|` | Logical or |
| 2 | `&&` | Logical and |
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

All binary operators are left-associative.

## §4 Type system — changes

**New numeric types.**
- **`u32` and `u64`** are unsigned integers of 32 and 64 bits.
  - Arithmetic, comparison and printing work as for the other integers, and overflow panics.
  - Negating one is E0404, as for `u8`.
  - An unsuffixed literal takes the expected type: `let h: u64 = 14695981039346656037;` fits, and a literal outside the type's range is E0402.
- **`f32`** is a 32-bit float, with `f64`'s operations and rules.
  - A float literal takes `f32` where `f32` is expected.
  - It prints as the shortest decimal that reads back as the same `f32`, in float-literal syntax.
- **Casts:** every numeric type casts to every other with `as`, under v0's rules:
  - integer narrowing truncates (`-1 as u32` is `4294967295`);
  - integer to float rounds to nearest;
  - float to integer truncates toward zero and saturates; NaN becomes 0;
  - `f64` to `f32` rounds to nearest.
- **Indexes and lengths stay `i64`.**

**Bit operators.**
- **`a & b`, `a | b` and `a ^ b`** take two operands of one integer type and give that type.
- **`~a`** flips every bit of an integer.
- **`a << n` and `a >> n`:**
  - `n` has `a`'s type, and the result has `a`'s type;
  - bits shifted past the width are lost, and `<<` never panics for overflow;
  - `>>` copies the sign bit for `i32` and `i64`, and fills with zeros for the unsigned types;
  - an `n` that is negative, or at least the width (8, 32 or 64), panics: `shift out of range`, at the operator.
- **On anything else,** including `bool` and the floats, these operators are E0404.

**Methods on integers.** Every integer type has:

| Method | Gives | Meaning |
| --- | --- | --- |
| `a.wrapping_add(b)`, `wrapping_sub(b)`, `wrapping_mul(b)` | the same type | the result modulo 2^width, never panics |
| `a.count_ones()` | `i64` | how many bits are 1 |
| `a.leading_zeros()` | `i64` | zero bits above the highest 1 (the width for 0) |
| `a.trailing_zeros()` | `i64` | zero bits below the lowest 1 (the width for 0) |

- **Rules:** `b` has `a`'s type, and a literal `b` takes it. Any other method name on an integer is E0415.
- **Literals:** an integer literal receiver with no expected type is `i64`, so `(5).count_ones()` works.

## §5 Expressions and statements — changes

- **The new compound assignments** `x &= y` and so on mean `x = x & y`, with the place evaluated once, as `+=` does.

## §7 Memory model — changes

- **New sizes:** `u32` is 4 bytes, `u64` 8 and `f32` 4, each aligned to its size. Arrays, structs and enums lay them out by the existing rules.

## §8 C interop — changes

- **Allowed in extern signatures:** `u32`, `u64` and `f32`, as C's `uint32_t`, `uint64_t` and `float`.

## §9 Compilation model — changes

- **Operators and methods:**
  - the bit operators lower to LLVM `and`, `or`, `xor`, `shl`, `lshr` and `ashr`, after a range check of the shift amount that panics through `lugha_rt_panic`;
  - the wrapping methods are plain `add`, `sub` and `mul`;
  - the counting methods are `llvm.ctpop`, `llvm.ctlz` and `llvm.cttz`.
- **The runtime** gains printing for `u32`, `u64` and `f32`: `lugha_rt_print_u64`, with `u32` widened to it, and `lugha_rt_print_f32` with `f32`'s shortest round-trip. It also gains a `shift out of range` panic message.
- **Programs that use none of this** give byte-identical IR.

**New error codes:** none. E0404 covers bad operands, E0415 unknown integer methods, E0402 literals out of range, and E0105, E0106 and E0108 bad binary literals. An out-of-range shift is a run-time panic.

## §10 Example programs — additions (v7 acceptance tests)

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

## §11 Milestones — additions

| # | Milestone | Adds | Done when |
| --- | --- | --- | --- |
| 27 | More numbers | `u32`, `u64`, `f32`: literals, arithmetic, casts, printing, externs | The more-numbers program passes |
| 28 | Bits | `& \| ^ ~ << >>`, the compound forms, `0b` literals, the shift panic | The bits program passes |
| 29 | Integer methods | `wrapping_add`/`sub`/`mul`, `count_ones`, `leading_zeros`, `trailing_zeros` | The hashing program passes, and the v7 spec is complete |

**Order:**
- **M27 first:** bit code is mostly written on unsigned types.
- **M28 next:** the operators.
- **M29 last:** the methods, which hash functions combine with both.

**Out of scope for v7** (for later, roughly in order):
- packages with `public`;
- or-patterns and `mut` bindings in `match`, and `unwrap()`;
- iterator helpers (`map`, `filter`, `collect` on `Iter<T>`), strings as iterators, `for n of mut c`;
- `Self` outside interfaces, and default methods called on concrete types;
- `i8`, `i16`, `u16`, rotations, and checked and saturating arithmetic methods; `parse_u64`;
- binary I/O, directories, streams and processes;
- shared references and closures that change their captures;
- explicit type arguments and `.len()`.

## §12 Open questions — additions

- [ ] **`%=`:** the compound assignments still lack `%=`, which v0 left out. Adding it is one line of grammar if programs want it.
- [ ] **Unsigned indexes:** `xs[i]` needs an `i64`, so a `u32` index needs a cast. Accepting any integer index would remove the cast, but it adds a rule for negative values.
