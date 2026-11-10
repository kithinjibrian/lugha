## FEATURE: Constants computed by pure code — a `const`'s value may be any pure expression, function calls included; milestone 147 (v17).

## OBJECTIVE

```lugha
fun crc_table(): u32[] {
    let mut t: u32[] = [];
    for i in 0..256 {
        let mut c = i as u32;
        for _ in 0..8 { c = if c & 1 == 1 { 0xEDB88320 ^ (c >> 1) } else { c >> 1 }; }
        t.push(c);
    }
    t
}

const CRC: u32[] = crc_table();
const MODE: string = if DEBUG { "dev" } else { "prod" };
const DEBUG: bool = false;
const BANNER: string = "crc[1] = {CRC[1]}, mode {MODE}";

fun main() { println(BANNER); }      // crc[1] = 1996959894, mode prod
```

```lugha
const CONFIG: string = read_file("app.toml").unwrap();
```
```
error[E0445]: a constant can only use pure code
  `read_file` reads files
```

Constants are still computed once, before `main`, by `lugha_const_init()`. Nothing runs while compiling.

## CONTEXT

- Read first: `src/check/consts.rs` (the constant-expression rule `non_constant`, deps, `order_consts`), `src/check/instances.rs` (`Use` edges per body, `monomorphize` roots), `src/check/input.rs` (`INPUT`), `src/check/float_ops.rs` (`$os`), `src/codegen/consts.rs` (`define_const_init`, which already compiles the value as an ordinary expression), `src/check/errors/v17.rs`, `docs/errors/E0445.md` and `E0446.md`.
- Creates `src/check/purity.rs` (effects and calls per body; the reachability walk), maybe `src/check/consts_tests.rs` if `consts.rs` reaches 500 lines.
- Spec §6 "Constants" (line ~1095), §8 (`lugha_const_init`, line ~1549), "Changes since v16", §11 row 147; Draft entry for milestone 147.
- DECISIONS: 195 (constants, PRP-141); 208 (lists this feature and says it "needs decision 4's 'no interpreter' amended"). Discovery (2026-10-31) settled it **without** amending decision 4, because values are computed at startup, not while compiling. New decision 212 records that, and that build-time evaluation is **rejected, not deferred**: Lugha's types never need a value (`T[]` carries its length; no const generics; generic bodies checked once, decisions 33 and 41; no comptime, decision 208), so the only gains are no startup cost and earlier panics, which don't justify running the runtime and GC inside `lughac`. No open decision blocks this.

## REQUIREMENTS

### Must Do

1. **The value can be any expression** (discovery Q3): the fixed constant-expression list (literals, operators, `as`, fields, indexing, literals of arrays, structs, tuples and variants; `if`, `match`, blocks, closures, interpolation, `#{…}` and `..base` refused) is gone. The value is checked like any expression of the written type, with the usual errors.
2. **The value must be pure** (Q2): neither the value nor anything it reaches through calls may use an **effect**:
   - the intrinsics that touch the outside: `print`, `println`, `eprint`, `eprintln`, `args`, `read_line`, `read_file`, `write_file`, `append_file`, `read_bytes`, `write_bytes`, `append_bytes`, `read_stdin_bytes`, `flush`, `exit`, `c_read`, `c_write`;
   - any `$os.*` built-in (everything in std that touches files, the network, time, the environment, random numbers or processes goes through these);
   - calling an `extern` C function;
   - concurrency: `spawn`, channels (`Chan`), `select`, actors (spawning one or messaging it).

   Everything else is allowed: local mutation, loops, recursion, arrays, maps, sets, `Ref`, closures, interface calls and `panic`. Purity is worked out by the compiler; there is no `pure` keyword.
3. **What counts as reached:** every function or method a body calls; every function named as a value (`map(f)`); for an interface value, the methods of every type converted to that interface in the reached code (`Checked.conversions`); `include_bytes`/`include_str` are pure.
4. **Generics are checked with their real types** (Q5): a call through a bound reaches the method at the concrete type argument, the same way `monomorphize` follows `Use { through }`. `total(boxes)` can be pure while `total(files)` isn't.
5. **E0445 names the path to the effect**, from the constant's call to the effect, at most 6 steps and then `…`: `` `load` calls `config.read`, which calls `read_file`, which reads files ``. One E0445 per constant (the first effect found, in source order). The help text explains purity and suggests computing the value in `main` instead.
6. **Dependencies follow calls** (Q4): a constant depends on every constant read in the value **and** in any function it reaches, so it is computed after them, whatever their order in the source. A cycle, direct or through functions, is E0446 with the path (`A calls f, which reads A`).
7. **Type rule unchanged:** a constant's type is still plain data (E0445 for functions, interfaces, `Ref`, `Task`, `Chan`, actors, handles, maps and sets). Those values may still be used *while* computing the constant.
8. **Codegen:** generic and lazy (prelude/std) functions reached from a constant's value become monomorphization roots. The value's body is given an owner (`$const.<key>`) so its `Use`s are recorded. `lugha_const_init` needs no other change.
9. **Tests and the language server:** unchanged behaviour. A constant that panics still fails every test of its executable (milestone 124); `check` and the language server report E0445/E0446 as before.
10. Docs: update `docs/errors/E0445.md` (the new meaning, with the path example) and `E0446.md` (cycles through functions); spec §6 "Constants", §8, "Changes since v16", §11 row 147; Draft; CHANGELOG; DECISIONS 212 (and a note on 208); remove TODO.md:81.

### Must NOT Do

- Evaluate anything while compiling: no JIT, no interpreter, no folding of calls into `.rodata` (decision 4 stands; decision 212).
- Add a `pure` keyword or annotation.
- Allow mutable globals or effects before `main`.
- Change how constants are stored, read or copied (copy-on-write arrays stay as PRP-141 built them).
- Add a crate.

## DESIGN

- **`check/purity.rs`.** While checking each body (functions, methods, default and derived methods, and each constant's value under its `$const.<key>` owner), the checker records per owner:
  - `effects: Vec<(Effect, Span)>` (`Effect::{Intrinsic(name), Os(op), Extern(key), Spawn, Channel, Select, Actor}`, each with a printable reason: "reads files", "prints", "calls C", "starts a task", …);
  - `calls: Vec<(Callee, Span)>`, covering plain calls, method calls, function values, interface conversions and bound calls (with the type parameter, resolved at the walk);
  - `reads: Vec<String>`, the constants read.

  This reuses the hooks that already record `callees`, `instances`, `uses` and `conversions`, and adds non-generic edges.
- **The walk** runs after every body, before `order_consts`. For each constant it does a depth-first search over `(key, type args)`, substituting bound calls as `monomorphize` does. It caches results per instance and reports the first effect with its path. The constants read along the way are added to the constant's `deps`, so `order_consts` and E0446 need only the path text changed.
- **Diagnostics:** E0445 keeps its code, with the new message "a constant can only use pure code" and a note giving the path. The type message (`a constant can't be of type T`) is unchanged. E0446's note shows the path through functions. No new codes.
- **Cost:** the walk visits each reached instance once per program (cached), and the call graph is already built for monomorphization.
- **Security:** no new external input, no `unsafe`, no new dependency, nothing runs at build time. User code that already runs at startup can now be longer, and it can only compute (no I/O), so there is nothing new to review.

## TESTS

- [ ] `check/purity.rs` unit tests: a pure call accepted; each effect kind refused with its reason (print, `read_file`, `$os` through `std.time.now_ms`, `extern`, `spawn`, a channel, an actor); an effect two calls deep reports the path; a path longer than 6 steps ends in `…`; a closure argument naming an impure function is refused; an interface value whose converted type's method prints is refused, a pure one accepted; a generic `total<T: Measure>` accepted at a pure type and refused at an impure one; recursion (pure) accepted; `Ref`, maps and `panic` inside accepted.
- [ ] `check/consts.rs` tests updated: `if`, `match`, blocks, interpolation, `..base` and closures in a value are now accepted; the type rule (E0445 for `Ref<i64>`, `fun(): i64`) unchanged; a dependency through a function orders correctly whatever the source order; a cycle through a function is E0446 with its path.
- [ ] `tests/programs/m147/`: `crc.la` (the OBJECTIVE program, stdout); `generic.la` (generic call and interface call in constants); `order.la` (a constant read only inside a function, declared after its user); `panics.la` (a constant whose function panics: exit 101 and the panic message at the function's line); `impure.la`/`.stderr` (E0445 path); `cycle.la`/`.stderr` (E0446 through a function); `module/` (a constant calling another module's public function and a std one, `math.sqrt`).
- [ ] `tests/explain.rs` still passes (updated pages).
- [ ] `cargo test --test programs` (m147 and the existing m109 constants cases), `--test spec`.

## ACCEPTANCE

- [ ] The OBJECTIVE program prints `crc[1] = 1996959894, mode prod`; `CONFIG` reading a file is E0445 with the path.
- [ ] No program accepted before changes behaviour (m109, m128 cases pass unchanged).
- [ ] `cargo test --lib` and the touched integration tests pass; `cargo fmt --check` and `cargo clippy --all-targets -- -D warnings` are clean; every file ≤ 500 lines.
- [ ] Every `pub` item documented; spec, Draft, CHANGELOG, DECISIONS (212, note on 208), TODO updated.
