## FEATURE: `and_then` on `Option` and `Result` — chain steps that may each fail; milestone 118 (v17).

**Status:** proposed 2026-10-18 (session 281), from the user's request ("Option.and_then: from the TOML work"); `std.toml`'s first example wanted `toml.get(doc, "app.name").and_then(toml.as_text)`.

## OBJECTIVE
```lugha
let name = toml.get(doc, "app.name").and_then(toml.as_text);     // Option<string>
let port = read_file("port").and_then(fun(t: string): Result<i64, string> = parse_i64(t.trim()).ok_or("not a number"));
```

## CONTEXT
- `Option` and `Result` helpers are prelude functions reached through a fixed list (`check/helpers.rs`: `option_*`, `result_*`; spec §5's helpers table): `map`, `ok_or`, `unwrap_or`, `map_err`, … — no `and_then`.
- Prelude panics report `<prelude>:line:col`, and tests expect some of those lines: new prelude functions go at the end of the last prelude file (`maps.la`) so no line moves.

## DECISIONS (draft decisions 237–240)
237. **`o.and_then(f: fun(T): Option<U>): Option<U>`**: `f` of the value, or `None` for `None`.
238. **`r.and_then(f: fun(T): Result<U, E>): Result<U, E>`**: `f` of the `Ok` value, or the `Err` unchanged (the same `E`).
239. **Written in Lugha** (`option_and_then`, `result_and_then`), listed in `check/helpers.rs` and the completion candidates; the closure's parameter is typed from the receiver as for `map` (an untyped `fun(x) = …` works).
240. **Success**: milestone 118, tag `m118`; spec §5's table; `std.toml`'s docs and the spec's `std.toml` paragraph switch back to `.and_then(toml.as_text)`; CHANGELOG.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- The two prelude functions (at the end of `src/prelude/maps.la`, doc comments), the helper lists, the completion candidates (`check/member_names.rs`), spec, `std.toml` docs.
### Must NOT Do
- Other combinators (`or_else`, `filter`, `flatten`, …) — later if wanted.
### New dependencies
- none.

## ERROR HANDLING REQUIREMENTS
- Type errors as for `map`: `f` returning another type than `Option<U>` / `Result<U, E>` with the same `E` is E0403.

## SECURITY CONSIDERATIONS
- None.

## TESTS TO WRITE
- [ ] Checker units: types of both, with a named function and an untyped closure; E0403 for `f` returning `U` (not `Option<U>`) and for a different error type.
- [ ] A program: `Some`/`None` chains, `Ok`/`Err` chains (the `Err` passed through unchanged), `toml.get(...).and_then(toml.as_text)`; completion offers `and_then` on both.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] Both work as specified; narrow tests (checker units, the program, `lsp_completion`, `programs`), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --lib -- and_then`, `cargo test --test programs --test lsp_completion`, fmt, clippy
