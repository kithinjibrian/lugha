## FEATURE: The unit type `()` — a real type with one value, replacing `void`; milestone 107 (v17).

**Status:** proposed 2026-10-17 (session 254), after discovery in the same session (the user: "add unit type"; Q1 "yes be a real type and value"; Q2 "warn").

## OBJECTIVE
```lugha
fun save(path: string): Result<(), string> {
    if path == "" { return Err("no path"); }
    Ok(())
}

let done: Chan<()> = Chan.new();
done.send(());
let t: Task<()> = spawn log("hi");

let x = log("hi");      // warning W0106: `x` holds `()`; drop the `let`, or write `let x: () = …`
```
`()` is a type and its only value. A function without a return type returns `()`; `void` leaves the language. Unit works everywhere a type does — generics, variables, fields, arrays, `Result`, `Task`, `Chan`, maps — and binding a call's `()` to a name warns.

## CONTEXT
- Today `void` is not a value (spec §4 "void is not a value"): no variable, parameter, field, element or type argument holds it; E0407 for a use as a value. So `Result<(), E>`, `Task` of a void call's result and `Chan` of a signal need a dummy type.
- Tuples (v14) are `$tuple<n>` structs; `()` is the empty tuple in spelling but is its own type (no `$tuple0`).
- Related: `src/check/types.rs` (`Type::Void`, 118 uses in 54 files), `src/check/errors/types.rs` (E0407), `src/parser/{tuples,primary}.rs` (`()` as a type and an expression), `src/codegen/value.rs` (LLVM types), `src/codegen/{show,equality,hashing,copy}.rs`, spec v17 §3, §4, §6, §8, §9.
- Open decisions: none.

## DECISIONS (draft decisions 114–122)
114. **`()`** is a type and its one value (the user, Q1). `Type::Void` becomes the unit type (renamed `Type::Unit`, printed `()`); omitting a return type means `: ()`; `return;` means `return ();`; a block without a tail, an `if` without `else`, loops and assignments have type `()`.
115. **Writing it:** `()` as a type (`Result<(), E>`, `fun f(): ()`, `let x: () = …`) and as an expression. `void` is not a keyword and stays unwritable; the spec's `void` becomes `()`.
116. **A value like any other:** variables, parameters, fields, array elements, type arguments, closures' results, `Option<()>`, `Map<K, ()>`. `Eq`, `Ord` (always equal), hashable, `Display`: prints `()`; interpolates as `()`.
117. **No memory:** codegen lowers `()` to LLVM's empty struct `{}`; it is never loaded or stored (fields and elements of unit take no bytes; `()[]` keeps only its length). Functions returning `()` stay `void` in LLVM; calls produce the constant `{}` where a value is needed.
118. **E0407 withdrawn:** a `()` value is a value. The code is retired, never reused; its page says so.
119. **W0106 (new):** `let x = e` (a name or pattern binding, no written type) where `e` has type `()` and is not the literal `()` — usually a mistake. Silenced by writing the type: `let x: () = e`. Not for parameters, fields, or `_`.
120. **C boundary:** an extern with no result is C's `void` as before; `()` as an extern parameter, field or callback parameter is E0409 / E0437. A callback returning `()` is C's `void`.
121. **Formatter:** keeps `: ()` if written; prints `()` literally.
122. **Success:** milestone 107, tag `m107`; spec §3/§4/§6/§8/§9, row 107; VS Code highlights nothing new (no keyword).

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Parser: `()` as a type (`TypeExpr::Unit`) and expression (`ExprKind::Unit`); s-expressions `()`; formatter.
- Checker: `Type::Void` → `Type::Unit` with value semantics (remove the "no value" checks), `Eq`/`Ord`/hash/print built in; W0106; withdraw E0407 (keep its page as retired).
- Codegen: `{}` values, zero-size layout, printing `()`, equality true, hash constant, copies none; calls of `()` functions yield `{}`.
- Spec, error pages (W0106, E0407 retired), CHANGELOG, LSP hover shows `()`.
### Must NOT Do
- A `void` keyword; unit at the C boundary; any change to tuples.
### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- W0106 (new). E0407 no longer reported. E0409/E0437 for `()` across C.

## SECURITY CONSIDERATIONS
- None: no new I/O, unsafe or C surface.

## TESTS TO WRITE
- [ ] Parser units: `()` type and expression; `(a)` stays a parenthesised expression; `(a,)`/tuples unchanged.
- [ ] Checker units: `Result<(), E>`, `Ok(())`, `let x: () = log()`, `()[]`, `Option<()>`, `fun f(): ()` with and without a tail, `return;`/`return ();`, W0106 cases and silences, E0409 for an extern `()` parameter.
- [ ] `tests/programs/m107/`: `Result<(), string>` with `?`, `Chan<()>` signals, `Task<()>` join, `Map<string, ()>`, printing `()` and `[(), ()]`, `==`, the W0106 warning's stderr.
- [ ] Formatter rule; `fmt_corpus` and `tests/explain.rs` (pages).
- [ ] LSP hover shows `()` for a unit binding.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE program compiles and runs; `let x = log()` warns W0106 only.
- [ ] Narrow tests (the areas touched, plus `--test programs` for m107 and the v1 programs that used E0407), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --lib -- unit void`, `cargo run -q -- run tests/programs/m107/*.la`, `cargo test --test explain --test fmt_corpus --test lsp_hover`, fmt, clippy
