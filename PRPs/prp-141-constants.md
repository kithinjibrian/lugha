## FEATURE: Global constants — `const NAME: T = expr;` at the top level of a module; milestone 109 (v17).

**Status:** approved 2026-10-17 (session 261); proposed in session 260, after discovery in the same session (the user: "what about global const and statics"; Q1: "constants only").

## OBJECTIVE
```lugha
const MAX_PLAYERS: i64 = 8;
private const LIMIT: u64 = u64.MAX / 2;
public const PRIMES: i64[] = [2, 3, 5, 7];
const ORIGIN: Point = Point { x: 0, y: 0 };

fun main() {
    println(MAX_PLAYERS * 2);
    let mut ps = PRIMES;     // copy-on-write: PRIMES never changes
    ps.push(11);
    println(shapes.UNIT);    // another module's constant
}
```

## CONTEXT
- Today a module holds only functions, types, externs and imports; nothing at the top level holds a value (spec §6 "no top-level variables").
- Related: `src/parser/item.rs` (items), `src/ast/mod.rs` (`Item`), `src/check/env.rs` (globals, names), `src/check/levels.rs`/`visibility.rs` (E0313), `src/check/modules/lookup.rs` (`m.f`), `src/check/named_imports.rs`/`globs.rs` (re-exports), `src/codegen/lower.rs` (C `main`), `src/check/assign.rs` (places), LSP hover/definition/references/completion.

## DECISIONS (draft decisions 129–140)
129. **Constants only** (the user, Q1): no mutable statics — actors and green threads share nothing; shared mutable state stays in `Ref` or an actor.
130. **Syntax:** `[private | public] const NAME: T = expr;` at the top level. The type is always written. Any name; no case rule.
131. **Visibility:** as functions (v8): `private` = this module, none = this package, `public` = other packages; `m.NAME` from other modules; `import m { NAME }`, `as`, `public import m { NAME }` and `{ * }` re-export it. E0313 for one not visible.
132. **Constant expressions:** literals (numbers, `bool`, `char`, strings, `()`), other constants, `I.MIN`/`I.MAX`, unary/binary operators and `as` casts on those, and array (list and repeat), struct, tuple, `Some`/`None`, `Ok`/`Err` and enum-variant literals of constant expressions. No calls, closures, interpolation, `if`/`match`/blocks, `#{…}`, or locals: E0445 (new) at the first such part.
133. **Types:** any type such values have: numbers, `bool`, `char`, `string`, `()`, arrays, structs, tuples, enums, `Option`/`Result` of those. Not functions, interfaces, `Ref`, `Task`, `Chan`, actors, handles, `CPtr`, maps or sets (E0445 on the type).
134. **Order and cycles:** constants may use each other in any order; a cycle (`const A: i64 = B; const B: i64 = A;`) is E0446 (new), naming the path.
135. **Evaluation:** once, before `main`, in dependency order (across modules too); overflow, division by zero or a bad shift panics then, at the constant's expression, as at run time. Each constant is a global `lugha_const_<module path>/<NAME>` (new symbol prefix, added to architecture rule 7). An array (or a value holding arrays) is marked shared after it's built, so copy-on-write keeps it unchanged: `let mut xs = PRIMES; xs.push(1);` copies.
136. **Uses:** a constant is a value anywhere an expression is (it reads the global); never a place: assigning to it, `mut` arguments, `swap`, `for … of mut` are E0501 (existing, "can't assign to a constant"). A local may shadow it. Not usable as a `match` pattern (later, if wanted).
137. **Names:** a constant shares the value namespace with functions in its module: a clash is E0302/E0303 (existing duplicate-name codes); a constant named like a type is fine.
138. **Tests and the editor:** `@test` functions read constants; hover shows `const NAME: T`, go-to-definition, references and rename work as for functions; completion offers them; the VS Code grammar highlights `const`.
139. **`check` without `main`, libraries:** constants are checked as any item; evaluation happens only in programs (`build`/`run`/`test`).
140. **Success:** milestone 109, tag `m109`; spec §2 (keyword), §3 (grammar), §6 (items), §8 (lowering), rows; E0445/E0446 pages.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- Lexer: `const` keyword (a program using `const` as a name breaks: noted in the spec's compatibility line).
- Parser/AST: `Item::Const { visibility, name, ty, value }`; sexp; formatter.
- Checker: collect constants with the functions; resolve types; check values against them with the constant-expression rule (E0445); dependency order and cycles (E0446); names, visibility, module lookup, imports and re-exports; not a place (E0501).
- Codegen: one global per constant; an init function per program called from C `main` before `lugha_fn_main` (and from test `main`s) evaluating constants in order; reads load the global; shared marking for arrays.
- LSP: hover, definition, references, rename, completion through the existing name machinery.
- VS Code grammar `const` (bump, package, install).
### Must NOT Do
- Mutable statics; constants in patterns; calls in constant expressions; compile-time evaluation beyond what LLVM folds.
### New dependencies
- none

## ERROR HANDLING REQUIREMENTS
- E0445 (new): a part of a constant's value that isn't a constant expression, or a type a constant can't have.
- E0446 (new): constants that depend on themselves.
- Existing: E0403 (value vs type), E0501 (assigning), E0302/E0303 (duplicates), E0313 (visibility), E0301 (unknown name).

## SECURITY CONSIDERATIONS
- None: no I/O, no unsafe, no C surface.

## TESTS TO WRITE
- [ ] Lexer/parser: `const` items with each visibility; sexp; formatter rule.
- [ ] Checker units: types and values; every E0445 case (call, closure, local, `if`, interpolation, `#{}`, a `Ref` type); E0446 for a two- and a three-cycle; E0501 for assignment and `swap`; duplicates; shadowing by a local.
- [ ] `tests/modules.rs`: `m.NAME`, a private constant E0313, `import m { NAME }`, a glob re-export.
- [ ] `tests/programs/m109/`: numbers, strings, arrays (copy-on-write keeps the constant), structs, enums, constants in any order and across modules, an overflow panic at startup (exit 101, location).
- [ ] LSP: hover and go-to-definition on a constant.
- [ ] VS Code grammar test for `const`.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The OBJECTIVE program runs; narrow tests (lib, `modules`, `programs` for m109, `lsp_hover`, `lsp_definition`, `explain`, `fmt_corpus`), fmt, clippy; CHANGELOG.

## VALIDATION
- `cargo test --lib -- const`, `cargo test --test modules --test explain --test lsp_hover --test lsp_definition`, m109 programs at -O0/-O2, `npm test`, fmt, clippy
