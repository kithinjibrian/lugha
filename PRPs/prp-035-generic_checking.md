## FEATURE: Checking generics — type parameters, `Eq`/`Ord` bounds, inference, generic bodies checked once.

**Status:** merged 2026-10-13 — session 44
**Milestone:** 11 (first of two PRPs: **035 checking**, 036 compiling — monomorphization, `Option<T>`, gate removed, spec folded)
**Spec:** `Language v2 Draft.md`:
- decisions 3–5;
- §3 (type parameters, bounds, type arguments);
- §4 ("Generic declarations", "Using a generic", "Generic recursion");
- §6 ("Generic functions");
- §9 (E0315, E0316, E0418, E0419).

The `Language v2 Specification.md` sentence on generics changes (below); the full fold is PRP-036.
**Decisions:**
- MEMORY 33 (v2 direction), MEMORY 40 (bounds stay on declarations; interfaces are M12);
- MEMORY 34 (gate), MEMORY 35 (`==` on every type, string order);
- MEMORY 39 (keys, side tables).

## OBJECTIVE
`lughac check` accepts generic programs and reports their mistakes.
- **Type parameters:** on functions, structs, enums and methods, with `Eq`/`Ord` bounds.
- **Inference:** type arguments are inferred from arguments and the expected type, never written at a use.
- **Checked once:** a generic body is checked once with its parameters abstract.
- **Bounds at a use:** a type argument that doesn't satisfy its bound is an error at the use.
- **Still gated:** `build` and `run` stop with "not implemented yet: generics (milestone 11)" until PRP-036 compiles them.

## CONTEXT

- **Starting state:**
  - PRP-030 parses all generic syntax, and the gate stops it before checking.
  - The checker has no type parameters: `Type::Struct(key)` and `Type::Enum(key)` name concrete types, and `Option<T>` is built in.
- **Ending state:**
  - **Types:** generic types carry their arguments, and type parameters are a kind of type.
  - **Checking:** declarations, generic bodies, inference at calls, literals, constructions and methods all work.
  - **Errors:** E0315, E0316, E0418, E0419, E0412 (extended) and E0404 (with a help hint).
  - **Side table:** the type arguments of every generic call, for PRP-036.
  - **Gate:** it moves from "before checking" to "before codegen".
- **Related existing code:**
  - `src/check/` (`types`, `env`, `structs`, `struct_literal`, `enums`, `methods`, `method_calls`, `call`, `ops`, `patterns`, `access`, `modules/`, `errors/`);
  - `src/codegen/` (matches on `Type` only);
  - `src/driver/{pipeline,gate}`;
  - `src/ast/header.rs` (`TypeParam`).
- **Open decisions that must be resolved first:** none.

### Discovery answers (session 44)
1. **M11 split:** two PRPs. This one is checking; PRP-036 is monomorphization, `Option<T>` as an ordinary generic enum, the gate removed and the spec folded, which completes milestone 11.
   - Between the two, `lughac check` accepts generic programs, and `build`/`run`/`--emit=ir` stop at the gate.

### Design choice for review (internal representation)
- **Types:** `Type::Struct(key, args)` and `Type::Enum(key, args)` carry their type arguments; `args` is empty for a non-generic type. `Type::Param(name)` is a type parameter inside generic code.
- **Display and messages:** a type prints as written, `Stack<i64>` or `Stack<T>`, so every existing message is unchanged.
- **Tables:** the struct and enum tables keep each declaration once, with field and payload types that mention its parameters. Reading a field or payload of `Stack<i64>` substitutes `T := i64`.
- **The alternative:** a string key per instance (`"Stack<i64>"`) would make codegen parse type names back apart to monomorphize.
- **Cost:** the 54 existing `Type::Struct`/`Type::Enum` matches in 21 files change mechanically.

## IMPLEMENTATION REQUIREMENTS

### Must Do

**Declarations** — new `check/generics.rs`
- **Recording parameters:** a struct's, enum's, function's and method's parameters are recorded with their bounds. A method sees its `impl`'s parameters (`impl Stack<T>`, named in the type's declared order and count; a mismatch is E0418) plus its own.
- **Bounds:** `Eq`, `Ord`, or both. Anything else is **E0315** "unknown bound `Hash`".
- **E0316, bad parameter names:**
  - a duplicate in one declaration;
  - a name like a type, an imported module, or a parameter of the enclosing `impl`.
- **Reserved names:** `Eq` and `Ord` join the built-in names, so declaring them is E0302.
- **Inside a declaration:** its parameters are types. `T` resolves to `Type::Param("T")`.

**Types**
- **`Name<args>` in a type position:** resolves through keys and modules as today. Too many or too few arguments is **E0418**; so is a bare generic name in a type position (`s: Stack`), with help showing the parameters.
- **Bounds on arguments:** each argument must satisfy its parameter's bounds, or it is **E0419** "`i64[]` doesn't satisfy `Ord`":
  - a concrete type is always `Eq`, and `Ord` when it is a number or `string`;
  - a parameter satisfies exactly its own declared bounds.
- **Field and payload types** of a generic instance are substituted.

**What a generic body can do with `T`** (draft §4)
- **Allowed:** store, move, copy, pass, return, and put it in arrays, structs and enums.
- **Operators:** `==`/`!=` need `T: Eq`, and `<`… need `T: Ord`. Otherwise the existing **E0404** is reported, with help "declare `T: Eq`".
- **Never allowed, through existing errors:** arithmetic and unary operators (E0404), casts (E0408), printing (E0403), `.len`, fields (E0410), indexing and `for … of` (E0411).
- **`T` fits only itself:** `let x: T = 0;` is E0401, as for any literal of the wrong kind.

**Inference** — new `check/infer.rs`, used by calls, associated functions, struct literals and variant constructions
- **Per use:** each generic use gets its own binding map from the callee's parameters to types. `unify(declared, actual)` binds parameters structurally, through arrays, `Option` and type arguments; a clash is the existing mismatch error at that argument.
- **Order:**
  1. the expected type, unified with the declared result (so `let s: Stack<string> = Stack.new();` works);
  2. the arguments that can be typed alone, each checked with the bound parts of its parameter as its expected type;
  3. the arguments that defer (numeric literals, `None`, `[]`, as in PRP-031), with what is bound by then. So `max(x_u8, 3)` makes `3` a `u8`.
- **Defaults:** a parameter bound only by a literal takes the literal's default (`i64`, `f64`).
- **Undecided:** a parameter still unbound is **E0412** "cannot infer `T`", extended, at the use, with help to annotate.
- **Bounds:** they are checked on the inferred arguments (**E0419**, at the use).
- **Methods:** a receiver's type arguments bind its `impl`'s parameters (`s.push("a")` on `Stack<string>`), and the method's own parameters are inferred as for functions.
- **Side table:** `Checked.instances: HashMap<ExprId, Vec<Type>>` holds the type arguments of every generic call, associated call, struct literal and construction, in declaration order. Codegen will monomorphize from these in PRP-036.

**Patterns and `==`**
- **Patterns:** a variant pattern on `Tree<i64>` has payload types substituted.
- **Equality:** `==` on `Pair<i64, string>` is deep, as today, and is allowed in generic code under `T: Eq`.

**Gate**
- The `generics` gate runs before codegen instead of before checking. `check` succeeds on a valid generic program; `build`, `run` and `--emit=ir` exit 2 with "not implemented yet: generics (milestone 11)".

**Spec** — `Language v2 Specification.md`
- The "Changes since v1.2" sentence on generics becomes: "Generics (milestone 11) parse and are checked by `lughac check`; `build` and `run` stop at them with 'not implemented yet (milestone 11)', exit 2."
- E0315, E0316, E0418 and E0419 join §9's table, marked "(v2, milestone 11)".
- The generic rules themselves fold in with PRP-036, once programs run.

**Docs**
- CLAUDE.md:
  - the tree (`check/generics.rs`, `check/infer.rs`);
  - the KNOWN ISSUES gate line ("generics check but don't compile yet").
- TODO, CHANGELOG, MEMORY decision 41, CONTEXT.

### Must NOT Do
- No monomorphization, generic symbols, E0420, or `Option<T>` rework (PRP-036).
- No interfaces (M12), explicit type arguments, or bounds on `impl` headers.
- No change to non-generic programs' behaviour, messages or symbols.
- No `unsafe`, no new dependencies.

### New dependencies
- None.

## ERROR HANDLING REQUIREMENTS

- **Diagnostics:** E0315, E0316, E0418, E0419, E0412, E0404 and E0302 are records, and checking continues.
- **One mistake, one diagnostic:** a use that fails inference has type `Error`, so it doesn't cascade.

## SECURITY CONSIDERATIONS

- **Bounded unification:** it recurses only as deep as the written types, which the parser already bounds (E0206). No instantiation happens in this PRP, so nothing can grow without bound.
- **No `unsafe`.**

## TESTS TO WRITE

Unit tests — `check/generics.rs`:
- [x] **Accepted declarations:** `fun max<T: Ord>`, `struct Pair<A, B>`, `enum Tree<T: Eq>`, `impl Stack<T>` with methods, and a method with its own `<U>`.
- [x] **E0315** for `T: Hash`.
- [x] **E0316** for:
  - `<T, T>`;
  - a parameter named like a struct or a module;
  - a method's parameter named like its `impl`'s.
- [x] **E0302** for `struct Eq {}`.
- [x] **E0418:** for `Pair<i64>`, `Stack<i64, i64>`, a bare `Stack` parameter, and `impl Stack<T, U>` on a one-parameter `Stack`.
- [x] **E0419:** for `struct Set<T: Ord>` used as `Set<i64[]>`; `Set<string>` is fine; `Set<U>` with `U: Ord` in scope is fine, and with an unbounded `U` it is E0419.
- [x] **Generic bodies:**
  - `a == b` without `Eq` is E0404, with help naming `T: Eq`;
  - `a < b` without `Ord` is E0404;
  - `a + b`, `a as i64`, `println(a)`, `a.len` and `a[0]` are their usual errors;
  - `let x: T = 0;` is E0401.

Unit tests — `check/infer.rs`:
- [x] **From arguments:** `max(3, 7)` is `i64`; `max(x_u8, 3)` is `u8`; `first(["a"])` is `Option<string>`.
- [x] **From the expected type:** `let s: Stack<string> = Stack.new();` and `let o: Pair<i64, f64> = Pair { a: 1, b: 2.0 };`.
- [x] **From fields and payloads:** `Wrap { v: 2.5 }` is `Wrap<f64>`, and `Tree.Node(1, Tree.Leaf)` with an expected `Tree<i64>`.
- [x] **Methods:** `s.push(1)` on `Stack<string>` is E0401 at `1`; `s.pop()` is `Option<string>`.
- [x] **E0412** for `let e = Stack.new();`, and **E0419** for `largest([[1]])`.
- [x] **Clashes:** `max(1, "a")` is E0401 at `1` (amended: see below).
- [x] **Side table:** `instances` records `[i64]` for `max(3, 7)` and `[string]` for `Stack.new()` with an expected `Stack<string>`.

Integration — `tests/cli.rs` (the existing gate test changes):
- [x] `check` on the draft's generics program exits 0.
- [x] `build`/`run` on it exit 2 with the milestone 11 message.
- [x] `--emit=ast` still works.

Acceptance — `tests/programs/m11/` (`check`-only goldens, through the reject mode):
- [x] `e0419.la`, `e0418.la` and `e0404_eq.la` (an `==` without `Eq`). Captured and reviewed.

Regression:
- [x] Every earlier test passes, with messages unchanged for non-generic programs.

## ROLLBACK PLAN

- Branch `prp-035-generic_checking`, merged into `main` after green CI.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] Generic programs check: the draft's generics program passes `lughac check`, and the errors above are reported.
- [x] `build`/`run` stop at the gate; non-generic programs are unchanged.
- [x] The spec sentence and codes are updated; CLAUDE.md, TODO, CHANGELOG, MEMORY updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` pass; CI green.

## VALIDATION
- `cargo fmt --check`
- `cargo clippy --all-targets -- -D warnings`
- `cargo test`
- `lughac check` on the draft's generics program → exit 0

## AMENDMENTS (during build, session 44)

- **Clash location:** `max(1, "a")` is E0401 at `1`, not E0403 at `"a"`. The inference order above decides `T` from the string first, because the literal defers. The checklist had contradicted that order.
- **No cascade:** when the expected type already failed (E0418 or E0419 on the annotation), an undecided parameter is not reported again as E0412.
- **Files (splits approved):** `check/construct.rs` holds construction (`E.V`, `Some`, `None`), moved out of `enums.rs`. `check/bounds.rs` holds `Eq`/`Ord` and E0419, moved out of `generics.rs`. `check/errors/generics.rs` holds the new codes' constructors, so `errors/types.rs` stays under 300 lines.
- **Test added:** a unit test checks the draft's own generics program (`infer.rs`), alongside the CLI test.
