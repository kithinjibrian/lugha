## FEATURE: Operator overloading — `+ - * / %` and unary `-` through methods, prelude `Add`…`Neg` and `Num` bounds, bounds on `impl` blocks, `0`/`1` as `T: Num`; milestone 52.

**Status:** merged 2026-10-14 — session 107. Approved with the milestone 52 additions to `Language v15 Draft.md`. The user asked for it "in one go" and raised the file-size limit to 500 lines.
**Milestone:** 52 (one PRP; tag `m52` at merge)
**Spec:** `Language v15 Draft.md` (decisions 8–13, the milestone 52 choices, §10 M52, §11 row 52).

**Decisions:** MEMORY 98 (v15), MEMORY 41–43 (generic checking, compiling generics, interfaces), MEMORY 60 and 63 (generic interfaces as bounds, `Self` in interfaces).

## OBJECTIVE
This runs and prints `Vec2 { x: 4.0, y: 6.0 }` and `Vec2 { x: 3.0, y: 8.0 }`:
```
struct Vec2 { x: f64, y: f64 }
impl Vec2 {
    fun add(self, o: Vec2): Vec2 = Vec2 { x: self.x + o.x, y: self.y + o.y };
    fun mul(self, o: Vec2): Vec2 = Vec2 { x: self.x * o.x, y: self.y * o.y };
}
fun main() {
    let a = Vec2 { x: 1.0, y: 2.0 };
    let b = Vec2 { x: 3.0, y: 4.0 };
    println(a + b);
    println(a * b);
}
```
So does `fun add<T: Add>(a: T, b: T): T = a + b;` for `i64`, `f64`, `string` and `Vec2`. So do a glowworm-style `impl NdArray<T: Num>` with element arithmetic, and `fun sum<T: Num>(xs: T[]): T { let mut s: T = 0; … }`.

## CONTEXT

- **Starting state:**
  - operators are built in;
  - a bare `T` allows no arithmetic;
  - bounds are `Eq`, `Ord` or interfaces, which primitives satisfy only when empty;
  - `impl` parameters take no bounds.
- **Ending state:** the draft's decisions 8–13, implemented.
- **Related existing code:**
  - **Parser:** `src/parser/decl.rs` (`impl` headers), `src/parser/sexp`, `src/ast/mod.rs` (`ImplDecl.params`).
  - **Checker:**
    - operators and bounds: `src/check/{ops,bounds,generics,literal,assign}.rs`;
    - interfaces: `src/check/{satisfy,interfaces}.rs`;
    - methods: `src/check/{methods,method_calls}.rs`;
    - instances: `src/check/{instances,infer}.rs`.
  - **Codegen:** `src/codegen/{arith,expr,methods,instances}.rs`.
  - **Prelude:** `src/prelude*.la`.
- **Open decisions that must be resolved first:** none.

### Design
- **Prelude:** a fifth file, `src/prelude_ops.la`, declares `Add`, `Sub`, `Mul`, `Div`, `Rem`, `Neg` and `Num`. `Num` is an interface listing all six, so as a bound it allows every operator.
- **Checker** (new `check/operators.rs`):
  - **Resolving an operator.** `binary`/`unary` first try the built-in rules as today. If an operand is a struct or enum (or an interface value), they look for the method on the left operand's type. A method qualifies when it has the operator's name and shape: a plain `self` receiver, one parameter (none for `neg`), and a result.
  - **When one qualifies:** the right operand is checked against its parameter (E0403), the result type is its return type, and the call is recorded in a new `Checked.operators: HashMap<ExprId, String>` (the method key), so codegen calls it.
  - **When none qualifies:** E0404, with help naming the needed shape when a method of that name exists.
  - **Through `T` with a bound:** an `Add`/`Num`/… bound allows `+` on `T`, typed by the bound's signature (`Self` → `T`), and is recorded as an operator through a bound.
  - **Compound assignment** goes through the same path.
- **Satisfaction** (a hook in `satisfy.rs`):
  - a number satisfies the six operator interfaces and `Num`, and `string` satisfies `Add`, as built-ins;
  - user types satisfy the single-operator interfaces structurally, as any interface;
  - `Num` is satisfied by built-in numbers only;
  - `5.add(3)` stays E0415.
- **Literals** (hook in `literal.rs`): an integer literal `0` or `1` with expected type `T` where `T: Num` has type `T`; other literals there are E0401.
- **`impl` bounds:**
  - **AST and parser:** `ImplDecl.params` becomes `Vec<TypeParam>`, so `impl NdArray<T: Num>` parses with the function's bound syntax, and the sexp shows bounds.
  - **The block's methods** get the bounds in their `type_params`.
  - **At a method use:** the instance's arguments are checked against those bounds (E0419), in `method_calls.rs` through the existing `meets_bounds`.
  - **Errors:** a bounded `impl` of a non-generic type is E0315, and two `impl` blocks defining one method are E0310 (unchanged).
- **Codegen** (new `codegen/operators.rs`):
  - an expression in `Checked.operators` lowers to a method call: receiver = left operand, argument = right;
  - through a bound, each instance looks at its concrete type: a number or a string lowers to the built-in operator (checked arithmetic as today), and anything else calls that type's method by its symbol;
  - a `0`/`1` literal of type `T` lowers to the instance's constant.
- **Programs without these features** give byte-identical IR.

### File size
`check/ops.rs` (292), `check/satisfy.rs` (298), `check/interfaces.rs` (300), `check/instances.rs` (298), `check/generics.rs` (294), `codegen/arith.rs` (282) and `codegen/methods.rs` (278) are near or at the limit.
- **New code** goes in `check/operators.rs`, `check/impl_bounds.rs` and `codegen/operators.rs`, with one- or two-line hooks.
- **If a hook can't fit** (most likely in `satisfy.rs` or `interfaces.rs`), I stop and propose a specific split for approval before continuing.

## IMPLEMENTATION REQUIREMENTS

### Must Do
- **Code:** everything above.
- **Spec:**
  - status and changes;
  - §3 (`impl` header with bounds);
  - §4:
    - generics: what `T: Num` and the operator bounds allow, and literals;
    - interfaces: built-in satisfaction;
    - `impl` bounds;
  - §5 (operators on user types: shapes, operand types, compound assignment, through bounds);
  - the prelude names list;
  - §9 (lowering);
  - the E0404/E0419/E0315 rows' examples;
  - the §10 "Operator overloading" program;
  - §11 row 52.
- **Docs:** CLAUDE.md, README, TODO, CHANGELOG, MEMORY decision 100, CONTEXT. The tag `m52` is added at the merge.

### Must NOT Do
- **No overloading** of `==`, `<`, the bit operators or indexing.
- **No method overloading** by argument type.
- **No `2.0 * v`.**
- **No methods on built-in types,** no user types satisfying `Num`, and no literals other than `0`/`1` as `T`.

### New dependencies
None.

## ERROR HANDLING REQUIREMENTS
- **E0404:**
  - an operator on a type without a fitting method, with help naming the shape;
  - `"a" - "b"`;
  - `+` on a `T` without an `Add`/`Num` bound.
- **E0403:** the right operand isn't the method's parameter type.
- **E0401:** a literal other than `0`/`1` as a `T: Num`.
- **E0419:** a `T: Num` argument that isn't a number; a bounded `impl` method used on an instance missing the bound.
- **E0415:** `5.add(3)`.
- **E0315:** a bounded `impl` of a non-generic type.

## SECURITY CONSIDERATIONS
- No new input, process, file or network access; no `unsafe`; no new dependencies.

## TESTS TO WRITE

Unit tests — parser: `impl NdArray<T: Num>` and its sexp; plain `impl Stack<T>` unchanged.

Unit tests — checker (`check/operators.rs`, `check/impl_bounds.rs`):
- [x] The Vec2 operators' types; `v * 2.0` with `fun mul(self, k: f64)`; `-v`; `+=`.
- [x] `add<T: Add>` over `i64`, `f64`, `string` and `Vec2`.
- [x] `sum<T: Num>` with `0`.
- [x] Each error above, including a `mut self` `add` not enabling `+`.
- [x] A bounded `impl` method on `NdArray<string>` (E0419) while an unbounded block's methods still work.

Unit tests — codegen: `Vec2 + Vec2` calls `lugha_fn_Vec2.add`; `add<i64>` uses the built-in checked `add` with no call.

Acceptance — `tests/programs/m52/`:
- [x] `operators.la`: the §10 program, starting with the objective.
- [x] `more.la`:
  - an enum with `add`;
  - a generic struct with a bounded `impl` and element arithmetic (a tiny NdArray);
  - `string` through `T: Add`;
  - overflow still panicking through `add<i64>`.
- [x] `e0404.la` and `e0419.la`: reject goldens.
- [x] The §10 program runs from the spec.

Regression:
- [x] Every earlier test passes, and earlier programs give byte-identical IR.

## ROLLBACK PLAN

- Branch `prp-081-operator_overloading`, merged into `main` after CI passes; tag `m52`.
- To abandon: delete the branch.

## ACCEPTANCE CRITERIA
- [x] The objective's program prints exactly `Vec2 { x: 4.0, y: 6.0 }` and `Vec2 { x: 3.0, y: 8.0 }`.
- [x] The `m52/` programs pass at `-O0`, and at `-O2` when checked by hand.
- [x] The spec is at milestone 52; docs updated.
- [x] No file over 300 lines; no new dependencies; no `unsafe`.
- [x] `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` and `cargo test` pass, and CI is green.

## VALIDATION
```bash
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo run -q -- run tests/programs/m52/operators.la
```

## AMENDMENTS (during build, session 107)

- **The operator is the method call's id.** The checker calls the method-call core (`on_value`, now `pub(super)`) with the operator expression as the call, so instances, bound uses and `callees` need nothing new. There is no `Checked.operators`: codegen decides by the operand's concrete type, so a `T` body is a built-in operator at numbers and strings and a method call at structs and enums.
- **Codegen** calls the method directly for structs and enums (`operator_on`, a pointer to a copy as the receiver; for `+=` the place is loaded once), and through `method_call` with a synthetic callee for interface values.
- **A literal facing a `T: Num` operand** (`a - b > 0`) takes type `T` (`second_operand` in `ops.rs`, split out of `operands`).
- **E0315 for a bounded `impl` of a non-generic type** isn't needed: such an `impl` has parameters the type lacks, which is E0418 as before.
- **`Num`'s E0419** has its own help ("only the number types are `Num`").
- **Unsigned numbers satisfy `Neg` and `Num`:** `-x` at `u8` is a checked `0 - x`, so it panics unless `x` is 0.
- **File-size limit:** raised to 500 lines by the user; no file needed splitting.
- **IR:** the 168 earlier programs that compile give byte-identical IR. `-O2` was checked by hand for `m52/operators` and `m52/more`.
