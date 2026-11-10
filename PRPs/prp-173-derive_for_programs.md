## FEATURE: Derive blocks for every module, and a type's own methods overriding a derived interface (custom JSON per type); milestone 142, v17.

**Status:** approved 2026-10-31 ("build"); proposed 2026-10-31, after discovery (5 questions, each answered with the recommended option): (1) `derive` is a contextual word, not a reserved keyword, and a type's own methods win over a block's (asked in conversation); (2) scope: programs write derive blocks for their own interfaces, `Json`'s items become nameable so a type can customise its JSON; printing, `==` and hashing stay built in; (3) overriding is per item, like default methods; (4) a type that doesn't meet a derivable interface is a new E0451 with a path (E0450 stays for `json.Json`); (5) block mistakes are a new E0452, item mismatches E0422, intrinsic misuse a new E0453; the intrinsics are written `derive.fields(…)` and so on.

Follows PRP-172 (milestone 141), which built derive blocks for bundled sources only; DECISIONS.md 208 left these two questions open.

## OBJECTIVE
```lugha
import std.json;

/** Anything that can describe itself on one line. */
interface Show { fun show(self): string; }

derive Show for i64 { fun show(self): string = "{self}"; }
derive Show for string { fun show(self): string = "\"{self}\""; }
derive<T: Show> Show for T[] {
    fun show(self): string = "[" + join(self.map(fun(x: T): string = x.show())) + "]";
}
derive Show for struct {
    fun show(self): string = "{{" + join(derive.fields(self, (), part)) + "}}";
}
derive Show for enum {
    fun show(self): string {
        let (name, parts) = derive.variant(self, (), value);
        if parts.len == 0 { name } else { "{name}(" + join(parts) + ")" }
    }
}

fun part<F: Show>(c: (), name: string, x: F): string = "{name}: {x.show()}";
fun value<F: Show>(c: (), name: string, x: F): string = x.show();
fun join(xs: string[]): string = xs.join(", ");

struct Date { y: i64, m: i64, d: i64 }
impl Date {
    /** Dates are JSON strings, not objects: this replaces the derived `to_json`. */
    fun to_json(self): json.Value = json.Value.Text("{self.y}-{self.m}-{self.d}");
}

enum Shape { Dot, Circle(i64) }
struct Event { title: string, on: Date, shapes: Shape[] }

fun main() {
    let e = Event { title: "launch", on: Date { y: 2026, m: 10, d: 31 }, shapes: [Shape.Dot, Shape.Circle(2)] };
    println(e.show());
    // {title: "launch", on: {y: 2026, m: 10, d: 31}, shapes: [Dot, Circle(2)]}
    println(json.stringify(json.encode(e)).unwrap());
    // {"title":"launch","on":"2026-10-31","shapes":["Dot",{"Circle":2}]}
}
```
A library can now say once how its interface applies to every struct and enum, and a program can override that for one type.

## CONTEXT
- Read first: PRP-172 and its BUILT notes; `src/parser/derive.rs`, `src/check/derive.rs`, `derive_meets.rs`, `derive_intrinsics.rs`, `src/codegen/derive.rs`; `src/std/json_value.la`'s blocks; default methods (`check/defaults.rs`, spec §4 "Default methods"), whose per-item override rule this copies.
- Changes: `src/parser/derive.rs` and `item.rs` (`derive` as a contextual word at an item's start; `$derive` goes away, std is rewritten to `derive`); `src/check/derive*.rs` (programs' blocks, override, E0451–E0453); a new `src/check/errors/derive.rs` (the builders); `src/check/instances.rs` and `src/codegen/derive.rs` (an own method wins); `src/formatter/items.rs` (prints `derive`); `src/std/json_value.la` (`Json`'s items renamed); `src/driver/lsp/` (hover on a block's items and on `derive.fields`, completion of `derive.` inside a block); `docs/errors/E0451.md`, `E0452.md`, `E0453.md`.
- Spec: §2 (`derive` contextual, like `type` after `extern`), §3 (`derive_decl`), §4 (derivable interfaces: blocks, satisfaction, per-item override, `json.Json` as one), §5 "Intrinsics" (the five `derive.` calls), §6 (where a block may be written), §9 (E0451–E0453; symbols unchanged from PRP-172), `std.json` (custom JSON per type), §11 row 142, "Changes since v16". The v17 Draft gets the milestone before the build.
- Decisions: 208 (resolves its two open questions; recorded as a new core decision), 33 and 41 (bodies checked once: unchanged), 3, 6. None open.

## REQUIREMENTS

### Must Do
- **`derive [<params>] I for X { items }`** in any module, where `I` is an interface declared in that module (else **E0452**). `X` is a type written with the block's parameters, or `struct`, `enum` or `tuple`. `derive` means this only at the start of an item followed by `<` or a name; everywhere else it is an ordinary name (`let derive = 1;`, `fun derive()`, a field `derive` all keep working).
- **Blocks don't overlap:** two blocks of one interface that could cover the same type (`Option<T>` and `Option<i64>`, or two `for struct`) are **E0452**, naming the type.
- **Items:** a block defines exactly `I`'s items with `Self` replaced by `X`; a missing, extra or mismatched item is **E0422** (as for any type that doesn't satisfy an interface), at the block. A block's items aren't visible as methods of `X` (`5.show()` stays E0415); they are reached only through the interface (a bound).
- **Meeting a derivable interface:** as PRP-172 (the covering block's parameters meet their bounds; a struct, enum or tuple block's parts meet `I` in turn, recursive types included). A miss is **E0451**: `` `Job` doesn't meet `Show`: its field `run` is `fun(i64): i64`, which no `derive Show` block covers ``, with help listing the types `Show` has blocks for. `json.Json` keeps **E0450** and its wording.
- **Override, per item:** a struct or enum whose own method or associated function has an item's name **and exact signature** uses it for that item; the block supplies the rest. A method of that name with another signature is **E0422** where the type must meet the interface, as for default methods. Built-in types have no methods, so their blocks always apply.
- **Field intrinsics** are written `derive.fields(x, ctx, f)`, `derive.build(ctx, f)`, `derive.variant(x, ctx, f)`, `derive.variants()`, `derive.build_variant(name, ctx, f)`, with PRP-172's types; `f` names a function of the same module generic over one `F: I`. Inside a derive block `derive.` always means them (a local named `derive` there is E0453); misuse — outside a block, the wrong kind of block, an `f` of the wrong shape — is **E0453**.
- **JSON:** `Json`'s items become `to_json(self): Value` and `from_json(v: Value, path: string): Result<Self, DecodeError>` (`$missing` and `$JsonKey` stay internal: a missing field and map keys aren't customised). `json.encode`/`json.decode` call them, so a type's own `to_json`/`from_json` replace the derived ones. Without such methods, every output stays byte for byte.
- Formatter, language server (hover on blocks, their items and the intrinsics; `derive.` completion inside a block), `lughac explain` pages for E0451–E0453.
- Milestone 142; tag `m142`.

### Must NOT Do
- Move printing, `==` or hashing onto derive blocks (a later PRP).
- Let a module write a block for an interface it doesn't declare (no orphans), or specialise one block over another.
- Reserve `derive`, `fields`, `build` or any other name in programs.
- Add attributes such as `@json(rename)` (TODO's "JSON, later").

## BUILT — milestone 142
- As planned, with these additions and differences:
  - **A type that writes every item itself needs no block**, so its fields needn't meet the interface (`impl Secret { fun show(self): string = "****"; }` with an `i64` field and no `i64` block). Found writing E0452's page; it is ordinary interface satisfaction. With fewer than all items, the type must still be covered, as the PRP said.
  - `derive.` outside a block means the intrinsics only when no local or import is named `derive`: `fun f(derive: D): i64 = derive.variants();` keeps calling a program's function field. Codegen reads the checker's `Checked::derive_calls` instead of the syntax.
  - A block counts as in its interface's module only if the interface is declared there: a named import (`import shapes { Show };`) is E0452, compared by `qualify(module, name)`.
  - Two non-overlapping blocks for one generic type at different arguments (`Box<i64>`, `Box<string>`) get distinct hidden owners (the second named by its whole type).
  - Hover: a block's item shows as `fun Show.show(self): string`; the intrinsics show their call's type like any expression (no page of their own). Completion: `derive.` offers the five intrinsics inside a block, also while the block doesn't parse (a text fallback: the nearest line starting `derive` with its braces still open).
  - `check/derive.rs`'s tests moved to `check/derive_tests.rs` (399 + 127 lines); `driver/lsp/completion.rs` (495) gave `enclosing` to the new `driver/lsp/derive_completion.rs`.
- Tests: `tests/programs/m142/` (`show.la`: a program's `Show` over integers, strings, arrays, generic and nested structs, tuples and a recursive enum, plus `Date`'s own `to_json`/`from_json` round trip and its error path, same at `-O2`; `not_shown`, `bad_block`, `bad_block_module/`, `bad_fields`, `json_clash`); unit tests for the contextual parse, overlap, override, each code, and completion; `tests/fmt.rs`, `tests/lsp_hover.rs`, `tests/lsp_completion.rs` one case each; E0451–E0453 pages pass `tests/explain.rs`. m139–m141 unchanged.
- Found, not fixed: `tests/json.rs` is 523 lines (522 since milestone 140), over the limit; a split is in TODO for the user to approve.

## DESIGN
- **Breaking change:** a program whose struct or enum already has `fun to_json(self): json.Value` (or `from_json` of `Json`'s shape) now gets it used by `json.encode`/`decode`; one with a `to_json` of any other shape gets E0422 where it is encoded. Listed under **Breaking** in the CHANGELOG. Today no program can call `json.Json`'s items, so nothing else changes.
- **Dispatch:** `Derives::target` takes a second closure, "does this type have its own item", used identically by the checker's `monomorphize` and codegen's `derive_method`, so the two can't disagree; an own method then goes through the ordinary method path.
- **New diagnostics:** E0451 (`check/derive_meets.rs`, the path search generalised; JSON's wording stays in `check/json.rs`), E0452 (`check/derive.rs`: wrong module, overlap), E0453 (`check/derive_intrinsics.rs`). Each with a `docs/errors/` page and `tests/explain.rs` coverage. The bundled-only E0201/E0415 messages from PRP-172 become these codes.
- **Overlap check:** two exact patterns overlap when one matches the other with parameters free; two kind blocks of the same kind always do.
- **DECISIONS.md:** a new core decision recording both answers (contextual `derive`, per-item override) and closing 208's open list.
- Sizes: parser +30, checker +250 across the derive files and `errors/derive.rs`, codegen +20, LSP +80, std edits ~40; every file under 500 lines (`check/derive.rs` is 387 now: its tests move to `check/derive_tests.rs` if it passes 500).
- No new dependency, no `unsafe`, no runtime change, no external input.

## TESTS
- [ ] `tests/programs/m142/show.la`: the OBJECTIVE's `Show` over integers, strings, arrays, nested and generic structs, a recursive enum and a tuple; the JSON with `Date`'s own `to_json`; a `from_json` override round trip.
- [ ] `tests/programs/m142/`: `not_shown.la` (E0451 with paths through a field, an array element and an enum payload), `bad_block.la` (E0452 overlap; E0422 missing item and wrong signature), `bad_block/` (a directory case: a block in another module, E0452), `bad_fields.la` (E0453 outside a block, wrong kind, wrong `f`), `json_clash.la` (a struct's `to_json(self): string` is E0422 when encoded).
- [ ] `tests/programs/m141/derive_name.la` still passes; m139/m140 byte for byte.
- [ ] Unit tests: the parser's contextual `derive` (an item vs `derive` as a name in every position), overlap detection, override choice (own, block, mismatched), each new diagnostic.
- [ ] `tests/fmt.rs`: a derive block round-trips; `tests/lsp_hover.rs` and `tests/lsp_completion.rs`: a block's item and `derive.` inside a block.
- [ ] `tests/explain.rs` picks up E0451–E0453.

## ACCEPTANCE
- [ ] The OBJECTIVE program prints what it shows.
- [ ] `cargo test --lib`, `--test programs`, `--test json`, `--test explain`, `--test fmt`, `--test fmt_corpus`, `--test lsp_hover`, `--test lsp_completion`, `--test lazy_std`; `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`.
- [ ] Every `pub` item documented; DECISIONS.md, spec (§2–§6, §9, `std.json`, §11 row 142, "Changes since v16"), the v17 Draft, CHANGELOG (with **Breaking**), TODO and CLAUDE.md updated.
