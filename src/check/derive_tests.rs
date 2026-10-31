//! Unit tests of derive blocks (v17, milestones 141–142): `check/derive.rs`
//! and `check/derive_meets.rs`, kept apart for the 500-line limit.
use crate::check::Type;
use crate::check::test_util::bundled;

/// A derivable `Show`: blocks for `i64`, arrays, structs and enums.
const SHOW: &str = "interface Show { fun show(self): string; }\n\
        derive Show for i64 { fun show(self): string = \"{self}\"; }\n\
        derive<T: Show> Show for T[] {\n\
            fun show(self): string { let mut s = \"\"; for x of self { s = s + x.show(); } s }\n\
        }\n\
        derive Show for struct { fun show(self): string = join(derive.fields(self, (), part)); }\n\
        derive Show for enum {\n\
            fun show(self): string { let (n, ps) = derive.variant(self, (), part); n + join(ps) }\n\
        }\n\
        fun part<F: Show>(c: (), name: string, x: F): string = name + \"=\" + x.show();\n\
        fun join(xs: string[]): string { let mut s = \"\"; for x of xs { s = s + x; } s }\n\
        fun show<T: Show>(x: T): string = x.show();\n";

/// `key<args>` of every instance of `src` after `SHOW`.
fn monos(src: &str) -> Vec<String> {
    let checked = bundled(&format!("{SHOW}{src}")).unwrap_or_else(|d| panic!("{d:?}"));
    checked
        .monos
        .iter()
        .map(|m| {
            let args: Vec<String> = m.args.iter().map(Type::to_string).collect();
            format!("{}<{}>", m.key, args.join(", "))
        })
        .collect()
}

/// The codes of checking `src` after `SHOW`.
fn codes(src: &str) -> Vec<&'static str> {
    match bundled(&format!("{SHOW}{src}")) {
        Ok(_) => Vec::new(),
        Err(d) => d.iter().map(|d| d.code).collect(),
    }
}

#[test]
fn a_type_uses_its_own_block_then_the_struct_block() {
    let found =
        monos("struct P { a: i64, b: i64[] }\nfun main() { let s = show(P { a: 1, b: [2] }); }");
    for want in [
        "show<P>",
        "Show@struct.show<P>",
        "part<i64>",
        "part<i64[]>",
        "Show@array.show<i64>",
    ] {
        assert!(found.iter().any(|m| m == want), "{want}: {found:?}");
    }
    // `i64`'s block isn't generic: listed once, without arguments.
    assert!(found.iter().any(|m| m == "Show@i64.show<>"), "{found:?}");
}

#[test]
fn recursive_and_generic_types_meet_it_through_their_parts() {
    let list = "enum List { Nil, Cons(i64, List) }\n\
            fun main() { let s = show(List.Cons(1, List.Nil)); }";
    assert!(monos(list).iter().any(|m| m == "Show@enum.show<List>"));
    let boxed = "struct Box<T> { v: T }\n\
            fun f<T: Show>(b: Box<T>): string = show(b);\nfun main() { let s = f(Box { v: 1 }); }";
    assert!(
        monos(boxed)
            .iter()
            .any(|m| m == "Show@struct.show<Box<i64>>")
    );
}

#[test]
fn a_part_no_block_covers_misses_the_bound() {
    let fun_field = "struct Job { run: fun(i64): i64 }\n\
            fun main() { let s = show(Job { run: fun(x: i64): i64 = x }); }";
    assert_eq!(codes(fun_field), ["E0451"]);
    let unbounded = "struct Box<T> { v: T }\nfun f<T>(b: Box<T>): string = show(b);\nfun main() {}";
    assert_eq!(codes(unbounded), ["E0451"]);
    assert_eq!(codes("fun main() { let s = show(true); }"), ["E0451"]);
}

#[test]
fn a_block_must_be_for_its_own_modules_interface_and_match_it() {
    let src = "derive Nope for i64 { fun show(self): string = \"\"; }\nfun main() {}";
    assert_eq!(codes(src), ["E0452"]);
    let wrong = "derive Show for bool { fun show(self): i64 = 1; }\nfun main() {}";
    assert_eq!(codes(wrong), ["E0422"]);
    let empty = "derive Show for bool {}\nfun main() {}";
    assert_eq!(codes(empty), ["E0422"]);
}

#[test]
fn blocks_that_could_cover_one_type_overlap() {
    // `SHOW` already covers `T[]` and every struct.
    assert_eq!(
        codes("derive Show for bool[] { fun show(self): string = \"\"; }\nfun main() {}"),
        ["E0452"]
    );
    assert_eq!(
        codes("derive Show for struct { fun show(self): string = \"\"; }\nfun main() {}"),
        ["E0452"]
    );
    // `bool` and `bool[]`-free types don't.
    assert_eq!(
        codes("derive Show for bool { fun show(self): string = \"\"; }\nfun main() {}"),
        Vec::<&str>::new()
    );
}

#[test]
fn a_types_own_item_wins_and_must_have_its_shape() {
    let own = "struct P { a: i64 }\nimpl P { fun show(self): string = \"mine\"; }\n\
            fun main() { let s = show(P { a: 1 }); }";
    let found = monos(own);
    assert!(found.iter().any(|m| m == "show<P>"), "{found:?}");
    assert!(
        !found.iter().any(|m| m.contains("Show@struct")),
        "{found:?}"
    );
    // Supplying every item itself, a type needs no block for its parts.
    let unshowable = "struct Q { f: fun(): i64 }\nimpl Q { fun show(self): string = \"q\"; }\n\
            fun main() { let s = show(Q { f: fun(): i64 = 1 }); }";
    assert_eq!(codes(unshowable), Vec::<&str>::new());
    let wrong = "struct R { a: i64 }\nimpl R { fun show(self): i64 = 1; }\n\
            fun main() { let s = show(R { a: 1 }); }";
    assert_eq!(codes(wrong), ["E0422"]);
}

/// `key<args>` of every instance of program `src` (prelude included).
fn program_monos(src: &str) -> Vec<String> {
    crate::check::test_util::ok(src)
        .monos
        .iter()
        .map(|m| {
            let args: Vec<String> = m.args.iter().map(Type::to_string).collect();
            format!("{}<{}>", m.key, args.join(", "))
        })
        .collect()
}

#[test]
fn eq_brings_the_preludes_hash_through_a_bound() {
    // `K: Eq` is all a map needs; its keys reach `$Hash`'s blocks.
    let found = program_monos(
        "struct P { x: i64 }\n\
         fun count<K: Eq>(xs: K[]): i64 { let mut m: Map<K, i64> = #{}; for x of xs { m.insert(x, 1); } m.len }\n\
         fun main() { let n = count([P { x: 1 }]); }",
    );
    assert!(
        found.iter().any(|m| m == "$prelude.$Hash@struct.$hash<P>"),
        "{found:?}"
    );
    assert!(
        found
            .iter()
            .any(|m| m.starts_with("$prelude.$Hash@i64.$hash")),
        "{found:?}"
    );
    // A program without maps or sets compiles no hashing at all.
    let none = program_monos("fun main() { let x = 1 == 2; }");
    assert!(!none.iter().any(|m| m.contains("$Hash")), "{none:?}");
}

#[test]
fn a_handle_block_covers_every_handle() {
    let src = "interface Id { fun id(self): u64; }\n\
        derive Id for handle { fun id(self): u64 = derive.address(self); }\n\
        extern type Window;\n\
        fun id<T: Id>(x: T): u64 = x.id();\n\
        fun f(w: Window, p: CPtr): u64 = id(w) + id(p);\nfun main() {}";
    assert_eq!(codes_of(src), Vec::<&str>::new());
}

/// The codes of checking `src` on its own.
fn codes_of(src: &str) -> Vec<&'static str> {
    match bundled(src) {
        Ok(_) => Vec::new(),
        Err(d) => d.iter().map(|d| d.code).collect(),
    }
}
