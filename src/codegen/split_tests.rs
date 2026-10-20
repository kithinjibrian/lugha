//! Tests of `split.rs` (v17, milestone 126, PRP-158).

use crate::codegen::test_util::ir;

/// The body of the function defining `symbol`.
fn body<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let start = ir
        .find(&format!("define {symbol}"))
        .or_else(|| {
            ir.lines()
                .find(|l| l.starts_with("define") && l.contains(symbol))
                .and_then(|l| ir.find(l))
        })
        .unwrap_or_else(|| panic!("no {symbol} in {ir}"));
    let end = ir[start..].find("\n}\n").map_or(ir.len(), |e| start + e);
    &ir[start..end]
}

/// Struct-typed instructions in `text`: whole-value loads, stores,
/// inserts, extracts, `phi`s and `select`s.
fn struct_ops(text: &str) -> Vec<&str> {
    text.lines()
        .filter(|l| {
            let l = l.trim();
            // Overflow intrinsics' `{ iN, i1 }` results are read as LLVM expects.
            let overflow = l.contains("extractvalue { i") && l.contains(", i1 }");
            !overflow
                && (l.contains("insertvalue ")
                    || l.contains("extractvalue ")
                    || l.contains(" = load %")
                    || l.contains(" = load {")
                    || l.starts_with("store %")
                    || l.starts_with("store {")
                    || l.contains(" = phi %")
                    || l.contains(" = phi {")
                    || (l.contains(" = select ") && (l.contains(", %\"") || l.contains("{ "))))
        })
        .collect()
}

const P: &str = "struct P { x: i64, y: i64 }\n";

#[test]
fn fields_are_read_built_and_copied_without_struct_values() {
    let ir = ir(&format!(
        "{P}fun make(n: i64): P = P {{ x: n, y: n + 1 }};\n\
         fun main() {{ let p = make(1); let q = p; println(q.x + q.y); }}"
    ));
    assert_eq!(
        struct_ops(body(&ir, "void @lugha_fn_main")),
        Vec::<&str>::new(),
        "{ir}"
    );
    assert_eq!(
        struct_ops(body(&ir, "void @lugha_fn_make")),
        Vec::<&str>::new(),
        "{ir}"
    );
}

#[test]
fn an_if_producing_a_struct_joins_each_field() {
    let ir = ir(&format!(
        "{P}fun pick(c: bool): P = if c {{ P {{ x: 1, y: 2 }} }} else {{ P {{ x: 3, y: 4 }} }};\n\
         fun main() {{ println(pick(true).x); }}"
    ));
    let pick = body(&ir, "void @lugha_fn_pick");
    assert_eq!(struct_ops(pick), Vec::<&str>::new(), "{pick}");
}

#[test]
fn enum_payloads_and_options_are_fields_too() {
    let ir = ir("enum E { A(i64), B(i64, i64), C }\n\
         fun e(n: i64): E = if n > 0 { E.B(n, n) } else { E.C };\n\
         fun o(n: i64): Option<i64> = if n > 0 { Some(n) } else { None };\n\
         fun main() { let s = match e(2) { B(a, b) => a + b, A(a) => a, C => 0 }; \
         let t = match o(1) { Some(v) => v, None => 0 }; println(s + t); }");
    for f in [
        "void @lugha_fn_e",
        "void @lugha_fn_o",
        "void @lugha_fn_main",
    ] {
        let b = body(&ir, f);
        assert_eq!(struct_ops(b), Vec::<&str>::new(), "{b}");
    }
}

#[test]
fn a_struct_parameter_becomes_its_fields() {
    // Function values are `{ ptr, ptr }`: a function taking one takes two pointers.
    let ir = ir(&format!(
        "{P}fun apply(f: fun(i64): i64, n: i64): i64 = f(n);\n\
         fun main() {{ println(apply(fun(x: i64): i64 = x + 1, 2)); println(P {{ x: 1, y: 2 }} == P {{ x: 1, y: 2 }}); }}"
    ));
    let apply = ir
        .lines()
        .find(|l| l.starts_with("define") && l.contains("@lugha_fn_apply("))
        .expect("apply is defined");
    // The function value's code and environment, then `n`.
    let params =
        &apply[apply.find('(').expect("a parameter list")..=apply.find(')').expect("closed")];
    assert!(
        params.starts_with("(ptr %") && params.contains(", ptr %"),
        "{apply}"
    );
    assert!(
        params.contains(", i64 %") && !params.contains('{'),
        "{apply}"
    );
    assert!(
        !ir.contains("{ ptr, ptr } %"),
        "no function value passed whole: {ir}"
    );
}

#[test]
fn rewritten_functions_leave_no_old_declarations() {
    let ir = ir(&format!(
        "{P}fun make(): P = P {{ x: 1, y: 2 }};\nfun main() {{ println(make().x); }}"
    ));
    assert!(!ir.contains("lugha_old_"), "{ir}");
}

/// A struct of `n` `i64` fields `v0`, `v1`, … and a literal of it.
fn big(n: usize) -> (String, String) {
    let fields: Vec<String> = (0..n).map(|i| format!("v{i}: i64")).collect();
    let values: Vec<String> = (0..n).map(|i| format!("v{i}: {i}")).collect();
    (
        format!("struct B {{ {} }}\n", fields.join(", ")),
        format!("B {{ {} }}", values.join(", ")),
    )
}

#[test]
fn a_big_struct_is_copied_and_read_through_memory() {
    let (decl, lit) = big(70);
    let ir = ir(&format!(
        "{decl}fun main() {{ let a = {lit}; let b = a; let c = b; println(c.v3 + b.v69); }}"
    ));
    let main = body(&ir, "void @lugha_fn_main");
    assert!(
        !main.contains("= load %B"),
        "no whole load of a big struct: {main}"
    );
    assert!(main.contains("@llvm.memcpy"), "copies go by memcpy: {main}");
}

#[test]
fn bool_results_are_zero_extended() {
    let ir = ir("fun pos(n: i64): bool = n > 0;\nfun main() { println(pos(1)); }");
    let def = ir
        .lines()
        .find(|l| l.starts_with("define") && l.contains("@lugha_fn_pos("))
        .expect("pos is defined");
    assert!(def.contains("zeroext i1 @lugha_fn_pos("), "{def}");
    assert!(ir.contains("call zeroext i1 @lugha_fn_pos("), "{ir}");
}
