//! Operator overloading in the checker (milestone 52).

use crate::check::test_util::{errors, let_type, ok};

const VEC2: &str = "struct Vec2 { x: f64, y: f64 }\n\
    impl Vec2 {\n\
        fun add(self, o: Vec2): Vec2 = Vec2 { x: self.x + o.x, y: self.y + o.y };\n\
        fun mul(self, k: f64): Vec2 = Vec2 { x: self.x * k, y: self.y * k };\n\
        fun neg(self): Vec2 = Vec2 { x: -self.x, y: -self.y };\n\
    }\n";

fn ty(body: &str) -> String {
    let_type(
        &format!("{VEC2}fun main() {{ let a = Vec2 {{ x: 1.0, y: 2.0 }}; {body} }}"),
        "r",
    )
    .to_string()
}

fn codes(src: &str) -> Vec<&'static str> {
    errors(src).into_iter().map(|(c, _)| c).collect()
}

#[test]
fn operators_call_methods_of_the_operator_shape() {
    assert_eq!(ty("let r = a + a;"), "Vec2");
    assert_eq!(ty("let r = a * 2.5;"), "Vec2");
    assert_eq!(ty("let r = -a;"), "Vec2");
    assert_eq!(ty("let r = (a + a) * 2.0 + -a;"), "Vec2");
    ok(&format!(
        "{VEC2}fun main() {{ let mut a = Vec2 {{ x: 1.0, y: 2.0 }}; a += a; a *= 3.0; }}"
    ));
}

#[test]
fn generic_code_uses_the_prelude_bounds() {
    let add = "fun add<T: Add>(a: T, b: T): T = a + b;\n";
    ok(&format!(
        "{VEC2}{add}fun main() {{ let i = add(1, 2); let f = add(1.5, 2.0); \
         let s = add(\"a\", \"b\"); let v = add(Vec2 {{ x: 1.0, y: 1.0 }}, Vec2 {{ x: 0.0, y: 0.0 }}); }}"
    ));
    ok(
        "fun sum<T: Num>(xs: T[]): T { let mut s: T = 0; for x of xs { s += x; } s }\n\
        fun main() { let a = sum([1, 2]); let b = sum([0.5, 1.5]); let c: u8 = sum([1, 2]); }",
    );
    ok("fun big<T: Num>(a: T, b: T): bool = a - b > 0 && a == a && -a < a * 1;\nfun main() {}");
}

#[test]
fn misuse_is_reported() {
    // No `add`, a `mut self` one, and `-` on strings.
    assert_eq!(
        codes("struct S {}\nfun main() { let s = S {}; let t = s + s; }"),
        ["E0404"]
    );
    let set = "struct Set { n: i64 }\nimpl Set { fun add(mut self, x: i64) { self.n += x; } }\n";
    assert_eq!(
        codes(&format!(
            "{set}fun main() {{ let s = Set {{ n: 0 }}; let t = s + 1; }}"
        )),
        ["E0404"]
    );
    assert_eq!(codes("fun main() { let s = \"a\" - \"b\"; }"), ["E0404"]);
    // A bare `T`, a float literal for a `T` that is only `Num` (any integer
    // literal is one since milestone 58), and a non-number for `Num`.
    assert_eq!(
        codes("fun f<T>(a: T): T = a + a;\nfun main() {}"),
        ["E0404"]
    );
    assert_eq!(
        codes("fun f<T: Num>(a: T): T = a * 2.5;\nfun main() {}"),
        ["E0401"]
    );
    assert_eq!(
        codes("fun f<T: Num>(a: T): T = a;\nfun main() { let s = f(\"x\"); }"),
        ["E0419"]
    );
    // The right operand must be the method's parameter type.
    assert_eq!(
        codes(&format!(
            "{VEC2}fun main() {{ let a = Vec2 {{ x: 1.0, y: 2.0 }}; let k = 1.0; let b = a + k; }}"
        )),
        ["E0403"]
    );
    // Numbers have no methods to call directly.
    assert_eq!(codes("fun main() { let n = 5.add(3); }"), ["E0415"]);
}
