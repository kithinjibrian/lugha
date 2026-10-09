//! Public-API tests for the lexer (PRP-002): every spec §10/§11 program lexes cleanly.

use lugha::lexer::{TokenKind, lex};

const HELLO: &str = r#"fun main() {
    println("Hello, world!");
}
"#;

const RECURSION: &str = r#"fun fib(n: i64): i64 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    println(fib(30));
    0
}
"#;

const PRIMES: &str = r#"fun count_primes(limit: i64): i64 {
    let mut is_composite = [false; limit + 1];
    let mut count = 0;
    for i in 2..limit + 1 {
        if !is_composite[i] {
            count += 1;
            let mut j = i * i;
            while j <= limit {
                is_composite[j] = true;
                j += i;
            }
        }
    }
    count
}

fun main() {
    println(count_primes(100));
}
"#;

const CENTROID: &str = r#"struct Point { x: f64, y: f64 }

fun centroid(pts: Point[]): Point {
    let mut sx = 0.0;
    let mut sy = 0.0;
    for p of pts {
        sx += p.x;
        sy += p.y;
    }
    let n = pts.len as f64;
    Point { x: sx / n, y: sy / n }
}

fun main() {
    let pts = [
        Point { x: 0.0, y: 0.0 },
        Point { x: 4.0, y: 0.0 },
        Point { x: 2.0, y: 3.0 },
    ];
    let c = centroid(pts);
    println("centroid: " + to_string(c.x) + ", " + to_string(c.y));
}
"#;

const CALLING_C: &str = r#"extern fun puts(s: string): i32;
extern fun sqrt(x: f64): f64;

fun main() {
    puts("hello from libc");
    println(sqrt(2.0));
}
"#;

const REJECTED: &str = r#"fun main() {
    let x: i32 = 5;
    let y = x + 2.5;
}
"#;

const MILESTONE_2: &str = r#"fun fib(n: i32): i32 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    let n: i32 = 3;
    let mut total: i32 = 0;
    for i in 0..n {
        total += i;
    }
    fib(10) + total - n
}
"#;

#[test]
fn every_spec_program_lexes_without_diagnostics() {
    for (name, src) in [
        ("hello", HELLO),
        ("recursion", RECURSION),
        ("primes", PRIMES),
        ("centroid", CENTROID),
        ("calling C", CALLING_C),
        // Its error is a type error, found by the checker, not the lexer.
        ("rejected", REJECTED),
        ("milestone 2", MILESTONE_2),
    ] {
        let (tokens, warnings) = lex(src).unwrap_or_else(|d| panic!("{name}: {d:?}"));
        assert!(warnings.is_empty(), "{name}: {warnings:?}");
        assert_eq!(
            tokens.last().map(|t| &t.kind),
            Some(&TokenKind::Eof),
            "{name}"
        );
    }
}

#[test]
fn milestone_1_program_has_the_expected_tokens() {
    use TokenKind::*;
    let (tokens, _) = lex("fun main(): i32 { 2 + 3 * 4 }").unwrap();
    let kinds: Vec<_> = tokens.into_iter().map(|t| t.kind).collect();
    let main = Ident("main".into());
    let expected = [
        Fun,
        main,
        LParen,
        RParen,
        Colon,
        TyI32,
        LBrace,
        Int(2),
        Plus,
        Int(3),
        Star,
        Int(4),
        RBrace,
        Eof,
    ];
    assert_eq!(kinds, expected);
}

#[test]
fn lexing_never_panics_on_any_prefix() {
    // Cutting a valid program at every char boundary produces every kind of
    // half-finished token: open strings, `0x`, `2.0e`, lone `&`, multi-byte chars.
    let sample = format!("{CENTROID}\nlet s = \"h\\é👋\"; let n = 0xFF + 2.0e-3 && x || y; // c");
    for (i, _) in sample.char_indices() {
        let _ = lex(&sample[..i]);
    }
}
