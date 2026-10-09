//! The spec §10 and §11 programs, shared by the lexer and parser tests.
//!
//! Copied verbatim from `docs/specs/Language v0 Specification.md`.

pub const HELLO: &str = r#"fun main() {
    println("Hello, world!");
}
"#;

pub const RECURSION: &str = r#"fun fib(n: i64): i64 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    println(fib(30));
    0
}
"#;

pub const PRIMES: &str = r#"fun count_primes(limit: i64): i64 {
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

pub const CENTROID: &str = r#"struct Point { x: f64, y: f64 }

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

pub const CALLING_C: &str = r#"extern fun puts(s: string): i32;
extern fun sqrt(x: f64): f64;

fun main() {
    puts("hello from libc");
    println(sqrt(2.0));
}
"#;

pub const REJECTED: &str = r#"fun main() {
    let x: i32 = 5;
    let y = x + 2.5;
}
"#;

pub const MILESTONE_2: &str = r#"fun fib(n: i32): i32 = if n < 2 { n } else { fib(n - 1) + fib(n - 2) };

fun main(): i32 {
    let n: i32 = 3;
    let mut total: i32 = 0;
    for i in 0..n {
        total += i;
    }
    fib(10) + total - n
}
"#;

/// Every program above with a short name, for table-driven tests.
pub const ALL: [(&str, &str); 7] = [
    ("hello", HELLO),
    ("recursion", RECURSION),
    ("primes", PRIMES),
    ("centroid", CENTROID),
    ("calling C", CALLING_C),
    // Its error is a type error, found by the checker, not the lexer or parser.
    ("rejected", REJECTED),
    ("milestone 2", MILESTONE_2),
];
