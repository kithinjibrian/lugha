//! The example programs of `docs/specs/Language v1 Draft.md` §10, as inline
//! fixtures. PRP-020 moves them into the v1 spec, where `spec_programs.rs`
//! extracts them like the v0 programs.

pub const SHAPES: &str = r#"enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Empty,
}

fun area(s: Shape): f64 = match s {
    Circle(r) => 3.14159 * r * r,
    Rect(w, h) => w * h,
    Empty => 0.0,
};

fun main() {
    let shapes = [Shape.Circle(2.0), Shape.Rect(2.0, 3.0), Shape.Empty];
    for s of shapes {
        println(area(s));
    }
}
"#;

pub const LIST: &str = r#"enum List {
    Nil,
    Cons(i64, List),
}

fun sum(l: List): i64 = match l {
    Nil => 0,
    Cons(x, rest) => x + sum(rest),
};

fun main() {
    let l = List.Cons(1, List.Cons(2, List.Cons(3, List.Nil)));
    println(sum(l));
}
"#;

pub const METHODS: &str = r#"struct Vec2 { x: f64, y: f64 }

impl Vec2 {
    fun new(x: f64, y: f64): Vec2 = Vec2 { x: x, y: y };
    fun len_sq(self): f64 = self.x * self.x + self.y * self.y;
    fun scale(mut self, k: f64) {
        self.x *= k;
        self.y *= k;
    }
}

fun main() {
    let mut v = Vec2.new(1.0, 2.0);
    println(v.len_sq());
    v.scale(2.0);
    println(v.x + v.y);
}
"#;

pub const STACK: &str = r#"fun main() {
    let mut stack: i64[] = [];
    stack.push(1);
    stack.push(2);
    stack.push(3);
    match stack.pop() {
        Some(x) => println(x),
        None => println("empty"),
    }
    println(stack.len);
    stack.clear();
    match stack.pop() {
        Some(x) => println(x),
        None => println("empty"),
    }
}
"#;

pub const REJECTED: &str = r#"enum Shape { Circle(f64), Empty }

fun area(s: Shape): f64 = match s {
    Circle(r) => r * r,
};

fun main() {}
"#;

/// Every v1 draft program with a short name.
pub const ALL: [(&str, &str); 5] = [
    ("shapes", SHAPES),
    ("list", LIST),
    ("methods", METHODS),
    ("stack", STACK),
    ("rejected", REJECTED),
];
