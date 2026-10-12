//! The v1 draft §10 programs not yet folded into the v1 specification, as
//! inline fixtures. Each moves into the spec (and out of here) with its
//! milestone; `spec_programs.rs` then extracts it like the others.

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

/// Every v1 draft program not yet in the v1 specification, with a short name.
pub const ALL: [(&str, &str); 3] = [("list", LIST), ("methods", METHODS), ("stack", STACK)];
