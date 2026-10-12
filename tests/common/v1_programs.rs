//! The v1 draft §10 programs not yet folded into the v1 specification, as
//! inline fixtures. Each moves into the spec (and out of here) with its
//! milestone; `spec_programs.rs` then extracts it like the others.

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
pub const ALL: [(&str, &str); 1] = [("stack", STACK)];
