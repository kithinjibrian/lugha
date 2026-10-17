//! The built-in methods: `push`, `pop` and `clear` on arrays (v1 §4,
//! milestone 8), `slice` on strings (v4), the wrapping and counting
//! methods on integers (v7), and `unwrap`/`expect` on `Option` and
//! `Result` (v9).

use super::Type;
use super::methods::Method;
use super::results::RESULT;
use crate::span::Span;

/// `push`, `pop` and `clear` on an array (v1 §4, milestone 8), with a
/// `mut self` receiver; `slice(from, to)` on a string (v4), with `self`.
pub(super) fn builtin(receiver: &Type, name: &str) -> Option<Method> {
    let (params, ret, mutable) = match (receiver, name) {
        (Type::Array(element), "push") => (vec![(**element).clone()], Type::Unit, true),
        (Type::Array(element), "pop") => (Vec::new(), Type::Option(element.clone()), true),
        (Type::Array(_), "clear") => (Vec::new(), Type::Unit, true),
        (Type::String, "slice") => (vec![Type::I64, Type::I64], Type::String, false),
        // v16: a box's whole API besides `Ref.new`; plain `self`, since they
        // change the box, not the handle (milestone 67).
        (Type::Ref(inner), "get") => (Vec::new(), (**inner).clone(), false),
        (Type::Ref(inner), "set") => (vec![(**inner).clone()], Type::Unit, false),
        (Type::Ref(inner), "update") => {
            let f = Type::Fun(vec![(**inner).clone()], inner.clone());
            (vec![f], Type::Unit, false)
        }
        (Type::Ref(_), "is") => (vec![receiver.clone()], Type::Bool, false),
        // v17: a task's result, and a channel's API besides `Chan.new` and
        // `Chan.buffered`; plain `self`, as for boxes.
        (Type::Task(inner), "join") => (Vec::new(), (**inner).clone(), false),
        (Type::Chan(inner), "send") => (vec![(**inner).clone()], Type::Unit, false),
        (Type::Chan(inner), "recv" | "try_recv") => {
            (Vec::new(), Type::Option(inner.clone()), false)
        }
        (Type::Chan(_), "close") => (Vec::new(), Type::Unit, false),
        (Type::Chan(_), "is") => (vec![receiver.clone()], Type::Bool, false),
        // v17, milestone 92: every actor handle's own messages.
        (Type::Actor(_), "stop") => (Vec::new(), Type::Unit, false),
        (Type::Actor(_), "join") => (Vec::new(), Type::Option(Box::new(Type::String)), false),
        (Type::Actor(_), "is") => (vec![receiver.clone()], Type::Bool, false),
        (Type::String, "to_bytes") => (Vec::new(), Type::Array(Box::new(Type::U8)), false),
        // v9: the success payload, or a panic at the call.
        (Type::Option(inner), "unwrap") => (Vec::new(), (**inner).clone(), false),
        (Type::Option(inner), "expect") => (vec![Type::String], (**inner).clone(), false),
        (Type::Enum(k, args), "unwrap") if k == RESULT => (Vec::new(), args[0].clone(), false),
        (Type::Enum(k, args), "expect") if k == RESULT => {
            (vec![Type::String], args[0].clone(), false)
        }
        (int, "wrapping_add" | "wrapping_sub" | "wrapping_mul") if int.is_integer() => {
            (vec![int.clone()], int.clone(), false)
        }
        // v10: `None` instead of a panic, clamped, or rotated.
        (int, "checked_add" | "checked_sub" | "checked_mul" | "checked_div")
            if int.is_integer() =>
        {
            (
                vec![int.clone()],
                Type::Option(Box::new(int.clone())),
                false,
            )
        }
        (int, "saturating_add" | "saturating_sub" | "saturating_mul") if int.is_integer() => {
            (vec![int.clone()], int.clone(), false)
        }
        (int, "rotate_left" | "rotate_right") if int.is_integer() => {
            (vec![Type::I64], int.clone(), false)
        }
        (int, "count_ones" | "leading_zeros" | "trailing_zeros") if int.is_integer() => {
            (Vec::new(), Type::I64, false)
        }
        // A float's IEEE 754 bits (v17, milestone 108).
        (float, "to_bits") if float.is_float() => (Vec::new(), super::bits_of(float), false),
        _ => return None,
    };
    Some(Method {
        receiver: Some(mutable),
        params,
        ret,
        span: Span::new(0, 0),
        generics: Vec::new(),
        outer: 0,
        key: String::new(),
    })
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    #[test]
    fn arrays_grow_through_push_pop_and_clear() {
        use crate::check::Type;
        use crate::check::test_util::let_type;
        let src = "fun main() { let mut xs = [1]; xs.push(2); let p = xs.pop(); xs.clear(); }";
        assert_eq!(let_type(src, "p"), Type::Option(Box::new(Type::I64)));
        ok("fun main() { let mut xs = [1]; xs.push(xs[0]); let mut g = [[1]]; g.push(g[0]); }");
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.push(2.5); }"),
            [("E0401", "2.5")]
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.push(); }"),
            [("E0405", "xs.push()")]
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; xs.insert(0); }"),
            [("E0415", "insert")]
        );
        assert_eq!(
            errors("fun main() { let mut s = \"a\"; s.push(1); }"),
            [("E0415", "push")]
        );
        let d = crate::check::test_util::diagnostics("fun main() { let xs = [1]; xs.push(2); }");
        assert_eq!(
            (d[0].code, d[0].message.as_str()),
            (
                "E0501",
                "cannot call `mut self` method `push` on `xs`: it is not mutable"
            )
        );
        assert_eq!(
            errors("fun main() { let mut xs = [1]; for x of xs { xs.push(x); } }"),
            [("E0507", "xs")]
        );
    }

    #[test]
    fn integers_have_wrapping_and_counting_methods() {
        use crate::check::Type;
        use crate::check::test_util::let_type;
        let at = |body: &str| let_type(&format!("fun main() {{ {body} }}"), "r");
        for (ty, expected) in [("u8", Type::U8), ("i32", Type::I32), ("u64", Type::U64)] {
            assert_eq!(
                at(&format!("let a: {ty} = 1; let r = a.wrapping_add(1);")),
                expected
            );
        }
        assert_eq!(at("let a: u32 = 1; let r = a.count_ones();"), Type::I64);
        assert_eq!(at("let a: i64 = 1; let r = a.leading_zeros();"), Type::I64);
        assert_eq!(at("let r = (5).trailing_zeros();"), Type::I64);
        let codes = |body: &str| {
            let src = format!("fun main() {{ {body} }}");
            errors(&src).into_iter().map(|(c, _)| c).collect::<Vec<_>>()
        };
        assert_eq!(codes("let x: u32 = 1; let r = x.swap_bytes();"), ["E0415"]);
        assert_eq!(
            codes("let f: f64 = 1.0; let r = f.count_ones();"),
            ["E0415"]
        );
        assert_eq!(
            codes("let b = true; let r = b.wrapping_add(true);"),
            ["E0415"]
        );
        assert_eq!(
            codes("let a: i64 = 1; let r = a.wrapping_add();"),
            ["E0405"]
        );
        assert_eq!(
            codes("let a: u32 = 1; let b: u64 = 2; let r = a.wrapping_add(b);"),
            ["E0403"]
        );
    }

    #[test]
    fn option_and_result_unwrap_and_expect() {
        use crate::check::Type;
        use crate::check::test_util::let_type;
        let at = |body: &str| let_type(&format!("fun main() {{ {body} }}"), "r");
        assert_eq!(at("let o = Some(1); let r = o.unwrap();"), Type::I64);
        assert_eq!(at("let o = Some(1.5); let r = o.expect(\"x\");"), Type::F64);
        let res = "let x: Result<string, i64> = Ok(\"a\");";
        assert_eq!(at(&format!("{res} let r = x.unwrap();")), Type::String);
        assert_eq!(
            at(&format!("{res} let r = x.expect(\"no\");")),
            Type::String
        );
        let codes = |body: &str| {
            let src = format!("fun main() {{ {body} }}");
            errors(&src).into_iter().map(|(c, _)| c).collect::<Vec<_>>()
        };
        assert_eq!(codes("let n = 1; let r = n.unwrap();"), ["E0415"]);
        assert_eq!(codes("let s = \"a\"; let r = s.expect(\"x\");"), ["E0415"]);
        assert_eq!(codes("let o = Some(1); let r = o.expect(1);"), ["E0401"]);
        assert_eq!(codes("let o = Some(1); let r = o.unwrap(1);"), ["E0405"]);
        ok("fun main() { let mut a = 7; a %= 2; let mut f = 7.5; f %= 2.0; }");
        assert_eq!(codes("let mut s = \"a\"; s %= 2;"), ["E0404"]);
    }

    #[test]
    fn checked_saturating_and_rotating_methods() {
        use crate::check::Type;
        use crate::check::test_util::let_type;
        let at = |body: &str| let_type(&format!("fun main() {{ {body} }}"), "r");
        assert_eq!(
            at("let a: i16 = 1; let r = a.checked_add(2);"),
            Type::Option(Box::new(Type::I16))
        );
        assert_eq!(
            at("let a: u32 = 1; let r = a.checked_div(0);"),
            Type::Option(Box::new(Type::U32))
        );
        assert_eq!(at("let a: u8 = 1; let r = a.saturating_mul(3);"), Type::U8);
        assert_eq!(at("let a: u16 = 1; let r = a.rotate_left(-3);"), Type::U16);
        assert_eq!(
            errors("fun main() { let a: i16 = 1; let b: u8 = 2; let r = a.checked_sub(b); }"),
            [("E0403", "b")]
        );
        assert_eq!(
            errors("fun main() { let a: u16 = 1; let n: i32 = 2; let r = a.rotate_right(n); }"),
            [("E0403", "n")]
        );
        assert_eq!(
            errors("fun main() { let f = 1.5; let r = f.saturating_add(1.0); }"),
            [("E0415", "saturating_add")]
        );
        assert_eq!(
            errors("fun main() { let a = 1; let r = a.checked_mul(); }"),
            [("E0405", "a.checked_mul()")]
        );
    }
}
