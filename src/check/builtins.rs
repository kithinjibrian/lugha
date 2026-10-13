//! The built-in methods: `push`, `pop` and `clear` on arrays (v1 §4,
//! milestone 8), `slice` on strings (v4), and the wrapping and counting
//! methods on integers (v7).

use super::Type;
use super::methods::Method;
use crate::span::Span;

/// `push`, `pop` and `clear` on an array (v1 §4, milestone 8), with a
/// `mut self` receiver; `slice(from, to)` on a string (v4), with `self`.
pub(super) fn builtin(receiver: &Type, name: &str) -> Option<Method> {
    let (params, ret, mutable) = match (receiver, name) {
        (Type::Array(element), "push") => (vec![(**element).clone()], Type::Void, true),
        (Type::Array(element), "pop") => (Vec::new(), Type::Option(element.clone()), true),
        (Type::Array(_), "clear") => (Vec::new(), Type::Void, true),
        (Type::String, "slice") => (vec![Type::I64, Type::I64], Type::String, false),
        (int, "wrapping_add" | "wrapping_sub" | "wrapping_mul") if int.is_integer() => {
            (vec![int.clone()], int.clone(), false)
        }
        (int, "count_ones" | "leading_zeros" | "trailing_zeros") if int.is_integer() => {
            (Vec::new(), Type::I64, false)
        }
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
        assert_eq!(
            codes("let x: u32 = 1; let r = x.rotate_left(1);"),
            ["E0415"]
        );
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
}
