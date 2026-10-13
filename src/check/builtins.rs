//! The array built-in methods `push`, `pop` and `clear` (v1 §4, milestone 8).

use super::Type;
use super::methods::Method;
use crate::span::Span;

/// `push`, `pop` and `clear` on an array (v1 §4, milestone 8).
pub(super) fn builtin(receiver: &Type, name: &str) -> Option<Method> {
    let Type::Array(element) = receiver else {
        return None;
    };
    let (params, ret) = match name {
        "push" => (vec![(**element).clone()], Type::Void),
        "pop" => (Vec::new(), Type::Option(element.clone())),
        "clear" => (Vec::new(), Type::Void),
        _ => return None,
    };
    Some(Method {
        receiver: Some(true),
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
}
