//! `format` (v14 draft §5, milestone 48): a literal template's holes are
//! matched to the arguments (E0432), then checked as the parts of an
//! interpolated string, so printing and specifiers follow v12–v13.

use super::{Checker, Type, errors};
use crate::ast::{Expr, FormatCall};
use crate::template;

impl Checker {
    /// `format("…", args…)`: a `string`.
    pub(super) fn format_call(&mut self, call: &FormatCall) -> Type {
        let span = call.template_span;
        let pieces = match template::pieces(&call.template) {
            Ok(pieces) => pieces,
            Err(why) => {
                self.report(errors::bad_template(why, span));
                self.unchecked_args(&call.args);
                return Type::String;
            }
        };
        let holes = template::holes(&pieces);
        if holes != call.args.len() {
            let why = format!(
                "the template has {holes} hole{} but {} value{} follow{}",
                if holes == 1 { "" } else { "s" },
                call.args.len(),
                if call.args.len() == 1 { "" } else { "s" },
                if call.args.len() == 1 { "s" } else { "" },
            );
            self.report(errors::bad_template(why, span));
            self.unchecked_args(&call.args);
            return Type::String;
        }
        let parts = template::parts(&pieces, &call.args, span, exact(call));
        self.interpolation(&parts)
    }

    /// `format(e, …)` whose template isn't a string literal.
    pub(super) fn format_without_template(&mut self, call: &Expr, args: &[Expr]) -> Type {
        let span = args.first().map_or(call.span, |a| a.span);
        let why = "the template of `format` must be a string literal".to_string();
        self.report(errors::bad_template(why, span));
        self.unchecked_args(args);
        Type::String
    }
}

/// Whether the template's text is its source text, quotes aside: no escape
/// changed its length, so offsets in it are offsets in the file.
pub fn exact(call: &FormatCall) -> bool {
    call.template.len() + 2 == call.template_span.end - call.template_span.start
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{errors, let_type};

    fn codes(body: &str) -> Vec<(&'static str, String)> {
        let src =
            format!("struct P {{ x: i64 }}\nfun v() {{}}\nfun main() {{ let n = 1; {body} }}");
        errors(&src)
            .into_iter()
            .map(|(c, t)| (c, t.to_string()))
            .collect()
    }

    #[test]
    fn format_gives_a_string() {
        let src = "fun main() { let r = format(\"{} {:>4} {:x} {{}}\", \"a\", 2.5, 255); }";
        assert_eq!(let_type(src, "r"), Type::String);
        let src = "struct P { x: i64 }\nfun main() { let r = format(\"{}\", P { x: 1 }); }";
        assert_eq!(let_type(src, "r"), Type::String);
    }

    #[test]
    fn bad_templates_are_e0432() {
        let one = |body: &str| codes(body).into_iter().map(|(c, _)| c).collect::<Vec<_>>();
        assert_eq!(one("let s = \"x\"; let r = format(s, n);"), ["E0432"]);
        assert_eq!(one("let r = format();"), ["E0432"]);
        assert_eq!(one("let r = format(\"{} {}\", n, n, n);"), ["E0432"]);
        assert_eq!(one("let r = format(\"{} {} {}\", n, n);"), ["E0432"]);
        assert_eq!(one("let r = format(\"{n}\");"), ["E0432"]);
        assert_eq!(one("let r = format(\"a {\", n);"), ["E0432"]);
        assert_eq!(one("let r = format(\"}\");"), ["E0432"]);
        // Arguments are still checked.
        assert_eq!(one("let r = format(\"{n}\", nope);"), ["E0432", "E0301"]);
    }

    #[test]
    fn holes_are_checked_like_interpolations() {
        assert_eq!(
            codes("let r = format(\"{:.2}\", n);"),
            [("E0431", ":.2".into())]
        );
        assert_eq!(codes("let r = format(\"\\t{:.2}\", n);").len(), 1);
        crate::check::test_util::ok("fun v() {}\nfun main() { let r = format(\"{}\", v()); }");
        assert_eq!(
            errors("fun format(): i64 = 1;\nfun main() {}"),
            [("E0302", "format")]
        );
    }
}
