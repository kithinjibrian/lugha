//! Attributes (v16 draft decision 81, milestone 84): `@name` before an
//! item. `@test` before a `fun` marks a test; any other name, or `@test`
//! before anything else, is E0210, and the item is still parsed.

use super::{PResult, Parser};
use crate::ast::Item;
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

/// The one attribute there is.
const TEST: &str = "test";

impl Parser<'_> {
    /// `@name` (one or more), then the item they go before.
    pub(super) fn attributed_item(&mut self) -> PResult<Item> {
        let mut test: Option<Span> = None;
        while self.at(&TokenKind::At) {
            let at = self.bump();
            let name = self.ident("attribute name")?;
            let span = Span::new(at.start, name.span.end);
            if name.name == TEST {
                test.get_or_insert(span);
            } else {
                let message = format!("unknown attribute `@{}`", name.name);
                self.error(
                    Diagnostic::error("E0210", message, span)
                        .with_help("the only attribute is `@test`, before a function"),
                );
            }
        }
        let mut item = self.item()?;
        if let Some(span) = test {
            match &mut item {
                Item::Fun(f) => f.test = Some(span),
                _ => {
                    let message = "`@test` goes before a function";
                    self.error(
                        Diagnostic::error("E0210", message, span)
                            .with_help("write `@test` on the line above `fun name() { … }`"),
                    );
                }
            }
        }
        Ok(item)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, program};

    #[test]
    fn test_marks_a_function() {
        assert_eq!(
            program("@test\nfun adds() { }"),
            "(@test fun adds () void (block))"
        );
        assert_eq!(
            program("@test private fun adds() { }"),
            "(@test private fun adds () void (block))"
        );
        // `test` stays an ordinary name.
        assert_eq!(program("fun test() { }"), "(fun test () void (block))");
    }

    #[test]
    fn unknown_attributes_and_misplaced_tests_are_e0210() {
        assert_eq!(errors("@inline fun f() { }"), [("E0210", "@inline")]);
        assert_eq!(errors("@test struct S { }"), [("E0210", "@test")]);
        assert_eq!(
            errors("@test extern fun abs(x: i32): i32;"),
            [("E0210", "@test")]
        );
    }
}
