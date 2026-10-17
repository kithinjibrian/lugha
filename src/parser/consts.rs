//! `const NAME: T = value;` at the top level (v17, milestone 109).

use super::{PResult, Parser};
use crate::ast::ConstDecl;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `const NAME: T = value;`, at its `const`; the type is always written.
    pub(super) fn const_decl(&mut self) -> PResult<ConstDecl> {
        let start = self.bump().start;
        let name = self.ident("constant name")?;
        self.expect(&TokenKind::Colon)?;
        let ty = self.ty()?;
        self.expect(&TokenKind::Eq)?;
        let value = self.expr()?;
        self.expect(&TokenKind::Semi)?;
        Ok(ConstDecl {
            visibility: None,
            name,
            ty,
            value,
            span: Span::new(start, self.prev_end()),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, program};

    #[test]
    fn constants_parse_with_every_visibility() {
        assert_eq!(program("const N: i64 = 1 + 2;"), "(const N i64 (+ 1 2))");
        assert_eq!(
            program("private const S: string = \"a\";\npublic const P: i64[] = [2, 3];"),
            "(private const S string \"a\")\n(public const P i64[] (array 2 3))"
        );
        assert_eq!(errors("const N = 1;")[0].0, "E0204");
        assert_eq!(errors("fun f() { let const = 1; }")[0].0, "E0201");
    }
}
