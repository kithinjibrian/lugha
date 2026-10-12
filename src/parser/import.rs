//! v2 module syntax at the top level: `import` lines, which come before
//! every item (E0208), and `pub` before an item (v2 draft §3).

use super::{PResult, Parser};
use crate::ast::{Import, Item};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `import a.b.c;`, at the current `import`.
    pub(super) fn import(&mut self) -> PResult<Import> {
        let start = self.bump().start;
        let mut path = vec![self.ident("module name")?];
        while self.eat(&TokenKind::Dot) {
            path.push(self.ident("module name")?);
        }
        self.expect(&TokenKind::Semi)?;
        Ok(Import {
            path,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// E0208 for an `import` after the first item, which is parsed anyway so
    /// parsing continues after it.
    pub(super) fn late_import(&mut self, first_item: Span) {
        let Ok(import) = self.import() else {
            return;
        };
        let diagnostic = Diagnostic::error(
            "E0208",
            "`import` must come before other items",
            import.span,
        )
        .with_label(first_item, "the first item is here")
        .with_help("move every `import` to the top of the file");
        self.error(diagnostic);
    }

    /// `pub` and the item it marks; `pub` fits only `fun`, `extern`,
    /// `struct` and `enum` (v2 draft §6).
    pub(super) fn pub_item(&mut self) -> PResult<Item> {
        let at = Some(self.bump());
        Ok(match self.peek() {
            TokenKind::Fun => {
                let mut f = self.fun_decl_with(false)?;
                f.public = at;
                Item::Fun(f)
            }
            TokenKind::Extern => {
                let mut e = self.extern_decl()?;
                e.public = at;
                Item::Extern(e)
            }
            TokenKind::Struct => {
                let mut s = self.struct_decl()?;
                s.public = at;
                Item::Struct(s)
            }
            TokenKind::Enum => {
                let mut e = self.enum_decl()?;
                e.public = at;
                Item::Enum(e)
            }
            _ => return Err(self.expected("`fun`, `extern`, `struct` or `enum` after `pub`")),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, program};

    #[test]
    fn imports_come_first() {
        assert_eq!(
            program("import a;\nimport a.b.c;\nfun main() {}"),
            "(import a)\n(import a b c)\n(fun main () void (block))"
        );
        assert_eq!(
            errors("fun f() {}\nimport a;\nfun g() {}"),
            [("E0208", "import a;")]
        );
        assert_eq!(errors("import ;"), [("E0201", ";")]);
        assert_eq!(errors("import a.;"), [("E0201", ";")]);
        assert_eq!(errors("fun f() { import a; }"), [("E0201", "import")]);
    }

    #[test]
    fn pub_marks_items_and_methods() {
        assert_eq!(
            program("pub fun f() {}\npub extern fun g();\npub struct S {}\npub enum E { A }"),
            "(pub fun f () void (block))\n(pub extern g () void)\n(pub struct S)\n(pub enum E A)"
        );
        assert_eq!(
            program("impl S { pub fun f(self) {} fun g() {} }"),
            "(impl S (pub fun f (self) void (block)) (fun g () void (block)))"
        );
        assert_eq!(errors("pub impl S {}"), [("E0201", "impl")]);
        assert_eq!(errors("fun f() { pub let x = 1; }"), [("E0201", "pub")]);
    }
}
