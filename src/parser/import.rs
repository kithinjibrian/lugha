//! v2 module syntax at the top level: `import` lines, which come before
//! every item (E0208), and `private`/`public` before an item, method or
//! field (v2 draft §3, decision 9).

use super::{PResult, Parser};
use crate::ast::{Import, Item, Visibility, VisibilityKind};
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

    /// `private` or `public` at the current token, if there is one.
    pub(super) fn visibility(&mut self) -> Option<Visibility> {
        let kind = match self.peek() {
            TokenKind::Private => VisibilityKind::Private,
            TokenKind::Public => VisibilityKind::Public,
            _ => return None,
        };
        Some(Visibility {
            kind,
            span: self.bump(),
        })
    }

    /// A visibility and the item it marks: only `fun`, `extern`, `struct`, `interface`
    /// and `enum` take one (v2 draft §3).
    pub(super) fn visible_item(&mut self) -> PResult<Item> {
        let visibility = self.visibility();
        Ok(match self.peek() {
            TokenKind::Fun => {
                let mut f = self.fun_decl_with(false)?;
                f.visibility = visibility;
                Item::Fun(f)
            }
            TokenKind::Extern => {
                let mut e = self.extern_decl()?;
                e.visibility = visibility;
                Item::Extern(e)
            }
            TokenKind::Struct => {
                let mut s = self.struct_decl()?;
                s.visibility = visibility;
                Item::Struct(s)
            }
            TokenKind::Enum => {
                let mut e = self.enum_decl()?;
                e.visibility = visibility;
                Item::Enum(e)
            }
            TokenKind::Interface => {
                let mut i = self.interface_decl()?;
                i.visibility = visibility;
                Item::Interface(i)
            }
            _ => return Err(self.expected("`fun`, `extern`, `struct`, `enum` or `interface`")),
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
            program(
                "private fun f() {}\npublic extern fun g();\nprivate struct S {}\npublic enum E { A }"
            ),
            "(private fun f () void (block))\n(public extern g () void)\n(private struct S)\n(public enum E A)"
        );
        assert_eq!(
            program("impl S { private fun f(self) {} fun g() {} }"),
            "(impl S (private fun f (self) void (block)) (fun g () void (block)))"
        );
        assert_eq!(
            program("struct Range { private lo: i64, public hi: i64, n: i64 }"),
            "(struct Range (private lo i64) (public hi i64) (n i64))"
        );
        assert_eq!(errors("private impl S {}"), [("E0201", "impl")]);
        assert_eq!(
            errors("fun f() { private let x = 1; }"),
            [("E0201", "private")]
        );
        assert_eq!(errors("fun f(private x: i64) {}"), [("E0201", "private")]);
        assert_eq!(errors("public import a;"), [("E0201", "public")]);
    }
}
