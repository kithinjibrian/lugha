//! v2 module syntax at the top level: `import` lines, which come before
//! every item (E0208), and `private`/`public` before an item, method or
//! field (v2 draft §3, decision 9).

use super::{PResult, Parser};
use crate::ast::{Ident, Import, ImportItem, Item, Visibility, VisibilityKind};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `import a.b.c;` or `import a.b { X, y };` (v11), at the current `import`.
    pub(super) fn import(&mut self) -> PResult<Import> {
        self.import_with(None)
    }

    /// The `public` or `private` before an `import`, if any: `private` is
    /// E0201, and `public` is kept for a re-export (v11).
    pub(super) fn import_visibility(&mut self) -> Option<Span> {
        let marked = matches!(self.peek(), TokenKind::Private | TokenKind::Public);
        if !marked || self.peek_at(1) != &TokenKind::Import {
            return None;
        }
        let public = self.at(&TokenKind::Public);
        let at = self.bump();
        if !public {
            self.error(Diagnostic::error(
                "E0201",
                "an import can't be `private`",
                at,
            ));
            return None;
        }
        Some(at)
    }

    /// An import after its optional `public`: `path [as name] [{ items }];`.
    /// `public import a;` re-exports the whole module (v14).
    pub(super) fn import_with(&mut self, public: Option<Span>) -> PResult<Import> {
        let start = public.map_or_else(|| self.span().start, |p| p.start);
        self.bump();
        let mut path = vec![self.ident("module name")?];
        while self.eat(&TokenKind::Dot) {
            path.push(self.ident("module name")?);
        }
        let alias = self.import_alias()?;
        let mut glob = None;
        let items = if self.at(&TokenKind::LBrace) {
            let open = self.bump();
            if self.at(&TokenKind::Star) {
                glob = Some(self.glob(public)?);
                Vec::new()
            } else {
                if self.at(&TokenKind::RBrace) {
                    return Err(self.expected("item name"));
                }
                self.comma_list(open, TokenKind::RBrace, |p| {
                    let name = p.ident("item name")?;
                    let alias = p.import_alias()?;
                    Ok(ImportItem { name, alias })
                })?
            }
        } else {
            Vec::new()
        };
        self.expect(&TokenKind::Semi)?;
        Ok(Import {
            path,
            alias,
            items,
            glob,
            public,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// `* }` at the `*` of a glob re-export (v17, milestone 100): it needs
    /// `public` and stands alone in its braces (E0201 otherwise).
    fn glob(&mut self, public: Option<Span>) -> PResult<Span> {
        let star = self.bump();
        if public.is_none() {
            self.error(
                Diagnostic::error("E0201", "`{ * }` is only for `public import`", star)
                    .with_help("list the items you need, or import the module: `import m;`"),
            );
        }
        if self.at(&TokenKind::Comma) {
            let at = self.span();
            return Err(self.error(
                Diagnostic::error("E0201", "`*` stands alone in its braces", at)
                    .with_help("re-export everything with `{ * }`, or list the items"),
            ));
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(star)
    }

    /// `as name` after an import path or item, if written (v14).
    fn import_alias(&mut self) -> PResult<Option<Ident>> {
        if !self.eat(&TokenKind::As) {
            return Ok(None);
        }
        self.ident("name").map(Some)
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
            TokenKind::Extern => match self.extern_item()? {
                Item::Extern(mut e) => {
                    e.visibility = visibility;
                    Item::Extern(e)
                }
                Item::ExternType(mut t) => {
                    t.visibility = visibility;
                    Item::ExternType(t)
                }
                Item::Struct(mut s) => {
                    s.visibility = visibility;
                    Item::Struct(s)
                }
                _ => unreachable!("`extern_item` makes only externs and extern types"),
            },
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
            TokenKind::Const => {
                let mut c = self.const_decl()?;
                c.visibility = visibility;
                Item::Const(c)
            }
            _ => {
                return Err(
                    self.expected("`fun`, `extern`, `const`, `struct`, `enum` or `interface`")
                );
            }
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
        assert_eq!(errors("private import a;"), [("E0201", "private")]);
    }

    #[test]
    fn named_imports_and_re_exports_v11() {
        assert_eq!(
            program(
                "import a.b { X, y };
public import c { Z, };
fun main() {}"
            ),
            "(import a b (items X y))
(public import c (items Z))
(fun main () void (block))"
        );
        assert_eq!(errors("import a { };"), [("E0201", "}")]);
        assert_eq!(errors("import a as;"), [("E0201", ";")]);
        assert_eq!(errors("import a { X"), [("E0202", "{")]);
    }

    #[test]
    fn glob_re_exports_v17() {
        assert_eq!(
            program("public import a.b { * };\nfun main() {}"),
            "(public import a b (items *))\n(fun main () void (block))"
        );
        assert_eq!(errors("import a { * };"), [("E0201", "*")]);
        assert_eq!(errors("public import a { *, X };"), [("E0201", ",")]);
        assert_eq!(errors("public import a { X, * };"), [("E0201", "*")]);
    }

    #[test]
    fn names_reach_through_a_re_exported_module_v14() {
        assert_eq!(
            program("fun f<T: m.v.I>(p: m.v.P<i64>): m.v.Q = m.v.Q { x: m.v.g() };"),
            "(fun f (type-params (T m.v.I)) ((p (applied m.v.P i64))) (applied m.v.Q) (block (struct-lit m.v.Q (x (call (. (. m v) g))))))"
        );
    }

    #[test]
    fn aliases_and_whole_module_re_exports_v14() {
        assert_eq!(
            program(
                "import a.b as c;
import d { X as Y, z };
public import e.f;
public import g as h { W };
fun main() {}"
            ),
            "(import a b (as c))
(import d (items (as X Y) z))
(public import e f)
(public import g (as h) (items W))
(fun main () void (block))"
        );
    }
}
