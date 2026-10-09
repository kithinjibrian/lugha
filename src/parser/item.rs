//! Top-level items and types (spec §3, §6, §8).

use super::{PResult, Parser};
use crate::ast::{Block, ExternDecl, FunDecl, Item, Param, StructDecl, Type, TypeKind};
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// Parses every item up to `Eof`, recovering at the next item after an error.
    pub(super) fn program(&mut self) -> Vec<Item> {
        let mut items = Vec::new();
        while !self.at(&TokenKind::Eof) {
            let before = self.pos;
            match self.item() {
                Ok(item) => items.push(item),
                Err(_) => {
                    self.sync_item();
                    if self.pos == before {
                        self.bump();
                    }
                }
            }
        }
        items
    }

    fn item(&mut self) -> PResult<Item> {
        match self.peek() {
            TokenKind::Fun => self.fun_decl().map(Item::Fun),
            TokenKind::Extern => self.extern_decl().map(Item::Extern),
            TokenKind::Struct => self.struct_decl().map(Item::Struct),
            other => {
                let is_statement = starts_statement(other);
                let reported = self.expected("`fun`, `extern` or `struct`");
                if is_statement && let Some(last) = self.diagnostics.last_mut() {
                    last.help = Some("statements and variables must be inside a function".into());
                }
                Err(reported)
            }
        }
    }

    fn fun_decl(&mut self) -> PResult<FunDecl> {
        let start = self.bump().start;
        let name = self.ident("function name")?;
        let params = self.params()?;
        let ret = self.return_type()?;
        let body = if self.eat(&TokenKind::Eq) {
            // `= e;` means exactly `{ e }` (spec §6).
            let tail = self.expr()?;
            self.expect(&TokenKind::Semi)?;
            Block {
                stmts: Vec::new(),
                span: tail.span,
                tail: Some(Box::new(tail)),
            }
        } else {
            self.block()?
        };
        Ok(FunDecl {
            name,
            params,
            ret,
            body,
            span: Span::new(start, self.prev_end()),
        })
    }

    fn extern_decl(&mut self) -> PResult<ExternDecl> {
        let start = self.bump().start;
        self.expect(&TokenKind::Fun)?;
        let name = self.ident("function name")?;
        let params = self.params()?;
        let ret = self.return_type()?;
        self.expect(&TokenKind::Semi)?;
        Ok(ExternDecl {
            name,
            params,
            ret,
            span: Span::new(start, self.prev_end()),
        })
    }

    fn struct_decl(&mut self) -> PResult<StructDecl> {
        let start = self.bump().start;
        let name = self.ident("struct name")?;
        let open = self.expect(&TokenKind::LBrace)?;
        let fields = self.comma_list(open, TokenKind::RBrace, |p| p.param("field name"))?;
        Ok(StructDecl {
            name,
            fields,
            span: Span::new(start, self.prev_end()),
        })
    }

    fn params(&mut self) -> PResult<Vec<Param>> {
        let open = self.expect(&TokenKind::LParen)?;
        self.comma_list(open, TokenKind::RParen, |p| p.param("parameter name"))
    }

    fn param(&mut self, what: &str) -> PResult<Param> {
        let name = self.ident(what)?;
        self.expect(&TokenKind::Colon)?;
        Ok(Param {
            name,
            ty: self.ty()?,
        })
    }

    fn return_type(&mut self) -> PResult<Option<Type>> {
        if self.eat(&TokenKind::Colon) {
            self.ty().map(Some)
        } else {
            Ok(None)
        }
    }

    /// Parses `base_type { "[" "]" }`.
    pub(super) fn ty(&mut self) -> PResult<Type> {
        let start = self.span().start;
        let kind = match self.peek() {
            TokenKind::TyI32 => TypeKind::I32,
            TokenKind::TyI64 => TypeKind::I64,
            TokenKind::TyU8 => TypeKind::U8,
            TokenKind::TyF64 => TypeKind::F64,
            TokenKind::TyBool => TypeKind::Bool,
            TokenKind::TyString => TypeKind::String,
            TokenKind::Ident(name) => TypeKind::Named(name.clone()),
            _ => return Err(self.expected("type")),
        };
        self.bump();
        let mut ty = Type {
            kind,
            span: Span::new(start, self.prev_end()),
        };
        while self.at(&TokenKind::LBracket) && self.peek_at(1) == &TokenKind::RBracket {
            self.bump();
            self.bump();
            ty = Type {
                kind: TypeKind::Array(Box::new(ty)),
                span: Span::new(start, self.prev_end()),
            };
        }
        Ok(ty)
    }
}

/// True for tokens that begin a statement or expression — likely code written outside a function.
fn starts_statement(kind: &TokenKind) -> bool {
    use TokenKind::*;
    matches!(
        kind,
        Let | While
            | For
            | Return
            | If
            | LBrace
            | LParen
            | LBracket
            | Minus
            | Bang
            | True
            | False
            | Int(_)
            | Float(_)
            | Str(_)
            | Ident(_)
    )
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, parse_src, program};

    #[test]
    fn functions() {
        assert_eq!(
            program("fun f(a: i64, b: i64,): i64 { a }"),
            "(fun f ((a i64) (b i64)) i64 (block a))"
        );
        assert_eq!(program("fun main() { }"), "(fun main () void (block))");
        assert_eq!(
            program("fun f(g: i64[][], p: Point) {}"),
            "(fun f ((g i64[][]) (p Point)) void (block))"
        );
    }

    #[test]
    fn expression_body_is_a_block_with_a_tail() {
        assert_eq!(
            program("fun sq(x: i64): i64 = x * x;"),
            program("fun sq(x: i64): i64 { x * x }")
        );
        assert_eq!(
            program("fun g() = println(1);"),
            "(fun g () void (block (call println 1)))"
        );
    }

    #[test]
    fn externs_and_structs() {
        assert_eq!(
            program("extern fun puts(s: string): i32;"),
            "(extern puts ((s string)) i32)"
        );
        assert_eq!(
            program("extern fun exit(code: i32);"),
            "(extern exit ((code i32)) void)"
        );
        assert_eq!(
            program("struct Point { x: f64, y: f64, }"),
            "(struct Point (x f64) (y f64))"
        );
        assert_eq!(
            program("struct B { ok: bool, n: u8, s: string, xs: Point[] }"),
            "(struct B (ok bool) (n u8) (s string) (xs Point[]))"
        );
    }

    #[test]
    fn statements_at_top_level_are_e0201_with_help() {
        assert_eq!(errors("let x = 1;\nfun main() {}"), [("E0201", "let")]);
        let diags = parse_src("let x = 1;").unwrap_err();
        assert!(diags[0].help.is_some(), "{diags:?}");
        assert_eq!(errors("fun main() {} 42"), [("E0201", "42")]);
    }

    #[test]
    fn errors_in_headers_recover_at_the_next_item() {
        let src = "fun f( { }\nstruct S { x }\nfun ok() {}";
        let codes: Vec<_> = errors(src).into_iter().map(|(code, _)| code).collect();
        assert_eq!(codes, ["E0201", "E0201"]);
    }
}
