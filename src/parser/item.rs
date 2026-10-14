//! Top-level items and types (spec §3, §6, §8; v2 draft §3).

use super::{PResult, Parser};
use crate::ast::{ExternDecl, Field, Ident, Import, Item, Param, StructDecl, Type, TypeKind};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// Parses the imports, then every item up to `Eof`, recovering at the
    /// next item after an error.
    pub(super) fn program(&mut self) -> (Vec<Import>, Vec<Item>) {
        let mut imports = Vec::new();
        loop {
            // `public import a.b { X };` re-exports; `private import` is E0201.
            let public = self.import_visibility();
            if !self.at(&TokenKind::Import) {
                break;
            }
            match self.import_with(public) {
                Ok(import) => imports.push(import),
                Err(_) => self.sync_item(),
            }
        }
        let mut items: Vec<Item> = Vec::new();
        let mut first_item = None;
        while !self.at(&TokenKind::Eof) {
            let before = self.pos;
            if self.at(&TokenKind::Import) {
                let first = first_item.unwrap_or_else(|| self.span());
                self.late_import(first);
                if self.pos == before {
                    self.bump();
                }
                continue;
            }
            first_item.get_or_insert(self.span());
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
        (imports, items)
    }

    fn item(&mut self) -> PResult<Item> {
        match self.peek() {
            TokenKind::Fun => self.fun_decl_with(false).map(Item::Fun),
            TokenKind::Extern => self.extern_decl().map(Item::Extern),
            TokenKind::Struct => self.struct_decl().map(Item::Struct),
            TokenKind::Enum => self.enum_decl().map(Item::Enum),
            TokenKind::Impl => self.impl_decl().map(Item::Impl),
            TokenKind::Interface => self.interface_decl().map(Item::Interface),
            TokenKind::Private | TokenKind::Public => self.visible_item(),
            other => {
                let is_statement = starts_statement(other);
                let reported =
                    self.expected("`fun`, `extern`, `struct`, `enum`, `impl` or `interface`");
                if is_statement && let Some(last) = self.diagnostics.last_mut() {
                    last.help = Some("statements and variables must be inside a function".into());
                }
                Err(reported)
            }
        }
    }

    pub(super) fn extern_decl(&mut self) -> PResult<ExternDecl> {
        let start = self.bump().start;
        self.expect(&TokenKind::Fun)?;
        let name = self.ident("function name")?;
        let params = self.params()?;
        let ret = self.return_type()?;
        self.expect(&TokenKind::Semi)?;
        Ok(ExternDecl {
            visibility: None,
            name,
            params,
            ret,
            span: Span::new(start, self.prev_end()),
        })
    }

    pub(super) fn struct_decl(&mut self) -> PResult<StructDecl> {
        let start = self.bump().start;
        let name = self.ident("struct name")?;
        let type_params = self.type_params()?;
        let open = self.expect(&TokenKind::LBrace)?;
        // `Self` in a field is this struct (v13), as in its `impl`.
        let params: Vec<Ident> = type_params.iter().map(|p| p.name.clone()).collect();
        let fields = self.in_impl(&name, &params, |this| {
            this.comma_list(open, TokenKind::RBrace, |p| {
                let visibility = p.visibility();
                let Param { name, ty } = p.param("field name")?;
                Ok(Field {
                    visibility,
                    name,
                    ty,
                })
            })
        })?;
        Ok(StructDecl {
            visibility: None,
            name,
            type_params,
            fields,
            span: Span::new(start, self.prev_end()),
        })
    }

    fn params(&mut self) -> PResult<Vec<Param>> {
        let open = self.expect(&TokenKind::LParen)?;
        self.comma_list(open, TokenKind::RParen, |p| p.param("parameter name"))
    }

    pub(super) fn param(&mut self, what: &str) -> PResult<Param> {
        // Fields read their visibility first; anywhere else one is a mistake
        // reported once, and parsing goes on as if it weren't there.
        if let Some(v) = self.visibility() {
            let message = "a parameter can't have a visibility";
            let help = "only items, methods and fields take `private` or `public`";
            self.error(Diagnostic::error("E0201", message, v.span).with_help(help));
        }
        let name = self.ident(what)?;
        self.expect(&TokenKind::Colon)?;
        Ok(Param {
            name,
            ty: self.ty()?,
        })
    }

    pub(super) fn return_type(&mut self) -> PResult<Option<Type>> {
        if self.eat(&TokenKind::Colon) {
            self.ty().map(Some)
        } else {
            Ok(None)
        }
    }

    /// Parses `base_type { "[" "]" }`.
    pub(super) fn ty(&mut self) -> PResult<Type> {
        let start = self.span().start;
        // `fun(…): R` and `(T)` are read whole by `compound_type` (v3).
        let compound = matches!(self.peek(), TokenKind::Fun | TokenKind::LParen);
        let kind = match self.peek() {
            TokenKind::Fun | TokenKind::LParen => self.compound_type()?,
            TokenKind::TyI32 => TypeKind::I32,
            TokenKind::TyI64 => TypeKind::I64,
            TokenKind::TyU8 => TypeKind::U8,
            TokenKind::TyF64 => TypeKind::F64,
            TokenKind::TyBool => TypeKind::Bool,
            TokenKind::TyString => TypeKind::String,
            TokenKind::TyU32 => TypeKind::U32,
            TokenKind::TyU64 => TypeKind::U64,
            TokenKind::TyF32 => TypeKind::F32,
            TokenKind::TyI8 => TypeKind::I8,
            TokenKind::TyI16 => TypeKind::I16,
            TokenKind::TyU16 => TypeKind::U16,
            // `Option<T>` (v1); elsewhere `Option` is an ordinary name.
            TokenKind::Ident(name) if name == "Option" && self.peek_at(1) == &TokenKind::Lt => {
                self.bump();
                self.bump();
                let inner = self.ty()?;
                self.expect(&TokenKind::Gt)?;
                TypeKind::Option(Box::new(inner))
            }
            TokenKind::Ident(_) => {
                let first = self.ident("type")?;
                self.named_type(first)?
            }
            _ => return Err(self.expected("type")),
        };
        if !compound
            && !matches!(
                kind,
                TypeKind::Option(_) | TypeKind::Named(_) | TypeKind::Applied(_)
            )
        {
            self.bump();
        }
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
