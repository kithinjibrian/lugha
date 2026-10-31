//! Top-level items and types (spec §3, §6, §8; v2 draft §3).

use super::{PResult, Parser};
use crate::ast::{
    ExternDecl, ExternTypeDecl, Field, Ident, Import, Item, Param, StructDecl, Type, TypeKind,
};
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
            // An actor is two items: its state and its methods (v17).
            if self.at_actor() {
                match self.actor_decl() {
                    Ok((state, methods)) => {
                        items.push(Item::Struct(state));
                        items.push(Item::Impl(methods));
                    }
                    Err(_) => {
                        self.sync_item();
                        if self.pos == before {
                            self.bump();
                        }
                    }
                }
                continue;
            }
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

    pub(super) fn item(&mut self) -> PResult<Item> {
        match self.peek() {
            TokenKind::Fun => self.fun_decl_with(false).map(Item::Fun),
            TokenKind::Extern => self.extern_item(),
            TokenKind::Struct => self.struct_decl().map(Item::Struct),
            TokenKind::Enum => self.enum_decl().map(Item::Enum),
            TokenKind::Impl => self.impl_decl().map(Item::Impl),
            TokenKind::Interface => self.interface_decl().map(Item::Interface),
            TokenKind::Const => self.const_decl().map(Item::Const),
            TokenKind::Private | TokenKind::Public => self.visible_item(),
            TokenKind::At => self.attributed_item(),
            // Bundled sources only: `$` lexes nowhere else (v17, milestone 141).
            _ if self.at_derive() => self.derive_decl(),
            other => {
                let is_statement = starts_statement(other);
                let reported = self
                    .expected("`fun`, `extern`, `const`, `struct`, `enum`, `impl` or `interface`");
                if is_statement && let Some(last) = self.diagnostics.last_mut() {
                    last.help = Some("statements and variables must be inside a function".into());
                }
                Err(reported)
            }
        }
    }

    /// `extern fun …;`, `extern "symbol" fun …;` or `extern type Name;` (v16).
    pub(super) fn extern_item(&mut self) -> PResult<Item> {
        let start = self.bump().start;
        // `type` isn't a keyword: only right after `extern` does it start a handle type.
        if matches!(self.peek(), TokenKind::Ident(word) if word == "type") {
            self.bump();
            let name = self.ident("type name")?;
            self.expect(&TokenKind::Semi)?;
            return Ok(Item::ExternType(ExternTypeDecl {
                visibility: None,
                name,
                span: Span::new(start, self.prev_end()),
            }));
        }
        // `extern union` (v17, milestone 104): C's union. `union` is a name
        // anywhere else.
        if matches!(self.peek(), TokenKind::Ident(word) if word == "union")
            && matches!(self.peek_at(1), TokenKind::Ident(_))
        {
            let union = self.span();
            let mut s = self.struct_decl()?;
            s.c_layout = Some(Span::new(start, start + "extern".len()));
            s.union = Some(union);
            s.span = Span::new(start, s.span.end);
            return Ok(Item::Struct(s));
        }
        // `extern struct` (v16): a struct with C's layout.
        if self.at(&TokenKind::Struct) {
            let mut s = self.struct_decl()?;
            s.c_layout = Some(Span::new(start, start + "extern".len()));
            s.span = Span::new(start, s.span.end);
            return Ok(Item::Struct(s));
        }
        let symbol = match self.peek() {
            TokenKind::Str(text) => {
                let name = text.clone();
                let span = self.bump();
                Some(Ident { name, span })
            }
            _ => None,
        };
        if !self.at(&TokenKind::Fun) {
            let what = if symbol.is_some() {
                "`fun`"
            } else {
                "`fun` or `type`"
            };
            return Err(self.expected(what));
        }
        self.bump();
        let name = self.ident("function name")?;
        let open = self.expect(&TokenKind::LParen)?;
        let params = self.comma_list(open, TokenKind::RParen, |p| p.extern_param())?;
        let ret = self.return_type()?;
        self.expect(&TokenKind::Semi)?;
        Ok(Item::Extern(ExternDecl {
            visibility: None,
            name,
            symbol,
            params,
            ret,
            span: Span::new(start, self.prev_end()),
        }))
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
                let Param { name, ty, .. } = p.param("field name")?;
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
            c_layout: None,
            union: None,
            actor: None,
        })
    }

    pub(super) fn param(&mut self, what: &str) -> PResult<Param> {
        // Fields read their visibility first; anywhere else one is a mistake
        // reported once, and parsing goes on as if it weren't there.
        if let Some(v) = self.visibility() {
            let message = "a parameter can't have a visibility";
            let help = "only items, methods and fields take `private` or `public`";
            self.error(Diagnostic::error("E0201", message, v.span).with_help(help));
        }
        self.misplaced_out();
        let name = self.ident(what)?;
        self.expect(&TokenKind::Colon)?;
        Ok(Param {
            name,
            ty: self.ty()?,
            mode: None,
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
            TokenKind::TyChar => TypeKind::Char,
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
    fn extern_types_and_renamed_externs_v16() {
        assert_eq!(program("extern type GtkWidget;"), "(extern-type GtkWidget)");
        assert_eq!(
            program("public extern type GtkWidget;"),
            "(public extern-type GtkWidget)"
        );
        assert_eq!(
            program(
                "extern \"g_signal_connect_data\" fun on_clicked(w: GtkWidget, f: fun(GtkWidget, data), u: data, d: release): u64;"
            ),
            "(extern on_clicked \"g_signal_connect_data\" ((w GtkWidget) (f (fun-type (GtkWidget data) void)) (u data) (d release)) u64)"
        );
        assert_eq!(
            program("private extern \"puts\" fun say(s: string): i32;"),
            "(private extern say \"puts\" ((s string)) i32)"
        );
        assert_eq!(errors("extern type T { }"), [("E0201", "{")]);
        assert_eq!(errors("extern type T<U>;"), [("E0201", "<")]);
        assert_eq!(errors("extern \"puts\" type T;"), [("E0201", "type")]);
        // v16: `extern struct` declares a struct with C's layout, so a body is required.
        assert_eq!(errors("extern struct S;")[0], ("E0201", ";"));
        assert_eq!(errors("extern enum E {}")[0], ("E0201", "enum"));
    }

    #[test]
    fn extern_structs_v16() {
        assert_eq!(
            program("extern struct Color { r: u8, g: u8, b: u8, a: u8 }"),
            "(extern-struct Color (r u8) (g u8) (b u8) (a u8))"
        );
        assert_eq!(
            program("public extern struct V { x: f32, y: f32 }"),
            "(public extern-struct V (x f32) (y f32))"
        );
        assert_eq!(errors("extern \"s\" struct S {}")[0], ("E0201", "struct"));
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
