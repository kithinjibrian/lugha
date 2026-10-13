//! v1 declarations: `enum`, `impl`, and parameter lists that may start with a
//! `self` receiver (v1 draft §3).

use super::{PResult, Parser};
use crate::ast::{Block, EnumDecl, FunDecl, ImplDecl, Param, Receiver, Variant};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `enum Name { Variant, Variant(T, …), … }`.
    pub(super) fn enum_decl(&mut self) -> PResult<EnumDecl> {
        let start = self.bump().start;
        let name = self.ident("enum name")?;
        let type_params = self.type_params()?;
        let open = self.expect(&TokenKind::LBrace)?;
        let variants = self.comma_list(open, TokenKind::RBrace, |p| {
            let name = p.ident("variant name")?;
            let payloads = if p.at(&TokenKind::LParen) {
                let open = p.bump();
                p.comma_list(open, TokenKind::RParen, |p| p.ty())?
            } else {
                Vec::new()
            };
            let span = Span::new(name.span.start, p.prev_end());
            Ok(Variant {
                name,
                payloads,
                span,
            })
        })?;
        Ok(EnumDecl {
            visibility: None,
            name,
            type_params,
            variants,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// `impl [m.]Type[<T, …>] { [pub] fun … }` (v2 draft §3).
    pub(super) fn impl_decl(&mut self) -> PResult<ImplDecl> {
        let start = self.bump().start;
        let first = self.ident("type name")?;
        let (module, ty) = if self.eat(&TokenKind::Dot) {
            (Some(first), self.ident("type name")?)
        } else {
            (None, first)
        };
        let params = self.impl_params()?;
        let open = self.expect(&TokenKind::LBrace)?;
        let mut methods = Vec::new();
        while !self.at(&TokenKind::RBrace) {
            let visibility = self.visibility();
            if !self.at(&TokenKind::Fun) {
                if self.at(&TokenKind::Eof) {
                    self.close(open, &TokenKind::RBrace)?;
                }
                return Err(self.expected("`fun` or `}`"));
            }
            let mut method = self.fun_decl_with(true)?;
            method.visibility = visibility;
            methods.push(method);
        }
        self.bump();
        Ok(ImplDecl {
            module,
            ty,
            params,
            methods,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// `fun name(params): ret body`; in an `impl`, the parameters may start
    /// with `self` or `mut self`.
    pub(super) fn fun_decl_with(&mut self, in_impl: bool) -> PResult<FunDecl> {
        let start = self.bump().start;
        let name = self.ident("function name")?;
        let type_params = self.type_params()?;
        let (receiver, params) = self.receiver_params(in_impl)?;
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
            visibility: None,
            name,
            type_params,
            receiver,
            params,
            ret,
            body,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// `( [receiver ,] params )`. Outside an `impl` a receiver is E0201, and
    /// parsing continues without it.
    pub(super) fn receiver_params(
        &mut self,
        in_impl: bool,
    ) -> PResult<(Option<Receiver>, Vec<Param>)> {
        let open = self.expect(&TokenKind::LParen)?;
        let starts_receiver = self.at(&TokenKind::SelfKw)
            || (self.at(&TokenKind::Mut) && self.peek_at(1) == &TokenKind::SelfKw);
        if !starts_receiver {
            let params = self.comma_list(open, TokenKind::RParen, |p| p.param("parameter name"))?;
            return Ok((None, params));
        }
        let start = self.span().start;
        let mutable = self.eat(&TokenKind::Mut);
        self.bump();
        let receiver = Receiver {
            mutable,
            span: Span::new(start, self.prev_end()),
        };
        if !in_impl {
            let diagnostic =
                Diagnostic::error("E0201", "`self` is only allowed in methods", receiver.span)
                    .with_help("put this function in an `impl` block, or remove `self`");
            self.error(diagnostic);
        }
        let params = if self.eat(&TokenKind::Comma) {
            self.comma_list(open, TokenKind::RParen, |p| p.param("parameter name"))?
        } else {
            self.close(open, &TokenKind::RParen)?;
            Vec::new()
        };
        Ok((in_impl.then_some(receiver), params))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, program};

    #[test]
    fn enums_have_positional_payloads() {
        assert_eq!(
            program("enum Shape { Circle(f64), Rect(f64, f64,), Empty, }"),
            "(enum Shape (Circle f64) (Rect f64 f64) Empty)"
        );
        assert_eq!(program("enum E {}"), "(enum E)");
        assert_eq!(
            program("enum List { Nil, Cons(i64, List) }"),
            "(enum List Nil (Cons i64 List))"
        );
    }

    #[test]
    fn impl_blocks_hold_methods_with_receivers() {
        let src = "impl Point {\n\
                   fun origin(): Point = Point { x: 0.0 };\n\
                   fun dist(self, q: Point): f64 = q.x;\n\
                   fun scale(mut self, k: f64) { self.x *= k; }\n\
                   }";
        assert_eq!(
            program(src),
            "(impl Point \
             (fun origin () Point (block (struct-lit Point (x 0.0)))) \
             (fun dist (self (q Point)) f64 (block (. q x))) \
             (fun scale ((mut self) (k f64)) void (block (*= (. self x) k))))"
        );
    }

    #[test]
    fn self_outside_impl_is_e0201() {
        assert_eq!(errors("fun f(self) {}"), [("E0201", "self")]);
        assert_eq!(
            errors("fun f(mut self, x: i64) {}"),
            [("E0201", "mut self")]
        );
        assert_eq!(errors("impl P { let x = 1; }"), [("E0201", "let")]);
    }

    #[test]
    fn option_types_and_self_expressions() {
        assert_eq!(
            program("fun f(a: Option<Option<i64>>, b: Option<i64>[]): Option<P> = None;"),
            "(fun f ((a (Option (Option i64))) (b (Option i64)[])) (Option P) (block None))"
        );
        assert_eq!(
            program("impl P { fun me(self): P = self; }"),
            "(impl P (fun me (self) P (block self)))"
        );
    }

    #[test]
    fn rust_paths_are_e0207_with_help_and_read_as_dots() {
        use crate::parser::test_util::{expr, parse_src};
        let diags = parse_src("fun t() { Shape::Circle(1.0) }").expect_err("E0207");
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert_eq!(diags[0].code, "E0207");
        assert_eq!(diags[0].help.as_deref(), Some("write `Shape.Circle`"));
        assert_eq!(expr("Shape.Circle(1.0)"), "(call (. Shape Circle) 1.0)");
    }

    #[test]
    fn top_level_recovery_stops_at_enum_and_impl() {
        assert_eq!(
            errors("let x = 1;\nenum E { A B }\nimpl P { fun f(self) {} }"),
            [("E0201", "let"), ("E0201", "B")]
        );
    }
}
