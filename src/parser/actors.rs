//! Actor declarations (v17, milestone 92): `[visibility] actor Name { … }`
//! holding fields and methods in any order. The parser gives two items: the
//! state as a struct marked `actor`, then its methods as an `impl` marked
//! `actor`, so the checker and codegen reuse struct and method handling.

use super::{PResult, Parser};
use crate::ast::{Field, Ident, ImplDecl, Param, StructDecl};
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// Whether an actor starts here: `actor`, or a visibility then `actor`.
    pub(super) fn at_actor(&self) -> bool {
        match self.peek() {
            TokenKind::Actor => true,
            TokenKind::Private | TokenKind::Public => self.peek_at(1) == &TokenKind::Actor,
            _ => false,
        }
    }

    /// `[visibility] actor Name { fields and methods }`: its state and its
    /// methods.
    pub(super) fn actor_decl(&mut self) -> PResult<(StructDecl, ImplDecl)> {
        let visibility = self.visibility();
        let start = visibility.map_or(self.span().start, |v| v.span.start);
        let keyword = self.bump();
        let name = self.ident("actor name")?;
        // Parsed so the checker can say actors aren't generic (E0439).
        let type_params = self.type_params()?;
        let open = self.expect(&TokenKind::LBrace)?;
        let params: Vec<Ident> = type_params.iter().map(|p| p.name.clone()).collect();
        let body = self.in_impl(&name, &params, |p| p.actor_body(open));
        let (fields, methods) = match body {
            Ok(members) => members,
            Err(reported) => {
                // One mistake, one error: skip the rest of the actor.
                self.skip_to_close();
                return Err(reported);
            }
        };
        let span = Span::new(start, self.prev_end());
        let state = StructDecl {
            visibility,
            name: name.clone(),
            type_params: type_params.clone(),
            fields,
            span,
            c_layout: None,
            actor: Some(keyword),
        };
        let methods = ImplDecl {
            module: None,
            ty: name,
            params: type_params.clone(),
            methods,
            span,
            actor: true,
        };
        Ok((state, methods))
    }

    /// Skips to just past the `}` closing the actor being read.
    fn skip_to_close(&mut self) {
        let mut depth = 1usize;
        loop {
            match self.peek() {
                TokenKind::Eof => return,
                TokenKind::LBrace => depth += 1,
                TokenKind::RBrace => {
                    depth -= 1;
                    if depth == 0 {
                        self.bump();
                        return;
                    }
                }
                _ => {}
            }
            self.bump();
        }
    }

    /// Fields (each followed by `,` unless last) and methods, in any order,
    /// up to and including the `}`.
    fn actor_body(&mut self, open: Span) -> PResult<(Vec<Field>, Vec<crate::ast::FunDecl>)> {
        let (mut fields, mut methods) = (Vec::new(), Vec::new());
        loop {
            if self.eat(&TokenKind::RBrace) {
                return Ok((fields, methods));
            }
            if self.at(&TokenKind::Eof) {
                self.close(open, &TokenKind::RBrace)?;
            }
            let visibility = self.visibility();
            match self.peek() {
                TokenKind::Fun => {
                    let mut method = self.fun_decl_with(true)?;
                    method.visibility = visibility;
                    methods.push(method);
                }
                TokenKind::Ident(_) => {
                    let Param { name, ty, .. } = self.param("field name")?;
                    fields.push(Field {
                        visibility,
                        name,
                        ty,
                    });
                    if !self.at(&TokenKind::RBrace) {
                        self.expect(&TokenKind::Comma)?;
                    }
                }
                _ => return Err(self.expected("a field, `fun` or `}`")),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, program};

    #[test]
    fn an_actor_is_its_state_and_its_methods() {
        assert_eq!(
            program(
                "actor Counter {
                    count: i64,
                    fun add(mut self, n: i64) { self.count += n; }
                    label: string,
                    fun get(self): i64 = self.count;
                }"
            ),
            "(actor Counter (count i64) (label string))\n\
             (actor-methods Counter (fun add ((mut self) (n i64)) void (block (+= (. self count) n))) \
             (fun get (self) i64 (block (. self count))))"
        );
        assert_eq!(
            program("public actor Log { private lines: i64 }"),
            "(public actor Log (private lines i64))\n(actor-methods Log)"
        );
        // `Self` is the state, as in an `impl`.
        assert_eq!(
            program("actor A { n: i64, fun new(): Self = Self { n: 0 }; }"),
            "(actor A (n i64))\n(actor-methods A (fun new () A (block (struct-lit A (n 0)))))"
        );
    }

    #[test]
    fn anything_else_in_an_actor_is_e0201() {
        assert_eq!(errors("actor A { enum E { X } }"), [("E0201", "enum")]);
        assert_eq!(errors("actor A { n: i64 m: i64 }"), [("E0201", "m")]);
    }
}
