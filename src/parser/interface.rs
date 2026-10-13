use super::{PResult, Parser};
use crate::ast::{Bound, InterfaceDecl, InterfaceItem};
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `interface Name { fun area(self): f64; … }`.
    pub(super) fn interface_decl(&mut self) -> PResult<InterfaceDecl> {
        let start = self.bump().start;
        let name = self.ident("interface name")?;
        let open = self.expect(&TokenKind::LBrace)?;
        let mut items = Vec::new();
        while !self.at(&TokenKind::RBrace) {
            if !self.at(&TokenKind::Fun) {
                if self.at(&TokenKind::Eof) {
                    self.close(open, &TokenKind::RBrace)?;
                }
                return Err(self.expected("`fun` or `}`"));
            }
            items.push(self.interface_item()?);
        }
        self.bump();
        Ok(InterfaceDecl {
            visibility: None,
            name,
            items,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// `fun name(receiver, params): ret;`: a signature, never a body.
    fn interface_item(&mut self) -> PResult<InterfaceItem> {
        let start = self.bump().start;
        let name = self.ident("method name")?;
        let (receiver, params) = self.receiver_params(true)?;
        let ret = self.return_type()?;
        self.expect(&TokenKind::Semi)?;
        Ok(InterfaceItem {
            name,
            receiver,
            params,
            ret,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// `Eq`, `Ord`, `Shape` or `m.Shape`.
    pub(super) fn bound(&mut self) -> PResult<Bound> {
        let first = self.ident("bound name")?;
        let (module, name) = if self.eat(&TokenKind::Dot) {
            (Some(first), self.ident("bound name")?)
        } else {
            (None, first)
        };
        let start = module.as_ref().map_or(name.span.start, |m| m.span.start);
        let span = Span::new(start, name.span.end);
        Ok(Bound { module, name, span })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, program};

    #[test]
    fn interfaces_list_signatures_without_bodies() {
        assert_eq!(
            program(
                "interface Shape { fun area(self): f64; fun grow(mut self, by: f64); fun make(r: f64): Circle; }"
            ),
            "(interface Shape (fun area (self) f64) (fun grow ((mut self) (by f64)) void) (fun make ((r f64)) Circle))"
        );
        assert_eq!(
            program("private interface Empty {}"),
            "(private interface Empty)"
        );
    }

    #[test]
    fn bounds_name_interfaces_here_or_in_a_module() {
        assert_eq!(
            program("fun f<T: Shape + Eq, U: shapes.Shape>(x: T) {}"),
            "(fun f (type-params (T Shape Eq) (U shapes.Shape)) ((x T)) void (block))"
        );
    }

    #[test]
    fn an_interface_item_with_a_body_is_an_error() {
        assert_eq!(
            errors("interface Shape { fun area(self): f64 { 1.0 } }"),
            [("E0201", "{")]
        );
    }
}
