use super::{PResult, Parser};
use crate::ast::{Bound, FunDecl, InterfaceDecl, InterfaceItem};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `interface Name { fun area(self): f64; … }`.
    pub(super) fn interface_decl(&mut self) -> PResult<InterfaceDecl> {
        let start = self.bump().start;
        let name = self.ident("interface name")?;
        let type_params = self.type_params()?;
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
            type_params,
            items,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// `fun name(receiver, params): ret;`, or with a default body after the
    /// signature (v5): `= expr;` or a block, for methods only (E0201).
    fn interface_item(&mut self) -> PResult<InterfaceItem> {
        let start = self.bump().start;
        let name = self.ident("method name")?;
        let (receiver, params) = self.receiver_params(true)?;
        let ret = self.return_type()?;
        let default = if self.eat(&TokenKind::Semi) {
            None
        } else {
            let body = self.fun_body()?;
            let span = Span::new(start, self.prev_end());
            if receiver.is_none() {
                let message = "a default body needs `self`: only methods can have one";
                self.error(Diagnostic::error("E0201", message, span));
            }
            Some(FunDecl {
                visibility: None,
                name: name.clone(),
                type_params: Vec::new(),
                receiver,
                params: params.clone(),
                ret: ret.clone(),
                body,
                span,
            })
        };
        Ok(InterfaceItem {
            name,
            receiver,
            params,
            ret,
            default,
            span: Span::new(start, self.prev_end()),
        })
    }

    /// `Eq`, `Ord`, `Shape`, `m.Shape`, or with arguments, `Source<T>` (v6).
    pub(super) fn bound(&mut self) -> PResult<Bound> {
        let first = self.ident("bound name")?;
        let (module, name) = self.qualified_name(first, "bound name")?;
        let args = if self.at(&TokenKind::Lt) {
            let open = self.bump();
            if self.at(&TokenKind::Gt) {
                return Err(self.expected("type"));
            }
            self.nested(|p| p.comma_list(open, TokenKind::Gt, |p| p.ty()))?
        } else {
            Vec::new()
        };
        let start = module.as_ref().map_or(name.span.start, |m| m.span.start);
        let span = Span::new(start, self.prev_end());
        Ok(Bound {
            module,
            name,
            args,
            span,
        })
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
    fn methods_may_have_default_bodies() {
        assert_eq!(
            program(
                "interface Shape { fun area(self): f64; fun name(self): string = \"s\"; fun big(self): bool { self.area() > 1.0 } }"
            ),
            "(interface Shape (fun area (self) f64) (fun name (self) string (block \"s\")) (fun big (self) bool (block (> (call (. self area)) 1.0))))"
        );
    }

    #[test]
    fn interfaces_take_type_parameters_and_bounds_arguments() {
        assert_eq!(
            program("interface Source<T: Eq> { fun next(mut self): Option<T>; }"),
            "(interface Source (type-params (T Eq)) (fun next ((mut self)) (Option T)))"
        );
        assert_eq!(
            program("fun f<T, S: Source<T> + geo.Sink<T[]>>(s: S) {}"),
            "(fun f (type-params T (S (Source T) (geo.Sink T[]))) ((s S)) void (block))"
        );
    }

    #[test]
    fn a_default_needs_self() {
        assert_eq!(
            errors("interface Make { fun make(): i64 = 1; }"),
            [("E0201", "fun make(): i64 = 1;")]
        );
    }
}
