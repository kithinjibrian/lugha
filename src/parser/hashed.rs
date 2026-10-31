//! Map and set literals (v17, milestone 101): `#{k: v, …}`, `#{x, …}` and
//! `#{}`. The first entry decides which; a trailing comma is allowed.

use super::{PResult, Parser};
use crate::ast::{Expr, ExprKind, HashKind, HashLiteral};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

impl Parser<'_> {
    /// `#{ … }` at its `#{`. Never inlined: `primary` is on every nesting
    /// level's stack, and E0206's depth limit assumes small frames.
    #[inline(never)]
    pub(super) fn hash_literal(&mut self, start: usize) -> PResult<Expr> {
        let open = self.bump();
        self.with_struct_literals(true, |p| p.hashed(start, open))
    }

    /// The rest of `#{ … }` after its `#{`.
    fn hashed(&mut self, start: usize, open: Span) -> PResult<Expr> {
        let (mut keys, mut values) = (Vec::new(), Vec::new());
        let mut kind = HashKind::Empty;
        while !self.at(&TokenKind::RBrace) {
            // `#{a: loop { … }}` is the entry `a`, not a labelled loop.
            let no_label = std::mem::replace(&mut self.no_label, true);
            let key = self.expr();
            self.no_label = no_label;
            let key = key?;
            let pair = self.eat(&TokenKind::Colon);
            let this = if pair { HashKind::Map } else { HashKind::Set };
            if kind != HashKind::Empty && kind != this {
                let message = match kind {
                    HashKind::Map => "this map's entries are `key: value`",
                    _ => "this set's entries are values, not `key: value`",
                };
                // Reported, and parsing goes on: the rest of the literal still reads.
                self.error(Diagnostic::error("E0201", message, key.span));
            } else {
                kind = this;
            }
            let value = if pair { Some(self.expr()?) } else { None };
            if this == kind {
                keys.push(key);
                values.extend(value);
            }
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        self.close(open, &TokenKind::RBrace)?;
        let keys = self.mk(start, ExprKind::Array(keys));
        let values = self.mk(start, ExprKind::Array(values));
        let literal = HashLiteral { kind, keys, values };
        Ok(self.mk(start, ExprKind::Hashed(Box::new(literal))))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_util::{errors, program};

    #[test]
    fn maps_sets_and_empty_v17() {
        assert_eq!(
            program("fun f() { let m = #{\"a\": 1, \"b\": 2,}; let s = #{1, 2}; let e = #{}; }"),
            "(fun f () void (block (let m (map (array \"a\" \"b\") (array 1 2))) (let s (set (array 1 2))) (let e (hash-empty))))"
        );
        assert_eq!(
            program("fun f() { let n = #{(1, 2): #{3}}; }"),
            "(fun f () void (block (let n (map (array (tuple 1 2)) (array (set (array 3)))))))"
        );
    }

    #[test]
    fn mixing_or_a_lone_hash_is_an_error_v17() {
        assert_eq!(
            errors("fun f() { let m = #{\"a\": 1, 2}; }"),
            [("E0201", "2")]
        );
        assert_eq!(
            errors("fun f() { let m = #{1, \"a\": 2}; }"),
            [("E0201", "\"a\"")]
        );
        assert_eq!(errors("fun f() { let m = #{1 2}; }")[0], ("E0201", "2"));
        // A `#` not followed by `{` is no token (E0101 from the lexer).
        let lexed = crate::lexer::lex("let m = # {1};");
        assert!(
            matches!(lexed, Err(ref d) if d[0].code == "E0101"),
            "{lexed:?}"
        );
    }
}
