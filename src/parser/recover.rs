//! Error recovery and parsing modes: nesting limit (E0206), struct-literal
//! mode, comma lists, and skipping to a safe point after an error.

use std::mem;

use super::{PResult, Parser};
use crate::diagnostic::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::Span;

/// Deepest nesting of expressions and blocks before E0206 (spec §3).
const MAX_DEPTH: u32 = 256;

impl Parser<'_> {
    /// Parses `item, item, … closer` after an opener at `open`; a trailing comma is allowed.
    pub(super) fn comma_list<T>(
        &mut self,
        open: Span,
        closer: TokenKind,
        mut item: impl FnMut(&mut Self) -> PResult<T>,
    ) -> PResult<Vec<T>> {
        let mut items = Vec::new();
        loop {
            if self.at(&closer) || self.at(&TokenKind::Eof) {
                self.close(open, &closer)?;
                return Ok(items);
            }
            items.push(item(self)?);
            if !self.eat(&TokenKind::Comma) {
                self.close(open, &closer)?;
                return Ok(items);
            }
        }
    }

    /// Runs `f` one nesting level deeper, failing with E0206 past `MAX_DEPTH`
    /// so hostile input can't overflow the stack.
    pub(super) fn nested<T>(&mut self, f: impl FnOnce(&mut Self) -> PResult<T>) -> PResult<T> {
        if self.depth >= MAX_DEPTH {
            let help = format!("expressions and blocks may nest at most {MAX_DEPTH} levels deep");
            let diagnostic = Diagnostic::error("E0206", "nesting too deep", self.span());
            return Err(self.error(diagnostic.with_help(help)));
        }
        self.depth += 1;
        let result = f(self);
        self.depth -= 1;
        result
    }

    /// Runs `f` with struct literals allowed or not, restoring the previous mode after.
    pub(super) fn with_struct_literals<T>(
        &mut self,
        allowed: bool,
        f: impl FnOnce(&mut Self) -> PResult<T>,
    ) -> PResult<T> {
        let saved = mem::replace(&mut self.no_struct, !allowed);
        let result = f(self);
        self.no_struct = saved;
        result
    }

    /// After an error in a block: skips past the next `;`, or up to the `}`
    /// closing this block, ignoring anything inside nested brackets.
    pub(super) fn sync_stmt(&mut self) {
        let mut depth = 0usize;
        loop {
            match self.peek() {
                TokenKind::Eof => return,
                TokenKind::Semi if depth == 0 => {
                    self.bump();
                    return;
                }
                TokenKind::RBrace if depth == 0 => return,
                TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => depth += 1,
                TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                    depth = depth.saturating_sub(1);
                }
                _ => {}
            }
            self.bump();
        }
    }

    /// After an error at top level: skips to the next `fun`, `extern` or `struct` outside brackets.
    pub(super) fn sync_item(&mut self) {
        let mut depth = 0usize;
        loop {
            match self.peek() {
                TokenKind::Eof => return,
                TokenKind::Fun | TokenKind::Extern | TokenKind::Struct if depth == 0 => return,
                TokenKind::LParen | TokenKind::LBracket | TokenKind::LBrace => depth += 1,
                TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                    depth = depth.saturating_sub(1);
                }
                _ => {}
            }
            self.bump();
        }
    }
}
