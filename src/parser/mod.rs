//! Parser — turns lexer tokens into an AST (spec §3).
//!
//! Recursive descent for items and statements, Pratt parsing for
//! expressions. Reports every syntax error it can (E0201–E0206), recovering
//! at statement and item boundaries. Does no type checking, name resolution,
//! place validation or literal range checks — those belong to the checker.
//!
//! Depends on: ast, lexer (tokens), diagnostic, span.

mod describe;
mod expr;
mod item;
mod primary;
mod recover;
pub mod sexp;
mod stmt;
#[cfg(test)]
mod test_util;

use crate::ast::{Expr, ExprId, ExprKind, Ident, Program};
use crate::diagnostic::Diagnostic;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

/// Parses a token list ending in `Eof` (as produced by `lex`) into a program.
///
/// # Errors
///
/// Returns every syntax error found (codes E0201–E0206) if there is at least one.
///
/// # Examples
///
/// ```
/// let (tokens, _) = lugha::lexer::lex("fun main(): i32 { 2 + 3 * 4 }").unwrap();
/// let (program, _) = lugha::parser::parse(&tokens).unwrap();
/// assert_eq!(lugha::parser::sexp::program(&program), "(fun main () i32 (block (+ 2 (* 3 4))))");
/// ```
pub fn parse(tokens: &[Token]) -> Result<(Program, Vec<Diagnostic>), Vec<Diagnostic>> {
    let mut parser = Parser {
        tokens,
        pos: 0,
        next_id: 0,
        depth: 0,
        no_struct: false,
        diagnostics: Vec::new(),
    };
    let items = parser.program();
    if parser.diagnostics.is_empty() {
        Ok((
            Program {
                items,
                expr_count: parser.next_id,
            },
            Vec::new(),
        ))
    } else {
        Err(parser.diagnostics)
    }
}

/// A diagnostic for this failure is already recorded; callers recover instead of reporting again.
struct Reported;

type PResult<T> = Result<T, Reported>;

/// Returned by `peek` past the end, so a token list missing its `Eof` can't cause a panic.
static EOF: TokenKind = TokenKind::Eof;

struct Parser<'t> {
    tokens: &'t [Token],
    /// Index of the current token; never moves past `Eof`.
    pos: usize,
    next_id: u32,
    depth: u32,
    /// True while parsing a condition, where `Name {` ends the expression (spec §3).
    no_struct: bool,
    diagnostics: Vec<Diagnostic>,
}

impl Parser<'_> {
    fn peek(&self) -> &TokenKind {
        self.peek_at(0)
    }

    fn peek_at(&self, n: usize) -> &TokenKind {
        self.tokens.get(self.pos + n).map_or(&EOF, |t| &t.kind)
    }

    fn at(&self, kind: &TokenKind) -> bool {
        self.peek() == kind
    }

    /// Span of the current token.
    fn span(&self) -> Span {
        match self.tokens.get(self.pos) {
            Some(token) => token.span,
            None => {
                let end = self.tokens.last().map_or(0, |t| t.span.end);
                Span::new(end, end)
            }
        }
    }

    /// End of the last consumed token: where a node that just finished ends.
    fn prev_end(&self) -> usize {
        self.pos
            .checked_sub(1)
            .and_then(|i| self.tokens.get(i))
            .map_or(0, |t| t.span.end)
    }

    /// Consumes the current token (never `Eof`) and returns its span.
    fn bump(&mut self) -> Span {
        let span = self.span();
        if !self.at(&TokenKind::Eof) {
            self.pos += 1;
        }
        span
    }

    fn eat(&mut self, kind: &TokenKind) -> bool {
        let hit = self.at(kind);
        if hit {
            self.bump();
        }
        hit
    }

    fn expect(&mut self, kind: &TokenKind) -> PResult<Span> {
        if self.at(kind) {
            Ok(self.bump())
        } else {
            Err(self.expected(&describe::text(kind)))
        }
    }

    fn ident(&mut self, what: &str) -> PResult<Ident> {
        let TokenKind::Ident(name) = self.peek() else {
            return Err(self.expected(what));
        };
        let name = name.clone();
        Ok(Ident {
            name,
            span: self.bump(),
        })
    }

    /// Creates an expression spanning `start` to the last consumed token, with the next id.
    fn mk(&mut self, start: usize, kind: ExprKind) -> Expr {
        let id = ExprId(self.next_id);
        self.next_id += 1;
        Expr {
            id,
            span: Span::new(start, self.prev_end()),
            kind,
        }
    }

    fn error(&mut self, diagnostic: Diagnostic) -> Reported {
        self.diagnostics.push(diagnostic);
        Reported
    }

    /// Reports "expected {what}, found …" at the current token (E0201) — or
    /// E0204 when an assignment operator stands where the expression should end.
    fn expected(&mut self, what: &str) -> Reported {
        let span = self.span();
        if let Some(op) = describe::assign_symbol(self.peek()) {
            let message = "assignment is a statement, not an expression";
            let mut diagnostic = Diagnostic::error("E0204", message, span);
            if op == "=" {
                diagnostic = diagnostic.with_help("use `==` to compare");
            }
            return self.error(diagnostic);
        }
        let message = format!("expected {what}, found {}", describe::found(self.peek()));
        self.error(Diagnostic::error("E0201", message, span))
    }

    /// Consumes `closer`, which matches the opener at `open`.
    fn close(&mut self, open: Span, closer: &TokenKind) -> PResult<Span> {
        if self.at(closer) {
            return Ok(self.bump());
        }
        let opener = describe::opener(closer);
        if self.at(&TokenKind::Eof) {
            return Err(self.error(Diagnostic::error(
                "E0202",
                format!("unclosed {opener}"),
                open,
            )));
        }
        if describe::assign_symbol(self.peek()).is_some() {
            return Err(self.expected(""));
        }
        let found = describe::found(self.peek());
        let message = format!("expected {}, found {found}", describe::text(closer));
        let diagnostic = Diagnostic::error("E0201", message, self.span())
            .with_label(open, format!("to match this {opener}"));
        Err(self.error(diagnostic))
    }
}
