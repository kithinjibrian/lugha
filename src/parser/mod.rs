//! Parser — turns lexer tokens into an AST (spec §3).
//!
//! Recursive descent for items and statements, Pratt parsing for
//! expressions. Reports every syntax error it can (E0201–E0206), recovering
//! at statement and item boundaries. Does no type checking, name resolution,
//! place validation or literal range checks — those belong to the checker.
//!
//! Depends on: ast, lexer (tokens), diagnostic, span.

mod closure;
mod conditional;
mod decl;
mod describe;
mod expr;
mod format;
mod generic;
mod import;
mod interface;
mod item;
mod operators;
mod pattern;
mod primary;
mod recover;
mod self_type;
pub mod sexp;
mod stmt;
#[cfg(test)]
mod test_util;
mod tuples;

use crate::ast::{Expr, ExprId, ExprKind, Ident, Program};
use crate::diagnostic::Diagnostic;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

/// Parses a token list ending in `Eof` (as produced by `lex`) into a program.
///
/// # Errors
///
/// Returns every syntax error found (codes E0201–E0208) if there is at least one.
///
/// # Examples
///
/// ```
/// let (tokens, _) = lugha::lexer::lex("fun main(): i32 { 2 + 3 * 4 }").unwrap();
/// let (program, _) = lugha::parser::parse(&tokens).unwrap();
/// assert_eq!(lugha::parser::sexp::program(&program), "(fun main () i32 (block (+ 2 (* 3 4))))");
/// ```
pub fn parse(tokens: &[Token]) -> Result<(Program, Vec<Diagnostic>), Vec<Diagnostic>> {
    parse_from(tokens, 0)
}

/// [`parse`], numbering expressions from `first`: the modules of one program
/// share one id space (PRP-034), so each starts where the last one ended.
/// `Program::expr_count` is then one past the last id used.
///
/// # Errors
///
/// As [`parse`].
pub fn parse_from(
    tokens: &[Token],
    first: u32,
) -> Result<(Program, Vec<Diagnostic>), Vec<Diagnostic>> {
    let (program, diagnostics) = parse_partial_from(tokens, first);
    if diagnostics.is_empty() {
        Ok((program, Vec::new()))
    } else {
        Err(diagnostics)
    }
}

/// [`parse_from`], always returning the program the parser recovered: every
/// import, item and statement that parsed, without the broken ones, beside
/// every diagnostic (v16, milestone 75). For the language server, which
/// checks text being edited; the compiler itself stops at any diagnostic.
pub fn parse_partial_from(tokens: &[Token], first: u32) -> (Program, Vec<Diagnostic>) {
    let mut parser = Parser {
        tokens,
        pos: 0,
        next_id: first,
        depth: 0,
        no_struct: false,
        scrutinee: false,
        impl_self: None,
        diagnostics: Vec::new(),
    };
    let (imports, items) = parser.program();
    let program = Program {
        imports,
        items,
        expr_count: parser.next_id,
    };
    (program, parser.diagnostics)
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
    /// True while parsing a `match` scrutinee, where `{ name:` starts an
    /// arm with a type pattern rather than a misplaced struct literal (v5).
    scrutinee: bool,
    /// The type and parameters of the `impl` being read: what `Self` means (v10).
    impl_self: Option<(Ident, Vec<Ident>)>,
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

#[cfg(test)]
mod partial_tests {
    use super::*;
    use crate::ast::{Item, StmtKind};
    use crate::lexer;

    fn tokens(src: &str) -> Vec<Token> {
        lexer::lex(src).expect("test source lexes").0
    }

    fn functions(program: &Program) -> Vec<String> {
        program
            .items
            .iter()
            .filter_map(|i| match i {
                Item::Fun(f) => Some(f.name.name.clone()),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn a_valid_program_is_the_same_either_way() {
        let t = tokens("fun f(): i64 = 1;\nfun main() { let x = f(); println(x); }\n");
        let (partial, diagnostics) = parse_partial_from(&t, 0);
        assert!(diagnostics.is_empty());
        assert_eq!(parse_from(&t, 0).expect("parses").0, partial);
    }

    #[test]
    fn a_broken_statement_leaves_its_function_and_the_other_statements() {
        let src =
            "fun main() {\n    let a = 1;\n    let b = a +;\n    println(a);\n}\nfun other() { }\n";
        let t = tokens(src);
        let (program, diagnostics) = parse_partial_from(&t, 0);
        assert_eq!(parse_from(&t, 0).expect_err("a syntax error"), diagnostics);
        assert_eq!(functions(&program), ["main", "other"]);
        let Item::Fun(main) = &program.items[0] else {
            panic!("main first")
        };
        let lets: Vec<&str> = main
            .body
            .stmts
            .iter()
            .filter_map(|s| match &s.kind {
                StmtKind::Let { name, .. } => Some(name.name.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(lets, ["a"]);
        assert_eq!(main.body.stmts.len(), 2, "the `let a` and the `println`");
    }

    #[test]
    fn a_broken_item_leaves_the_other_items() {
        let t = tokens("fun ok(): i64 = 1;\nstruct { x: i64 }\nfun main() { }\n");
        let (program, diagnostics) = parse_partial_from(&t, 0);
        assert!(!diagnostics.is_empty());
        assert_eq!(functions(&program), ["ok", "main"]);
    }

    #[test]
    fn a_broken_import_leaves_the_rest() {
        let t = tokens("import geo;\nimport ;\nfun main() { }\n");
        let (program, diagnostics) = parse_partial_from(&t, 0);
        assert!(!diagnostics.is_empty());
        assert_eq!(program.imports.len(), 1);
        assert_eq!(functions(&program), ["main"]);
    }

    #[test]
    fn expression_ids_stay_below_the_count() {
        let t = tokens("fun main() { let a = 1 + 2; let b = (; let c = a * 3; }\n");
        let (program, _) = parse_partial_from(&t, 5);
        assert!(program.expr_count > 5);
        let Item::Fun(main) = &program.items[0] else {
            panic!()
        };
        for stmt in &main.body.stmts {
            if let StmtKind::Let { init, .. } = &stmt.kind {
                assert!(init.id.0 >= 5 && init.id.0 < program.expr_count);
            }
        }
    }
}
