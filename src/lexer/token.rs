//! Token kinds produced by the lexer (spec §2).

use crate::span::Span;

/// One token and where it came from.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// What the token is.
    pub kind: TokenKind,
    /// The source bytes it covers.
    pub span: Span,
}

/// Every token in spec §2. Keywords and punctuation each get their own variant.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    /// Integer literal; per-type range checks happen in the checker.
    Int(u64),
    /// Float literal, correctly rounded.
    Float(f64),
    /// String literal with escapes decoded.
    Str(String),
    /// Identifier that is not a keyword.
    Ident(String),

    // Keywords.
    Fun,
    Extern,
    Struct,
    Let,
    Mut,
    If,
    Else,
    While,
    For,
    In,
    Of,
    Return,
    Break,
    Continue,
    True,
    False,
    As,
    // Built-in type names are keywords too, so structs can't shadow them.
    TyI32,
    TyI64,
    TyU8,
    TyF64,
    TyBool,
    TyString,

    // Operators.
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqEq,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    AndAnd,
    OrOr,
    Bang,
    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,

    // Punctuation.
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semi,
    Colon,
    Dot,
    DotDot,

    /// End of input; always the last token.
    Eof,
}

/// Returns the keyword token for `word`, or `None` if it is an ordinary identifier.
pub fn keyword(word: &str) -> Option<TokenKind> {
    use TokenKind::*;
    Some(match word {
        "fun" => Fun,
        "extern" => Extern,
        "struct" => Struct,
        "let" => Let,
        "mut" => Mut,
        "if" => If,
        "else" => Else,
        "while" => While,
        "for" => For,
        "in" => In,
        "of" => Of,
        "return" => Return,
        "break" => Break,
        "continue" => Continue,
        "true" => True,
        "false" => False,
        "as" => As,
        "i32" => TyI32,
        "i64" => TyI64,
        "u8" => TyU8,
        "f64" => TyF64,
        "bool" => TyBool,
        "string" => TyString,
        _ => return None,
    })
}
