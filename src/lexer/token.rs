//! Token kinds produced by the lexer.

use crate::span::Span;

/// One token and where it came from.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// What the token is.
    pub kind: TokenKind,
    /// The source bytes it covers.
    pub span: Span,
}

/// Every token of the language. Keywords and punctuation each get their own variant.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    /// Integer literal; per-type range checks happen in the checker.
    Int(u64),
    /// Float literal, correctly rounded.
    Float(f64),
    /// String literal with escapes decoded.
    Str(String),
    /// A `char` literal's code point.
    Char(u32),
    /// An interpolated string's text before its first `{`.
    StrStart(String),
    /// Text between an interpolation's `}` and the next `{`.
    StrMid(String),
    /// Text after the last interpolation's `}`, up to the closing `"`.
    StrEnd(String),
    /// A format specifier, the raw text after an interpolation's
    /// top-level `:` up to its `}`.
    FmtSpec(String),
    /// Identifier that is not a keyword.
    Ident(String),

    // Keywords.
    Fun,
    Extern,
    /// `const`.
    Const,
    Struct,
    Enum,
    Impl,
    /// `import`.
    Import,
    /// `interface`.
    Interface,
    /// `private`.
    Private,
    /// `public`.
    Public,
    Match,
    SelfKw,
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
    /// `spawn`, starting a green thread.
    Spawn,
    /// `select`, waiting on several channels.
    Select,
    /// `defer`, running a statement when its block is left.
    Defer,
    /// `actor`, declaring an actor.
    Actor,
    /// `loop`, repeating until a `break`.
    Loop,
    // Built-in type names are keywords too, so structs can't shadow them.
    TyI32,
    TyI64,
    TyU8,
    TyF64,
    TyBool,
    TyString,
    TyU32,
    TyU64,
    TyF32,
    /// `i8`, `i16`, `u16`.
    TyI8,
    TyI16,
    TyU16,
    /// `char`.
    TyChar,

    // Operators.
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    /// `%=`.
    PercentEq,
    /// `?`, the postfix early-return operator.
    Question,
    /// `@`, starting an attribute: `@test`.
    At,
    /// `#{`, opening a map or set literal.
    HashBrace,
    // The bit operators; `>>` is two `Gt` tokens, joined in `parser::operators`.
    Amp,
    AmpEq,
    Pipe,
    PipeEq,
    Caret,
    CaretEq,
    Tilde,
    Shl,
    ShlEq,
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
    /// `=>`, between a `match` pattern and its arm.
    FatArrow,
    /// `::` is not Lugha syntax; lexed only so the parser can report E0207.
    ColonColon,

    /// End of input; always the last token.
    Eof,
}

/// Returns the keyword token for `word`, or `None` if it is an ordinary identifier.
pub fn keyword(word: &str) -> Option<TokenKind> {
    use TokenKind::*;
    Some(match word {
        "fun" => Fun,
        "extern" => Extern,
        "const" => Const,
        "struct" => Struct,
        "enum" => Enum,
        "impl" => Impl,
        "import" => Import,
        "interface" => Interface,
        "private" => Private,
        "public" => Public,
        "match" => Match,
        "self" => SelfKw,
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
        "spawn" => Spawn,
        "select" => Select,
        "defer" => Defer,
        "actor" => Actor,
        "loop" => Loop,
        "i32" => TyI32,
        "i64" => TyI64,
        "u8" => TyU8,
        "f64" => TyF64,
        "bool" => TyBool,
        "string" => TyString,
        "u32" => TyU32,
        "u64" => TyU64,
        "f32" => TyF32,
        "i8" => TyI8,
        "i16" => TyI16,
        "u16" => TyU16,
        "char" => TyChar,
        _ => return None,
    })
}
