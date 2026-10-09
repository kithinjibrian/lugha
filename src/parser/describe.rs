//! How tokens are named in parse error messages.

use crate::lexer::TokenKind;

/// A token as quoted in "expected …": `` `;` ``.
pub(super) fn text(kind: &TokenKind) -> String {
    format!("`{}`", symbol(kind))
}

/// The token that was found instead: `` `;` ``, ``identifier `x` ``, `end of file`.
pub(super) fn found(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Int(value) => format!("integer `{value}`"),
        TokenKind::Float(value) => format!("float `{value:?}`"),
        TokenKind::Str(_) => "string literal".to_string(),
        TokenKind::Ident(name) => format!("identifier `{name}`"),
        TokenKind::Eof => "end of file".to_string(),
        other => text(other),
    }
}

/// The opener matching a closing bracket, quoted.
pub(super) fn opener(closer: &TokenKind) -> &'static str {
    match closer {
        TokenKind::RParen => "`(`",
        TokenKind::RBracket => "`[`",
        _ => "`{`",
    }
}

/// The symbol of an assignment operator, or `None` for any other token.
pub(super) fn assign_symbol(kind: &TokenKind) -> Option<&'static str> {
    Some(match kind {
        TokenKind::Eq => "=",
        TokenKind::PlusEq => "+=",
        TokenKind::MinusEq => "-=",
        TokenKind::StarEq => "*=",
        TokenKind::SlashEq => "/=",
        _ => return None,
    })
}

fn symbol(kind: &TokenKind) -> &'static str {
    use TokenKind::*;
    match kind {
        Int(_) => "integer",
        Float(_) => "float",
        Str(_) => "string",
        Ident(_) => "identifier",
        Fun => "fun",
        Extern => "extern",
        Struct => "struct",
        Let => "let",
        Mut => "mut",
        If => "if",
        Else => "else",
        While => "while",
        For => "for",
        In => "in",
        Of => "of",
        Return => "return",
        Break => "break",
        Continue => "continue",
        True => "true",
        False => "false",
        As => "as",
        TyI32 => "i32",
        TyI64 => "i64",
        TyU8 => "u8",
        TyF64 => "f64",
        TyBool => "bool",
        TyString => "string",
        Plus => "+",
        Minus => "-",
        Star => "*",
        Slash => "/",
        Percent => "%",
        EqEq => "==",
        BangEq => "!=",
        Lt => "<",
        LtEq => "<=",
        Gt => ">",
        GtEq => ">=",
        AndAnd => "&&",
        OrOr => "||",
        Bang => "!",
        Eq => "=",
        PlusEq => "+=",
        MinusEq => "-=",
        StarEq => "*=",
        SlashEq => "/=",
        LParen => "(",
        RParen => ")",
        LBrace => "{",
        RBrace => "}",
        LBracket => "[",
        RBracket => "]",
        Comma => ",",
        Semi => ";",
        Colon => ":",
        Dot => ".",
        DotDot => "..",
        Eof => "end of file",
    }
}
