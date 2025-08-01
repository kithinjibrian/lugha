use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, digit1, multispace0},
    combinator::{map, map_res, recognize},
    multi::many0,
    sequence::delimited,
};
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Let(Span<'a>),
    Return(Span<'a>),
    Ident(&'a str, Span<'a>),
    Number(i32, Span<'a>),
    Equal(Span<'a>),
    Plus(Span<'a>),
    Minus(Span<'a>),
    Star(Span<'a>),
    Slash(Span<'a>),
    Semicolon(Span<'a>),
    LParen(Span<'a>),
    RParen(Span<'a>),
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn parse_keyword_or_ident(input: Span) -> IResult<Span, Token> {
    let (rest, ident_span) = recognize(take_while1(is_ident_char))(input)?;

    let ident_str = *ident_span.fragment();
    let token = match ident_str {
        "let" => Token::Let(ident_span),
        "return" => Token::Return(ident_span),
        _ => Token::Ident(ident_str, ident_span),
    };

    Ok((rest, token))
}

fn parse_number(input: Span) -> IResult<Span, Token> {
    let (rest, span) = digit1(input)?;
    let value = span.fragment().parse::<i32>().unwrap();
    Ok((rest, Token::Number(value, span)))
}

fn parse_symbol(input: Span) -> IResult<Span, Token> {
    alt((
        map(char('='), |_c| Token::Equal(input)),
        map(char('+'), |_c| Token::Plus(input)),
        map(char('-'), |_c| Token::Minus(input)),
        map(char('*'), |_c| Token::Star(input)),
        map(char('/'), |_c| Token::Slash(input)),
        map(char(';'), |_c| Token::Semicolon(input)),
        map(char('('), |_c| Token::LParen(input)),
        map(char(')'), |_c| Token::RParen(input)),
    ))(input)
}

fn parse_token(input: Span) -> IResult<Span, Token> {
    delimited(
        multispace0,
        alt((
            parse_keyword_or_ident,
            parse_number,
            parse_symbol,
        )),
        multispace0,
    )(input)
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let span = Span::new(input);
    let (rest, tokens) = many0(parse_token)(span)
        .map_err(|e| format!("Lexing error: {:?}", e))?;

    if !rest.fragment().trim().is_empty() {
        return Err(format!(
            "Unparsed input at line {}: {:?}",
            rest.location_line(),
            rest.fragment()
        ));
    }

    Ok(tokens)
}