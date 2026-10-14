//! Format specifiers (v13 draft §5, milestone 44): the text after a `:` in
//! an interpolation, `[[fill]align][#][0][width][.precision][kind]`, parsed
//! once here for the checker (E0431) and codegen alike.

/// The largest width or precision a specifier may ask for.
pub const MAX_WIDTH: u32 = 1024;

/// Where padding goes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Align {
    Left,
    Right,
    Centre,
}

/// The base of an integer's digits.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Hex,
    UpperHex,
    Binary,
    Octal,
}

/// A parsed specifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FormatSpec {
    /// One ASCII byte; a space unless given.
    pub fill: u8,
    pub align: Option<Align>,
    /// `#`: a `0x`, `0b` or `0o` prefix.
    pub alternate: bool,
    /// `0`: zeros after the sign and prefix.
    pub zero: bool,
    pub width: u32,
    pub precision: Option<u32>,
    pub kind: Option<Kind>,
}

/// What a specifier is applied to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Value {
    Int,
    Float,
    Other,
}

fn align_of(c: char) -> Option<Align> {
    match c {
        '<' => Some(Align::Left),
        '>' => Some(Align::Right),
        '^' => Some(Align::Centre),
        _ => None,
    }
}

/// A decimal number of at most [`MAX_WIDTH`] at the start of `text`, and
/// the rest; `Ok(None, text)` when no digit starts it.
fn number<'a>(text: &'a str, what: &str) -> Result<(Option<u32>, &'a str), String> {
    let end = text
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(text.len());
    if end == 0 {
        return Ok((None, text));
    }
    match text[..end].parse::<u32>() {
        Ok(n) if n <= MAX_WIDTH => Ok((Some(n), &text[end..])),
        _ => Err(format!("a {what} can be at most {MAX_WIDTH}")),
    }
}

/// Parses `text`, the specifier without its `:`.
pub fn parse(text: &str) -> Result<FormatSpec, String> {
    if text.is_empty() {
        return Err("an empty format specifier: remove the `:` or add a format".to_string());
    }
    let mut spec = FormatSpec {
        fill: b' ',
        align: None,
        alternate: false,
        zero: false,
        width: 0,
        precision: None,
        kind: None,
    };
    let mut chars = text.chars();
    let (first, second) = (chars.next(), chars.next());
    let mut rest = text;
    if let (Some(fill), Some(a)) = (first, second.and_then(align_of)) {
        if !fill.is_ascii() || fill == '{' || fill == '}' {
            return Err(format!("`{fill}` can't be a fill: use one ASCII character"));
        }
        spec.fill = fill as u8;
        spec.align = Some(a);
        rest = &text[fill.len_utf8() + 1..];
    } else if let Some(a) = first.and_then(align_of) {
        spec.align = Some(a);
        rest = &text[1..];
    }
    if let Some(r) = rest.strip_prefix('#') {
        spec.alternate = true;
        rest = r;
    }
    if let Some(r) = rest.strip_prefix('0') {
        spec.zero = true;
        rest = r;
    }
    let (width, r) = number(rest, "width")?;
    spec.width = width.unwrap_or(0);
    rest = r;
    if let Some(r) = rest.strip_prefix('.') {
        let (precision, r) = number(r, "precision")?;
        spec.precision = Some(precision.ok_or("`.` needs a precision after it")?);
        rest = r;
    }
    spec.kind = match rest {
        "" => None,
        "x" => Some(Kind::Hex),
        "X" => Some(Kind::UpperHex),
        "b" => Some(Kind::Binary),
        "o" => Some(Kind::Octal),
        other => {
            return Err(format!(
                "`{other}` isn't a format: use `x`, `X`, `b` or `o`"
            ));
        }
    };
    Ok(spec)
}

/// Whether `spec` applies to a value of class `value`.
pub fn check(spec: &FormatSpec, value: Value) -> Result<(), String> {
    if spec.precision.is_some() && value != Value::Float {
        return Err("a precision (`.2`) applies only to floats".to_string());
    }
    if spec.kind.is_some() && value != Value::Int {
        return Err("`x`, `X`, `b` and `o` apply only to integers".to_string());
    }
    if spec.alternate && spec.kind.is_none() {
        return Err("`#` needs `x`, `X`, `b` or `o` after it".to_string());
    }
    if spec.zero && value == Value::Other {
        return Err("`0` padding applies only to numbers".to_string());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{Align, Kind, Value, check, parse};

    #[test]
    fn every_part_parses() {
        let s = parse("*^#012.3x").unwrap();
        assert_eq!(
            (
                s.fill,
                s.align,
                s.alternate,
                s.zero,
                s.width,
                s.precision,
                s.kind
            ),
            (
                b'*',
                Some(Align::Centre),
                true,
                true,
                12,
                Some(3),
                Some(Kind::Hex)
            )
        );
        let s = parse(">6").unwrap();
        assert_eq!((s.fill, s.align, s.width), (b' ', Some(Align::Right), 6));
        assert_eq!(parse(".2").unwrap().precision, Some(2));
        assert!(parse("05").unwrap().zero);
        assert_eq!(parse("<").unwrap().align, Some(Align::Left));
        assert_eq!(parse("X").unwrap().kind, Some(Kind::UpperHex));
        assert_eq!(parse("-<4").unwrap().fill, b'-');
    }

    #[test]
    fn bad_specifiers_are_refused() {
        for bad in [
            "", "q", "5q", ".", "2000", ".2000", "é<5", "{<5", "5.2.1", ">>>",
        ] {
            assert!(parse(bad).is_err(), "{bad:?}");
        }
    }

    #[test]
    fn specifiers_fit_their_values() {
        let p = |t: &str| parse(t).unwrap();
        assert!(check(&p(".2"), Value::Float).is_ok());
        assert!(check(&p(".2"), Value::Int).is_err());
        assert!(check(&p("x"), Value::Int).is_ok());
        assert!(check(&p("x"), Value::Float).is_err());
        assert!(check(&p("#5"), Value::Int).is_err());
        assert!(check(&p("05"), Value::Other).is_err());
        assert!(check(&p("*>9"), Value::Other).is_ok());
    }
}
