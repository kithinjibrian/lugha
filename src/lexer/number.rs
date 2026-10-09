//! Integer and float literals (spec §2).
//!
//! Every number error consumes the whole number-like run, so `123abc` is one
//! error, not a number followed by an identifier.

use super::{Lexer, TokenKind, is_ident_byte};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

impl Lexer<'_> {
    /// Lexes a number starting at `start`, which holds an ASCII digit.
    pub(super) fn number(&mut self, start: usize) {
        if self.byte_at(0) == Some(b'0') && self.byte_at(1) == Some(b'x') {
            self.pos += 2;
            self.skip_while(is_ident_byte);
            self.hex(start);
        } else {
            self.decimal(start);
        }
    }

    fn hex(&mut self, start: usize) {
        let span = Span::new(start, self.pos);
        let body = &self.src[start + 2..self.pos];
        let diagnostic = if body.is_empty() {
            Diagnostic::error("E0106", "hex literal has no digits", span)
        } else if let Some(c) = body.chars().find(|c| !c.is_ascii_hexdigit() && *c != '_') {
            invalid_char(c, span)
        } else if !underscores_between_digits(body) {
            misplaced_underscore(span)
        } else {
            match u64::from_str_radix(&body.replace('_', ""), 16) {
                Ok(value) => return self.push(TokenKind::Int(value), start),
                Err(_) => too_large(span),
            }
        };
        self.error(diagnostic);
    }

    fn decimal(&mut self, start: usize) {
        // `_` is scanned in every part so misplaced ones are reported, not split off.
        let digit_or_underscore = |b: u8| b.is_ascii_digit() || b == b'_';
        self.skip_while(digit_or_underscore);
        let int_end = self.pos;
        // A `.` must be followed by a digit to start a fraction; `1..10` and `1.x` stay integers.
        let has_fraction =
            self.byte_at(0) == Some(b'.') && self.byte_at(1).is_some_and(|b| b.is_ascii_digit());
        if has_fraction {
            self.pos += 1;
            self.skip_while(digit_or_underscore);
        }
        let exponent_start = self.pos;
        let exponent = self.exponent(has_fraction);
        let suffix_start = self.pos;
        let has_suffix = self.skip_while(is_ident_byte) > 0;

        let span = Span::new(start, self.pos);
        let text = &self.src[start..self.pos];
        let diagnostic = if has_suffix {
            let c = self.src[suffix_start..]
                .chars()
                .next()
                .expect("suffix is non-empty");
            invalid_char(c, span)
        } else if exponent.is_some() && !has_fraction {
            let int = &self.src[start..int_end];
            let exp = &self.src[exponent_start..self.pos];
            Diagnostic::error("E0107", "float literal needs a fractional part", span)
                .with_help(format!("write {int}.0{exp}"))
        } else if exponent == Some(Exponent::MissingDigits) {
            Diagnostic::error("E0106", "exponent has no digits", span)
        } else if has_fraction {
            if text.contains('_') {
                Diagnostic::error("E0105", "`_` is not allowed in float literals", span)
            } else {
                match text.parse::<f64>() {
                    Ok(value) if value.is_finite() => {
                        return self.push(TokenKind::Float(value), start);
                    }
                    _ => Diagnostic::error("E0109", "float literal is out of range", span),
                }
            }
        } else if !underscores_between_digits(text) {
            misplaced_underscore(span)
        } else {
            match text.replace('_', "").parse::<u64>() {
                Ok(value) => return self.push(TokenKind::Int(value), start),
                Err(_) => too_large(span),
            }
        };
        self.error(diagnostic);
    }

    /// Consumes an exponent (`e`, optional sign, digits) if one starts here.
    ///
    /// Without a fraction, `e` not followed by a digit isn't an exponent at all
    /// (`2else` is a number with a bad suffix). After a fraction, a bare `e` or
    /// `e+` is an exponent with missing digits.
    fn exponent(&mut self, has_fraction: bool) -> Option<Exponent> {
        if !matches!(self.byte_at(0), Some(b'e' | b'E')) {
            return None;
        }
        let sign = usize::from(matches!(self.byte_at(1), Some(b'+' | b'-')));
        if self.byte_at(1 + sign).is_some_and(|b| b.is_ascii_digit()) {
            self.pos += 1 + sign;
            self.skip_while(|b| b.is_ascii_digit() || b == b'_');
            Some(Exponent::Digits)
        } else if has_fraction {
            self.pos += 1 + sign;
            Some(Exponent::MissingDigits)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Exponent {
    Digits,
    MissingDigits,
}

/// True if every `_` in `digits` sits between two digits.
fn underscores_between_digits(digits: &str) -> bool {
    !digits.starts_with('_') && !digits.ends_with('_') && !digits.contains("__")
}

fn invalid_char(c: char, span: Span) -> Diagnostic {
    Diagnostic::error(
        "E0108",
        format!("invalid character {c:?} in number literal"),
        span,
    )
}

fn misplaced_underscore(span: Span) -> Diagnostic {
    Diagnostic::error("E0105", "`_` must sit between two digits", span)
}

fn too_large(span: Span) -> Diagnostic {
    Diagnostic::error("E0104", "integer literal is too large", span)
        .with_help(format!("the largest integer literal is {}", u64::MAX))
}

#[cfg(test)]
mod tests {
    use crate::lexer::TokenKind::*;
    use crate::lexer::lex;
    use crate::lexer::test_util::{errors, kinds};

    #[test]
    fn integers() {
        let src = "0 42 007 1_000_000 0xFF 0xff 0xDEAD_BEEF 18446744073709551615";
        let expected = [0, 42, 7, 1_000_000, 0xFF, 0xFF, 0xDEAD_BEEF, u64::MAX];
        assert_eq!(kinds(src), expected.map(Int));
    }

    #[test]
    fn floats() {
        let src = "3.25 2.0e-3 2.0E5 1.5e+2 0.0";
        assert_eq!(kinds(src), [3.25, 2.0e-3, 2.0e5, 1.5e2, 0.0].map(Float));
    }

    #[test]
    fn dot_without_digit_ends_an_integer() {
        assert_eq!(kinds("1..10"), [Int(1), DotDot, Int(10)]);
        assert_eq!(kinds("1.x"), [Int(1), Dot, Ident("x".into())]);
    }

    #[test]
    fn integer_too_large_is_e0104() {
        assert_eq!(
            errors("18446744073709551616"),
            [("E0104", "18446744073709551616")]
        );
        assert_eq!(
            errors("0x1_0000_0000_0000_0000"),
            [("E0104", "0x1_0000_0000_0000_0000")]
        );
    }

    #[test]
    fn misplaced_underscore_is_e0105() {
        for src in ["1__0", "1_", "0x_FF", "1_0.5", "0xF__F", "1.0_5", "2.0e1_0"] {
            assert_eq!(errors(src), [("E0105", src)], "{src:?}");
        }
    }

    #[test]
    fn missing_digits_is_e0106() {
        for src in ["0x", "2.0e", "2.0e+"] {
            assert_eq!(errors(src), [("E0106", src)], "{src:?}");
        }
    }

    #[test]
    fn exponent_without_fraction_is_e0107_with_help() {
        assert_eq!(errors("2e5"), [("E0107", "2e5")]);
        assert_eq!(errors("2E-3"), [("E0107", "2E-3")]);
        let diags = lex("2e5").unwrap_err();
        assert_eq!(diags[0].help.as_deref(), Some("write 2.0e5"));
    }

    #[test]
    fn letters_after_a_number_are_one_e0108() {
        for src in ["123abc", "0XFF", "0xFG", "1.5x", "2else"] {
            assert_eq!(errors(src), [("E0108", src)], "{src:?}");
        }
    }

    #[test]
    fn float_out_of_range_is_e0109() {
        assert_eq!(errors("1.0e999"), [("E0109", "1.0e999")]);
        // Underflow just rounds to zero, like any other float rounding.
        assert_eq!(kinds("1.0e-999"), [Float(0.0)]);
    }
}
