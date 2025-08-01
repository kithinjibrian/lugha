use std::fmt;
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;

/// Error codes used throughout the compiler
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCode {
    UnexpectedToken,
    MissingSemicolon,
    ParserError
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code_str = match self {
            ErrorCode::UnexpectedToken => "E0001",
            ErrorCode::MissingSemicolon => "E0002",
            ErrorCode::ParserError => "E0003",
        };
        write!(f, "{}", code_str)
    }
}

/// Represents a compiler error with location info and context
#[derive(Debug, Clone)]
pub struct CompilerError {
    pub code: ErrorCode,
    pub message: String,
    pub line: u32,
    pub column: usize,
    pub line_snippet: String,
}

impl<'a> CompilerError {
    /// Low-level constructor for direct use in handlers
    pub fn new(code: ErrorCode, message: impl Into<String>, span: Span<'a>) -> Self {
        let line = span.location_line();
        let column = span.get_utf8_column();
        let line_snippet = String::from_utf8_lossy(span.get_line_beginning()).to_string();

        Self {
            code,
            message: message.into(),
            line,
            column,
            line_snippet,
        }
    }

    /// Public factory: constructs an error based on a known error code
    pub fn from_code(code: ErrorCode, span: Span<'a>) -> Self {
        if let Some(handler) = get_error_handler(code) {
            handler(span)
        } else {
            CompilerError::new(code, "Unknown compiler error", span)
        }
    }

    /// Human-readable error message with line context
    pub fn display(&self) -> String {
        format!(
            "error[{}] at line {}, column {}: {}\n  {}\n  {:>width$}^\n",
            self.code,
            self.line,
            self.column,
            self.message,
            self.line_snippet.trim_end(),
            "",
            width = self.column
        )
    }
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display())
    }
}

impl std::error::Error for CompilerError {}

/// Type alias for error handler functions
type ErrorHandler<'a> = fn(Span<'a>) -> CompilerError;

/// Maps error codes to their dedicated handler functions
fn get_error_handler<'a>(code: ErrorCode) -> Option<ErrorHandler<'a>> {
    match code {
        ErrorCode::UnexpectedToken => Some(handle_unexpected_token),
        ErrorCode::MissingSemicolon => Some(handle_missing_semicolon),
        ErrorCode::ParserError => Some(handle_parser_error)
    }
}

fn handle_unexpected_token<'a>(span: Span<'a>) -> CompilerError {
    CompilerError::new(
        ErrorCode::UnexpectedToken,
        "Unexpected token encountered",
        span,
    )
}

fn handle_missing_semicolon<'a>(span: Span<'a>) -> CompilerError {
    CompilerError::new(
        ErrorCode::MissingSemicolon,
        "Missing semicolon at end of statement",
        span,
    )
}

fn handle_parser_error<'a>(span: Span<'a>) -> CompilerError {
    CompilerError::new(
        ErrorCode::ParserError,
        "Parser error",
        span,
    )
}
