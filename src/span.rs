//! Source locations as byte ranges.
//!
//! Spans are byte offsets into a single source file. Line and column numbers
//! are derived from them only when a diagnostic is rendered.

/// A half-open byte range `start..end` into one source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Offset of the first byte.
    pub start: usize,
    /// Offset one past the last byte.
    pub end: usize,
}

impl Span {
    /// Creates the span `start..end`.
    ///
    /// # Panics
    ///
    /// In debug builds, panics if `start > end` — a span running backwards is a compiler bug.
    ///
    /// # Examples
    ///
    /// ```
    /// let span = lugha::span::Span::new(4, 7);
    /// assert_eq!(&"let mut x"[span.start..span.end], "mut");
    /// ```
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end, "span runs backwards: {start}..{end}");
        Span { start, end }
    }
}
