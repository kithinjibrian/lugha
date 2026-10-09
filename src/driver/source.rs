//! Loading a source file, and mapping byte offsets to lines and columns.

use std::path::Path;

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// A loaded source file.
#[derive(Debug, Clone)]
pub(super) struct Source {
    /// The path as given on the command line, used in messages and JSON.
    pub name: String,
    /// The file's text (for invalid UTF-8, only the valid prefix).
    pub text: String,
}

/// Why a file could not be loaded.
pub(super) enum LoadError {
    /// The file can't be read — an internal error (exit 2).
    Io(std::io::Error),
    /// The file isn't UTF-8 — a program error (E0110, exit 1). Boxed: large and rare.
    Utf8(Source, Box<Diagnostic>),
}

/// Reads `path` as UTF-8 source.
pub(super) fn load(path: &Path) -> Result<Source, LoadError> {
    let bytes = std::fs::read(path).map_err(LoadError::Io)?;
    let name = path.display().to_string();
    match String::from_utf8(bytes) {
        Ok(text) => Ok(Source { name, text }),
        Err(error) => {
            let valid = error.utf8_error().valid_up_to();
            let mut bytes = error.into_bytes();
            bytes.truncate(valid);
            let text = String::from_utf8(bytes).expect("bytes before valid_up_to are UTF-8");
            // Empty span at the first bad byte: the valid prefix is all there is to show.
            let span = Span::new(valid, valid);
            let diagnostic = Diagnostic::error("E0110", "source is not valid UTF-8", span)
                .with_help("save the file as UTF-8");
            Err(LoadError::Utf8(Source { name, text }, Box::new(diagnostic)))
        }
    }
}

/// 1-based line and byte column of `offset` in `text` (spec §9).
pub(super) fn line_col(text: &str, offset: usize) -> (usize, usize) {
    let before = &text.as_bytes()[..offset.min(text.len())];
    let line = before.iter().filter(|&&b| b == b'\n').count() + 1;
    let line_start = before
        .iter()
        .rposition(|&b| b == b'\n')
        .map_or(0, |i| i + 1);
    (line, before.len() - line_start + 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn columns_are_one_based_and_count_bytes() {
        let text = "ab\ncé x\n";
        assert_eq!(line_col(text, 0), (1, 1));
        assert_eq!(line_col(text, 2), (1, 3));
        assert_eq!(line_col(text, 3), (2, 1));
        // `é` is two bytes, so `x` sits at byte column 5.
        assert_eq!(line_col(text, 7), (2, 5));
        assert_eq!(line_col(text, text.len()), (3, 1));
    }

    #[test]
    fn invalid_utf8_is_e0110_at_the_first_bad_byte() {
        let path = std::env::temp_dir().join(format!("lugha-source-{}.la", std::process::id()));
        std::fs::write(&path, b"fun main() {\n  \xff }").expect("temp file is writable");
        let result = load(&path);
        let _ = std::fs::remove_file(&path);
        let Err(LoadError::Utf8(source, d)) = result else {
            panic!("expected E0110")
        };
        assert_eq!(d.code, "E0110");
        assert_eq!((d.span.start, d.span.end), (15, 15));
        assert_eq!(source.text, "fun main() {\n  ");
    }
}
