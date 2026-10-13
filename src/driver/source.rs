//! The program's source files, and mapping offsets back to a file, line and
//! column (PRP-033). All files share one offset space (`modules`): each
//! starts at its base, so a `Span` needs no file id.

use crate::modules::Loaded;

/// One source file and where it sits in the shared offset space.
#[derive(Debug, Clone)]
pub(super) struct SourceFile {
    /// The main file as given on the command line; a module as its path
    /// relative to the main file's directory (`shapes/circle.la`).
    pub name: String,
    /// The offset of the file's first byte.
    pub base: usize,
    /// The file's text.
    pub text: String,
}

/// Every loaded source file; the main file first.
#[derive(Debug, Clone)]
pub(super) struct Source {
    pub files: Vec<SourceFile>,
}

impl Source {
    /// A program of one file, starting at offset 0.
    pub(super) fn single(name: impl Into<String>, text: impl Into<String>) -> Self {
        Source {
            files: vec![SourceFile {
                name: name.into(),
                base: 0,
                text: text.into(),
            }],
        }
    }

    /// The files of `loaded`, modules and manifests (v8), named for
    /// messages and ordered by base, as `index_at` needs.
    pub(super) fn of(main: &str, root: &std::path::Path, loaded: &Loaded) -> Self {
        let relative = |file: &std::path::Path| {
            file.strip_prefix(root)
                .unwrap_or(file)
                .display()
                .to_string()
        };
        let modules = loaded.modules.iter().map(|m| SourceFile {
            name: if m.path.is_empty() {
                main.to_string()
            } else {
                relative(&m.file)
            },
            base: m.base,
            text: m.text.clone(),
        });
        let manifests = loaded.manifests.iter().map(|m| SourceFile {
            name: relative(&m.file),
            base: m.base,
            text: m.text.clone(),
        });
        let mut files: Vec<SourceFile> = modules.chain(manifests).collect();
        // The main file has base 0, so it stays first.
        files.sort_by_key(|f| f.base);
        Source { files }
    }

    /// Adds a file that isn't one of the program's own, such as the prelude.
    pub(super) fn add(&mut self, name: &str, base: usize, text: &str) {
        self.files.push(SourceFile {
            name: name.to_string(),
            base,
            text: text.to_string(),
        });
    }

    /// The main file.
    pub(super) fn main(&self) -> &SourceFile {
        &self.files[0]
    }

    /// The index of the file holding `offset`: the last one starting at or before it.
    pub(super) fn index_at(&self, offset: usize) -> usize {
        self.files
            .iter()
            .rposition(|f| f.base <= offset)
            .unwrap_or(0)
    }

    /// The file holding `offset`.
    pub(super) fn file_at(&self, offset: usize) -> &SourceFile {
        &self.files[self.index_at(offset)]
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
    fn offsets_find_their_file() {
        let mut source = Source::single("main.la", "fun main() {}\n");
        source.files.push(SourceFile {
            name: "geometry.la".into(),
            base: 15,
            text: "fun f() {}\n\nfun g() {}\n".into(),
        });
        assert_eq!(source.file_at(3).name, "main.la");
        assert_eq!(source.file_at(15).name, "geometry.la");
        let file = source.file_at(28);
        assert_eq!(line_col(&file.text, 28 - file.base), (3, 2));
    }
}
