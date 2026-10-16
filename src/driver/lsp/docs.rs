//! Doc comments (v16 draft decisions 47–49): the `/** … */` right before a
//! declaration, its Markdown text and `@param`/`@returns` tags, for hover,
//! completion and signature help; and a file's module doc (v17, PRP-130).

use std::cell::RefCell;
use std::collections::HashMap;

use super::hover::Snapshot;
use crate::span::Span;

/// A declaration's documentation.
#[derive(Debug, Clone, PartialEq, Default)]
pub(super) struct Doc {
    /// The description: Markdown, other `@tags` kept as written.
    pub text: String,
    /// Each `@param name text`, in order.
    pub params: Vec<(String, String)>,
    /// `@returns text`.
    pub returns: Option<String>,
}

impl Doc {
    /// The whole doc as Markdown: the description, then a line per
    /// parameter and the result.
    pub(super) fn markdown(&self) -> String {
        let mut parts = Vec::new();
        if !self.text.is_empty() {
            parts.push(self.text.clone());
        }
        let mut tags: Vec<String> = self
            .params
            .iter()
            .map(|(name, text)| format!("*{name}* — {text}"))
            .collect();
        if let Some(returns) = &self.returns {
            tags.push(format!("*returns* — {returns}"));
        }
        if !tags.is_empty() {
            parts.push(tags.join("  \n"));
        }
        parts.join("\n\n")
    }

    /// The description and the result, for signature help (where each
    /// parameter's text shows under its own highlight).
    pub(super) fn summary(&self) -> String {
        let mut parts = Vec::new();
        if !self.text.is_empty() {
            parts.push(self.text.clone());
        }
        if let Some(returns) = &self.returns {
            parts.push(format!("*returns* — {returns}"));
        }
        parts.join("\n\n")
    }

    /// The `@param` text for `name`.
    pub(super) fn param(&self, name: &str) -> Option<&str> {
        self.params
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, t)| t.as_str())
    }
}

/// Parses a doc comment's text, `/**` and `*/` included.
pub(super) fn parse(comment: &str) -> Doc {
    let inner = comment.strip_prefix("/**").unwrap_or(comment);
    let inner = inner.strip_suffix("*/").unwrap_or(inner);
    // Each line without its leading spaces and one `*` (and the space after).
    let lines: Vec<&str> = inner
        .lines()
        .map(|l| {
            let l = l.trim_start();
            let l = l.strip_prefix('*').unwrap_or(l);
            l.strip_prefix(' ').unwrap_or(l)
        })
        .collect();
    let mut doc = Doc::default();
    let mut text: Vec<&str> = Vec::new();
    // Where a continuation line goes: the description, a parameter, the result.
    enum Into {
        Text,
        Param(usize),
        Returns,
    }
    let mut into = Into::Text;
    for line in lines {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("@param") {
            let rest = rest.trim_start();
            let (name, body) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));
            doc.params.push((name.to_string(), body.trim().to_string()));
            into = Into::Param(doc.params.len() - 1);
        } else if let Some(rest) = trimmed
            .strip_prefix("@returns")
            .or_else(|| trimmed.strip_prefix("@return"))
        {
            doc.returns = Some(rest.trim().to_string());
            into = Into::Returns;
        } else if trimmed.starts_with('@') {
            text.push(line);
            into = Into::Text;
        } else {
            match into {
                Into::Text => text.push(line),
                Into::Param(i) if !trimmed.is_empty() => {
                    let body = &mut doc.params[i].1;
                    body.push(' ');
                    body.push_str(trimmed);
                }
                Into::Returns if !trimmed.is_empty() => {
                    let body = doc.returns.get_or_insert_with(String::new);
                    body.push(' ');
                    body.push_str(trimmed);
                }
                _ => into = Into::Text,
            }
        }
    }
    doc.text = dedent(&text);
    doc
}

/// `lines` without their common indentation and the blank lines around them.
fn dedent(lines: &[&str]) -> String {
    let indent = lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .min()
        .unwrap_or(0);
    let lines: Vec<&str> = lines
        .iter()
        .map(|l| l.get(indent..).unwrap_or("").trim_end())
        .collect();
    let start = lines
        .iter()
        .position(|l| !l.is_empty())
        .unwrap_or(lines.len());
    let end = lines
        .iter()
        .rposition(|l| !l.is_empty())
        .map_or(start, |e| e + 1);
    lines[start..end].join("\n")
}

/// Words that may stand between a doc comment and a declaration's name.
const LEAD: [&str; 7] = [
    "fun",
    "extern",
    "struct",
    "enum",
    "interface",
    "public",
    "private",
];

/// The doc comment among `docs` (spans into `text`) that documents the
/// declaration whose name starts at `name`: right before it, apart from
/// whitespace and the words in `LEAD`.
pub(super) fn attached(text: &str, docs: &[Span], name: usize) -> Option<Span> {
    let mut at = name.min(text.len());
    loop {
        let before = text[..at].trim_end();
        let word_start = before
            .rfind(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
            .map_or(0, |i| i + 1);
        if LEAD.contains(&&before[word_start..]) {
            at = word_start;
        } else if &before[word_start..] == "test" && before[..word_start].ends_with('@') {
            // `@test` (milestone 84) stands between them too.
            at = word_start - 1;
        } else {
            at = before.len();
            break;
        }
    }
    docs.iter().find(|d| d.end == at).copied()
}

/// The module doc of a file (v17, milestone 98): its first doc comment,
/// when only whitespace and plain comments come before it, and a blank
/// line, an `import` (or `public import`) or the end of the file after it,
/// apart from whitespace and plain comments. Directly above an item, it is
/// the item's instead.
pub(super) fn module_doc(text: &str, docs: &[Span]) -> Option<Span> {
    let first = *docs.first()?;
    let (start, _) = skip_comments(text, 0, docs);
    if start != first.start {
        return None;
    }
    let (next, blank) = skip_comments(text, first.end, docs);
    let rest = &text[next..];
    let word = |w: &str| {
        rest.strip_prefix(w).is_some_and(|after| {
            !after.starts_with(|c: char| c.is_ascii_alphanumeric() || c == '_')
        })
    };
    let public_import = rest.strip_prefix("public").is_some_and(|after| {
        after.starts_with(char::is_whitespace) && after.trim_start().starts_with("import")
    });
    (blank || rest.is_empty() || word("import") || public_import).then_some(first)
}

/// From `at`, past whitespace and plain comments (`//`, nested `/* */`,
/// not the doc comments in `docs`): where the next thing starts, and
/// whether a blank line was passed.
fn skip_comments(text: &str, mut at: usize, docs: &[Span]) -> (usize, bool) {
    let bytes = text.as_bytes();
    let (mut blank, mut line_empty) = (false, false);
    while at < bytes.len() {
        match bytes[at] {
            b'\n' => {
                blank |= line_empty;
                line_empty = true;
                at += 1;
            }
            b' ' | b'\t' | b'\r' => at += 1,
            b'/' if bytes.get(at + 1) == Some(&b'/') => {
                line_empty = false;
                at = text[at..].find('\n').map_or(bytes.len(), |n| at + n);
            }
            b'/' if bytes.get(at + 1) == Some(&b'*') && !docs.iter().any(|d| d.start == at) => {
                line_empty = false;
                at = block_end(text, at);
            }
            _ => break,
        }
    }
    (at, blank)
}

/// The end of the (nested) block comment starting at `at`; the text's end
/// when it isn't closed.
fn block_end(text: &str, mut at: usize) -> usize {
    let bytes = text.as_bytes();
    let mut depth = 0;
    while at + 1 < bytes.len() {
        match (bytes[at], bytes[at + 1]) {
            (b'/', b'*') => {
                depth += 1;
                at += 2;
            }
            (b'*', b'/') => {
                depth -= 1;
                at += 2;
                if depth == 0 {
                    return at;
                }
            }
            _ => at += 1,
        }
    }
    bytes.len()
}

/// [`attached`], unless that comment is the file's module doc (v17).
pub(super) fn item_doc(text: &str, docs: &[Span], name: usize) -> Option<Span> {
    let span = attached(text, docs, name)?;
    (module_doc(text, docs) != Some(span)).then_some(span)
}

/// Each file's doc comments in a snapshot, by the file's base, found once.
#[derive(Default)]
pub(super) struct DocCache {
    files: RefCell<HashMap<usize, Vec<Span>>>,
}

/// The doc of the declaration whose name is at `decl` (in the check's shared
/// offset space), if it has one.
pub(super) fn doc_for(snapshot: &Snapshot, decl: Span) -> Option<Doc> {
    let file = snapshot.source.file_at(decl.start);
    let mut files = snapshot.docs.files.borrow_mut();
    let docs = files
        .entry(file.base)
        .or_insert_with(|| crate::lexer::doc_comments(&file.text));
    let span = item_doc(&file.text, docs, decl.start.checked_sub(file.base)?)?;
    Some(parse(&file.text[span.start..span.end]))
}

/// The module doc of the file starting at `base` in a snapshot (v17).
pub(super) fn module_doc_at(snapshot: &Snapshot, base: usize) -> Option<Doc> {
    let file = snapshot.source.file_at(base);
    let mut files = snapshot.docs.files.borrow_mut();
    let docs = files
        .entry(file.base)
        .or_insert_with(|| crate::lexer::doc_comments(&file.text));
    let span = module_doc(&file.text, docs)?;
    Some(parse(&file.text[span.start..span.end]))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_text_loses_its_stars_and_indentation() {
        let doc =
            parse("/**\n     * Returns the larger.\n     *\n     *     indented code\n     */");
        assert_eq!(doc.text, "Returns the larger.\n\n    indented code");
        assert_eq!(parse("/** One line. */").text, "One line.");
        assert_eq!(
            parse("/**\n  No stars here.\n  Second.\n*/").text,
            "No stars here.\nSecond."
        );
    }

    #[test]
    fn tags_collect_their_text_and_continuation_lines() {
        let doc = parse(
            "/**\n * Max.\n * @param a the first\n *   value\n * @param b the second\n * @returns the larger\n *   one\n * @since v16\n */",
        );
        assert_eq!(doc.text, "Max.\n@since v16");
        assert_eq!(
            doc.params,
            [
                ("a".into(), "the first value".into()),
                ("b".into(), "the second".into())
            ]
        );
        assert_eq!(doc.returns.as_deref(), Some("the larger one"));
        assert_eq!(doc.param("b"), Some("the second"));
        assert_eq!(
            doc.markdown(),
            "Max.\n@since v16\n\n*a* — the first value  \n*b* — the second  \n*returns* — the larger one"
        );
        assert_eq!(
            doc.summary(),
            "Max.\n@since v16\n\n*returns* — the larger one"
        );
        assert_eq!(parse("/** @return it */").returns.as_deref(), Some("it"));
    }

    #[test]
    fn a_module_doc_is_the_top_comment_before_a_blank_line_or_an_import_v17() {
        fn found(t: &str) -> Option<&str> {
            let docs = crate::lexer::doc_comments(t);
            module_doc(t, &docs).map(|s| &t[s.start..s.end])
        }
        assert_eq!(found("/** M. */\n\nfun f() {}"), Some("/** M. */"));
        assert_eq!(found("/** M. */\nimport a;"), Some("/** M. */"));
        assert_eq!(found("/** M. */\npublic import a;"), Some("/** M. */"));
        assert_eq!(found("// licence\n/* c */\n/** M. */\n"), Some("/** M. */"));
        assert_eq!(found("/** M. */\n// note\n\nfun f() {}"), Some("/** M. */"));
        assert_eq!(found("/** M. */"), Some("/** M. */"));
        // Directly above an item it is the item's.
        assert_eq!(found("/** F. */\nfun f() {}"), None);
        assert_eq!(found("/** F. */\n// note\nfun f() {}"), None);
        // Only the first thing in the file.
        assert_eq!(found("import a;\n/** X. */\n\nfun f() {}"), None);
        assert_eq!(found("fun g() {}\n/** X. */\n\nfun f() {}"), None);
        assert_eq!(found("fun f() {}"), None);
    }

    #[test]
    fn a_module_doc_is_never_an_items_v17() {
        let text = "/** M. */\n\n/** F. */\nfun f() {}\n";
        let docs = crate::lexer::doc_comments(text);
        let item = item_doc(text, &docs, text.find("f()").unwrap());
        assert_eq!(item.map(|s| &text[s.start..s.end]), Some("/** F. */"));
        let text = "/** M. */\n\nfun f() {}\n";
        let docs = crate::lexer::doc_comments(text);
        assert_eq!(item_doc(text, &docs, text.find("f()").unwrap()), None);
    }

    #[test]
    fn a_doc_attaches_through_whitespace_and_leading_words_only() {
        let docs = |t: &str| crate::lexer::doc_comments(t);
        let text = "/** Doc. */\npublic fun max() {}";
        assert!(attached(text, &docs(text), text.find("max").unwrap()).is_some());
        let text = "/** Doc. */\n@test\nfun adds() {}";
        assert!(attached(text, &docs(text), text.find("adds").unwrap()).is_some());
        let text = "/** Doc. */\n\nstruct P {\n    /** The x. */\n    x: f64,\n}";
        assert!(attached(text, &docs(text), text.find("P").unwrap()).is_some());
        assert_eq!(
            attached(text, &docs(text), text.find("x:").unwrap()).map(|s| &text[s.start..s.end]),
            Some("/** The x. */")
        );
        let text = "/** Doc. */\nlet y = 1;\nfun f() {}";
        assert!(attached(text, &docs(text), text.find("f()").unwrap()).is_none());
        let text = "/* plain */ fun f() {}";
        assert!(attached(text, &docs(text), text.find("f()").unwrap()).is_none());
    }
    /// Every `public` std function and method, and every prelude function,
    /// without a doc comment (milestone 77: the library is documented).
    fn undocumented(name: &str, text: &str, public_only: bool) -> Vec<String> {
        use crate::ast::{Item, VisibilityKind};
        let (tokens, _) = crate::lexer::lex_bundled(text).expect("bundled sources lex");
        let program = crate::parser::parse(&tokens)
            .expect("bundled sources parse")
            .0;
        let docs = crate::lexer::doc_comments(text);
        let public = |v: &Option<crate::ast::Visibility>| {
            !public_only || v.as_ref().is_some_and(|v| v.kind == VisibilityKind::Public)
        };
        let mut missing = Vec::new();
        let mut check = |f: &crate::ast::FunDecl| {
            if public(&f.visibility) && attached(text, &docs, f.name.span.start).is_none() {
                missing.push(format!("{name}: {}", f.name.name));
            }
        };
        for item in &program.items {
            match item {
                Item::Fun(f) => check(f),
                Item::Impl(i) if public_only => i.methods.iter().for_each(&mut check),
                _ => {}
            }
        }
        missing
    }

    #[test]
    fn the_library_is_documented() {
        let mut missing: Vec<String> = Vec::new();
        for (name, text) in crate::STD {
            missing.extend(undocumented(name, text, true));
        }
        missing.extend(undocumented("prelude", crate::PRELUDE, false));
        assert_eq!(missing, Vec::<String>::new());
        // The check itself finds a missing doc.
        assert_eq!(
            undocumented(
                "t",
                "public fun f() {}\n/** Yes. */\npublic fun g() {}",
                true
            ),
            ["t: f"]
        );
    }
}
