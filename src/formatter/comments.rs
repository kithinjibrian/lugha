//! Sequences and their comments (decision 62): the items of a file, the
//! statements of a block, and fields, variants, methods and arms, one per
//! line. A comment on its own line in a sequence prints above the element
//! after it; one after an element on its line stays at the end of that
//! line; one inside an element that its own sequences don't place keeps
//! that whole element exactly as written.

use super::Printer;
use super::doc::{Doc, concat, nest, text};
use crate::span::Span;

impl Printer<'_> {
    /// The elements of a sequence whose comments may sit anywhere in
    /// `inner`, one per line: `span_of` locates each, `sep` follows it
    /// (`,` after a field) and `print` lays it out. `None` when there is
    /// neither an element nor a comment.
    pub(super) fn sequence<T>(
        &mut self,
        inner: Span,
        elems: &[T],
        span_of: impl Fn(&Self, &T) -> Span,
        sep: impl Fn(&T) -> &'static str,
        mut print: impl FnMut(&mut Self, &T) -> Doc,
    ) -> Option<Doc> {
        // (a blank line before it, the line's document)
        let mut lines: Vec<(bool, Doc)> = Vec::new();
        let mut prev = inner.start;
        for elem in elems {
            let span = span_of(self, elem);
            prev = self.own_lines(prev, span.start, &mut lines);
            let blank = !lines.is_empty() && self.blank_between(prev, span.start);
            let doc = self.element(span, |p| print(p, elem));
            let mut line = vec![doc, text(sep(elem))];
            prev = self.trailing(span.end, &mut line);
            lines.push((blank, concat(line)));
        }
        self.own_lines(prev, inner.end, &mut lines);
        if lines.is_empty() {
            return None;
        }
        let mut out = Vec::new();
        for (i, (blank, doc)) in lines.into_iter().enumerate() {
            if i > 0 {
                out.push(Doc::Hard);
                if blank {
                    out.push(Doc::Hard);
                }
            }
            out.push(doc);
        }
        Some(concat(out))
    }

    /// `{ … }` around a sequence, or `{}` for none.
    pub(super) fn braced(&self, body: Option<Doc>) -> Doc {
        match body {
            None => text("{}"),
            Some(body) => concat(vec![
                text("{"),
                nest(concat(vec![Doc::Hard, body])),
                Doc::Hard,
                text("}"),
            ]),
        }
    }

    /// The position just after the `{` that follows `from`, skipping
    /// comments: where a declaration's body begins.
    pub(super) fn after_brace(&self, from: usize, end: usize) -> usize {
        let mut at = from;
        while at < end {
            if let Some(c) = self.comments.iter().find(|c| c.span.start == at) {
                at = c.span.end;
                continue;
            }
            if self.src.as_bytes()[at] == b'{' {
                return at + 1;
            }
            at += 1;
        }
        end
    }

    /// The comments between `from` and `to` not yet placed, each as its own
    /// line; returns where the last one ends.
    fn own_lines(&mut self, mut from: usize, to: usize, lines: &mut Vec<(bool, Doc)>) -> usize {
        for i in 0..self.comments.len() {
            let span = self.comments[i].span;
            if self.claimed[i] || span.start < from || span.end > to {
                continue;
            }
            let blank = !lines.is_empty() && self.blank_between(from, span.start);
            self.claimed[i] = true;
            lines.push((blank, self.verbatim(span)));
            from = span.end;
        }
        from
    }

    /// Adds the comments after `end` on the same line to `line`; returns
    /// where the last one ends.
    fn trailing(&mut self, mut end: usize, line: &mut Vec<Doc>) -> usize {
        while let Some(i) = (0..self.comments.len()).find(|&i| {
            let start = self.comments[i].span.start;
            !self.claimed[i]
                && start >= end
                && self.src[end..start]
                    .chars()
                    .all(|c| matches!(c, ' ' | '\t' | ',' | ';'))
        }) {
            self.claimed[i] = true;
            let span = self.comments[i].span;
            line.push(text(" "));
            line.push(self.verbatim(span));
            end = span.end;
        }
        end
    }

    /// `print`'s layout of the element at `span`, or its source text when a
    /// comment inside it would otherwise be lost.
    fn element(&mut self, span: Span, print: impl FnOnce(&mut Self) -> Doc) -> Doc {
        let before = self.claimed.clone();
        let doc = print(self);
        let inside = |c: &Span| c.start >= span.start && c.end <= span.end;
        let lost =
            (0..self.comments.len()).any(|i| !self.claimed[i] && inside(&self.comments[i].span));
        if !lost {
            return doc;
        }
        self.claimed = before;
        for i in 0..self.comments.len() {
            if inside(&self.comments[i].span) {
                self.claimed[i] = true;
            }
        }
        self.verbatim(span)
    }

    /// The source text at `span`, its later lines moved with its first:
    /// the indentation its first line had is replaced by the output's.
    pub(super) fn verbatim(&self, span: Span) -> Doc {
        let line_start = self.src[..span.start].rfind('\n').map_or(0, |i| i + 1);
        let column = span.start - line_start;
        let mut parts = Vec::new();
        for (i, line) in self.text(span).split('\n').enumerate() {
            let line = line.strip_suffix('\r').unwrap_or(line);
            if i == 0 {
                parts.push(text(line));
                continue;
            }
            let indent = line.len() - line.trim_start_matches([' ', '\t']).len();
            parts.push(Doc::Hard);
            parts.push(text(&line[indent.min(column)..]));
        }
        concat(parts)
    }

    /// Whether the text from `a` to `b` holds an empty line.
    fn blank_between(&self, a: usize, b: usize) -> bool {
        if a >= b {
            return false;
        }
        let parts: Vec<&str> = self.src[a..b].split('\n').collect();
        parts.len() > 2
            && parts[1..parts.len() - 1]
                .iter()
                .any(|l| l.trim().is_empty())
    }
}
