//! A small document algebra and its printer (Wadler's "prettier printer",
//! simplified): text, breakable lines, nesting and groups that print flat
//! when they fit in the width, broken otherwise.

/// A document to lay out.
#[derive(Debug, Clone)]
pub(super) enum Doc {
    Text(String),
    /// A space when its group is flat, a line break when broken.
    Line,
    /// Nothing when flat, a line break when broken.
    SoftLine,
    /// Always a line break; it breaks every group around it.
    Hard,
    /// `broken` when its group is broken, `flat` when flat (trailing commas).
    IfBreak(&'static str, &'static str),
    /// Indents the line breaks inside it by four more spaces.
    Nest(Box<Doc>),
    /// Flat if it fits on the line, else broken.
    Group(Box<Doc>),
    Concat(Vec<Doc>),
}

/// The width a line may reach (decision 61).
pub(super) const WIDTH: usize = 100;
const INDENT: usize = 4;

pub(super) fn text(s: impl Into<String>) -> Doc {
    Doc::Text(s.into())
}

pub(super) fn concat(docs: Vec<Doc>) -> Doc {
    Doc::Concat(docs)
}

pub(super) fn group(doc: Doc) -> Doc {
    Doc::Group(Box::new(doc))
}

pub(super) fn nest(doc: Doc) -> Doc {
    Doc::Nest(Box::new(doc))
}

/// `open`, then `items` separated by `,` — on one line when the group
/// fits, else one per line with a trailing comma — then `close`.
pub(super) fn list(open: &str, items: Vec<Doc>, close: &str) -> Doc {
    separated(open, items, close, Doc::SoftLine)
}

/// [`list`] with a space inside the delimiters when flat: `{ a, b }`.
pub(super) fn spaced(open: &str, items: Vec<Doc>, close: &str) -> Doc {
    separated(open, items, close, Doc::Line)
}

fn separated(open: &str, items: Vec<Doc>, close: &str, edge: Doc) -> Doc {
    if items.is_empty() {
        return text(format!("{open}{close}"));
    }
    // A last element that spans lines (a closure or a block) hugs the
    // delimiters instead of breaking the list: `f(a, fun(x) {` … `})`.
    let count = items.len();
    if has_hard(&items[count - 1]) && !items[..count - 1].iter().any(has_hard) {
        let pad = if matches!(edge, Doc::Line) { " " } else { "" };
        let mut parts = vec![text(format!("{open}{pad}"))];
        for (i, item) in items.into_iter().enumerate() {
            if i > 0 {
                parts.push(text(", "));
            }
            parts.push(item);
        }
        parts.push(text(format!("{pad}{close}")));
        return concat(parts);
    }
    let mut inner = vec![edge.clone()];
    for (i, item) in items.into_iter().enumerate() {
        inner.push(item);
        if i + 1 < count {
            inner.push(text(","));
            inner.push(Doc::Line);
        }
    }
    inner.push(Doc::IfBreak(",", ""));
    group(concat(vec![
        text(open),
        nest(concat(inner)),
        edge,
        text(close),
    ]))
}

/// Whether `doc` holds a hard line break anywhere.
fn has_hard(doc: &Doc) -> bool {
    match doc {
        Doc::Hard => true,
        Doc::Nest(d) | Doc::Group(d) => has_hard(d),
        Doc::Concat(ds) => ds.iter().any(has_hard),
        _ => false,
    }
}

/// Lays `doc` out within `WIDTH`, starting at column 0 with `indent`.
pub(super) fn print(doc: &Doc, indent: usize) -> String {
    let mut out = String::new();
    // A stack of (indent, broken, doc), processed front to back.
    let mut stack: Vec<(usize, bool, &Doc)> = vec![(indent, true, doc)];
    let mut column = 0;
    while let Some((ind, broken, d)) = stack.pop() {
        match d {
            Doc::Text(s) => {
                out.push_str(s);
                column += s.chars().count();
            }
            Doc::Line | Doc::SoftLine | Doc::Hard if broken || matches!(d, Doc::Hard) => {
                // No trailing spaces: drop them before breaking.
                while out.ends_with(' ') {
                    out.pop();
                }
                out.push('\n');
                out.push_str(&" ".repeat(ind));
                column = ind;
            }
            Doc::Line => {
                out.push(' ');
                column += 1;
            }
            Doc::SoftLine | Doc::Hard => {}
            Doc::IfBreak(b, f) => {
                let s = if broken { b } else { f };
                out.push_str(s);
                column += s.chars().count();
            }
            Doc::Nest(inner) => stack.push((ind + INDENT, broken, inner)),
            Doc::Group(inner) => {
                let flat = !has_hard(inner) && fits(inner, WIDTH.saturating_sub(column), &stack);
                stack.push((ind, !flat, inner));
            }
            Doc::Concat(ds) => {
                for part in ds.iter().rev() {
                    stack.push((ind, broken, part));
                }
            }
        }
    }
    out
}

/// Whether `doc`, flat, and then what follows it up to the next line
/// break, fit in `left` columns.
fn fits(doc: &Doc, left: usize, rest: &[(usize, bool, &Doc)]) -> bool {
    let mut left = left as isize;
    let mut todo: Vec<(bool, &Doc)> = vec![(false, doc)];
    let mut rest = rest.iter().rev();
    loop {
        let Some((broken, d)) = todo.pop().or_else(|| rest.next().map(|(_, b, d)| (*b, *d))) else {
            return left >= 0;
        };
        if left < 0 {
            return false;
        }
        match d {
            Doc::Text(s) => left -= s.chars().count() as isize,
            Doc::Line | Doc::SoftLine | Doc::Hard if broken => return true,
            Doc::Hard => return true,
            Doc::Line => left -= 1,
            Doc::SoftLine => {}
            Doc::IfBreak(b, f) => left -= if broken { b.len() } else { f.len() } as isize,
            Doc::Nest(inner) => todo.push((broken, inner)),
            // What follows decides on its own; measure it flat.
            Doc::Group(inner) => todo.push((false, inner)),
            Doc::Concat(ds) => ds.iter().rev().for_each(|part| todo.push((broken, part))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn call(name: &str, args: &[&str]) -> Doc {
        concat(vec![
            text(name),
            list("(", args.iter().map(|a| text(*a)).collect(), ")"),
        ])
    }

    #[test]
    fn a_list_that_fits_stays_on_one_line() {
        assert_eq!(print(&call("f", &["a", "b"]), 0), "f(a, b)");
        assert_eq!(print(&call("f", &[]), 0), "f()");
    }

    #[test]
    fn a_list_too_wide_breaks_one_item_per_line_with_a_trailing_comma() {
        let long = "x".repeat(60);
        let doc = call("compute", &[&long, &long]);
        assert_eq!(
            print(&doc, 0),
            format!("compute(\n    {long},\n    {long},\n)")
        );
    }

    #[test]
    fn hard_breaks_force_their_group_and_indent_inside_nests() {
        let doc = group(concat(vec![
            text("{"),
            nest(concat(vec![Doc::Hard, text("a;")])),
            Doc::Hard,
            text("}"),
        ]));
        assert_eq!(print(&doc, 0), "{\n    a;\n}");
        assert_eq!(print(&doc, 4), "{\n        a;\n    }");
    }

    #[test]
    fn a_last_element_spanning_lines_hugs_the_delimiters() {
        let block = concat(vec![
            text("{"),
            nest(concat(vec![Doc::Hard, text("x")])),
            Doc::Hard,
            text("}"),
        ]);
        let doc = concat(vec![text("f"), list("(", vec![text("a"), block], ")")]);
        assert_eq!(print(&doc, 0), "f(a, {\n    x\n})");
    }

    #[test]
    fn no_trailing_spaces_before_a_break() {
        let doc = concat(vec![text("a "), Doc::Hard, text("b")]);
        assert_eq!(print(&doc, 0), "a\nb");
    }
}
