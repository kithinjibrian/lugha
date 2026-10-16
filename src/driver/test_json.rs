//! `lughac test --list` and `--json` (v16 draft decisions 87–88, milestone
//! 85): the lines tools such as the VS Code extension read. JSON is written
//! by hand, as `json.rs` does, so no serialisation crate is needed.

use std::path::Path;

use super::fmt::display;
use super::json::string;
use super::source::line_col;
use super::update;
use crate::ast::Item;
use crate::modules::{self, fetch::Mode};
use crate::{lexer, parser};

/// Where a panic happened, from its `panic: <message> at <file>:<line>:<col>`.
#[derive(Debug, PartialEq)]
pub(super) struct Panic {
    pub message: String,
    pub at: Option<(String, usize, usize)>,
}

/// The panic in a test's stderr, if it printed one.
pub(super) fn panic_of(stderr: &str) -> Option<Panic> {
    let line = stderr.lines().find_map(|l| l.strip_prefix("panic: "))?;
    let located = line.rsplit_once(" at ").and_then(|(message, place)| {
        let mut parts = place.rsplitn(3, ':');
        let col = parts.next()?.parse().ok()?;
        let line = parts.next()?.parse().ok()?;
        let file = parts.next()?.to_string();
        Some((message.to_string(), (file, line, col)))
    });
    Some(match located {
        Some((message, at)) => Panic {
            message,
            at: Some(at),
        },
        // `stack overflow` has no location.
        None => Panic {
            message: line.to_string(),
            at: None,
        },
    })
}

/// The tests of a file's `text` by parsing alone: name and 1-based line and column of
/// each name; `None` if the file doesn't lex or parse.
pub(super) fn listed(text: &str) -> Option<Vec<(String, usize, usize)>> {
    let (tokens, _) = lexer::lex(text).ok()?;
    let (program, _) = parser::parse(&tokens).ok()?;
    let tests = program
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Fun(f) if f.test.is_some() => {
                let (line, col) = line_col(text, f.name.span.start);
                Some((f.name.name.clone(), line, col))
            }
            _ => None,
        })
        .collect();
    Some(tests)
}

/// One `--list` line.
pub(super) fn list_line(json: bool, name: &str, file: &str, line: usize, col: usize) -> String {
    if json {
        format!(
            "{{\"test\":{},\"file\":{},\"line\":{line},\"col\":{col}}}\n",
            string(name),
            string(file)
        )
    } else {
        format!("test {name} {file}:{line}:{col}\n")
    }
}

/// A finished test as `--json` reports it.
pub(super) struct Result<'a> {
    pub name: &'a str,
    pub file: &'a str,
    pub line: usize,
    /// `passed`, `failed` or `timed_out`.
    pub status: &'a str,
    pub message: Option<String>,
    pub at: Option<(String, usize, usize)>,
    pub output: &'a str,
    pub ms: u128,
}

/// One `--json` result line.
pub(super) fn result_line(r: &Result) -> String {
    let mut out = format!(
        "{{\"test\":{},\"file\":{},\"line\":{},\"status\":\"{}\"",
        string(r.name),
        string(r.file),
        r.line,
        r.status
    );
    if let Some(message) = &r.message {
        out.push_str(&format!(",\"message\":{}", string(message)));
    }
    if let Some((file, line, col)) = &r.at {
        out.push_str(&format!(
            ",\"at\":{{\"file\":{},\"line\":{line},\"col\":{col}}}",
            string(file)
        ));
    }
    out.push_str(&format!(
        ",\"output\":{},\"ms\":{}}}\n",
        string(r.output),
        r.ms
    ));
    out
}

/// The `--json` summary line.
pub(super) fn summary_line(passed: usize, failed: usize, skipped: usize) -> String {
    format!("{{\"summary\":{{\"passed\":{passed},\"failed\":{failed},\"skipped\":{skipped}}}}}\n")
}

/// The reason a test ended through `skip(reason)` (v17): exit code 77 *and*
/// the runtime's `lugha-skip: ` line, so a plain `exit(77)` isn't a skip.
pub(super) fn skipped(code: Option<i32>, stderr: &str) -> Option<String> {
    if code != Some(77) {
        return None;
    }
    stderr
        .lines()
        .rev()
        .find_map(|l| l.strip_prefix("lugha-skip: "))
        .map(str::to_string)
}

/// A test's module as reported: a file in its package's `source` folder
/// is that package's module (`glow/src/diff.la` → `glow.diff`, v17);
/// otherwise the file's path without `.la`, folders joined with `.`
/// (`src/geo.la` → `src.geo`).
pub(super) fn module_name(file: &Path) -> String {
    let mut read = |p: &Path| std::fs::read(p);
    let env = update::env(Mode {
        offline: true,
        update: false,
    });
    match modules::layout::module_path(file, &mut read, &env) {
        Some((_, path)) => path.join("."),
        None => display(&file.with_extension("")).replace(['/', '\\'], "."),
    }
}

/// A test's own name: the last part of the checker's qualified name
/// (`glow.diff.patches` → `patches`).
pub(super) fn bare(test: &str) -> &str {
    test.rsplit('.').next().unwrap_or(test)
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn module_names_follow_the_path() {
        assert_eq!(super::module_name(Path::new("geo.la")), "geo");
        assert_eq!(super::module_name(Path::new("./src/geo.la")), "src.geo");
    }

    #[test]
    fn bare_names_drop_the_module() {
        assert_eq!(super::bare("glow.diff.patches"), "patches");
        assert_eq!(super::bare("patches"), "patches");
    }

    use super::*;

    #[test]
    fn a_panic_splits_into_message_and_location() {
        let stderr = "panic: assert_eq failed: left \"a at b\", right 2 at src/m.la:3:5\n";
        assert_eq!(
            panic_of(stderr),
            Some(Panic {
                message: "assert_eq failed: left \"a at b\", right 2".into(),
                at: Some(("src/m.la".into(), 3, 5)),
            })
        );
        assert_eq!(
            panic_of("panic: stack overflow\n"),
            Some(Panic {
                message: "stack overflow".into(),
                at: None
            })
        );
        assert_eq!(panic_of("error: no test named `x`\n"), None);
    }

    #[test]
    fn json_lines_escape_their_strings() {
        let r = Result {
            name: "m.t",
            file: "m.la",
            line: 1,
            status: "failed",
            message: Some("say \"hi\"".into()),
            at: None,
            output: "a\nb",
            ms: 3,
        };
        assert_eq!(
            result_line(&r),
            "{\"test\":\"m.t\",\"file\":\"m.la\",\"line\":1,\"status\":\"failed\",\"message\":\"say \\\"hi\\\"\",\"output\":\"a\\nb\",\"ms\":3}\n"
        );
        assert_eq!(list_line(false, "m.t", "m.la", 2, 5), "test m.t m.la:2:5\n");
        assert_eq!(
            summary_line(1, 0, 2),
            "{\"summary\":{\"passed\":1,\"failed\":0,\"skipped\":2}}\n"
        );
        assert_eq!(
            skipped(Some(77), "x\nlugha-skip: no display\n"),
            Some("no display".into())
        );
        assert_eq!(skipped(Some(77), "x\n"), None);
        assert_eq!(skipped(Some(1), "lugha-skip: no\n"), None);
    }
}
