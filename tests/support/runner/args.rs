//! Command-line arguments for a test program (PRP-038): an optional
//! `<name>.args` beside a run case, one argument per line, passed to the
//! program after `lughac run <file> --`.

use super::Mode;

/// `mode` with the arguments from its `.args` file, if it has one: only a
/// run case takes arguments.
pub fn with_args(mode: Mode, args: Option<Vec<u8>>) -> Result<(Mode, Vec<String>), String> {
    match (args, &mode) {
        (Some(_), Mode::Reject { .. }) => Err(".args beside a rejected program".into()),
        (args, _) => Ok((mode, args.map_or_else(Vec::new, |a| parse_args(&a)))),
    }
}

/// The arguments in an `.args` file: one per line, the last line's newline optional.
pub fn parse_args(bytes: &[u8]) -> Vec<String> {
    let text = String::from_utf8_lossy(bytes);
    let text = text.strip_suffix('\n').unwrap_or(&text);
    if text.is_empty() {
        return Vec::new();
    }
    text.split('\n').map(str::to_string).collect()
}

#[cfg(test)]
mod tests {
    use super::parse_args;
    use crate::support::fixture::Fixture;
    use crate::support::runner::discover;

    #[test]
    fn each_line_is_one_argument() {
        assert_eq!(parse_args(b"21\nb c\n"), ["21", "b c"]);
        assert_eq!(parse_args(b"x"), ["x"]);
        assert!(parse_args(b"").is_empty());
    }

    #[test]
    fn args_files_belong_to_run_cases() {
        let f = Fixture::new();
        f.file("p.la", "")
            .file("p.stdout", "")
            .file("p.exit", "0")
            .file("p.args", "21\n");
        let case = discover(f.path()).unwrap().remove(0);
        assert_eq!(case.args, ["21"]);

        let f = Fixture::new();
        f.file("bad.la", "")
            .file("bad.stderr", "e\n")
            .file("bad.args", "1\n");
        let reasons: Vec<String> = discover(f.path())
            .unwrap_err()
            .into_iter()
            .map(|m| m.reason)
            .collect();
        assert_eq!(reasons, [".args beside a rejected program"]);

        let f = Fixture::new();
        f.file("a.la", "")
            .file("a.stdout", "")
            .file("a.exit", "0")
            .file("lost.args", "1\n");
        let malformed = discover(f.path()).unwrap_err();
        assert_eq!(malformed[0].name, "lost.args");
        assert_eq!(malformed[0].reason, "no matching .la");
    }
}
