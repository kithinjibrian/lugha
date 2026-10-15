//! What a run case may have beside its expectations:
//! - `<name>.args` (PRP-038): one argument per line, passed after `lughac run <file> --`;
//! - `<name>.stdin` (PRP-044): fed to the program's stdin;
//! - `<name>.scratch` (PRP-044, empty): run in a fresh temporary directory;
//! - `<name>.envvars` (v15, PRP-093): `KEY=VALUE` lines set in the program's environment.

use std::path::PathBuf;

use super::Mode;

/// A run case's arguments, stdin and scratch-directory request.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Extras {
    pub args: Vec<String>,
    /// `None` gives the program an empty stdin.
    pub stdin: Option<Vec<u8>>,
    pub scratch: bool,
    /// Environment variables to set, in file order.
    pub envvars: Vec<(String, String)>,
}

/// `mode` with the extras its `files` give; only a run case takes any.
pub fn extras(
    mode: Mode,
    files: &[PathBuf],
    read: impl Fn(&PathBuf) -> Result<Vec<u8>, String>,
) -> Result<(Mode, Extras), String> {
    let mut extras = Extras::default();
    for file in files {
        let kind = file
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or_default();
        if let Mode::Reject { .. } = mode {
            return Err(format!(".{kind} beside a rejected program"));
        }
        match kind {
            "args" => extras.args = parse_args(&read(file)?),
            "stdin" => extras.stdin = Some(read(file)?),
            "envvars" => extras.envvars = parse_envvars(&read(file)?)?,
            _ => extras.scratch = true,
        }
    }
    Ok((mode, extras))
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

/// The variables in an `.envvars` file: `KEY=VALUE` per line; a line
/// without `=` or with an empty key is an error.
pub fn parse_envvars(bytes: &[u8]) -> Result<Vec<(String, String)>, String> {
    let text = String::from_utf8_lossy(bytes);
    let mut vars = Vec::new();
    for line in text.lines().filter(|l| !l.is_empty()) {
        match line.split_once('=') {
            Some((key, value)) if !key.is_empty() => {
                vars.push((key.to_string(), value.to_string()))
            }
            _ => return Err(format!("`{line}` in an .envvars file isn't KEY=VALUE")),
        }
    }
    Ok(vars)
}

#[cfg(test)]
mod tests {
    use super::{parse_args, parse_envvars};
    use crate::support::fixture::Fixture;
    use crate::support::runner::discover;

    #[test]
    fn envvars_are_key_value_lines() {
        assert_eq!(
            parse_envvars(b"A=1\nB=x=y\n").unwrap(),
            [
                ("A".to_string(), "1".to_string()),
                ("B".to_string(), "x=y".to_string())
            ]
        );
        assert!(parse_envvars(b"NOEQUALS\n").is_err());
        assert!(parse_envvars(b"=v\n").is_err());
    }

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
        assert_eq!(case.extras.args, ["21"]);

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

    #[test]
    fn stdin_and_scratch_belong_to_run_cases() {
        let f = Fixture::new();
        f.file("p.la", "")
            .file("p.stdout", "")
            .file("p.exit", "0")
            .file("p.stdin", "5\n7\n")
            .file("p.scratch", "");
        let case = discover(f.path()).unwrap().remove(0);
        assert_eq!(case.extras.stdin.as_deref(), Some(&b"5\n7\n"[..]));
        assert!(case.extras.scratch);
        let f = Fixture::new();
        f.file("bad.la", "")
            .file("bad.stderr", "e\n")
            .file("bad.scratch", "");
        let reasons: Vec<String> = discover(f.path())
            .unwrap_err()
            .into_iter()
            .map(|m| m.reason)
            .collect();
        assert_eq!(reasons, [".scratch beside a rejected program"]);
        let f = Fixture::new();
        f.file("a.la", "")
            .file("a.stdout", "")
            .file("a.exit", "0")
            .file("lost.stdin", "x");
        assert_eq!(discover(f.path()).unwrap_err()[0].reason, "no matching .la");
    }
}
