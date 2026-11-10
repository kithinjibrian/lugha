//! Turns the files under a root folder into test cases (`.args`, `.stdin`
//! and `.scratch`: `args.rs`).

use std::collections::BTreeMap;
use std::io;
use std::path::{Path, PathBuf};

use super::{Case, Malformed, Mode, dirs, lossy};

#[derive(Default)]
struct Group {
    la: Option<PathBuf>,
    stdout: Option<PathBuf>,
    stderr: Option<PathBuf>,
    exit: Option<PathBuf>,
    /// `.args`, `.stdin` and `.scratch` files (`args.rs`).
    extra: Vec<PathBuf>,
}

/// Finds every `.la` program under `root` and loads its expectations. A
/// directory with expectations beside it is one multi-file case (`dirs.rs`).
///
/// # Errors
///
/// Returns every [`Malformed`] entry if any exist: a program with no `.exit` or
/// `.stderr`, `.exit` without `.stdout`, `.stdout` without `.exit`, an `.exit`
/// that isn't 0–255, an expectation file with no `.la`, an unreadable file,
/// or no programs at all.
pub fn discover(root: &Path) -> Result<Vec<Case>, Vec<Malformed>> {
    let rel_name = |p: &Path| p.to_string_lossy().into_owned();
    let mut files = Vec::new();
    if let Err(e) = walk(root, root, &mut files) {
        let reason = format!("cannot read {}: {e}", root.display());
        return Err(vec![Malformed {
            name: rel_name(root),
            reason,
        }]);
    }

    let case_dirs = dirs::case_dirs(root, &files);
    let mut groups: BTreeMap<PathBuf, Group> = BTreeMap::new();
    for rel in files {
        // A case directory's own files are its modules.
        if case_dirs.iter().any(|dir| rel.starts_with(dir)) {
            continue;
        }
        let group = groups.entry(rel.with_extension("")).or_default();
        match rel.extension().and_then(|e| e.to_str()) {
            Some("la") => group.la = Some(rel),
            Some("stdout") => group.stdout = Some(rel),
            Some("stderr") => group.stderr = Some(rel),
            Some("exit") => group.exit = Some(rel),
            Some("args" | "stdin" | "scratch" | "envvars") => group.extra.push(rel),
            // Other files (notes, READMEs) are not part of any case.
            _ => {}
        }
    }

    let mut cases = Vec::new();
    let mut malformed = Vec::new();
    for (stem, mut group) in groups {
        if case_dirs.contains(&stem) {
            match dirs::main_of(root, &stem) {
                Ok(main) => group.la = Some(main),
                Err(bad) => {
                    malformed.push(bad);
                    continue;
                }
            }
        }
        let Some(la) = &group.la else {
            for orphan in [&group.stdout, &group.stderr, &group.exit]
                .into_iter()
                .flatten()
                .chain(&group.extra)
            {
                let reason = "no matching .la".to_string();
                malformed.push(Malformed {
                    name: rel_name(orphan),
                    reason,
                });
            }
            continue;
        };
        let name = rel_name(la);
        match load_mode(root, &group) {
            Ok((mode, extras)) => cases.push(Case {
                name,
                path: root.join(la),
                mode,
                extras,
            }),
            Err(reason) => malformed.push(Malformed { name, reason }),
        }
    }

    if cases.is_empty() && malformed.is_empty() {
        let reason = format!("no programs found in {}", root.display());
        malformed.push(Malformed {
            name: rel_name(root),
            reason,
        });
    }
    if malformed.is_empty() {
        Ok(cases)
    } else {
        Err(malformed)
    }
}

fn walk(root: &Path, dir: &Path, out: &mut Vec<PathBuf>) -> io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            walk(root, &path, out)?;
        } else {
            let rel = path
                .strip_prefix(root)
                .expect("walked paths are under root");
            out.push(rel.to_path_buf());
        }
    }
    Ok(())
}

fn load_mode(root: &Path, group: &Group) -> Result<(Mode, super::args::Extras), String> {
    let read = |rel: &PathBuf| {
        std::fs::read(root.join(rel)).map_err(|e| format!("cannot read {}: {e}", rel.display()))
    };
    let mode: Result<Mode, String> = match (&group.exit, &group.stdout, &group.stderr) {
        (Some(exit), Some(stdout), stderr) => Ok(Mode::Run {
            stdout: read(stdout)?,
            stderr: stderr.as_ref().map(read).transpose()?,
            exit: parse_exit(&read(exit)?)?,
        }),
        (Some(_), None, _) => Err(".exit without .stdout".into()),
        (None, Some(_), _) => Err(".stdout without .exit".into()),
        (None, None, Some(stderr)) => Ok(Mode::Reject {
            stderr: read(stderr)?,
        }),
        (None, None, None) => Err("no .exit or .stderr file".into()),
    };
    super::args::extras(mode?, &group.extra, read)
}

fn parse_exit(bytes: &[u8]) -> Result<i32, String> {
    let digits = bytes.strip_suffix(b"\n").unwrap_or(bytes);
    let invalid = || format!(".exit must be an integer 0-255, got {:?}", lossy(bytes));
    if digits.is_empty() || !digits.iter().all(u8::is_ascii_digit) {
        return Err(invalid());
    }
    match lossy(digits).parse::<u16>() {
        Ok(code) if code <= 255 => Ok(i32::from(code)),
        _ => Err(invalid()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::support::fixture::Fixture;

    fn reasons(fixture: &Fixture) -> Vec<String> {
        let malformed = discover(fixture.path()).expect_err("fixture is malformed");
        malformed
            .into_iter()
            .map(|m| format!("{}: {}", m.name, m.reason))
            .collect()
    }

    #[test]
    fn discovers_cases_recursively_in_sorted_order() {
        let f = Fixture::new();
        f.file("m2/b.la", "")
            .file("m2/b.stdout", "")
            .file("m2/b.exit", "0\n");
        f.file("m1/a.la", "")
            .file("m1/a.stdout", "")
            .file("m1/a.exit", "0");
        let names: Vec<_> = discover(f.path())
            .unwrap()
            .into_iter()
            .map(|c| c.name)
            .collect();
        assert_eq!(names, ["m1/a.la", "m2/b.la"]);
    }

    #[test]
    fn a_directory_with_main_la_is_one_case() {
        let f = Fixture::new();
        f.file("m10/two/main.la", "")
            .file("m10/two/geo.la", "")
            .file("m10/two/shapes/circle.la", "");
        f.file("m10/two.stdout", "").file("m10/two.exit", "0\n");
        f.file("m10/one.la", "").file("m10/one.stderr", "e\n");
        let cases = discover(f.path()).unwrap();
        let names: Vec<_> = cases.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(names, ["m10/one.la", "m10/two/main.la"]);
        assert!(cases[1].path.ends_with("m10/two/main.la"));
    }

    #[test]
    fn a_case_directory_needs_main_la() {
        let f = Fixture::new();
        f.file("two/geo.la", "").file("two.stderr", "e\n");
        assert_eq!(reasons(&f), ["two/: a case directory needs a main.la"]);
    }

    #[test]
    fn exit_file_selects_run_mode_with_optional_stderr() {
        let f = Fixture::new();
        f.file("p.la", "")
            .file("p.stdout", "hi\n")
            .file("p.exit", "101\n");
        f.file("p.stderr", "panic\n");
        let case = discover(f.path()).unwrap().remove(0);
        let stderr = Some(b"panic\n".to_vec());
        assert_eq!(
            case.mode,
            Mode::Run {
                stdout: b"hi\n".to_vec(),
                stderr,
                exit: 101
            }
        );
    }

    #[test]
    fn stderr_without_exit_selects_reject_mode() {
        let f = Fixture::new();
        f.file("bad.la", "").file("bad.stderr", "error[E0401]\n");
        let case = discover(f.path()).unwrap().remove(0);
        assert_eq!(
            case.mode,
            Mode::Reject {
                stderr: b"error[E0401]\n".to_vec()
            }
        );
    }

    #[test]
    fn program_without_expectations_is_malformed() {
        let f = Fixture::new();
        f.file("typo.la", "");
        assert_eq!(reasons(&f), ["typo.la: no .exit or .stderr file"]);
    }

    #[test]
    fn exit_without_stdout_is_malformed() {
        let f = Fixture::new();
        f.file("p.la", "").file("p.exit", "0");
        assert_eq!(reasons(&f), ["p.la: .exit without .stdout"]);
    }

    #[test]
    fn stdout_in_reject_mode_is_malformed() {
        let f = Fixture::new();
        f.file("p.la", "")
            .file("p.stderr", "e")
            .file("p.stdout", "");
        assert_eq!(reasons(&f), ["p.la: .stdout without .exit"]);
    }

    #[test]
    fn invalid_exit_codes_are_malformed() {
        for bad in ["abc", "256", "-1", "", "1\n\n"] {
            let f = Fixture::new();
            f.file("p.la", "").file("p.stdout", "").file("p.exit", bad);
            let got = reasons(&f);
            assert_eq!(got.len(), 1, "{bad:?}");
            assert!(
                got[0].starts_with("p.la: .exit must be an integer 0-255"),
                "{bad:?}: {got:?}"
            );
        }
    }

    #[test]
    fn orphan_expectation_file_is_malformed() {
        let f = Fixture::new();
        f.file("a.la", "")
            .file("a.stdout", "")
            .file("a.exit", "0")
            .file("gone.stdout", "");
        assert_eq!(reasons(&f), ["gone.stdout: no matching .la"]);
    }

    #[test]
    fn empty_root_is_malformed() {
        let f = Fixture::new();
        let got = reasons(&f);
        assert_eq!(got.len(), 1);
        assert!(got[0].contains("no programs found"), "{got:?}");
    }
}
