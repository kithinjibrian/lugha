//! Building `lughac test`'s executables (v16 draft decisions 82–85; v17
//! milestone 124, PRP-156): the test files of one folder under a
//! `lugha.toml` are checked and built together, as one program whose
//! roots are those files, so the package is compiled once for all of
//! them. A file outside any package is built alone, as before. When the
//! group doesn't check, each file is checked alone to find the broken
//! ones, which are reported by themselves; the rest are built together.

use std::path::{Path, PathBuf};

use super::pipeline::{self, Failure};
use super::render::Report;
use super::source::Source;
use super::{test_json, update};
use crate::codegen::{Cpu, OptLevel, Target};
use crate::modules::{self, fetch::Mode};

/// A test of a built executable: its key (what `LUGHA_TEST` names), and
/// its name's line and column in its file.
pub(super) type Listed = (String, usize, usize);

/// What building one group gave.
pub(super) struct GroupResult {
    /// Each file that built (its index into the files), with its tests.
    pub built: Vec<(usize, Vec<Listed>)>,
    /// What to report, each with how many files it kept from running.
    pub failures: Vec<(Failure, usize)>,
}

/// The load's mode: packages fetched as `lughac test` always has.
const MODE: Mode = Mode {
    offline: false,
    update: false,
};

/// `files` in groups (indexes, each group in file order, groups in the
/// order of their first file): the files of one folder governed by a
/// `lugha.toml` together, every other file alone.
pub(super) fn groups(files: &[PathBuf]) -> Vec<Vec<usize>> {
    let mut read = |p: &Path| std::fs::read(p);
    let env = update::env(MODE);
    let mut groups: Vec<(Option<PathBuf>, Vec<usize>)> = Vec::new();
    for (i, file) in files.iter().enumerate() {
        let folder = file.parent().unwrap_or(Path::new("")).to_path_buf();
        let key = modules::layout::has_manifest(&folder, &mut read, &env)
            .then(|| modules::paths::normalized(&folder));
        match groups.iter_mut().find(|(k, _)| key.is_some() && *k == key) {
            Some((_, members)) => members.push(i),
            None => groups.push((key, vec![i])),
        }
    }
    groups.into_iter().map(|(_, members)| members).collect()
}

/// Checks and builds the files `members` of `files` into the executable
/// `exe` for `cpu`, reporting a broken file alone (decision 278).
pub(super) fn build_group(
    files: &[PathBuf],
    members: &[usize],
    exe: &Path,
    cpu: Cpu,
) -> GroupResult {
    let mut result = GroupResult {
        built: Vec::new(),
        failures: Vec::new(),
    };
    if let [only] = members {
        match build(&files[*only], exe, cpu) {
            Ok(tests) => result.built.push((*only, tests)),
            Err(failure) => result.failures.push(counted(failure, 1)),
        }
        return result;
    }
    let paths: Vec<PathBuf> = members.iter().map(|&i| files[i].clone()).collect();
    let front = match pipeline::front_test_roots(&paths, MODE) {
        Ok(front) => front,
        Err(Failure::Program(source, diagnostics)) => {
            return apart(files, members, (exe, cpu), (source, diagnostics));
        }
        Err(failure) => {
            result.failures.push((failure, 0));
            return result;
        }
    };
    let keys = front
        .checked
        .as_ref()
        .and_then(|c| c.tests.clone())
        .unwrap_or_default();
    if !keys.is_empty()
        && let Err(failure) = pipeline::build(&front, (OptLevel::O0, cpu, Target::default()), exe)
    {
        result.failures.push(counted(failure, members.len()));
        return result;
    }
    // The roots come first among the modules, in order (`load_all`).
    for (&i, module) in members.iter().zip(&front.modules) {
        let tests = test_json::listed(&module.text)
            .unwrap_or_default()
            .into_iter()
            .map(|(name, line, col)| (crate::check::qualify(&module.path, &name), line, col))
            .filter(|(key, _, _)| keys.contains(key))
            .collect();
        result.built.push((i, tests));
    }
    result
}

/// The group didn't check: each file is checked alone, the broken ones
/// reported, and the rest built together. If every file checks alone, the
/// files only fail together, which is lughac's fault (exit 2).
fn apart(
    files: &[PathBuf],
    members: &[usize],
    (exe, cpu): (&Path, Cpu),
    (source, diagnostics): (Source, Vec<crate::diagnostic::Diagnostic>),
) -> GroupResult {
    let mut failures = Vec::new();
    let mut good = Vec::new();
    for &i in members {
        match pipeline::front_tests(&files[i], MODE) {
            Ok(_) => good.push(i),
            Err(failure) => failures.push(counted(failure, 1)),
        }
    }
    if failures.is_empty() {
        let report = Report::internal(
            "these test files check alone but not together; please report it",
            None,
        );
        failures.push((Failure::Program(source.clone(), diagnostics), 0));
        failures.push((Failure::Internal(source, Box::new(report)), 0));
        return GroupResult {
            built: Vec::new(),
            failures,
        };
    }
    let mut result = match good.is_empty() {
        true => GroupResult {
            built: Vec::new(),
            failures: Vec::new(),
        },
        false => build_group(files, &good, exe, cpu),
    };
    failures.append(&mut result.failures);
    result.failures = failures;
    result
}

/// A failure, counted against `files` files when it is the program's.
fn counted(failure: Failure, files: usize) -> (Failure, usize) {
    match failure {
        Failure::Program(..) => (failure, files),
        Failure::Internal(..) => (failure, 0),
    }
}

/// Checks `file` for tests and builds its test executable at `exe`; returns
/// its tests in declaration order, each with its name's line and column.
fn build(file: &Path, exe: &Path, cpu: Cpu) -> Result<Vec<Listed>, Failure> {
    let front = pipeline::front_tests(file, MODE)?;
    let tests = front
        .checked
        .as_ref()
        .and_then(|c| c.tests.clone())
        .unwrap_or_default();
    if !tests.is_empty() {
        pipeline::build(&front, (OptLevel::O0, cpu, Target::default()), exe)?;
    }
    // Positions from the main file's own text (offset 0 in the source map).
    let text = &front.source.main().text;
    let positions = test_json::listed(text).unwrap_or_default();
    Ok(tests
        .into_iter()
        .map(|t| {
            let (line, col) = positions
                .iter()
                .find(|(n, _, _)| n == test_json::bare(&t))
                .map_or((1, 1), |(_, l, c)| (*l, *c));
            (t, line, col)
        })
        .collect())
}
