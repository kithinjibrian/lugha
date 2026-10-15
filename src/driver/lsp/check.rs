//! One check of a main file for the language server: the CLI's front end
//! over the open documents, offline, writing nothing, with panics caught
//! (v16 draft decisions 13 and 15).

use std::collections::HashMap;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};

use lsp_types::Diagnostic;

use super::documents::Documents;
use super::publish;
use crate::driver::guard;
use crate::driver::pipeline::{self, Failure};
use crate::driver::render::Report;
use crate::driver::source::Source;
use crate::modules::fetch::Mode;

/// What one check found.
#[derive(Default)]
pub(super) struct Outcome {
    /// Diagnostics by file, including files with none that the check loaded,
    /// so stale ones are cleared.
    pub files: HashMap<PathBuf, Vec<Diagnostic>>,
    /// Every program file the check loaded, the main file first.
    pub loaded: Vec<PathBuf>,
    /// Lines for the client's log.
    pub log: Vec<String>,
}

/// Checks `main` with `docs`' unsaved text, dropping E0303 when `alone`.
pub(super) fn check(main: &Path, docs: &Documents, alone: bool) -> Outcome {
    check_with(main, alone, || {
        let mut read = |file: &Path| docs.read(file);
        // Offline always: no `git` from an editor (decision 13).
        let mode = Mode {
            offline: true,
            update: false,
        };
        pipeline::front_with(main, mode, &mut read)
    })
}

/// `check` around any front end, so tests can make it panic.
fn check_with(
    main: &Path,
    alone: bool,
    front: impl FnOnce() -> Result<pipeline::Front, Failure>,
) -> Outcome {
    let root = main.parent().unwrap_or(Path::new("/"));
    let mut outcome = Outcome::default();
    // The compiler's state for this check is dropped after a panic, so
    // nothing observes it half-updated: unwind safety holds.
    let (source, reports) = match panic::catch_unwind(AssertUnwindSafe(front)) {
        Ok(Ok(front)) => (
            front.source,
            front.warnings.iter().map(Report::from).collect(),
        ),
        Ok(Err(Failure::Program(source, diagnostics))) => {
            (source, diagnostics.iter().map(Report::from).collect())
        }
        Ok(Err(Failure::Internal(source, report))) => (source, vec![*report]),
        Err(payload) => {
            let mut report = Report::internal(
                format!("internal compiler error: {}", guard::message(&*payload)),
                None,
            );
            report.help = Some(guard::help());
            outcome
                .log
                .push(format!("{}: {}", main.display(), report.message));
            (Source::single(main.display().to_string(), ""), vec![report])
        }
    };
    for file in &source.files {
        if let Some(path) = publish::file_path(file, root) {
            outcome.files.entry(path.clone()).or_default();
            outcome.loaded.push(path);
        }
    }
    let main = publish::file_path(source.main(), root).unwrap_or_else(|| main.to_path_buf());
    for report in &reports {
        if alone && report.code == Some("E0303") {
            continue;
        }
        let (file, diagnostic) = match publish::convert(report, &source, root) {
            Some(found) => found,
            None if report.span.is_none() => (main.clone(), publish::top(report)),
            // Inside `<std>` or the prelude: no file to show it in.
            None => continue,
        };
        outcome.files.entry(file).or_default().push(diagnostic);
    }
    outcome
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temp(name: &str, files: &[(&str, &str)]) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("lugha-lsp-check-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        for (file, text) in files {
            let path = dir.join(file);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        dir
    }

    #[test]
    fn a_panic_becomes_one_internal_error_at_the_top() {
        let main = Path::new("/w/main.la");
        let outcome = check_with(main, false, || panic!("boom"));
        let shown = &outcome.files[main];
        assert_eq!(shown.len(), 1);
        assert!(
            shown[0]
                .message
                .starts_with("internal compiler error: boom\nhelp: this is a bug in lughac")
        );
        assert_eq!(shown[0].range, lsp_types::Range::default());
        assert_eq!(outcome.log.len(), 1);
    }

    #[test]
    fn errors_land_in_their_files_and_loaded_files_are_listed() {
        let dir = temp(
            "files",
            &[
                (
                    "main.la",
                    "import util;\nfun main(): i32 { return util.f(); }\n",
                ),
                ("util.la", "fun f(): i32 { return \"x\"; }\n"),
            ],
        );
        let outcome = check(&dir.join("main.la"), &Documents::default(), false);
        assert_eq!(outcome.loaded, [dir.join("main.la"), dir.join("util.la")]);
        assert_eq!(outcome.files[&dir.join("main.la")], []);
        assert_eq!(outcome.files[&dir.join("util.la")].len(), 1);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn alone_drops_e0303_only() {
        let dir = temp("alone", &[("util.la", "fun f(): i32 { return \"x\"; }\n")]);
        let file = dir.join("util.la");
        let codes = |alone| {
            let outcome = check(&file, &Documents::default(), alone);
            let mut codes: Vec<String> = outcome.files[&file]
                .iter()
                .map(|d| match &d.code {
                    Some(lsp_types::NumberOrString::String(c)) => c.clone(),
                    _ => String::new(),
                })
                .collect();
            codes.sort();
            codes
        };
        assert_eq!(codes(false), ["E0303", "E0403"]);
        assert_eq!(codes(true), ["E0403"]);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn an_unreadable_main_file_is_one_error_at_its_top() {
        let main = Path::new("/nonexistent/lugha/main.la");
        let outcome = check(main, &Documents::default(), false);
        let shown = &outcome.files[main];
        assert_eq!(shown.len(), 1);
        assert!(
            shown[0].message.contains("cannot read"),
            "{}",
            shown[0].message
        );
    }
}
