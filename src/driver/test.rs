//! `lughac test [paths…] [--filter TEXT] [--timeout SECS]` (v16 draft
//! decisions 82–85, milestone 84).
//!
//! Each file holding `@test` functions is built, with its imports, into one
//! test executable; each test then runs as its own process of it, named by
//! `LUGHA_TEST`, so a panic fails only that test. Tests run in parallel, one
//! per CPU, each killed after the time limit. The executables are started
//! directly (no shell) from a fresh temporary folder, removed afterwards.

use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::time::{Duration, Instant};

use super::Output;
use super::fmt::{display, la_files};
use super::pipeline::{self, Failure, TempDir};
use crate::codegen::OptLevel;
use crate::lexer::{self, TokenKind};
use crate::modules::fetch::Mode;

/// What `lughac test` was asked to run.
pub(super) struct Request<'a> {
    pub paths: &'a [PathBuf],
    pub filter: Option<&'a str>,
    pub timeout: Duration,
}

/// One test to run: its name as reported, and its file's executable.
struct Job {
    name: String,
    test: String,
    exe: PathBuf,
}

/// How one test ended.
enum Outcome {
    Passed,
    /// Exited with this code (101 for a panic), with its output.
    Failed {
        code: Option<i32>,
        stdout: String,
        stderr: String,
    },
    TimedOut,
    /// Couldn't be started at all: lughac's own failure.
    NotStarted(String),
}

/// Runs `lughac test`; returns the exit code.
pub(super) fn test(request: &Request, out: &mut Output) -> u8 {
    let here = [PathBuf::from(".")];
    let paths = if request.paths.is_empty() {
        &here[..]
    } else {
        request.paths
    };
    let mut code = 0;
    let mut files = Vec::new();
    for path in paths {
        if path.is_dir() {
            if let Err(error) = la_files(path, &mut files) {
                out.error(&format!("cannot read `{}`: {error}", path.display()));
                code = 2;
            }
        } else {
            files.push(path.clone());
        }
    }
    let temp = match TempDir::new() {
        Ok(temp) => temp,
        Err(error) => {
            out.error(&format!("cannot create a temporary directory: {error}"));
            return 2;
        }
    };
    let mut jobs = Vec::new();
    for (i, file) in files.iter().enumerate() {
        // `./geo.la` from walking `.` reads, and reports, as `geo.la`.
        let file = file.strip_prefix(".").unwrap_or(file);
        match std::fs::read_to_string(file) {
            Ok(text) if !has_tests(&text) => continue,
            Ok(_) => {}
            Err(error) => {
                out.error(&format!("cannot read `{}`: {error}", display(file)));
                code = 2;
                continue;
            }
        }
        let exe = temp.path.join(format!("test{i}"));
        match build(file, &exe) {
            Ok(tests) => {
                let module = module_name(file);
                jobs.extend(tests.into_iter().map(|test| Job {
                    name: format!("{module}.{test}"),
                    test,
                    exe: exe.clone(),
                }));
            }
            Err(Failure::Program(source, diagnostics)) => {
                out.diagnostics(&source, &diagnostics);
                code = code.max(1);
            }
            Err(Failure::Internal(source, report)) => {
                out.report(&source, &report);
                code = 2;
            }
        }
    }
    jobs.retain(|j| request.filter.is_none_or(|f| j.name.contains(f)));
    let outcomes = run_all(&jobs, request.timeout);
    let (mut passed, mut failed) = (0, 0);
    for (job, outcome) in jobs.iter().zip(&outcomes) {
        match outcome {
            Outcome::Passed => {
                passed += 1;
                out.print(&format!("test {} ... ok\n", job.name));
            }
            Outcome::NotStarted(why) => {
                out.error(&format!("cannot run test `{}`: {why}", job.name));
                code = 2;
            }
            Outcome::TimedOut => {
                failed += 1;
                let secs = request.timeout.as_secs();
                out.print(&format!(
                    "test {} ... FAILED (timed out after {secs} s)\n",
                    job.name
                ));
            }
            Outcome::Failed {
                code: exit,
                stdout,
                stderr,
            } => {
                failed += 1;
                out.print(&failure(&job.name, *exit, stdout, stderr));
            }
        }
    }
    let total = passed + failed;
    let summary = match total {
        0 => "0 tests\n".to_string(),
        1 => format!("\n1 test: {passed} passed, {failed} failed\n"),
        n => format!("\n{n} tests: {passed} passed, {failed} failed\n"),
    };
    out.print(&summary);
    if failed > 0 {
        code = code.max(1);
    }
    code
}

/// Whether `text` holds `@test`, as tokens; a file that doesn't lex but
/// mentions it is built anyway, so its errors are reported.
fn has_tests(text: &str) -> bool {
    match lexer::lex(text) {
        Ok((tokens, _)) => tokens
            .windows(2)
            .any(|w| w[0].kind == TokenKind::At && w[1].kind == TokenKind::Ident("test".into())),
        Err(_) => text.contains("@test"),
    }
}

/// Checks `file` for tests and builds its test executable at `exe`; returns
/// its tests in declaration order.
fn build(file: &Path, exe: &Path) -> Result<Vec<String>, Failure> {
    let mode = Mode {
        offline: false,
        update: false,
    };
    let front = pipeline::front_tests(file, mode)?;
    let tests = front
        .checked
        .as_ref()
        .and_then(|c| c.tests.clone())
        .unwrap_or_default();
    if !tests.is_empty() {
        pipeline::build(&front, OptLevel::O0, exe)?;
    }
    Ok(tests)
}

/// A test's module as reported: the file's path without `.la`, folders
/// joined with `.` (`src/geo.la` → `src.geo`).
fn module_name(file: &Path) -> String {
    let shown = display(&file.with_extension(""));
    shown.replace(['/', '\\'], ".")
}

/// Runs every job, up to one per CPU at a time; the outcomes in job order.
fn run_all(jobs: &[Job], timeout: Duration) -> Vec<Outcome> {
    let workers = thread::available_parallelism()
        .map_or(1, |n| n.get())
        .min(jobs.len().max(1));
    let next = AtomicUsize::new(0);
    let mut outcomes: Vec<Option<Outcome>> = (0..jobs.len()).map(|_| None).collect();
    let results = std::sync::Mutex::new(&mut outcomes);
    thread::scope(|scope| {
        for _ in 0..workers {
            scope.spawn(|| {
                loop {
                    let i = next.fetch_add(1, Ordering::Relaxed);
                    let Some(job) = jobs.get(i) else {
                        break;
                    };
                    let outcome = run_one(job, timeout);
                    // A worker that panicked holding the lock is a bug; the
                    // results it left are still usable.
                    let mut slots = results.lock().unwrap_or_else(|e| e.into_inner());
                    slots[i] = Some(outcome);
                }
            });
        }
    });
    outcomes
        .into_iter()
        .map(|o| o.unwrap_or(Outcome::NotStarted("never ran".to_string())))
        .collect()
}

/// Runs one test as its own process, capturing what it prints.
fn run_one(job: &Job, timeout: Duration) -> Outcome {
    let child = Command::new(&job.exe)
        .env("LUGHA_TEST", &job.test)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn();
    let mut child = match child {
        Ok(child) => child,
        Err(error) => return Outcome::NotStarted(error.to_string()),
    };
    // Readers on their own threads, so a test printing a lot never blocks.
    let stdout = reader(child.stdout.take());
    let stderr = reader(child.stderr.take());
    let status = wait(&mut child, timeout);
    let stdout = stdout.join().unwrap_or_default();
    let stderr = stderr.join().unwrap_or_default();
    match status {
        None => Outcome::TimedOut,
        Some(Ok(status)) if status.success() => Outcome::Passed,
        Some(Ok(status)) => Outcome::Failed {
            code: status.code(),
            stdout,
            stderr,
        },
        Some(Err(error)) => Outcome::NotStarted(error.to_string()),
    }
}

fn reader(pipe: Option<impl Read + Send + 'static>) -> thread::JoinHandle<String> {
    thread::spawn(move || {
        let mut text = Vec::new();
        if let Some(mut pipe) = pipe {
            // A read error only loses output that is shown on failure anyway.
            let _ = pipe.read_to_end(&mut text);
        }
        String::from_utf8_lossy(&text).into_owned()
    })
}

/// Waits for `child` up to `timeout`; `None` when it was killed for it.
fn wait(child: &mut Child, timeout: Duration) -> Option<std::io::Result<std::process::ExitStatus>> {
    let deadline = Instant::now() + timeout;
    loop {
        match child.try_wait() {
            Ok(Some(status)) => return Some(Ok(status)),
            Ok(None) if Instant::now() >= deadline => {
                // Already gone or not ours to kill: either way it's over.
                let _ = child.kill();
                let _ = child.wait();
                return None;
            }
            Ok(None) => thread::sleep(Duration::from_millis(5)),
            Err(error) => return Some(Err(error)),
        }
    }
}

/// The report of a failed test: its panic (or exit code), then its output.
fn failure(name: &str, code: Option<i32>, stdout: &str, stderr: &str) -> String {
    let mut text = match code {
        Some(101) | None => format!("test {name} ... FAILED\n"),
        Some(code) => format!("test {name} ... FAILED (exit code {code})\n"),
    };
    for line in stderr.lines() {
        text.push_str(&format!("    {line}\n"));
    }
    if !stdout.is_empty() {
        text.push_str("    --- output ---\n");
        for line in stdout.lines() {
            text.push_str(&format!("    {line}\n"));
        }
    }
    text
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tests_are_found_by_tokens() {
        assert!(has_tests("@test fun t() {}"));
        assert!(!has_tests("// @test\nfun t() {}"));
        assert!(!has_tests("fun main() { println(\"@test\"); }"));
        // Doesn't lex, mentions it: built, so the error is reported.
        assert!(has_tests("@test fun t() { ` }"));
    }

    #[test]
    fn module_names_follow_the_path() {
        assert_eq!(module_name(Path::new("geo.la")), "geo");
        assert_eq!(module_name(Path::new("./src/geo.la")), "src.geo");
    }

    #[test]
    fn a_failure_shows_the_panic_then_the_output() {
        assert_eq!(
            failure("m.t", Some(101), "hi\n", "panic: no at m.la:1:1\n"),
            "test m.t ... FAILED\n    panic: no at m.la:1:1\n    --- output ---\n    hi\n"
        );
        assert_eq!(
            failure("m.t", Some(3), "", ""),
            "test m.t ... FAILED (exit code 3)\n"
        );
    }
}
