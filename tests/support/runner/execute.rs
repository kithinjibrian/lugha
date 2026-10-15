//! Runs `lughac` (or any command) with a timeout and captures its output.

use std::io::Read;
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

use super::{Case, Mode, Outcome};
use crate::support::fixture::Fixture;

/// Runs `lughac run` (run mode) or `lughac check` (reject mode) on one case.
///
/// The program path is passed relative to `workdir`, so diagnostics show
/// `tests/programs/...` rather than an absolute path.
pub fn run_case(lughac: &Path, workdir: &Path, case: &Case, timeout: Duration) -> Outcome {
    let (cmd, _scratch) = case_command(lughac, workdir, case);
    run_with_stdin(cmd, timeout, case.extras.stdin.as_deref())
}

/// The command for `case`, and the scratch directory it runs in, if any
/// (PRP-044), which is removed when dropped: keep it until the run ends.
fn case_command(lughac: &Path, workdir: &Path, case: &Case) -> (Command, Option<Fixture>) {
    let subcommand = match case.mode {
        Mode::Run { .. } => "run",
        Mode::Reject { .. } => "check",
    };
    let mut cmd = Command::new(lughac);
    let scratch = case.extras.scratch.then(Fixture::new);
    match &scratch {
        Some(dir) => cmd.arg(subcommand).arg(&case.path).current_dir(dir.path()),
        None => {
            let arg = case.path.strip_prefix(workdir).unwrap_or(&case.path);
            cmd.arg(subcommand).arg(arg).current_dir(workdir)
        }
    };
    cmd.envs(case.extras.envvars.iter().map(|(k, v)| (k, v)));
    if !case.extras.args.is_empty() {
        cmd.arg("--").args(&case.extras.args);
    }
    (cmd, scratch)
}

/// Runs `cmd` with an empty stdin, capturing stdout and stderr, killing it after `timeout`.
pub fn run_command(cmd: Command, timeout: Duration) -> Outcome {
    run_with_stdin(cmd, timeout, None)
}

/// `run_command`, with `stdin` (if any) fed to the process.
pub fn run_with_stdin(mut cmd: Command, timeout: Duration, stdin: Option<&[u8]>) -> Outcome {
    // Own process group, so a timeout also kills what it spawned (`lughac run`
    // starts the compiled program; a surviving child would hold the pipes open).
    let input = if stdin.is_some() {
        Stdio::piped()
    } else {
        Stdio::null()
    };
    cmd.stdin(input)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .process_group(0);
    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(e) => return Outcome::SpawnFailed(e.to_string()),
    };
    // Drain both pipes concurrently; a child blocked on a full pipe would never exit.
    let drain = |pipe: Option<Box<dyn Read + Send>>| {
        thread::spawn(move || {
            let mut buf = Vec::new();
            if let Some(mut pipe) = pipe {
                let _ = pipe.read_to_end(&mut buf);
            }
            buf
        })
    };
    if let (Some(bytes), Some(mut pipe)) = (stdin, child.stdin.take()) {
        let bytes = bytes.to_vec();
        // A thread, so a program that stops reading can't block the runner;
        // a write error only means it stopped reading early.
        thread::spawn(move || {
            let _ = std::io::Write::write_all(&mut pipe, &bytes);
        });
    }
    let out = drain(child.stdout.take().map(|p| Box::new(p) as _));
    let err = drain(child.stderr.take().map(|p| Box::new(p) as _));

    let deadline = Instant::now() + timeout;
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break Some(status),
            Ok(None) if Instant::now() >= deadline => break None,
            Ok(None) => thread::sleep(Duration::from_millis(10)),
            Err(e) => return Outcome::SpawnFailed(e.to_string()),
        }
    };
    let Some(status) = status else {
        // std can only signal the direct child; `kill` reaches the whole group.
        let group = format!("-{}", child.id());
        let _ = Command::new("kill").args(["-KILL", "--", &group]).status();
        let _ = child.kill();
        let _ = child.wait();
        return Outcome::TimedOut(timeout);
    };
    let stdout = out.join().unwrap_or_default();
    let stderr = err.join().unwrap_or_default();
    Outcome::Exited {
        stdout,
        stderr,
        code: status.code(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::support::fixture::exited;
    use crate::support::runner::DEFAULT_TIMEOUT;

    #[test]
    fn slow_process_is_killed_at_the_timeout() {
        let mut cmd = Command::new("sh");
        cmd.args(["-c", "sleep 30"]);
        let started = Instant::now();
        let outcome = run_command(cmd, Duration::from_millis(200));
        assert_eq!(outcome, Outcome::TimedOut(Duration::from_millis(200)));
        assert!(
            started.elapsed() < Duration::from_secs(5),
            "child was not killed"
        );
    }

    #[test]
    fn process_output_and_exit_code_are_captured() {
        let mut cmd = Command::new("sh");
        cmd.args(["-c", "printf out; printf err >&2; exit 3"]);
        assert_eq!(run_command(cmd, DEFAULT_TIMEOUT), exited("out", "err", 3));
    }

    #[test]
    fn stdin_is_fed_to_the_process() {
        let cmd = Command::new("cat");
        let outcome = run_with_stdin(cmd, DEFAULT_TIMEOUT, Some(b"5\n7\n"));
        assert_eq!(outcome, exited("5\n7\n", "", 0));
    }

    #[test]
    fn a_scratch_case_runs_in_a_directory_that_is_removed() {
        use crate::support::runner::{Case, Mode, args::Extras};
        let case = Case {
            name: "p.la".into(),
            path: "/nonexistent/p.la".into(),
            mode: Mode::Run {
                stdout: Vec::new(),
                stderr: None,
                exit: 0,
            },
            extras: Extras {
                scratch: true,
                ..Extras::default()
            },
        };
        let (cmd, scratch) = case_command(Path::new("lughac"), Path::new("."), &case);
        let dir = cmd
            .get_current_dir()
            .expect("a scratch case sets its directory")
            .to_path_buf();
        assert!(dir.starts_with(std::env::temp_dir()), "{}", dir.display());
        assert!(dir.exists());
        drop(scratch);
        assert!(!dir.exists(), "the scratch directory is removed");
    }
}
