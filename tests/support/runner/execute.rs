//! Runs `lughac` (or any command) with a timeout and captures its output.

use std::io::Read;
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

use super::{Case, Mode, Outcome};

/// Runs `lughac run` (run mode) or `lughac check` (reject mode) on one case.
///
/// The program path is passed relative to `workdir`, so diagnostics show
/// `tests/programs/...` rather than an absolute path.
pub fn run_case(lughac: &Path, workdir: &Path, case: &Case, timeout: Duration) -> Outcome {
    let subcommand = match case.mode {
        Mode::Run { .. } => "run",
        Mode::Reject { .. } => "check",
    };
    let arg = case.path.strip_prefix(workdir).unwrap_or(&case.path);
    let mut cmd = Command::new(lughac);
    cmd.arg(subcommand).arg(arg).current_dir(workdir);
    run_command(cmd, timeout)
}

/// Runs `cmd` with null stdin, capturing stdout and stderr, killing it after `timeout`.
pub fn run_command(mut cmd: Command, timeout: Duration) -> Outcome {
    // Own process group, so a timeout also kills what it spawned (`lughac run`
    // starts the compiled program; a surviving child would hold the pipes open).
    cmd.stdin(Stdio::null())
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
}
