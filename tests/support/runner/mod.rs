//! Acceptance-test runner for `tests/programs/` (PRP-001).
//!
//! Finds `.la` programs, loads their expectation files, runs `lughac` on each
//! and compares the outcome byte-for-byte. Knows nothing about the Lugha
//! language itself — only files, processes and exit codes.
//!
//! `discover` reads the folder, `execute` runs processes, `report` compares
//! and formats.

mod discover;
mod execute;
mod report;

use std::path::PathBuf;
use std::time::Duration;

pub use discover::discover;
pub use execute::run_case;
pub use report::{compare, format_malformed, format_report};

/// How long one program may run before it is killed.
pub const DEFAULT_TIMEOUT: Duration = Duration::from_secs(10);

/// What a program is expected to do.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mode {
    /// `lughac run`: compiles, runs, and produces this output and exit code.
    /// `stderr` is compared only when a `.stderr` file exists (panic tests).
    Run {
        stdout: Vec<u8>,
        stderr: Option<Vec<u8>>,
        exit: i32,
    },
    /// `lughac check`: the compiler rejects the program with this stderr and exit code 1.
    Reject { stderr: Vec<u8> },
}

/// One `.la` program and its expectations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Case {
    /// Path relative to the discovery root, e.g. `m1/arith.la`. Used in reports.
    pub name: String,
    /// Full path to the `.la` file.
    pub path: PathBuf,
    /// What the program must do.
    pub mode: Mode,
}

/// A file layout under the root that can't be turned into a [`Case`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Malformed {
    /// The offending file, relative to the root.
    pub name: String,
    /// What is wrong with it.
    pub reason: String,
}

/// What happened when a program was run.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Outcome {
    /// The process finished. `code` is `None` if it was killed by a signal.
    Exited {
        stdout: Vec<u8>,
        stderr: Vec<u8>,
        code: Option<i32>,
    },
    /// The process ran past the timeout and was killed.
    TimedOut(Duration),
    /// The process could not be started.
    SpawnFailed(String),
}

/// One way an outcome differed from its expectation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mismatch {
    /// What differed: `exit code`, `stdout`, `stderr`, or a one-line problem.
    pub what: String,
    /// Expected and actual values, already formatted for display.
    pub values: Option<(String, String)>,
}

fn lossy(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}
