//! Throwaway files for the runner's unit tests.

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use super::runner::Outcome;

/// A temporary directory removed on drop, so fixtures vanish even when a test fails.
pub struct Fixture(PathBuf);

impl Fixture {
    /// Creates an empty, uniquely named directory under the system temp dir.
    pub fn new() -> Self {
        // Tests run as parallel threads of one process, so the pid alone isn't unique.
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-runner-{}-{n}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        Fixture(dir)
    }

    /// Writes `contents` to `rel` inside the fixture, creating parent folders.
    pub fn file(&self, rel: &str, contents: &str) -> &Self {
        let path = self.0.join(rel);
        std::fs::create_dir_all(path.parent().expect("fixture paths have a parent"))
            .expect("temp dir is writable");
        std::fs::write(path, contents).expect("temp dir is writable");
        self
    }

    /// The fixture's root directory.
    pub fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for Fixture {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir is harmless and must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// An [`Outcome::Exited`] with the given output and exit code.
pub fn exited(stdout: &str, stderr: &str, code: i32) -> Outcome {
    Outcome::Exited {
        stdout: stdout.into(),
        stderr: stderr.into(),
        code: Some(code),
    }
}
