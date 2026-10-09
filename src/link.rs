//! Linking — turns object files into an executable with the system C
//! compiler, compiling the runtime in the same step (spec §9 stage 6,
//! DECISION-009).
//!
//! Runs `cc <objects> <tmp>/lugha_rt.c -lgc -lm -o <output>` directly, never
//! through a shell. The runtime source is embedded in lughac, so linking
//! works from any directory with no install layout.

use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

/// The C runtime linked into every program (`runtime/lugha_rt.c`).
pub const RUNTIME_SOURCE: &str = include_str!("../runtime/lugha_rt.c");

/// Why linking failed.
#[derive(Debug, thiserror::Error)]
pub enum LinkError {
    /// No `cc` on `PATH`.
    #[error("C compiler `cc` not found; install gcc or clang")]
    CcNotFound,
    /// `cc` could not be started, or the runtime source could not be written.
    #[error("could not run `cc`: {0}")]
    Io(std::io::Error),
    /// `cc` ran and reported an error.
    #[error("`cc` failed ({}):\n{stderr}", exit_text(*.status))]
    Failed { status: Option<i32>, stderr: String },
}

fn exit_text(status: Option<i32>) -> String {
    status.map_or_else(
        || "killed by a signal".to_string(),
        |code| format!("exit code {code}"),
    )
}

/// Links `objects` and the runtime into the executable `output`.
///
/// # Errors
///
/// [`LinkError::CcNotFound`] if there is no `cc`, [`LinkError::Io`] if it
/// can't be started or the runtime can't be written to the temp dir,
/// [`LinkError::Failed`] with `cc`'s stderr if it fails.
pub fn link(objects: &[&Path], output: &Path) -> Result<(), LinkError> {
    let runtime = RuntimeFile::write().map_err(LinkError::Io)?;
    let result = Command::new("cc")
        .args(objects)
        .arg(&runtime.0)
        .args(["-lgc", "-lm", "-o"])
        .arg(output)
        .output();
    let out = match result {
        Ok(out) => out,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Err(LinkError::CcNotFound),
        Err(e) => return Err(LinkError::Io(e)),
    };
    if out.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
        Err(LinkError::Failed {
            status: out.status.code(),
            stderr,
        })
    }
}

/// The runtime source in a private temp file, removed on drop.
struct RuntimeFile(PathBuf);

impl RuntimeFile {
    fn write() -> io::Result<Self> {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!("lughac-rt-{}-{n}.c", std::process::id()));
        // create_new: fail rather than reuse a file someone planted under that name.
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)?;
        io::Write::write_all(&mut file, RUNTIME_SOURCE.as_bytes())?;
        Ok(RuntimeFile(path))
    }
}

impl Drop for RuntimeFile {
    fn drop(&mut self) {
        // Best effort: a leftover temp file is harmless and must not hide the result.
        let _ = std::fs::remove_file(&self.0);
    }
}

#[cfg(test)]
mod tests {
    use super::RUNTIME_SOURCE;

    #[test]
    fn runtime_defines_every_symbol_codegen_calls() {
        for name in crate::codegen::RUNTIME_SYMBOLS {
            assert!(
                RUNTIME_SOURCE.contains(&format!("{name}(")),
                "lugha_rt.c lacks {name}"
            );
        }
    }
}
