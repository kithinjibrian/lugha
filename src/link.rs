//! Linking — turns object files into an executable with the system C
//! compiler (spec §9 stage 6).
//!
//! Runs `cc <objects> -lgc -lm -o <output>` directly, never through a shell.
//! `lugha_rt.o` joins the link in milestone 4.

use std::io;
use std::path::Path;
use std::process::Command;

/// Why linking failed.
#[derive(Debug, thiserror::Error)]
pub enum LinkError {
    /// No `cc` on `PATH`.
    #[error("C compiler `cc` not found; install gcc or clang")]
    CcNotFound,
    /// `cc` could not be started for another reason.
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

/// Links `objects` into the executable `output`.
///
/// # Errors
///
/// [`LinkError::CcNotFound`] if there is no `cc`, [`LinkError::Io`] if it
/// can't be started, [`LinkError::Failed`] with `cc`'s stderr if it fails.
pub fn link(objects: &[&Path], output: &Path) -> Result<(), LinkError> {
    let result = Command::new("cc")
        .args(objects)
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
