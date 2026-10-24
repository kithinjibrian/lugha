//! Why linking failed (`LinkError`), and the diagnostics for the failures a
//! program's `[link]` tables cause (E0329–E0332). Split from `mod.rs` in
//! v17 (milestone 132, PRP-164).

use std::path::PathBuf;

use super::PlanError;
use crate::diagnostic::Diagnostic;
use crate::span::Span;

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
    /// A `[link]` C file doesn't compile (v16): E0330 at its `c` line.
    #[error("`{}` doesn't compile:\n{stderr}", file.display())]
    CFile {
        span: Span,
        file: PathBuf,
        stderr: String,
    },
    /// Linking failed with `[link]` input (v16): E0331 at its first line.
    #[error("linking failed:\n{stderr}")]
    Libraries { span: Span, stderr: String },
    /// A TLS or crypto program, and no OpenSSL to build it with (v17): E0332 at its import.
    #[error("OpenSSL is missing")]
    Tls { span: Span },
}

impl LinkError {
    /// The diagnostic for a failure the program's `[link]` tables caused
    /// (E0330, E0331); `None` for lughac's own, an internal error.
    pub fn diagnostic(&self) -> Option<Diagnostic> {
        match self {
            LinkError::CFile { span, file, stderr } => Some(
                Diagnostic::error(
                    "E0330",
                    format!("the C file `{}` doesn't compile", file.display()),
                    *span,
                )
                .with_help(format!("the C compiler said:\n{}", stderr.trim_end())),
            ),
            LinkError::Libraries { span, stderr } => {
                // The advice follows what the linker said, not a guess.
                let advice = if stderr.contains("multiple definition") {
                    "\na symbol is defined twice: keep it in one C file or library"
                } else if stderr.contains("cannot find -l") {
                    "\ninstall the missing library, or add the folder holding it to `paths`"
                } else if stderr.contains("undefined reference") {
                    "\nadd the library or C file that defines it to `libs` or `c`"
                } else {
                    ""
                };
                Some(
                    Diagnostic::error("E0331", "linking failed", *span)
                        .with_help(format!("the linker said:\n{}{advice}", stderr.trim_end())),
                )
            }
            LinkError::Tls { span } => Some(
                Diagnostic::error(
                    "E0332",
                    "this program needs OpenSSL, which isn't installed",
                    *span,
                )
                .with_help("install OpenSSL's development files: `sudo apt install libssl-dev`"),
            ),
            LinkError::CcNotFound | LinkError::Io(_) | LinkError::Failed { .. } => None,
        }
    }
}

impl PlanError {
    /// E0329 at the `pkg-config` line.
    pub fn diagnostic(&self) -> Diagnostic {
        Diagnostic::error("E0329", "pkg-config failed", self.span).with_help(self.message.clone())
    }
}

fn exit_text(status: Option<i32>) -> String {
    status.map_or_else(
        || "killed by a signal".to_string(),
        |code| format!("exit code {code}"),
    )
}
