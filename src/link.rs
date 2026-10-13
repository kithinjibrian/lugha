//! Linking — turns object files into an executable with the system C
//! compiler, compiling the runtime in the same step (spec §9 stage 6,
//! DECISION-009).
//!
//! Runs `cc <objects> <tmp>/lugha_rt.c -lgc -lm -o <output>` directly, never
//! through a shell. The runtime source is embedded in lughac, so linking
//! works from any directory with no install layout. It is kept as three
//! files under `runtime/` and joined here into that one `lugha_rt.c`.

use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

/// The runtime's source files, in the order they are joined: the core first,
/// since it holds every `#include`, then the fragments that rely on it.
pub const RUNTIME_PARTS: [(&str, &str); 4] = [
    ("lugha_rt.c", include_str!("../runtime/lugha_rt.c")),
    ("stack.c", include_str!("../runtime/stack.c")),
    ("print.c", include_str!("../runtime/print.c")),
    ("input.c", include_str!("../runtime/input.c")),
];

/// Every runtime function codegen may call; `lugha_rt.c` defines them all.
pub const RUNTIME_SYMBOLS: [&str; 33] = [
    "lugha_rt_init",
    "lugha_rt_read_line",
    "lugha_rt_read_file",
    "lugha_rt_write_file",
    "lugha_rt_eprint",
    "lugha_rt_exit",
    "lugha_rt_str_slice",
    "lugha_rt_args",
    "lugha_rt_parse_i64",
    "lugha_rt_parse_f64",
    "lugha_rt_alloc",
    "lugha_rt_panic",
    "lugha_rt_panic_bounds",
    "lugha_rt_str_concat",
    "lugha_rt_str_eq",
    "lugha_rt_str_cmp",
    "lugha_rt_array_grow",
    "lugha_rt_print_i32",
    "lugha_rt_print_i64",
    "lugha_rt_print_u8",
    "lugha_rt_print_f64",
    "lugha_rt_print_u64",
    "lugha_rt_print_f32",
    "lugha_rt_print_bool",
    "lugha_rt_print_str",
    "lugha_rt_print_newline",
    "lugha_rt_to_string_i32",
    "lugha_rt_to_string_i64",
    "lugha_rt_to_string_u8",
    "lugha_rt_to_string_f64",
    "lugha_rt_to_string_u64",
    "lugha_rt_to_string_f32",
    "lugha_rt_to_string_bool",
];

/// The C runtime linked into every program: [`RUNTIME_PARTS`] joined in order,
/// compiled as one `lugha_rt.c`.
pub const RUNTIME_SOURCE: &str = concat!(
    include_str!("../runtime/lugha_rt.c"),
    include_str!("../runtime/stack.c"),
    include_str!("../runtime/print.c"),
    include_str!("../runtime/input.c"),
);

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
    use super::{RUNTIME_PARTS, RUNTIME_SOURCE};

    #[test]
    fn runtime_parts_join_in_order_core_first() {
        let joined: String = RUNTIME_PARTS.iter().map(|(_, src)| *src).collect();
        assert_eq!(joined, RUNTIME_SOURCE);
        let (name, core) = RUNTIME_PARTS[0];
        assert_eq!(name, "lugha_rt.c");
        let feature = core
            .find("#define _GNU_SOURCE")
            .expect("the core enables GNU extensions");
        let first_include = core.find("#include").expect("the core has the includes");
        assert!(
            feature < first_include,
            "_GNU_SOURCE must precede every #include"
        );
    }

    #[test]
    fn fragments_rely_on_the_core_and_stay_small() {
        for (i, (name, src)) in RUNTIME_PARTS.iter().enumerate() {
            assert!(!src.trim().is_empty(), "{name} is empty");
            assert!(src.lines().count() <= 300, "{name} is over 300 lines");
            let includes = src.lines().any(|l| l.starts_with("#include"));
            assert_eq!(includes, i == 0, "{name}: only the core has #include lines");
        }
    }

    #[test]
    fn runtime_defines_every_symbol_codegen_calls() {
        for name in super::RUNTIME_SYMBOLS {
            assert!(
                RUNTIME_SOURCE.contains(&format!("{name}(")),
                "lugha_rt.c lacks {name}"
            );
        }
    }
}
