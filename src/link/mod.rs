//! Linking — turns object files into an executable with the system C
//! compiler, compiling the runtime in the same step (spec §9 stage 6,
//! DECISION-009).
//!
//! Runs `cc <objects> <tmp>/lugha_rt.c -lgc -lm -o <output>` directly, never
//! through a shell. The runtime source is embedded in lughac, so linking
//! works from any directory with no install layout. It is kept as three
//! files under `runtime/` and joined here into that one `lugha_rt.c`.
//!
//! v16 (milestone 87, PRP-117): the `[link]` tables of `lugha.toml` add C
//! files, compiled first (`c_files`), and folders and libraries after
//! `-lm` (`inputs`), some from `pkg-config` (`pkg_config`).

pub mod c_files;
pub mod inputs;
pub mod pkg_config;

use std::ffi::OsString;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

pub use self::inputs::{Plan, PlanError};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// The runtime's source files, in the order they are joined: the core first,
/// since it holds every `#include`, then the fragments that rely on it.
pub const RUNTIME_PARTS: [(&str, &str); 18] = [
    ("lugha_rt.c", include_str!("../../runtime/lugha_rt.c")),
    ("stack.c", include_str!("../../runtime/stack.c")),
    ("print.c", include_str!("../../runtime/print.c")),
    ("input.c", include_str!("../../runtime/input.c")),
    ("format.c", include_str!("../../runtime/format.c")),
    ("bytes.c", include_str!("../../runtime/bytes.c")),
    ("chars.c", include_str!("../../runtime/chars.c")),
    ("random.c", include_str!("../../runtime/random.c")),
    ("env.c", include_str!("../../runtime/env.c")),
    ("fs.c", include_str!("../../runtime/fs.c")),
    ("process.c", include_str!("../../runtime/process.c")),
    ("test.c", include_str!("../../runtime/test.c")),
    ("callbacks.c", include_str!("../../runtime/callbacks.c")),
    ("cstr.c", include_str!("../../runtime/cstr.c")),
    ("tasks.c", include_str!("../../runtime/tasks.c")),
    ("spawn.c", include_str!("../../runtime/spawn.c")),
    ("chans.c", include_str!("../../runtime/chans.c")),
    ("actors.c", include_str!("../../runtime/actors.c")),
];

/// Every runtime function codegen may call; `lugha_rt.c` defines them all.
pub const RUNTIME_SYMBOLS: [&str; 78] = [
    "lugha_rt_init",
    "lugha_rt_read_line",
    "lugha_rt_read_file",
    "lugha_rt_write_file",
    "lugha_rt_eprint",
    "lugha_rt_exit",
    "lugha_rt_str_slice",
    "lugha_rt_args",
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
    "lugha_rt_str_quote",
    "lugha_rt_fmt_int",
    "lugha_rt_fmt_float",
    "lugha_rt_pad",
    "lugha_rt_read_bytes",
    "lugha_rt_write_bytes",
    "lugha_rt_read_stdin_bytes",
    "lugha_rt_str_to_bytes",
    "lugha_rt_str_from_bytes",
    "lugha_rt_env",
    "lugha_rt_cwd",
    "lugha_rt_chdir",
    "lugha_rt_list_dir",
    "lugha_rt_make_dir",
    "lugha_rt_remove_file",
    "lugha_rt_remove_dir",
    "lugha_rt_stat_kind",
    "lugha_rt_spawn",
    "lugha_rt_show_enter",
    "lugha_rt_show_leave",
    "lugha_rt_print_char",
    "lugha_rt_to_string_char",
    "lugha_rt_char_quote",
    "lugha_rt_print_newline",
    "lugha_rt_to_string_i32",
    "lugha_rt_to_string_i64",
    "lugha_rt_to_string_u8",
    "lugha_rt_to_string_f64",
    "lugha_rt_to_string_u64",
    "lugha_rt_to_string_f32",
    "lugha_rt_to_string_bool",
    "lugha_rt_os_seed",
    "lugha_rt_test_is",
    "lugha_rt_test_unknown",
    "lugha_rt_test_skip",
    "lugha_rt_cb_pin",
    "lugha_rt_cb_release",
    "lugha_rt_str_from_c",
    "lugha_rt_task_spawn",
    "lugha_rt_task_join",
    "lugha_rt_task_yield",
    "lugha_rt_task_sleep",
    "lugha_rt_task_preempt",
    "lugha_rt_task_select",
    "lugha_rt_chan_new",
    "lugha_rt_chan_send",
    "lugha_rt_chan_recv",
    "lugha_rt_chan_try_recv",
    "lugha_rt_chan_close",
    "lugha_rt_actor_spawn",
    "lugha_rt_actor_call",
    "lugha_rt_actor_cast",
    "lugha_rt_actor_stop",
    "lugha_rt_actor_join",
];

/// The C runtime linked into every program: [`RUNTIME_PARTS`] joined in order,
/// compiled as one `lugha_rt.c`.
pub const RUNTIME_SOURCE: &str = concat!(
    include_str!("../../runtime/lugha_rt.c"),
    include_str!("../../runtime/stack.c"),
    include_str!("../../runtime/print.c"),
    include_str!("../../runtime/input.c"),
    include_str!("../../runtime/format.c"),
    include_str!("../../runtime/bytes.c"),
    include_str!("../../runtime/chars.c"),
    include_str!("../../runtime/random.c"),
    include_str!("../../runtime/env.c"),
    include_str!("../../runtime/fs.c"),
    include_str!("../../runtime/process.c"),
    include_str!("../../runtime/test.c"),
    include_str!("../../runtime/callbacks.c"),
    include_str!("../../runtime/cstr.c"),
    include_str!("../../runtime/tasks.c"),
    include_str!("../../runtime/spawn.c"),
    include_str!("../../runtime/chans.c"),
    include_str!("../../runtime/actors.c"),
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
            LinkError::Libraries { span, stderr } => Some(
                Diagnostic::error("E0331", "linking failed", *span).with_help(format!(
                    "the linker said:\n{}\ninstall the missing library, or add the folder \
                     holding it to `paths`",
                    stderr.trim_end()
                )),
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

/// Links `objects` and the runtime into the executable `output`.
///
/// # Errors
///
/// [`LinkError::CcNotFound`] if there is no `cc`, [`LinkError::Io`] if it
/// can't be started or the runtime can't be written to the temp dir,
/// [`LinkError::Failed`] with `cc`'s stderr if it fails.
pub fn link(objects: &[&Path], output: &Path) -> Result<(), LinkError> {
    link_with(objects, &Plan::default(), Path::new(""), output)
}

/// Links `objects`, the C files of `plan` compiled into `temp`, and the
/// runtime into `output`, with `plan`'s folders and libraries (v16).
///
/// # Errors
///
/// As [`link`], and [`LinkError::CFile`] for a C file that doesn't compile.
/// When `plan` isn't empty, a failing link is [`LinkError::Libraries`]
/// rather than [`LinkError::Failed`]: it is most likely the program's.
pub fn link_with(
    objects: &[&Path],
    plan: &Plan,
    temp: &Path,
    output: &Path,
) -> Result<(), LinkError> {
    let shims = c_files::compile(&plan.c_files, temp)?;
    let runtime = RuntimeFile::write().map_err(LinkError::Io)?;
    let result = Command::new("cc")
        .args(args(objects, &shims, &runtime.0, plan, output))
        .output();
    let out = match result {
        Ok(out) => out,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Err(LinkError::CcNotFound),
        Err(e) => return Err(LinkError::Io(e)),
    };
    if out.status.success() {
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    match plan.at {
        Some(span) => Err(LinkError::Libraries { span, stderr }),
        None => Err(LinkError::Failed {
            status: out.status.code(),
            stderr,
        }),
    }
}

/// `cc`'s arguments: objects, C objects, the runtime, `-lgc -lm`, the
/// plan's folders and libraries, the output. Libraries go after every
/// object, as a static link needs.
fn args(
    objects: &[&Path],
    shims: &[PathBuf],
    runtime: &Path,
    plan: &Plan,
    output: &Path,
) -> Vec<OsString> {
    let mut args: Vec<OsString> = objects.iter().map(|o| o.as_os_str().to_owned()).collect();
    args.extend(shims.iter().map(|o| o.as_os_str().to_owned()));
    args.push(runtime.as_os_str().to_owned());
    args.extend(["-lgc", "-lm"].map(OsString::from));
    args.extend(plan.link_args());
    args.push(OsString::from("-o"));
    args.push(output.as_os_str().to_owned());
    args
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

    fn strings(args: &[std::ffi::OsString]) -> Vec<String> {
        args.iter()
            .map(|a| a.to_string_lossy().into_owned())
            .collect()
    }

    #[test]
    fn a_build_without_link_tables_links_as_before_v16() {
        use std::path::Path;
        let plan = super::Plan::default();
        let args = super::args(
            &[Path::new("prog.o")],
            &[],
            Path::new("/t/rt.c"),
            &plan,
            Path::new("out"),
        );
        assert_eq!(
            strings(&args),
            ["prog.o", "/t/rt.c", "-lgc", "-lm", "-o", "out"]
        );
    }

    #[test]
    fn a_plan_adds_c_objects_and_libraries_v16() {
        use crate::modules::link_table::{LinkKey, LinkLine, LinkTable, PackageLink};
        use crate::span::Span;
        use std::path::{Path, PathBuf};
        let line = LinkLine {
            key: LinkKey::Libs,
            values: vec!["z".into()],
            span: Span::new(0, 1),
        };
        let link = PackageLink {
            package: None,
            folder: PathBuf::from("."),
            table: LinkTable { lines: vec![line] },
        };
        let plan = super::Plan::new(&[link], &mut |_, _| Ok(Vec::new())).unwrap();
        let objects = [Path::new("prog.o")];
        let shims = [PathBuf::from("/t/c0.o")];
        let args = super::args(
            &objects,
            &shims,
            Path::new("/t/rt.c"),
            &plan,
            Path::new("out"),
        );
        assert_eq!(
            strings(&args),
            [
                "prog.o", "/t/c0.o", "/t/rt.c", "-lgc", "-lm", "-lz", "-o", "out"
            ]
        );
    }

    #[test]
    fn link_errors_the_user_can_fix_are_diagnostics_v16() {
        use super::LinkError;
        use crate::span::Span;
        let at = Span::new(3, 9);
        let c = LinkError::CFile {
            span: at,
            file: "shim.c".into(),
            stderr: "shim.c:1:1: error: expected ';'".into(),
        };
        let d = c.diagnostic().unwrap();
        assert_eq!((d.code, d.span), ("E0330", at));
        assert!(d.help.unwrap().contains("expected ';'"));
        let libs = LinkError::Libraries {
            span: at,
            stderr: "/usr/bin/ld: cannot find -lnosuch".into(),
        };
        let d = libs.diagnostic().unwrap();
        assert_eq!((d.code, d.span), ("E0331", at));
        assert!(d.help.unwrap().contains("cannot find -lnosuch"));
        let ours = LinkError::Failed {
            status: Some(1),
            stderr: String::new(),
        };
        assert!(ours.diagnostic().is_none());
        assert!(LinkError::CcNotFound.diagnostic().is_none());
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

    #[test]
    fn the_runtime_no_longer_parses_or_checks_utf8_v15() {
        // Written in Lugha in the prelude since milestone 62.
        for name in [
            "lugha_rt_parse_i64",
            "lugha_rt_parse_u64",
            "lugha_rt_utf8_check",
        ] {
            assert!(
                !RUNTIME_SOURCE.contains(name),
                "{name} is still in the runtime"
            );
            assert!(
                !super::RUNTIME_SYMBOLS.contains(&name),
                "{name} is still listed"
            );
        }
    }
}
