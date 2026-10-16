//! Running the system `pkg-config` for a `[link]` table's `pkg-config`
//! names (v16, milestone 87, PRP-117), and keeping only the flags lughac
//! passes on: `-l`, `-L` and `-pthread` from `--libs`; `-I`, `-D`,
//! `-pthread` and [`X86_64_DEFAULTS`] from `--cflags`. Anything else is
//! refused, never dropped, so a package can't slip linker or compiler
//! options into a build.
//!
//! The program is `$PKG_CONFIG` when set (the usual convention, and how the
//! tests substitute a fake), else `pkg-config` on `PATH`; it runs from an
//! argument list, never a shell. The names are checked by the manifest
//! (`modules::link_table`): none starts with `-`, so none is an option.

use std::ffi::OsString;
use std::io;
use std::process::Command;

/// The `-m` flags `--cflags` may give, exactly (v17, PRP-129): GTK's on
/// x86-64. They select what x86-64 System V, Lugha's only target, already
/// has (SSE2 is its baseline, SSE its floating point), so they change
/// nothing; every other `-m` flag can change the ABI and stays refused.
pub const X86_64_DEFAULTS: [&str; 3] = ["-mfpmath=sse", "-msse", "-msse2"];

/// Which flags are asked for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Query {
    /// `pkg-config --libs`: for the link.
    Libs,
    /// `pkg-config --cflags`: for compiling a package's C files.
    Cflags,
}

/// One flag lughac passes on.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Flag {
    /// `-l<name>`.
    Lib(String),
    /// `-L<folder>`.
    Search(String),
    /// `-I<folder>`.
    Include(String),
    /// `-D<name>` or `-D<name>=<value>`.
    Define(String),
    /// `-pthread`.
    Pthread,
    /// One of [`X86_64_DEFAULTS`], from `--cflags` (v17).
    X86_64(&'static str),
}

impl Flag {
    /// The flag as one `cc` argument.
    pub fn arg(&self) -> String {
        match self {
            Flag::Lib(name) => format!("-l{name}"),
            Flag::Search(folder) => format!("-L{folder}"),
            Flag::Include(folder) => format!("-I{folder}"),
            Flag::Define(text) => format!("-D{text}"),
            Flag::Pthread => "-pthread".to_string(),
            Flag::X86_64(flag) => flag.to_string(),
        }
    }
}

/// The flags in `pkg-config`'s output `out` for `query`, or why one isn't
/// allowed.
pub fn flags(out: &str, query: Query) -> Result<Vec<Flag>, String> {
    out.split_whitespace()
        .map(|token| {
            let after = |prefix: &str| token.strip_prefix(prefix).filter(|rest| !rest.is_empty());
            let flag = match query {
                _ if token == "-pthread" => Some(Flag::Pthread),
                Query::Libs => after("-l")
                    .map(|n| Flag::Lib(n.into()))
                    .or_else(|| after("-L").map(|f| Flag::Search(f.into()))),
                Query::Cflags => after("-I")
                    .map(|f| Flag::Include(f.into()))
                    .or_else(|| after("-D").map(|d| Flag::Define(d.into())))
                    .or_else(|| {
                        let known = X86_64_DEFAULTS.iter().find(|f| **f == token);
                        known.map(|f| Flag::X86_64(f))
                    }),
            };
            flag.ok_or_else(|| {
                let allowed = match query {
                    Query::Libs => "`-l`, `-L` and `-pthread` from `--libs`",
                    Query::Cflags => {
                        "`-I`, `-D`, `-pthread`, `-mfpmath=sse`, `-msse` and `-msse2` from `--cflags`"
                    }
                };
                format!("pkg-config gave `{token}`, which lughac doesn't pass on; it keeps only {allowed}")
            })
        })
        .collect()
}

/// Runs `pkg-config` for `names` and returns the flags it gives for `query`.
///
/// # Errors
///
/// A message for the diagnostic (E0329) when `pkg-config` isn't installed,
/// can't be started, fails (its own message included), or gives a flag
/// [`flags`] refuses.
pub fn run(names: &[String], query: Query) -> Result<Vec<Flag>, String> {
    let program = std::env::var_os("PKG_CONFIG").unwrap_or_else(|| OsString::from("pkg-config"));
    let option = match query {
        Query::Libs => "--libs",
        Query::Cflags => "--cflags",
    };
    let out = match Command::new(&program).arg(option).args(names).output() {
        Ok(out) => out,
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            return Err(format!(
                "`{}` isn't installed: install pkg-config or pkgconf",
                program.to_string_lossy()
            ));
        }
        Err(e) => {
            return Err(format!(
                "could not run `{}`: {e}",
                program.to_string_lossy()
            ));
        }
    };
    if !out.status.success() {
        let said = String::from_utf8_lossy(&out.stderr);
        return Err(format!(
            "`pkg-config {option} {}` failed: {}",
            names.join(" "),
            said.trim()
        ));
    }
    flags(&String::from_utf8_lossy(&out.stdout), query)
}

#[cfg(test)]
mod tests {
    use super::{Flag, Query, flags};

    #[test]
    fn libs_keep_l_big_l_and_pthread() {
        let out = " -L/opt/x/lib -lgtk-4 -lpango-1.0 -pthread\n";
        assert_eq!(
            flags(out, Query::Libs).unwrap(),
            [
                Flag::Search("/opt/x/lib".into()),
                Flag::Lib("gtk-4".into()),
                Flag::Lib("pango-1.0".into()),
                Flag::Pthread,
            ]
        );
        assert_eq!(flags("\n", Query::Libs).unwrap(), []);
    }

    #[test]
    fn cflags_keep_i_d_and_pthread() {
        let out = "-I/usr/include/gtk-4.0 -DGTK_X=1 -pthread";
        assert_eq!(
            flags(out, Query::Cflags).unwrap(),
            [
                Flag::Include("/usr/include/gtk-4.0".into()),
                Flag::Define("GTK_X=1".into()),
                Flag::Pthread,
            ]
        );
    }

    #[test]
    fn cflags_keep_gtks_x86_64_defaults_v17() {
        let out = "-I/usr/include/gtk-4.0 -mfpmath=sse -msse -msse2 -pthread\n";
        assert_eq!(
            flags(out, Query::Cflags).unwrap(),
            [
                Flag::Include("/usr/include/gtk-4.0".into()),
                Flag::X86_64("-mfpmath=sse"),
                Flag::X86_64("-msse"),
                Flag::X86_64("-msse2"),
                Flag::Pthread,
            ]
        );
        assert_eq!(Flag::X86_64("-msse2").arg(), "-msse2");
    }

    #[test]
    fn anything_else_is_refused_by_name() {
        for (out, query, bad) in [
            ("-lx -Wl,-rpath,/x", Query::Libs, "-Wl,-rpath,/x"),
            ("-framework Cocoa", Query::Libs, "-framework"),
            ("/usr/lib/libx.a", Query::Libs, "/usr/lib/libx.a"),
            ("-I/x", Query::Libs, "-I/x"),
            ("-lx", Query::Cflags, "-lx"),
            ("-l", Query::Libs, "-l"),
            ("-m32", Query::Cflags, "-m32"),
            ("-march=native", Query::Cflags, "-march=native"),
            ("-mno-red-zone", Query::Cflags, "-mno-red-zone"),
            ("-msse3", Query::Cflags, "-msse3"),
            ("-msse2", Query::Libs, "-msse2"),
        ] {
            let err = flags(out, query).expect_err(out);
            assert!(err.contains(&format!("`{bad}`")), "{out}: {err}");
        }
    }

    #[test]
    fn flags_become_arguments() {
        assert_eq!(Flag::Lib("z".into()).arg(), "-lz");
        assert_eq!(Flag::Search("/a".into()).arg(), "-L/a");
        assert_eq!(Flag::Include("/i".into()).arg(), "-I/i");
        assert_eq!(Flag::Define("X=1".into()).arg(), "-DX=1");
        assert_eq!(Flag::Pthread.arg(), "-pthread");
    }
}
