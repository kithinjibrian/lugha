//! `include_bytes(path)` and `include_str(path)` (v17, milestone 128,
//! PRP-160): a file's contents built into the program. The checker
//! resolves and reads each file once, inside its package's folder (E0447),
//! checks `include_str`'s text (E0448), and hands the bytes to codegen in
//! `Checked::includes`, so codegen never reads a file itself.

use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use super::expr::Expect;
use super::partial::{Name, Target};
use super::{Checker, Type, errors};
use crate::ast::{Expr, ExprKind};
use crate::modules::paths::normalized;
use crate::span::Span;

/// The intrinsics' names, reserved like every intrinsic's (E0302).
pub(super) const INCLUDES: [&str; 2] = ["include_bytes", "include_str"];

/// The largest file that can be included: 16 MiB.
pub const MAX_INCLUDE: u64 = 16 * 1024 * 1024;

/// A file's contents built into the program, by the call that names it.
#[derive(Clone, PartialEq)]
pub struct Included {
    /// The file's bytes; valid UTF-8 when `text`.
    pub bytes: Arc<[u8]>,
    /// `include_str`: a `string`, else a `u8[]`.
    pub text: bool,
}

// The bytes can be megabytes: a debug print shows their length only.
impl fmt::Debug for Included {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Included")
            .field("len", &self.bytes.len())
            .field("text", &self.text)
            .finish()
    }
}

/// Why a path can't be included; each is E0447 with this text.
#[derive(Debug)]
enum Refusal {
    Bundled,
    Empty,
    Absolute,
    Backslash,
    Outside,
    Missing,
    NotFile,
    TooBig(u64),
    Unreadable(io::Error),
}

impl fmt::Display for Refusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Refusal::Bundled => write!(f, "only a package's own files can be included"),
            Refusal::Empty => write!(f, "the path is empty"),
            Refusal::Absolute => write!(f, "the path is absolute; write it relative to this file"),
            Refusal::Backslash => write!(f, "paths separate folders with `/`, not `\\`"),
            Refusal::Outside => write!(f, "it leads outside the package's folder"),
            Refusal::Missing => write!(f, "no such file"),
            Refusal::NotFile => write!(f, "it is not a file"),
            Refusal::TooBig(n) => write!(f, "it is {n} bytes, over the {MAX_INCLUDE}-byte limit"),
            Refusal::Unreadable(e) => write!(f, "{e}"),
        }
    }
}

/// The real path of `path`, written in `file`, inside `folder`, and the
/// path from `folder` to it, with `/`.
fn locate(path: &str, file: &Path, folder: &Path) -> Result<(PathBuf, String), Refusal> {
    if path.is_empty() {
        return Err(Refusal::Empty);
    }
    if path.contains('\\') || path.contains('\0') {
        return Err(Refusal::Backslash);
    }
    if Path::new(path).has_root() {
        return Err(Refusal::Absolute);
    }
    // A file in the current folder has an empty parent: `.`.
    let real_folder = |f: &Path| {
        let f = if f.as_os_str().is_empty() {
            Path::new(".")
        } else {
            f
        };
        fs::canonicalize(f).map_err(Refusal::Unreadable)
    };
    let folder = real_folder(folder)?;
    let dir = real_folder(file.parent().unwrap_or(Path::new("")))?;
    // Lexically first, so `../x` is refused whether or not `x` exists.
    let joined = normalized(&dir.join(path));
    if !joined.starts_with(&folder) {
        return Err(Refusal::Outside);
    }
    let real = fs::canonicalize(&joined).map_err(|e| match e.kind() {
        io::ErrorKind::NotFound => Refusal::Missing,
        _ => Refusal::Unreadable(e),
    })?;
    // Then with links resolved: a link inside may point out.
    let Ok(inside) = real.strip_prefix(&folder) else {
        return Err(Refusal::Outside);
    };
    let shown = inside
        .components()
        .map(|c| c.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/");
    Ok((real, shown))
}

/// The contents of `real`, a path [`locate`] returned.
fn read(real: &Path) -> Result<Vec<u8>, Refusal> {
    // Before opening: opening a FIFO would wait for a writer.
    if !fs::metadata(real).map_err(Refusal::Unreadable)?.is_file() {
        return Err(Refusal::NotFile);
    }
    let file = fs::File::open(real).map_err(Refusal::Unreadable)?;
    let meta = file.metadata().map_err(Refusal::Unreadable)?;
    if !meta.is_file() {
        return Err(Refusal::NotFile);
    }
    if meta.len() > MAX_INCLUDE {
        return Err(Refusal::TooBig(meta.len()));
    }
    // A link swapped in since `locate` would resolve elsewhere now.
    if fs::canonicalize(real).map_err(Refusal::Unreadable)? != real {
        return Err(Refusal::Outside);
    }
    let mut bytes = Vec::with_capacity(meta.len() as usize);
    // The file may grow while being read: never past the limit.
    file.take(MAX_INCLUDE + 1)
        .read_to_end(&mut bytes)
        .map_err(Refusal::Unreadable)?;
    if bytes.len() as u64 > MAX_INCLUDE {
        return Err(Refusal::TooBig(bytes.len() as u64));
    }
    Ok(bytes)
}

impl super::partial::Recorder {
    /// An include's path at `at`, naming `file` of `size` bytes, `shown`
    /// from its package's folder: hover and go-to-definition read it.
    pub(super) fn include(&mut self, at: Span, file: PathBuf, shown: String, size: usize) {
        self.definitions.insert(at, Target::File(file.clone()));
        self.names.insert(at, Name::Included { file, shown, size });
    }
}

/// The files read so far in one check, by real path, so each is read once.
pub(super) type Reads = HashMap<PathBuf, Arc<[u8]>>;

impl Checker {
    /// `include_bytes(path)` or `include_str(path)`: a `u8[]` or a `string`.
    pub(super) fn include(&mut self, call: &Expr, name: &str, args: &[Expr]) -> Type {
        let text = name == "include_str";
        let ty = if text {
            Type::String
        } else {
            Type::Array(Box::new(Type::U8))
        };
        let [arg] = args else {
            self.unchecked_args(args);
            self.report(errors::include_literal(name, call.span));
            return ty;
        };
        self.expect_type(arg, &Expect::of(Type::String));
        let ExprKind::Str(path) = &arg.kind else {
            self.report(errors::include_literal(name, arg.span));
            return ty;
        };
        let bytes = match self.included_file(path) {
            Ok((real, shown, bytes)) => {
                self.hover.include(arg.span, real, shown, bytes.len());
                bytes
            }
            Err(why) => {
                self.report(errors::cant_include(path, &why.to_string(), arg.span));
                return ty;
            }
        };
        if text && let Err(e) = std::str::from_utf8(&bytes) {
            let at = e.valid_up_to();
            self.report(errors::include_not_utf8(path, at, bytes[at], arg.span));
            return ty;
        }
        self.includes.insert(call.id, Included { bytes, text });
        ty
    }

    /// The real path and bytes of `path` in the current module.
    fn included_file(&mut self, path: &str) -> Result<(PathBuf, String, Arc<[u8]>), Refusal> {
        let (file, folder) = self.origin.as_ref().ok_or(Refusal::Bundled)?;
        let (real, shown) = locate(path, file, folder)?;
        if let Some(bytes) = self.include_reads.get(&real) {
            return Ok((real, shown, bytes.clone()));
        }
        let bytes: Arc<[u8]> = read(&real)?.into();
        self.include_reads.insert(real.clone(), bytes.clone());
        Ok((real, shown, bytes))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A folder `pkg/` with `a.txt` and `sub/b.txt`, removed on drop.
    struct Pkg(PathBuf);

    impl Pkg {
        fn new(tag: &str) -> Pkg {
            let root = std::env::temp_dir().join(format!("lugha-inc-{tag}-{}", std::process::id()));
            // A leftover from an earlier run would change what the test sees.
            let _ = fs::remove_dir_all(&root);
            fs::create_dir_all(root.join("pkg/sub")).unwrap();
            fs::write(root.join("pkg/a.txt"), "a").unwrap();
            fs::write(root.join("pkg/sub/b.txt"), "b").unwrap();
            fs::write(root.join("out.txt"), "out").unwrap();
            Pkg(root)
        }

        fn at(&self, path: &str, from: &str) -> Result<PathBuf, Refusal> {
            let pkg = self.0.join("pkg");
            locate(path, &pkg.join(from), &pkg).map(|(real, _)| real)
        }
    }

    impl Drop for Pkg {
        fn drop(&mut self) {
            // Best effort: a leftover temp dir must not mask the real failure.
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    #[test]
    fn paths_are_relative_to_the_naming_file() {
        let p = Pkg::new("rel");
        let b = p.at("b.txt", "sub/m.la").unwrap();
        assert_eq!(read(&b).unwrap(), b"b");
        let a = p.at("../a.txt", "sub/m.la").unwrap();
        assert_eq!(read(&a).unwrap(), b"a");
        assert!(p.at("./sub/../a.txt", "main.la").is_ok());
        let pkg = p.0.join("pkg");
        let (_, shown) = locate("b.txt", &pkg.join("sub/m.la"), &pkg).unwrap();
        assert_eq!(shown, "sub/b.txt");
    }

    #[test]
    fn written_paths_are_checked_before_any_lookup() {
        let p = Pkg::new("written");
        assert!(matches!(p.at("", "main.la"), Err(Refusal::Empty)));
        assert!(matches!(
            p.at("/etc/passwd", "main.la"),
            Err(Refusal::Absolute)
        ));
        assert!(matches!(
            p.at("sub\\b.txt", "main.la"),
            Err(Refusal::Backslash)
        ));
        assert!(matches!(
            p.at("a\0.txt", "main.la"),
            Err(Refusal::Backslash)
        ));
    }

    #[test]
    fn nothing_outside_the_folder_is_found() {
        let p = Pkg::new("outside");
        assert!(matches!(
            p.at("../out.txt", "main.la"),
            Err(Refusal::Outside)
        ));
        // Outside even when missing: the path alone says so.
        assert!(matches!(
            p.at("../nope.txt", "main.la"),
            Err(Refusal::Outside)
        ));
        assert!(matches!(p.at("nope.txt", "main.la"), Err(Refusal::Missing)));
    }

    #[test]
    fn only_regular_files_are_read() {
        let p = Pkg::new("dir");
        let sub = p.at("sub", "main.la").unwrap();
        assert!(matches!(read(&sub), Err(Refusal::NotFile)));
    }

    #[test]
    fn refusals_read_as_reasons() {
        assert_eq!(Refusal::Missing.to_string(), "no such file");
        assert_eq!(
            Refusal::TooBig(16777217).to_string(),
            "it is 16777217 bytes, over the 16777216-byte limit"
        );
    }
}
