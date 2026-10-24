//! The Boehm collector built into every program (v17, milestone 131,
//! PRP-163): bdwgc's source, vendored unmodified in `third_party/bdwgc/`,
//! is embedded here, written to `<cache root>/gc/<version>-<hash>/` and
//! compiled once through the object cache (`extra/gc.c`, the whole
//! collector as one file). Programs link that object instead of the
//! system's `-lgc`, so neither lughac nor its programs need `libgc`
//! installed, and a cross build needs only a C compiler for the target.

use std::ffi::OsString;
use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::LinkError;
use super::object_cache::{Cache, fnv1a};

/// The bdwgc release vendored in `third_party/bdwgc/` (see its `VERSION`).
pub const GC_VERSION: &str = "8.2.12";

/// Every file `extra/gc.c` reads, by its path in the release.
pub const GC_FILES: [(&str, &str); 50] = [
    (
        "allchblk.c",
        include_str!("../../third_party/bdwgc/allchblk.c"),
    ),
    ("alloc.c", include_str!("../../third_party/bdwgc/alloc.c")),
    (
        "backgraph.c",
        include_str!("../../third_party/bdwgc/backgraph.c"),
    ),
    (
        "blacklst.c",
        include_str!("../../third_party/bdwgc/blacklst.c"),
    ),
    (
        "checksums.c",
        include_str!("../../third_party/bdwgc/checksums.c"),
    ),
    (
        "darwin_stop_world.c",
        include_str!("../../third_party/bdwgc/darwin_stop_world.c"),
    ),
    (
        "dbg_mlc.c",
        include_str!("../../third_party/bdwgc/dbg_mlc.c"),
    ),
    (
        "dyn_load.c",
        include_str!("../../third_party/bdwgc/dyn_load.c"),
    ),
    (
        "extra/gc.c",
        include_str!("../../third_party/bdwgc/extra/gc.c"),
    ),
    (
        "finalize.c",
        include_str!("../../third_party/bdwgc/finalize.c"),
    ),
    (
        "fnlz_mlc.c",
        include_str!("../../third_party/bdwgc/fnlz_mlc.c"),
    ),
    (
        "gc_dlopen.c",
        include_str!("../../third_party/bdwgc/gc_dlopen.c"),
    ),
    (
        "gcj_mlc.c",
        include_str!("../../third_party/bdwgc/gcj_mlc.c"),
    ),
    (
        "headers.c",
        include_str!("../../third_party/bdwgc/headers.c"),
    ),
    (
        "include/gc_config_macros.h",
        include_str!("../../third_party/bdwgc/include/gc_config_macros.h"),
    ),
    (
        "include/gc.h",
        include_str!("../../third_party/bdwgc/include/gc.h"),
    ),
    (
        "include/gc_inline.h",
        include_str!("../../third_party/bdwgc/include/gc_inline.h"),
    ),
    (
        "include/gc_mark.h",
        include_str!("../../third_party/bdwgc/include/gc_mark.h"),
    ),
    (
        "include/gc_pthread_redirects.h",
        include_str!("../../third_party/bdwgc/include/gc_pthread_redirects.h"),
    ),
    (
        "include/gc_tiny_fl.h",
        include_str!("../../third_party/bdwgc/include/gc_tiny_fl.h"),
    ),
    (
        "include/gc_typed.h",
        include_str!("../../third_party/bdwgc/include/gc_typed.h"),
    ),
    (
        "include/gc_version.h",
        include_str!("../../third_party/bdwgc/include/gc_version.h"),
    ),
    (
        "include/javaxfc.h",
        include_str!("../../third_party/bdwgc/include/javaxfc.h"),
    ),
    (
        "include/private/dbg_mlc.h",
        include_str!("../../third_party/bdwgc/include/private/dbg_mlc.h"),
    ),
    (
        "include/private/gc_alloc_ptrs.h",
        include_str!("../../third_party/bdwgc/include/private/gc_alloc_ptrs.h"),
    ),
    (
        "include/private/gcconfig.h",
        include_str!("../../third_party/bdwgc/include/private/gcconfig.h"),
    ),
    (
        "include/private/gc_hdrs.h",
        include_str!("../../third_party/bdwgc/include/private/gc_hdrs.h"),
    ),
    (
        "include/private/gc_locks.h",
        include_str!("../../third_party/bdwgc/include/private/gc_locks.h"),
    ),
    (
        "include/private/gc_pmark.h",
        include_str!("../../third_party/bdwgc/include/private/gc_pmark.h"),
    ),
    (
        "include/private/gc_priv.h",
        include_str!("../../third_party/bdwgc/include/private/gc_priv.h"),
    ),
    (
        "include/private/pthread_support.h",
        include_str!("../../third_party/bdwgc/include/private/pthread_support.h"),
    ),
    (
        "include/private/thread_local_alloc.h",
        include_str!("../../third_party/bdwgc/include/private/thread_local_alloc.h"),
    ),
    (
        "mach_dep.c",
        include_str!("../../third_party/bdwgc/mach_dep.c"),
    ),
    ("malloc.c", include_str!("../../third_party/bdwgc/malloc.c")),
    (
        "mallocx.c",
        include_str!("../../third_party/bdwgc/mallocx.c"),
    ),
    ("mark.c", include_str!("../../third_party/bdwgc/mark.c")),
    (
        "mark_rts.c",
        include_str!("../../third_party/bdwgc/mark_rts.c"),
    ),
    ("misc.c", include_str!("../../third_party/bdwgc/misc.c")),
    (
        "new_hblk.c",
        include_str!("../../third_party/bdwgc/new_hblk.c"),
    ),
    (
        "obj_map.c",
        include_str!("../../third_party/bdwgc/obj_map.c"),
    ),
    ("os_dep.c", include_str!("../../third_party/bdwgc/os_dep.c")),
    (
        "pthread_start.c",
        include_str!("../../third_party/bdwgc/pthread_start.c"),
    ),
    (
        "pthread_stop_world.c",
        include_str!("../../third_party/bdwgc/pthread_stop_world.c"),
    ),
    (
        "pthread_support.c",
        include_str!("../../third_party/bdwgc/pthread_support.c"),
    ),
    (
        "ptr_chck.c",
        include_str!("../../third_party/bdwgc/ptr_chck.c"),
    ),
    (
        "reclaim.c",
        include_str!("../../third_party/bdwgc/reclaim.c"),
    ),
    (
        "specific.c",
        include_str!("../../third_party/bdwgc/specific.c"),
    ),
    (
        "thread_local_alloc.c",
        include_str!("../../third_party/bdwgc/thread_local_alloc.c"),
    ),
    (
        "typd_mlc.c",
        include_str!("../../third_party/bdwgc/typd_mlc.c"),
    ),
    (
        "win32_threads.c",
        include_str!("../../third_party/bdwgc/win32_threads.c"),
    ),
];

/// How the collector is compiled. Without OS threads: Lugha runs on one,
/// and green threads are fibers `tasks.c` shows the collector. Without
/// threads, `GC_clear_stack` would clear down to a high-water mark taken
/// on the main stack, and called on a green thread's 1 MiB stack it ran
/// off the end (`panic: stack overflow`); a small fixed clear avoids that.
pub const GC_FLAGS: [&str; 3] = ["-O2", "-fno-strict-aliasing", "-DALWAYS_SMALL_CLEAR_STACK"];

/// The compiled collector and where its headers are; temporary files go on drop.
pub struct Bundled {
    /// The object to link: the cache's, or one compiled for this build.
    pub object: PathBuf,
    /// The folder holding `gc.h` and `gc_mark.h`, for the runtime's `-isystem`.
    pub include: PathBuf,
    temp: Vec<PathBuf>,
    scratch: Option<PathBuf>,
}

impl Drop for Bundled {
    fn drop(&mut self) {
        // Best effort: leftover temp files are harmless and must not hide the result.
        for file in &self.temp {
            let _ = std::fs::remove_file(file);
        }
        if let Some(dir) = &self.scratch {
            let _ = std::fs::remove_dir_all(dir);
        }
    }
}

/// The collector, compiled with the user's object cache, for a harness
/// that links the runtime itself (`tests/tasks_runtime.rs`).
///
/// # Errors
///
/// As [`compile`].
pub fn bundled() -> Result<Bundled, LinkError> {
    compile(&Cache::open())
}

/// The collector's object from `cache` when it has a fresh one, else
/// compiled from the sources, which are first restored in the cache's
/// `gc/` folder (or a temporary one when that can't be written).
///
/// # Errors
///
/// [`LinkError::Io`] when no folder can hold the sources,
/// [`LinkError::CcNotFound`] or [`LinkError::Failed`] from `cc`.
pub(super) fn compile(cache: &Cache) -> Result<Bundled, LinkError> {
    let name = format!("{GC_VERSION}-{:016x}", fnv1a(&source()));
    let in_cache = cache.root().map(|root| root.join("gc").join(&name));
    let (dir, scratch) = match in_cache.filter(|dir| restore(dir).is_ok()) {
        Some(dir) => (dir, None),
        None => {
            let dir = temp_path("dir");
            restore(&dir).map_err(LinkError::Io)?;
            (dir.clone(), Some(dir))
        }
    };
    let include = dir.join("include");
    let mut flags: Vec<OsString> = GC_FLAGS.iter().map(OsString::from).collect();
    flags.extend([OsString::from("-I"), include.clone().into()]);
    let temp_object = temp_path("o");
    let temp = vec![temp_object.clone(), temp_object.with_extension("d")];
    let entry = dir.join("extra/gc.c");
    let object = cache.object("gc", &source(), &flags, &temp_object, |extra| {
        run(&entry, &flags, extra, &temp_object)
    })?;
    Ok(Bundled {
        object,
        include,
        temp,
        scratch,
    })
}

/// Every file's path and text, for the cache key and the folder's name.
fn source() -> Vec<u8> {
    let mut bytes = Vec::new();
    for (path, text) in GC_FILES {
        bytes.extend_from_slice(path.as_bytes());
        bytes.push(0);
        bytes.extend_from_slice(text.as_bytes());
        bytes.push(0);
    }
    bytes
}

/// Makes `dir` hold exactly [`GC_FILES`]: a missing or changed file is
/// written again, through a temporary file and a rename, so a build running
/// at the same time never reads half a file.
fn restore(dir: &Path) -> io::Result<()> {
    for (path, text) in GC_FILES {
        let file = dir.join(path);
        if std::fs::read(&file).is_ok_and(|b| b == text.as_bytes()) {
            continue;
        }
        let parent = file.parent().unwrap_or(dir);
        std::fs::create_dir_all(parent)?;
        let partial = parent.join(format!(".{}.{}", std::process::id(), unique()));
        std::fs::write(&partial, text)?;
        std::fs::rename(&partial, &file)?;
    }
    Ok(())
}

/// `cc -c entry flags extra -o object`.
fn run(
    entry: &Path,
    flags: &[OsString],
    extra: &[OsString],
    object: &Path,
) -> Result<(), LinkError> {
    let result = Command::new("cc")
        .arg("-c")
        .arg(entry)
        .args(flags)
        .args(extra)
        .arg("-o")
        .arg(object)
        .output();
    let out = match result {
        Ok(out) => out,
        Err(e) if e.kind() == io::ErrorKind::NotFound => return Err(LinkError::CcNotFound),
        Err(e) => return Err(LinkError::Io(e)),
    };
    if out.status.success() {
        return Ok(());
    }
    Err(LinkError::Failed {
        status: out.status.code(),
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    })
}

fn unique() -> usize {
    static NEXT: AtomicUsize = AtomicUsize::new(0);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// A fresh path in the temp folder: `lughac-gc-<pid>-<n>.<extension>`.
fn temp_path(extension: &str) -> PathBuf {
    std::env::temp_dir().join(format!(
        "lughac-gc-{}-{}.{extension}",
        std::process::id(),
        unique()
    ))
}

#[cfg(test)]
mod tests {
    use super::{GC_FILES, GC_FLAGS, restore, source, temp_path};

    #[test]
    fn every_file_gc_c_includes_is_embedded() {
        let (_, entry) = GC_FILES
            .iter()
            .find(|(path, _)| *path == "extra/gc.c")
            .expect("the one-file entry");
        let included: Vec<&str> = entry
            .lines()
            .filter_map(|l| l.split("\"../").nth(1))
            .filter_map(|rest| rest.split('"').next())
            .collect();
        assert!(included.len() > 20, "{included:?}");
        for file in included {
            assert!(GC_FILES.iter().any(|(p, _)| *p == file), "{file}");
        }
        for header in [
            "include/gc.h",
            "include/gc_mark.h",
            "include/private/gc_priv.h",
        ] {
            assert!(GC_FILES.iter().any(|(p, _)| *p == header), "{header}");
        }
    }

    #[test]
    fn single_threaded_with_a_small_stack_clear() {
        assert!(GC_FLAGS.contains(&"-DALWAYS_SMALL_CLEAR_STACK"));
        assert!(
            !GC_FLAGS.iter().any(|f| f.contains("THREADS")),
            "{GC_FLAGS:?}"
        );
    }

    #[test]
    fn the_key_covers_every_files_path_and_text() {
        let all = source();
        let size: usize = GC_FILES.iter().map(|(p, t)| p.len() + t.len() + 2).sum();
        assert_eq!(all.len(), size);
    }

    #[test]
    fn restore_rewrites_a_changed_or_missing_file_and_nothing_else() {
        let dir = temp_path("test");
        restore(&dir).expect("writable");
        std::fs::write(dir.join("include/gc.h"), "changed").unwrap();
        std::fs::remove_file(dir.join("misc.c")).unwrap();
        restore(&dir).expect("writable");
        for (path, text) in GC_FILES {
            assert_eq!(
                std::fs::read_to_string(dir.join(path)).unwrap(),
                text,
                "{path}"
            );
        }
        let leftovers = std::fs::read_dir(dir.join("include"))
            .unwrap()
            .filter(|e| {
                e.as_ref()
                    .is_ok_and(|e| e.file_name().to_string_lossy().starts_with('.'))
            })
            .count();
        assert_eq!(leftovers, 0, "no partial files left");
        let _ = std::fs::remove_dir_all(&dir);
    }
}
