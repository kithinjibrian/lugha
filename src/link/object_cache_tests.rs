//! Tests of `object_cache.rs` (v17, milestone 127, PRP-159).

use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};

use super::{Entry, Fingerprints, fnv1a, headers_in, key};

/// An empty folder of its own, removed on drop.
struct Dir(PathBuf);

impl Dir {
    fn new() -> Dir {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-objcache-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("writable");
        Dir(dir)
    }
}

impl Drop for Dir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn flags(f: &[&str]) -> Vec<OsString> {
    f.iter().map(OsString::from).collect()
}

#[test]
fn fnv1a_matches_its_reference_values() {
    assert_eq!(fnv1a(b""), 0xcbf29ce484222325);
    assert_eq!(fnv1a(b"a"), 0xaf63dc4c8601ec8c);
    assert_eq!(fnv1a(b"foobar"), 0x85944171f73967e8);
}

#[test]
fn keys_differ_by_every_input() {
    let base = key("cc 1", "c", b"int x;", &flags(&["-I/a"]));
    assert_eq!(base.len(), 16);
    assert!(base.chars().all(|c| c.is_ascii_hexdigit()));
    assert_ne!(base, key("cc 2", "c", b"int x;", &flags(&["-I/a"])));
    assert_ne!(base, key("cc 1", "runtime", b"int x;", &flags(&["-I/a"])));
    assert_ne!(base, key("cc 1", "c", b"int y;", &flags(&["-I/a"])));
    assert_ne!(base, key("cc 1", "c", b"int x;", &flags(&["-I/b"])));
    // Field boundaries count: `-Ia -b` is not `-I` `a -b`.
    assert_ne!(
        key("cc", "c", b"", &flags(&["-Ia", "-b"])),
        key("cc", "c", b"", &flags(&["-I", "a-b"]))
    );
}

#[test]
fn dependency_files_list_the_headers_after_the_source() {
    let deps = "obj.o: src/shim.c /usr/include/gtk-4.0/gtk/gtk.h \\\n /usr/include/my\\ dir/x.h\n  inc/local.h\n";
    assert_eq!(
        headers_in(deps),
        [
            PathBuf::from("/usr/include/gtk-4.0/gtk/gtk.h"),
            PathBuf::from("/usr/include/my dir/x.h"),
            PathBuf::from("inc/local.h"),
        ]
    );
    assert_eq!(headers_in("obj.o: only.c\n"), Vec::<PathBuf>::new());
    assert_eq!(headers_in(""), Vec::<PathBuf>::new());
}

#[test]
fn an_entry_matches_only_its_own_inputs_and_headers() {
    let dir = Dir::new();
    let header = dir.0.join("a.h");
    std::fs::write(&header, "#define A 1\n").unwrap();
    let object = dir.0.join("in.o");
    std::fs::write(&object, b"OBJ").unwrap();
    let entry_dir = dir.0.join("objects").join("k");
    let entry = Entry {
        compiler: "cc 1".into(),
        source: b"int x;".to_vec(),
        flags: flags(&["-I/a"]),
    };
    entry
        .install(
            &dir.0.join("objects"),
            "k",
            &object,
            (std::slice::from_ref(&header), &Fingerprints::default()),
        )
        .expect("installs");
    let hit = |e: &Entry| e.hit(&entry_dir, &Fingerprints::default());
    assert_eq!(
        hit(&entry).as_deref(),
        Some(entry_dir.join("object.o").as_path())
    );
    assert_eq!(std::fs::read(entry_dir.join("object.o")).unwrap(), b"OBJ");
    let other = |f: &dyn Fn(&mut Entry)| {
        let mut e = Entry {
            compiler: entry.compiler.clone(),
            source: entry.source.clone(),
            flags: entry.flags.clone(),
        };
        f(&mut e);
        e
    };
    assert_eq!(hit(&other(&|e| e.compiler = "cc 2".into())), None);
    assert_eq!(hit(&other(&|e| e.source = b"int y;".to_vec())), None);
    assert_eq!(hit(&other(&|e| e.flags = flags(&["-I/b"]))), None);
    // A header changed, or gone: stale.
    std::fs::write(&header, "#define A 2\n").unwrap();
    assert_eq!(hit(&entry), None);
    std::fs::remove_file(&header).unwrap();
    assert_eq!(hit(&entry), None);
}

#[test]
fn a_garbled_entry_is_a_miss_and_installing_replaces_it() {
    let dir = Dir::new();
    let root = dir.0.join("objects");
    let object = dir.0.join("in.o");
    std::fs::write(&object, b"OBJ").unwrap();
    let entry = Entry {
        compiler: "cc".into(),
        source: b"s".to_vec(),
        flags: Vec::new(),
    };
    entry
        .install(&root, "k", &object, (&[], &Fingerprints::default()))
        .expect("installs");
    std::fs::write(root.join("k").join("headers"), "not a fingerprint\n").unwrap();
    assert_eq!(entry.hit(&root.join("k"), &Fingerprints::default()), None);
    std::fs::write(&object, b"NEW").unwrap();
    entry
        .install(&root, "k", &object, (&[], &Fingerprints::default()))
        .expect("replaces");
    assert!(
        entry
            .hit(&root.join("k"), &Fingerprints::default())
            .is_some()
    );
    assert_eq!(
        std::fs::read(root.join("k").join("object.o")).unwrap(),
        b"NEW"
    );
    // No temporary folder is left behind.
    let left: Vec<_> = std::fs::read_dir(&root).unwrap().collect();
    assert_eq!(left.len(), 1, "{left:?}");
    let _ = Path::new("");
}

#[test]
fn a_damaged_object_is_a_miss() {
    let dir = Dir::new();
    let root = dir.0.join("objects");
    let object = dir.0.join("in.o");
    std::fs::write(&object, b"OBJECT BYTES").unwrap();
    let entry = Entry {
        compiler: "cc".into(),
        source: b"s".to_vec(),
        flags: Vec::new(),
    };
    entry
        .install(&root, "k", &object, (&[], &Fingerprints::default()))
        .expect("installs");
    assert!(
        entry
            .hit(&root.join("k"), &Fingerprints::default())
            .is_some()
    );
    std::fs::write(root.join("k").join("object.o"), b"OBJ").unwrap();
    assert_eq!(entry.hit(&root.join("k"), &Fingerprints::default()), None);
}
