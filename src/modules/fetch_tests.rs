//! The loader with git entries (milestone 50), through a fake `git` that
//! "clones" by writing files, in real temporary folders.

use std::cell::Cell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::{AtomicU32, Ordering};

use super::{Fetcher, Mode};
use crate::modules::git::Git;
use crate::modules::{Loaded, cache, load};

/// Tags of one repository: version → (commit, files).
type Repo = HashMap<&'static str, (String, Vec<(&'static str, &'static str)>)>;

struct FakeGit {
    repos: HashMap<String, Repo>,
    cloned: HashMap<PathBuf, String>,
    calls: Rc<Cell<u32>>,
}

impl Git for FakeGit {
    fn ls_remote(&mut self, url: &str) -> Result<String, String> {
        self.calls.set(self.calls.get() + 1);
        let repo = self.repos.get(url).ok_or("repository not found")?;
        Ok(repo
            .iter()
            .map(|(v, (c, _))| format!("{c}\trefs/tags/v{v}\n"))
            .collect())
    }

    fn clone_tag(&mut self, url: &str, tag: &str, dir: &Path) -> Result<(), String> {
        self.calls.set(self.calls.get() + 1);
        let repo = self.repos.get(url).ok_or("repository not found")?;
        let (commit, files) = repo.get(&tag[1..]).ok_or("no such tag")?;
        std::fs::create_dir_all(dir.join(".git")).map_err(|e| e.to_string())?;
        for (name, text) in files {
            std::fs::write(dir.join(name), text).map_err(|e| e.to_string())?;
        }
        self.cloned.insert(dir.to_path_buf(), commit.clone());
        Ok(())
    }

    fn head(&mut self, dir: &Path) -> Result<String, String> {
        self.cloned.get(dir).cloned().ok_or("not a clone".into())
    }
}

fn commit(n: u8) -> String {
    format!("{n:02x}").repeat(20)
}

fn temp() -> PathBuf {
    static N: AtomicU32 = AtomicU32::new(0);
    let n = N.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("lugha-fetch-{}-{n}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir); // a leftover from an earlier run
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

const GEO: &str = "file:///repos/geo";
const MAIN: &str = "import geo.shapes;\nfun main() { println(shapes.area()); }\n";

/// A program folder whose manifest lists `geo` from `GEO` at `requirement`.
fn program(requirement: &str) -> PathBuf {
    let dir = temp();
    std::fs::write(dir.join("main.la"), MAIN).unwrap();
    let manifest =
        format!("[packages]\ngeo = {{ git = \"{GEO}\", version = \"{requirement}\" }}\n");
    std::fs::write(dir.join("lugha.toml"), manifest).unwrap();
    dir
}

/// `geo` with tags 1.0.0, 1.2.0 and 2.0.0 (commits 1, 2, 3).
fn geo() -> HashMap<String, Repo> {
    let shapes = |n: &'static str| vec![("shapes.la", n)];
    let mut repo = Repo::new();
    repo.insert(
        "1.0.0",
        (commit(1), shapes("public fun area(): i64 = 1;\n")),
    );
    repo.insert(
        "1.2.0",
        (commit(2), shapes("public fun area(): i64 = 2;\n")),
    );
    repo.insert(
        "2.0.0",
        (commit(3), shapes("public fun area(): i64 = 3;\n")),
    );
    HashMap::from([(GEO.to_string(), repo)])
}

fn build(dir: &Path, cache: &Path, repos: HashMap<String, Repo>, mode: Mode) -> (Loaded, u32) {
    let calls = Rc::new(Cell::new(0));
    let git = FakeGit {
        repos,
        cloned: HashMap::new(),
        calls: calls.clone(),
    };
    let fetcher = Fetcher::new(Box::new(git), Some(cache.to_path_buf()), mode);
    let mut read = |p: &Path| std::fs::read(p);
    let loaded = load(&dir.join("main.la"), &mut read, fetcher).unwrap();
    (loaded, calls.get())
}

fn codes(loaded: &Loaded) -> Vec<&'static str> {
    loaded.diagnostics.iter().map(|d| d.code).collect()
}

#[test]
fn the_highest_matching_tag_is_fetched_cached_and_locked() {
    let (dir, cache) = (program("1"), temp());
    let (loaded, calls) = build(&dir, &cache, geo(), Mode::default());
    assert!(codes(&loaded).is_empty(), "{:?}", loaded.diagnostics);
    assert_eq!(calls, 2, "ls-remote and clone");
    let lock = loaded.lock.expect("a new lock");
    assert!(
        lock.contains("version = \"1.2.0\"") && lock.contains(&commit(2)),
        "{lock}"
    );
    let folder = cache::folder(&cache, GEO, &commit(2));
    assert!(folder.join("shapes.la").exists() && !folder.join(".git").exists());
    // With the lock written, the next build runs no `git` and changes nothing.
    std::fs::write(dir.join("lugha.lock"), lock).unwrap();
    let (again, calls) = build(
        &dir,
        &cache,
        geo(),
        Mode {
            offline: true,
            update: false,
        },
    );
    assert!(codes(&again).is_empty() && again.lock.is_none() && calls == 0);
}

#[test]
fn a_lock_pins_the_commit_and_catches_a_moved_tag() {
    let (dir, cache) = (program("1"), temp());
    let lock = |c: &str| {
        format!("[locked]\ngeo = {{ git = \"{GEO}\", version = \"1.0.0\", commit = \"{c}\" }}\n")
    };
    std::fs::write(dir.join("lugha.lock"), lock(&commit(1))).unwrap();
    let (loaded, _) = build(&dir, &cache, geo(), Mode::default());
    assert!(codes(&loaded).is_empty() && loaded.lock.is_none());
    assert!(cache::folder(&cache, GEO, &commit(1)).exists());
    // The tag now gives another commit than the lock's.
    std::fs::write(dir.join("lugha.lock"), lock(&commit(9))).unwrap();
    let (moved, _) = build(&dir, &cache, geo(), Mode::default());
    assert_eq!(codes(&moved), ["E0326"]);
    // `lughac update` ignores the lock.
    let (updated, _) = build(
        &dir,
        &cache,
        geo(),
        Mode {
            offline: false,
            update: true,
        },
    );
    assert!(updated.lock.is_some_and(|l| l.contains(&commit(2))));
}

#[test]
fn fetch_failures_are_e0325() {
    let cache = temp();
    let (none, _) = build(&program("3"), &cache, geo(), Mode::default());
    assert_eq!(codes(&none), ["E0325"]);
    assert!(none.lock.is_none());
    let (offline, calls) = build(
        &program("1"),
        &cache,
        geo(),
        Mode {
            offline: true,
            update: false,
        },
    );
    assert_eq!((codes(&offline), calls), (vec!["E0325"], 0));
    let (missing, _) = build(&program("1"), &cache, HashMap::new(), Mode::default());
    assert_eq!(codes(&missing), ["E0325"]);
}

#[test]
fn a_git_package_may_list_git_packages() {
    let (dir, cache) = (program("2"), temp());
    let mut repos = geo();
    let manifest = "[packages]\nmathx = { git = \"file:///repos/mathx\", version = \"1\" }\n";
    let shapes = "import mathx.ops;\npublic fun area(): i64 = ops.two();\n";
    let geo = repos.get_mut(GEO).unwrap();
    geo.insert(
        "2.0.0",
        (
            commit(3),
            vec![("lugha.toml", manifest), ("shapes.la", shapes)],
        ),
    );
    let ops = vec![("ops.la", "public fun two(): i64 = 2;\n")];
    repos.insert(
        "file:///repos/mathx".into(),
        Repo::from([("1.0.0", (commit(4), ops))]),
    );
    let (loaded, _) = build(&dir, &cache, repos, Mode::default());
    assert!(codes(&loaded).is_empty(), "{:?}", loaded.diagnostics);
    let lock = loaded.lock.unwrap();
    assert!(lock.contains("geo =") && lock.contains("mathx =") && lock.contains(&commit(4)));
}
