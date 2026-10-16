//! Git packages end to end (PRP-079, milestone 50): a real `file://`
//! repository made with the system `git`, fetched into a private cache,
//! locked, built `--offline`, a moved tag caught, and `lughac update`.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A temporary folder removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn new() -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-git-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        Temp(dir)
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn git(dir: &Path, args: &[&str]) -> String {
    let out = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args([
            "-c",
            "user.name=t",
            "-c",
            "user.email=t@t",
            "-c",
            "commit.gpgsign=false",
        ])
        .args(args)
        .output()
        .expect("git runs");
    assert!(
        out.status.success(),
        "git {args:?}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Commits `shapes.la` with `area` returning `n`, and tags it `tag`.
fn release(repo: &Path, n: i64, tag: &str) -> String {
    std::fs::write(
        repo.join("shapes.la"),
        format!("public fun area(): i64 = {n};\n"),
    )
    .unwrap();
    git(repo, &["add", "-A"]);
    git(repo, &["commit", "-q", "-m", tag]);
    git(repo, &["tag", "-f", tag]);
    git(repo, &["rev-parse", "HEAD"])
}

/// A repository with `v1.0.0` (area 10) and `v1.1.0` (area 11), holding a
/// symlink to a file outside it; and a program needing `version`.
fn setup(t: &Temp, version: &str) -> (PathBuf, PathBuf, String) {
    let repo = t.0.join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    git(&repo, &["init", "-q"]);
    std::os::unix::fs::symlink("/etc/hostname", repo.join("secret.la")).unwrap();
    release(&repo, 10, "v1.0.0");
    let newest = release(&repo, 11, "v1.1.0");
    let app = t.0.join("app");
    std::fs::create_dir_all(&app).unwrap();
    std::fs::write(
        app.join("main.la"),
        "import geo.shapes;\nfun main() { println(shapes.area()); }\n",
    )
    .unwrap();
    let url = format!("file://{}", repo.display());
    let manifest = format!("[packages]\ngeo = {{ git = \"{url}\", version = \"{version}\" }}\n");
    std::fs::write(app.join("lugha.toml"), manifest).unwrap();
    (repo, app, newest)
}

fn lughac(t: &Temp, app: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .arg(app.join("main.la"))
        .env("LUGHA_CACHE", t.0.join("cache"))
        .output()
        .expect("lughac runs")
}

fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

#[test]
fn a_git_package_is_fetched_locked_and_built_offline() {
    let t = Temp::new();
    let (_, app, newest) = setup(&t, "1");
    let run = lughac(&t, &app, &["run"]);
    assert_eq!(text(&run.stdout), "11\n", "{}", text(&run.stderr));
    let lock = std::fs::read_to_string(app.join("lugha.lock")).expect("a lock is written");
    assert!(
        lock.contains(&newest) && lock.contains("version = \"1.1.0\""),
        "{lock}"
    );
    // Offline, with the lock and the cache: the same program, no `git`.
    let offline = lughac(&t, &app, &["run", "--offline"]);
    assert_eq!(text(&offline.stdout), "11\n", "{}", text(&offline.stderr));
    // The symlink was checked out as a plain file holding its target's name.
    let cached = t.0.join("cache/git");
    let package = std::fs::read_dir(&cached)
        .unwrap()
        .next()
        .unwrap()
        .unwrap()
        .path();
    let folder = package.join(&newest);
    let secret = std::fs::symlink_metadata(folder.join("secret.la")).unwrap();
    assert!(secret.file_type().is_file());
    assert_eq!(
        std::fs::read_to_string(folder.join("secret.la")).unwrap(),
        "/etc/hostname"
    );
    assert!(!folder.join(".git").exists());
}

#[test]
fn a_moved_tag_is_e0326_until_update() {
    let t = Temp::new();
    let (repo, app, _) = setup(&t, "1.1");
    assert!(lughac(&t, &app, &["check"]).status.success());
    let moved = release(&repo, 12, "v1.1.0");
    std::fs::remove_dir_all(t.0.join("cache")).unwrap();
    let check = lughac(&t, &app, &["check"]);
    assert_eq!(check.status.code(), Some(1));
    assert!(
        text(&check.stderr).contains("E0326"),
        "{}",
        text(&check.stderr)
    );
    let update = lughac(&t, &app, &["update"]);
    assert!(update.status.success(), "{}", text(&update.stderr));
    let lock = std::fs::read_to_string(app.join("lugha.lock")).unwrap();
    assert!(lock.contains(&moved), "{lock}");
    assert_eq!(text(&lughac(&t, &app, &["run"]).stdout), "12\n");
}

#[test]
fn unfetchable_packages_are_e0325() {
    let t = Temp::new();
    let (_, app, _) = setup(&t, "3");
    let check = lughac(&t, &app, &["check"]);
    assert_eq!(check.status.code(), Some(1));
    assert!(
        text(&check.stderr).contains("E0325"),
        "{}",
        text(&check.stderr)
    );
    assert!(!app.join("lugha.lock").exists());
    let fresh = Temp::new();
    let (_, offline_app, _) = setup(&fresh, "1");
    let offline = lughac(&t, &offline_app, &["check", "--offline"]);
    assert!(
        text(&offline.stderr).contains("E0325"),
        "{}",
        text(&offline.stderr)
    );
}

/// `lughac <args> <file>` with the test's cache.
fn lughac_on(t: &Temp, file: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .arg(file)
        .env("LUGHA_CACHE", t.0.join("cache"))
        .output()
        .expect("lughac runs")
}

#[test]
fn one_lock_per_package_beside_its_manifest_v17() {
    let t = Temp::new();
    let (_, app, newest) = setup(&t, "1");
    let main = "import geo.shapes;\nfun main() { println(shapes.area()); }\n";
    std::fs::create_dir_all(app.join("examples")).unwrap();
    std::fs::write(app.join("examples/demo.la"), main).unwrap();
    let demo = app.join("examples/demo.la");
    let run = lughac_on(&t, &demo, &["run"]);
    assert_eq!(text(&run.stdout), "11\n", "{}", text(&run.stderr));
    let lock = std::fs::read_to_string(app.join("lugha.lock")).expect("beside the manifest");
    assert!(lock.contains(&newest), "{lock}");
    assert!(!app.join("examples/lugha.lock").exists());
    // An old lock beside the main file is read once, then written beside the manifest.
    std::fs::rename(app.join("lugha.lock"), app.join("examples/lugha.lock")).unwrap();
    let offline = lughac_on(&t, &demo, &["run", "--offline"]);
    assert_eq!(text(&offline.stdout), "11\n", "{}", text(&offline.stderr));
    let moved =
        std::fs::read_to_string(app.join("lugha.lock")).expect("written beside the manifest");
    assert!(moved.contains(&newest), "{moved}");
}
