//! The C object cache end to end (v17, milestone 127, PRP-159): builds
//! through `lughac` with `$LUGHA_CACHE` in a temporary folder and a `cc`
//! on `PATH` that logs each call and runs the real one. The bundled
//! collector (milestone 131, PRP-163) is compiled once like the runtime.

use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A project with a `[link]` C file including a header, a logging `cc`
/// and a cache folder, removed on drop.
struct Project(PathBuf);

impl Project {
    fn new() -> Project {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-ccache-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        let p = Project(dir);
        p.write(
            "lugha.toml",
            "[link]\nc = [\"shim/shim.c\"]\ninclude = [\"inc\"]\n",
        );
        p.write("inc/factor.h", "#define FACTOR 2\n");
        p.write(
            "shim/shim.c",
            "#include \"factor.h\"\nlong shim_times(long x) { return x * FACTOR; }\n",
        );
        p.write(
            "main.la",
            "extern fun shim_times(x: i64): i64;\nfun main() { println(shim_times(21)); }\n",
        );
        let real = Command::new("sh")
            .args(["-c", "command -v cc"])
            .output()
            .expect("sh runs");
        let real = String::from_utf8(real.stdout)
            .expect("a path")
            .trim()
            .to_string();
        // Logs each call; with FAIL_OPENSSL, the runtime's compile fails as without OpenSSL.
        let body = format!(
            "#!/bin/sh\necho \"$@\" >> \"{log}\"\n\
             case \"$*\" in *lughac-rt*) if [ -n \"$FAIL_OPENSSL\" ] && [ \"$1\" = -c ]; then \
             echo 'fatal error: openssl/ssl.h: No such file or directory' >&2; exit 1; fi;; esac\n\
             exec \"{real}\" \"$@\"\n",
            log = p.0.join("cc.log").display()
        );
        p.write("bin/cc", &body);
        let cc = p.0.join("bin/cc");
        std::fs::set_permissions(&cc, std::fs::Permissions::from_mode(0o755)).unwrap();
        p
    }

    fn write(&self, name: &str, text: &str) {
        let path = self.0.join(name);
        std::fs::create_dir_all(path.parent().expect("in a folder")).unwrap();
        std::fs::write(path, text).unwrap();
    }

    /// `lughac run main.la` with the logging `cc` first on `PATH` and the
    /// cache at `cache`; the run's output and the `cc -c` calls it made.
    fn run_with(&self, file: &str, cache: &PathBuf, env: &[(&str, &str)]) -> (Output, Vec<String>) {
        let _ = std::fs::remove_file(self.0.join("cc.log"));
        let path = format!(
            "{}:{}",
            self.0.join("bin").display(),
            std::env::var("PATH").unwrap_or_default()
        );
        let mut command = Command::new(env!("CARGO_BIN_EXE_lughac"));
        command
            .args(["run", file])
            .current_dir(&self.0)
            .env("HOME", &self.0)
            .env("PATH", path)
            .env("LUGHA_CACHE", cache)
            .env_remove("XDG_CACHE_HOME");
        for (k, v) in env {
            command.env(k, v);
        }
        let out = command.output().expect("lughac runs");
        let log = std::fs::read_to_string(self.0.join("cc.log")).unwrap_or_default();
        let compiles = log
            .lines()
            .filter(|l| l.starts_with("-c "))
            .map(str::to_string)
            .collect();
        (out, compiles)
    }

    fn run(&self) -> (Output, Vec<String>) {
        self.run_with("main.la", &self.0.join("cache"), &[])
    }

    fn entries(&self) -> Vec<PathBuf> {
        std::fs::read_dir(self.0.join("cache/objects"))
            .map(|d| d.filter_map(|e| e.ok()).map(|e| e.path()).collect())
            .unwrap_or_default()
    }
}

impl Drop for Project {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn stdout(out: &Output) -> String {
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn a_second_build_compiles_no_c() {
    let p = Project::new();
    let (first, compiles) = p.run();
    assert_eq!(
        stdout(&first),
        "42\n",
        "{}",
        String::from_utf8_lossy(&first.stderr)
    );
    assert_eq!(
        compiles.len(),
        3,
        "the shim, the collector and the runtime: {compiles:?}"
    );
    assert_eq!(
        compiles.iter().filter(|c| c.contains("extra/gc.c")).count(),
        1,
        "{compiles:?}"
    );
    assert_eq!(p.entries().len(), 3);
    let (second, compiles) = p.run();
    assert_eq!(stdout(&second), "42\n");
    assert_eq!(compiles, Vec::<String>::new());
}

#[test]
fn a_changed_header_source_or_flag_recompiles_just_that_file() {
    let p = Project::new();
    p.run();
    p.write("inc/factor.h", "#define FACTOR 3\n");
    let (out, compiles) = p.run();
    assert_eq!(stdout(&out), "63\n");
    assert_eq!(compiles.len(), 1, "{compiles:?}");
    assert!(compiles[0].contains("shim.c"), "{compiles:?}");
    p.write(
        "shim/shim.c",
        "#include \"factor.h\"\nlong shim_times(long x) { return x * FACTOR + 1; }\n",
    );
    let (out, compiles) = p.run();
    assert_eq!(stdout(&out), "64\n");
    assert_eq!(compiles.len(), 1, "{compiles:?}");
    p.write("other/empty.h", "\n");
    p.write(
        "lugha.toml",
        "[link]\nc = [\"shim/shim.c\"]\ninclude = [\"inc\", \"other\"]\n",
    );
    let (out, compiles) = p.run();
    assert_eq!(stdout(&out), "64\n");
    assert_eq!(compiles.len(), 1, "a new flag: {compiles:?}");
    let (_, compiles) = p.run();
    assert_eq!(compiles, Vec::<String>::new());
}

#[test]
fn a_damaged_entry_recompiles_and_the_program_still_runs() {
    let p = Project::new();
    p.run();
    for entry in p.entries() {
        std::fs::write(entry.join("object.o"), b"garbage").unwrap();
    }
    let (out, compiles) = p.run();
    assert_eq!(
        stdout(&out),
        "42\n",
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(compiles.len(), 3, "the collector too: {compiles:?}");
    let (_, compiles) = p.run();
    assert_eq!(compiles, Vec::<String>::new());
}

#[test]
fn a_cache_that_cant_be_written_still_builds() {
    let p = Project::new();
    // A file where the cache's folder should be.
    p.write("blocked", "not a folder\n");
    let blocked = p.0.join("blocked");
    for _ in 0..2 {
        let (out, compiles) = p.run_with("main.la", &blocked, &[]);
        assert_eq!(
            stdout(&out),
            "42\n",
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
        assert_eq!(compiles.len(), 3, "nothing cached: {compiles:?}");
    }
}

#[test]
fn a_tls_program_has_its_own_runtime_entry_and_e0332_still_shows() {
    let p = Project::new();
    p.run();
    p.write(
        "tls.la",
        "import std.tls;\nfun main() { println(tls.options().timeout_ms >= 0); }\n",
    );
    let cache = p.0.join("cache");
    let (out, compiles) = p.run_with("tls.la", &cache, &[]);
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    if stderr.contains("E0332") {
        // No OpenSSL here at all: nothing more to check.
        return;
    }
    assert_eq!(stdout(&out), "true\n", "{stderr}");
    assert_eq!(
        compiles.len(),
        1,
        "only the TLS runtime; the shim's entry is reused: {compiles:?}"
    );
    assert_eq!(
        p.entries().len(),
        4,
        "plain runtime, TLS runtime, shim, collector"
    );
    // The TLS runtime's compile failing for OpenSSL's headers is E0332.
    let fresh = p.0.join("fresh");
    let (out, _) = p.run_with("tls.la", &fresh, &[("FAIL_OPENSSL", "1")]);
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(stderr.contains("E0332"), "{stderr}");
    assert_eq!(out.status.code(), Some(1));
}

#[test]
fn the_collectors_sources_are_restored_when_changed_v17() {
    let p = Project::new();
    p.run();
    // Each version's sources sit in one folder under `<cache>/gc/`.
    let folders: Vec<PathBuf> = std::fs::read_dir(p.0.join("cache/gc"))
        .expect("the collector's sources are in the cache")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .collect();
    assert_eq!(folders.len(), 1, "{folders:?}");
    let header = folders[0].join("include/gc.h");
    std::fs::write(&header, "#error tampered\n").unwrap();
    let (out, compiles) = p.run();
    assert_eq!(
        stdout(&out),
        "42\n",
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        compiles.is_empty(),
        "restored byte for byte, so no recompile: {compiles:?}"
    );
    assert_ne!(
        std::fs::read_to_string(&header).unwrap(),
        "#error tampered\n"
    );
}

/// A compiler wrapper at `bin/<name>` that logs its arguments to
/// `<name>.log`, checks its first one is `extra-word`, and runs the real `cc`.
fn wrapper(p: &Project, name: &str) -> String {
    let real = std::fs::read_to_string(p.0.join("bin/cc")).unwrap();
    let real = real
        .lines()
        .find_map(|l| l.strip_prefix("exec \""))
        .and_then(|l| l.split('"').next())
        .expect("the logging cc names the real one")
        .to_string();
    let body = format!(
        "#!/bin/sh\necho \"$@\" >> \"{log}\"\n[ \"$1\" = extra-word ] || exit 9\nshift\nexec \"{real}\" \"$@\"\n",
        log = p.0.join(format!("{name}.log")).display()
    );
    p.write(&format!("bin/{name}"), &body);
    let path = p.0.join(format!("bin/{name}"));
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    format!("{} extra-word", path.display())
}

#[test]
fn lugha_cc_runs_every_compile_and_the_link_and_keys_the_cache_v17() {
    let p = Project::new();
    let cache = p.0.join("cache");
    let first = wrapper(&p, "first");
    let (out, plain) = p.run_with("main.la", &cache, &[("LUGHA_CC", &first)]);
    assert_eq!(
        stdout(&out),
        "42\n",
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(plain.is_empty(), "the plain `cc` ran: {plain:?}");
    let log = std::fs::read_to_string(p.0.join("first.log")).unwrap();
    let compiles = log
        .lines()
        .filter(|l| l.starts_with("extra-word -c "))
        .count();
    assert_eq!(compiles, 3, "shim, collector, runtime: {log}");
    assert!(
        log.lines()
            .any(|l| l.contains(" -o ") && !l.contains(" -c ")),
        "the link: {log}"
    );
    // Another compiler recompiles all three; the first one's entries stay.
    let second = wrapper(&p, "second");
    p.run_with("main.la", &cache, &[("LUGHA_CC", &second)]);
    let log = std::fs::read_to_string(p.0.join("second.log")).unwrap();
    assert_eq!(
        log.lines()
            .filter(|l| l.starts_with("extra-word -c "))
            .count(),
        3,
        "{log}"
    );
    let _ = std::fs::remove_file(p.0.join("first.log"));
    let (out, _) = p.run_with("main.la", &cache, &[("LUGHA_CC", &first)]);
    assert_eq!(stdout(&out), "42\n");
    let log = std::fs::read_to_string(p.0.join("first.log")).unwrap();
    assert_eq!(
        log.lines()
            .filter(|l| l.starts_with("extra-word -c "))
            .count(),
        0,
        "{log}"
    );
}
