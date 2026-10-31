//! The benchmark suite's Lugha programs still build and print the right
//! thing (PRP-184). Timings are never taken here; `bench/run.py` does that.

use std::path::{Path, PathBuf};
use std::process::Command;

/// One benchmark: its `.la` source, the small size and the expected output.
struct Case {
    name: String,
    source: PathBuf,
    size: String,
    expected: String,
}

/// Reads `<dir>/check.txt`: the small size on the first line, then exactly
/// what the program prints at that size.
fn load(dir: &Path) -> Result<Case, String> {
    let name = dir
        .file_name()
        .and_then(|n| n.to_str())
        .ok_or_else(|| format!("{} has no UTF-8 name", dir.display()))?
        .to_string();
    let check = std::fs::read_to_string(dir.join("check.txt"))
        .map_err(|e| format!("benchmark {name}: can't read check.txt: {e}"))?;
    let (size, expected) = check
        .split_once('\n')
        .ok_or_else(|| format!("benchmark {name}: check.txt has no output after its size"))?;
    Ok(Case {
        source: dir.join(format!("{name}.la")),
        size: size.trim().to_string(),
        expected: expected.to_string(),
        name,
    })
}

/// Every directory under `bench/programs/`, loaded.
fn cases() -> Vec<Result<Case, String>> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("bench/programs");
    let mut dirs: Vec<PathBuf> = std::fs::read_dir(&root)
        .expect("bench/programs exists in the repository")
        .map(|e| e.expect("bench/programs is readable").path())
        .filter(|p| p.is_dir())
        .collect();
    dirs.sort();
    dirs.iter().map(|d| load(d)).collect()
}

/// Builds and runs one case, returning a description of any mismatch.
fn run(case: &Case, out_dir: &Path) -> Option<String> {
    let lughac = Path::new(env!("CARGO_BIN_EXE_lughac"));
    let exe = out_dir.join(&case.name);
    let build = Command::new(lughac)
        .arg("build")
        .arg(&case.source)
        .arg("-o")
        .arg(&exe)
        .output()
        .expect("lughac runs");
    if !build.status.success() {
        return Some(format!(
            "{}: build failed\n{}",
            case.name,
            String::from_utf8_lossy(&build.stderr)
        ));
    }
    let ran = Command::new(&exe)
        .arg(&case.size)
        .output()
        .expect("the built benchmark runs");
    let stdout = String::from_utf8_lossy(&ran.stdout);
    if !ran.status.success() || stdout != case.expected {
        return Some(format!(
            "{}: expected\n{}got ({})\n{}{}",
            case.name,
            case.expected,
            ran.status,
            stdout,
            String::from_utf8_lossy(&ran.stderr)
        ));
    }
    None
}

/// A directory under the system temp dir, removed when dropped.
struct TempDir(PathBuf);

impl TempDir {
    fn new(tag: &str) -> TempDir {
        let dir = std::env::temp_dir().join(format!("lugha-bench-{tag}-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        TempDir(dir)
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir is harmless and must not mask a failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[test]
fn every_benchmark_prints_its_check() {
    let out = TempDir::new("build");
    let cases = cases();
    assert!(
        cases.len() >= 6,
        "expected the six benchmarks, found {}",
        cases.len()
    );
    let failures: Vec<String> = cases
        .iter()
        .filter_map(|c| match c {
            Ok(case) => run(case, &out.0),
            Err(e) => Some(e.clone()),
        })
        .collect();
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

#[test]
fn a_benchmark_without_check_txt_is_named() {
    let dir = TempDir::new("nocheck");
    let bench = dir.0.join("mystery");
    std::fs::create_dir_all(&bench).expect("temp dir is writable");
    let Err(e) = load(&bench) else {
        panic!("loading a benchmark with no check.txt succeeded");
    };
    assert!(e.contains("benchmark mystery"), "{e}");
}

#[test]
fn a_check_txt_without_output_is_rejected() {
    let dir = TempDir::new("short");
    let bench = dir.0.join("short");
    std::fs::create_dir_all(&bench).expect("temp dir is writable");
    std::fs::write(bench.join("check.txt"), "10").expect("temp dir is writable");
    let Err(e) = load(&bench) else {
        panic!("a check.txt with only a size was accepted");
    };
    assert!(e.contains("no output"), "{e}");
}
