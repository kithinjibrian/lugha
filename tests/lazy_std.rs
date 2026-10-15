//! Lazy std modules through the `lughac` binary (v15, milestone 60): an
//! imported std module's functions and methods are compiled only when the
//! program reaches them, however it reaches them.

use std::path::PathBuf;
use std::process::Command;

/// A temporary directory, removed on drop.
struct Dir(PathBuf);

impl Dir {
    fn new(name: &str) -> Self {
        let dir = std::env::temp_dir().join(format!("lugha-lazy-{name}-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        Dir(dir)
    }
}

impl Drop for Dir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

/// The names of the std functions `src`'s IR defines.
fn std_defined(name: &str, src: &str) -> Vec<String> {
    let dir = Dir::new(name);
    std::fs::write(dir.0.join("main.la"), src).expect("temp dir is writable");
    let out = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(["build", "--emit=ir", "main.la"])
        .current_dir(&dir.0)
        .output()
        .expect("lughac runs");
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .filter(|l| l.starts_with("define "))
        .filter_map(|l| l.split("@\"lugha_fn_std/").nth(1))
        .map(|rest| rest.split('"').next().unwrap_or_default().to_string())
        .collect()
}

#[test]
fn only_what_a_program_reaches_is_compiled() {
    let defined = std_defined(
        "random",
        "import std.random;\nfun main() { let mut r = random.seeded(1); println(r.below(6)); }\n",
    );
    for used in ["random/seeded", "random/Rng.below", "random/Rng.next_u64"] {
        assert!(defined.iter().any(|d| d == used), "{used}: {defined:?}");
    }
    for unused in [
        "random/from_os",
        "random/Rng.uniform",
        "random/Rng.between",
        "random/Rng.chance",
        "math/pi",
        "math/e",
    ] {
        assert!(
            !defined.iter().any(|d| d == unused),
            "{unused}: {defined:?}"
        );
    }
    let math = std_defined(
        "math",
        "import std.math;\nfun main() { println(math.sqrt(2.0)); }\n",
    );
    assert!(math.iter().all(|d| d.starts_with("math/sqrt")), "{math:?}");
}

#[test]
fn every_way_of_reaching_compiles_the_target() {
    let src = "import std.math;
import std.random;

interface Draw { fun uniform(mut self): f64; }
interface Bits { fun next_u64(mut self): u64; }

fun first<B: Bits>(b: B): u64 {
    let mut c = b;
    c.next_u64()
}

fun main() {
    let f = math.pi;
    println(f());
    let mut d: Draw = random.seeded(2);
    println(d.uniform());
    println(first(random.seeded(3)));
    println(math.clamp(5, 0, 3));
}
";
    let defined = std_defined("reach", src);
    for used in [
        "math/pi",
        "random/seeded",
        "random/Rng.uniform",
        "random/Rng.next_u64",
    ] {
        assert!(defined.iter().any(|d| d == used), "{used}: {defined:?}");
    }
    // Generic std functions are instances, as before.
    assert!(
        defined.iter().any(|d| d.starts_with("math/clamp<")),
        "{defined:?}"
    );
    assert!(
        !defined.iter().any(|d| d == "random/from_os"),
        "{defined:?}"
    );
}
