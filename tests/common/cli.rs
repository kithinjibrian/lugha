//! Helpers for running the `lughac` binary in a temporary folder, shared by the CLI tests.

// Each test file uses only some of these helpers.
#![allow(dead_code)]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::atomic::{AtomicUsize, Ordering};

/// A program whose exit code is 14.
pub const ARITH: &str = "fun main(): i32 { 2 + 3 * 4 }\n";

/// A temporary working directory removed on drop.
pub struct TempDir(pub PathBuf);

impl TempDir {
    pub fn with(files: &[(&str, &[u8])]) -> Self {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-cli-{}-{n}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("temp dir is writable");
        for (name, contents) in files {
            std::fs::write(dir.join(name), contents).expect("temp dir is writable");
        }
        TempDir(dir)
    }

    pub fn entries(&self) -> Vec<String> {
        let mut names: Vec<_> = std::fs::read_dir(&self.0)
            .expect("temp dir is readable")
            .map(|e| e.expect("entry").file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        names
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

pub fn lughac(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(args)
        .current_dir(dir)
        .output()
        .expect("lughac runs")
}

pub fn text(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}
