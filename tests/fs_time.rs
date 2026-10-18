//! `fs.rename`, `fs.set_mode`, `flush()` and `std.time` end to end (v17,
//! milestone 115, PRP-147), through Lugha programs in temp folders.

use std::io::Read;
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

/// A temporary folder, removed on drop.
struct Temp(PathBuf);

impl Temp {
    fn new() -> Temp {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let dir = std::env::temp_dir().join(format!("lugha-fst-{}-{n}", std::process::id()));
        // A leftover from an earlier run would change what the test sees.
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        Temp(dir)
    }

    fn command(&self, main: &str) -> Command {
        std::fs::write(self.0.join("main.la"), main).unwrap();
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_lughac"));
        cmd.args(["run", "main.la"])
            .current_dir(&self.0)
            .env("HOME", &self.0);
        cmd
    }

    fn run(&self, main: &str) -> String {
        let out = self.command(main).output().unwrap();
        assert_eq!(
            out.status.code(),
            Some(0),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
        String::from_utf8_lossy(&out.stdout).into_owned()
    }
}

impl Drop for Temp {
    fn drop(&mut self) {
        // Best effort: a leftover temp dir must not mask the real failure.
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[test]
fn rename_replaces_and_set_mode_sets_bits() {
    let t = Temp::new();
    let out = t.run(
        "import std.fs;

fun main() {
    write_file(\"old\", \"old contents\").unwrap();
    write_file(\"new.tmp\", \"new contents\").unwrap();
    println(fs.rename(\"new.tmp\", \"old\"));
    println(read_file(\"old\"));
    println(fs.exists(\"new.tmp\"));
    println(fs.rename(\"missing\", \"x\"));
    println(fs.rename(string_from_utf8([97, 0, 98]).unwrap(), \"x\"));
    write_file(\"key\", \"secret\").unwrap();
    println(fs.set_mode(\"key\", 0o600));
    println(fs.set_mode(\"key\", -1));
    println(fs.set_mode(\"key\", 4096));
    println(fs.set_mode(\"nope\", 0b110_000_000));
}
",
    );
    assert_eq!(
        out,
        "Ok(())\nOk(\"new contents\")\nfalse\nErr(\"No such file or directory\")\n\
         Err(\"path contains a NUL byte\")\nOk(())\nErr(\"mode must be from 0 to 4095 (0o7777)\")\n\
         Err(\"mode must be from 0 to 4095 (0o7777)\")\nErr(\"No such file or directory\")\n"
    );
    let mode = std::fs::metadata(t.0.join("key"))
        .unwrap()
        .permissions()
        .mode()
        & 0o7777;
    assert_eq!(mode, 0o600);
}

#[test]
fn a_rename_across_devices_is_an_error() {
    let shm = Path::new("/dev/shm");
    let t = Temp::new();
    let differ = shm.is_dir()
        && std::fs::metadata(shm).unwrap().dev() != std::fs::metadata(&t.0).unwrap().dev();
    if !differ {
        eprintln!("skipped: /dev/shm and the temp folder share a file system");
        return;
    }
    let source = shm.join(format!("lugha-fst-{}", std::process::id()));
    std::fs::write(&source, "x").unwrap();
    let out = t.run(&format!(
        "import std.fs;\n\nfun main() {{\n    println(fs.rename(\"{}\", \"here\"));\n}}\n",
        source.display()
    ));
    // Best effort: the file is ours, in shared memory.
    let _ = std::fs::remove_file(&source);
    assert_eq!(out, "Err(\"Invalid cross-device link\")\n");
}

#[test]
fn flush_shows_output_before_the_program_ends() {
    let t = Temp::new();
    let mut child = t
        .command(
            "import std.task;\n\nfun main() {\n    print(\"ready\");\n    flush();\n    task.sleep(30000);\n}\n",
        )
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut got = [0u8; 5];
    stdout.read_exact(&mut got).unwrap();
    // Without `flush` the bytes would come only when the program ends, 30 s
    // later: still running means `flush` sent them.
    let running = child.try_wait().unwrap().is_none();
    let _ = child.kill();
    let _ = child.wait();
    assert_eq!(&got, b"ready");
    assert!(running, "the output came only when the program ended");
}

#[test]
fn the_clocks_tell_the_time() {
    let t = Temp::new();
    let before = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64;
    let out = t.run(
        "import std.task;
import std.time;

fun main() {
    println(time.now_ms());
    let a = time.monotonic_ms();
    task.sleep(20);
    let b = time.monotonic_ms();
    println(b - a >= 20);
}
",
    );
    let after = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64;
    let mut lines = out.lines();
    let now: i64 = lines.next().unwrap().parse().unwrap();
    assert!(
        now >= before - 1000 && now <= after + 1000,
        "{before} {now} {after}"
    );
    assert_eq!(lines.next(), Some("true"));
}

#[test]
fn flush_is_a_built_in_name() {
    let t = Temp::new();
    let out = t
        .command("fun flush() {}\n\nfun main() {}\n")
        .arg("--diagnostics=json")
        .output()
        .unwrap();
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("\"code\":\"E0302\""),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn sync_writes_files_and_folders_out() {
    let t = Temp::new();
    let out = t.run(
        "import std.fs;

fun main() {
    write_file(\"state\", \"old\").unwrap();
    write_file(\"state.tmp\", \"new\").unwrap();
    println(fs.sync(\"state.tmp\"));
    println(fs.rename(\"state.tmp\", \"state\"));
    println(fs.sync(\".\"));
    println(read_file(\"state\"));
    println(fs.sync(\"missing\"));
    println(fs.sync(string_from_utf8([97, 0, 98]).unwrap()));
}
",
    );
    assert_eq!(
        out,
        "Ok(())\nOk(())\nOk(())\nOk(\"new\")\nErr(\"No such file or directory\")\nErr(\"path contains a NUL byte\")\n"
    );
    assert!(lugha::link::RUNTIME_SOURCE.contains("result = fsync(fd);"));
}
