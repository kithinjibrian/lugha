//! Every program in the repository formats, passes verification and is
//! idempotent (decisions 63–64, milestone 80): the cases in
//! `tests/programs/` that parse, the spec's programs, `std` and the prelude.

use lugha::formatter::{FormatError, format, format_bundled};
use std::path::{Path, PathBuf};

#[path = "common/spec_programs.rs"]
mod spec_programs;

/// Formats `src` twice; the second pass must change nothing.
fn round_trip(name: &str, src: &str, bundled: bool) -> Result<(), String> {
    let run = |s: &str| {
        if bundled {
            format_bundled(s)
        } else {
            format(s)
        }
    };
    let once = match run(src) {
        Ok(out) => out,
        // Cases that test syntax errors aren't formatted.
        Err(FormatError::Syntax(_)) => return Ok(()),
        Err(e) => return Err(format!("{name}: {e}")),
    };
    let twice = run(&once).map_err(|e| format!("{name} (second pass): {e}"))?;
    if once != twice {
        return Err(format!(
            "{name}: not idempotent\n--- once\n{once}\n--- twice\n{twice}"
        ));
    }
    Ok(())
}

fn la_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let mut entries: Vec<_> = std::fs::read_dir(dir)
        .expect("tests/programs is readable")
        .map(|e| e.expect("a readable entry").path())
        .collect();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            la_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "la") {
            out.push(path);
        }
    }
}

#[test]
fn every_program_in_tests_programs_formats_verified_and_idempotent() {
    let mut files = Vec::new();
    la_files(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/programs"),
        &mut files,
    );
    assert!(files.len() > 100, "found {} files", files.len());
    let failures: Vec<String> = files
        .iter()
        .filter_map(|path| {
            let src = std::fs::read_to_string(path).ok()?;
            round_trip(&path.display().to_string(), &src, false).err()
        })
        .collect();
    assert!(
        failures.is_empty(),
        "{} failures:\n{}",
        failures.len(),
        failures.join("\n\n")
    );
}

#[test]
fn every_spec_program_formats_verified_and_idempotent() {
    let failures: Vec<String> = spec_programs::all()
        .iter()
        .filter_map(|(name, src)| round_trip(name, src, false).err())
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

#[test]
fn std_and_the_prelude_format_verified_and_idempotent() {
    let mut failures = Vec::new();
    for (name, src) in lugha::STD {
        failures.extend(round_trip(name, src, true).err());
    }
    failures.extend(round_trip("prelude", lugha::PRELUDE, true).err());
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}
