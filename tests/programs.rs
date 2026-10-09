//! Acceptance tests: every `.la` program under `tests/programs/` (PRP-001).
//!
//! The runner and its unit tests live in `tests/support/`.

mod support;

use std::path::Path;

use support::runner::{
    DEFAULT_TIMEOUT, compare, discover, format_malformed, format_report, run_case,
};

#[test]
#[ignore = "enable in milestone 1 driver PRP"]
fn programs() {
    let workdir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let lughac = Path::new(env!("CARGO_BIN_EXE_lughac"));
    let cases = match discover(&workdir.join("tests/programs")) {
        Ok(cases) => cases,
        Err(malformed) => panic!("\n{}", format_malformed(&malformed)),
    };
    let failures: Vec<_> = cases
        .iter()
        .map(|case| {
            let outcome = run_case(lughac, workdir, case, DEFAULT_TIMEOUT);
            (case.name.clone(), compare(case, &outcome))
        })
        .filter(|(_, mismatches)| !mismatches.is_empty())
        .collect();
    if !failures.is_empty() {
        panic!("\n{}", format_report(&failures, cases.len()));
    }
}
