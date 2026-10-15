//! Internal compiler errors (PRP-024, spec §9): a panic inside lughac is a
//! compiler bug. It is reported as one internal error in the chosen
//! diagnostics format, and lughac exits with 2, never 101.

use std::any::Any;
use std::io::Write;
use std::panic::{self, AssertUnwindSafe};
use std::sync::Mutex;

use super::Format;
use super::json;
use super::render::{self, Report};
use super::source::Source;

/// Where the last panic happened in lughac's source, set by the hook.
static LOCATION: Mutex<Option<String>> = Mutex::new(None);

/// Replaces Rust's panic message with a hook that only records the panic's
/// location: `guarded` reports it instead. Installed once, by the binary.
pub(super) fn install_hook() {
    panic::set_hook(Box::new(|info| {
        if let (Some(at), Ok(mut slot)) = (info.location(), LOCATION.lock()) {
            *slot = Some(format!("{}:{}", at.file(), at.line()));
        }
    }));
}

/// Runs `work`; if it panics, writes one internal error about `file` to
/// `stderr` in `format` and returns 2.
pub(super) fn guarded(
    format: Format,
    file: Option<&str>,
    stderr: &mut dyn Write,
    work: impl FnOnce() -> u8,
) -> u8 {
    // The compiler's state is dropped after a panic, so nothing can observe
    // it half-updated: unwind safety holds.
    match panic::catch_unwind(AssertUnwindSafe(work)) {
        Ok(code) => code,
        Err(payload) => {
            let mut report = Report::internal(
                format!("internal compiler error: {}", message(&*payload)),
                None,
            );
            report.help = Some(help());
            let source = Source::single(file.unwrap_or_default(), "");
            let text = match format {
                Format::Human => render::human(&report, &source),
                Format::Json => json::line(&report, &source),
            };
            // If even this write fails, the exit code still reports the bug.
            let _ = stderr.write_all(text.as_bytes());
            2
        }
    }
}

/// The text of a panic payload: `panic!` gives a `&str` or a `String`.
/// The help line for a caught panic, naming where it happened when the hook
/// recorded it; taking the location clears it for the next panic.
pub(super) fn help() -> String {
    match LOCATION.lock().ok().and_then(|mut slot| slot.take()) {
        Some(at) => format!("this is a bug in lughac ({at}); please report it"),
        None => "this is a bug in lughac; please report it".to_string(),
    }
}

/// A panic's message, from its payload.
pub(super) fn message(payload: &(dyn Any + Send)) -> String {
    if let Some(text) = payload.downcast_ref::<&str>() {
        (*text).to_string()
    } else if let Some(text) = payload.downcast_ref::<String>() {
        text.clone()
    } else {
        "unknown panic".to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::{Format, guarded};

    fn run(format: Format, work: impl FnOnce() -> u8) -> (u8, String) {
        let mut err = Vec::new();
        let code = guarded(format, Some("prog.la"), &mut err, work);
        (code, String::from_utf8(err).expect("UTF-8"))
    }

    #[test]
    fn a_panic_is_one_internal_error_with_exit_2() {
        let (code, err) = run(Format::Human, || panic!("boom"));
        assert_eq!(code, 2);
        assert!(
            err.starts_with("error: internal compiler error: boom\n"),
            "{err}"
        );
        assert!(err.contains("this is a bug in lughac"), "{err}");
        let (_, err) = run(Format::Human, || panic!("{} went wrong", 2 + 2));
        assert!(
            err.starts_with("error: internal compiler error: 4 went wrong\n"),
            "{err}"
        );
        let (_, err) = run(Format::Human, || std::panic::panic_any(5));
        assert!(
            err.starts_with("error: internal compiler error: unknown panic\n"),
            "{err}"
        );
    }

    #[test]
    fn json_gets_a_single_null_code_line() {
        let (code, err) = run(Format::Json, || panic!("boom"));
        assert_eq!(code, 2);
        assert_eq!(err.lines().count(), 1, "{err}");
        assert!(err.starts_with(r#"{"severity":"error","code":null,"message":"internal compiler error: boom","file":"prog.la","span":null,"#), "{err}");
    }

    #[test]
    fn normal_exits_pass_through_silently() {
        for expected in [0, 1, 2] {
            assert_eq!(run(Format::Human, || expected), (expected, String::new()));
        }
    }
}
