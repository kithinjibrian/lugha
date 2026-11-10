//! lughac's stdout, remembering whether a write failed so the exit code can say so.
//!
//! Doesn't decide the exit code: `driver::run` does, from what this remembers.

use std::io::{self, Write};

/// A writer that passes everything to `inner` and keeps the first error that matters.
///
/// A broken pipe doesn't matter: a reader that stops early (`lughac spec | head`) got what it
/// wanted.
pub(super) struct Watched<'a> {
    inner: &'a mut dyn Write,
    failed: Option<io::Error>,
}

impl<'a> Watched<'a> {
    /// Wraps `inner`, with nothing failed yet.
    pub(super) fn new(inner: &'a mut dyn Write) -> Self {
        Watched {
            inner,
            failed: None,
        }
    }

    /// Flushes what is left and returns the first write error that matters, if any.
    pub(super) fn finish(mut self) -> Option<io::Error> {
        // Its error, if any, is kept by `flush` itself.
        let _ = self.flush();
        self.failed
    }

    fn keep<T>(&mut self, result: io::Result<T>) -> io::Result<T> {
        if let Err(error) = &result
            && error.kind() != io::ErrorKind::BrokenPipe
            && self.failed.is_none()
        {
            self.failed = Some(io::Error::new(error.kind(), error.to_string()));
        }
        result
    }
}

impl Write for Watched<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let result = self.inner.write(buf);
        self.keep(result)
    }

    fn flush(&mut self) -> io::Result<()> {
        let result = self.inner.flush();
        self.keep(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A writer whose every write and flush fails with `kind`.
    struct Failing(io::ErrorKind);

    impl Write for Failing {
        fn write(&mut self, _: &[u8]) -> io::Result<usize> {
            Err(self.0.into())
        }

        fn flush(&mut self) -> io::Result<()> {
            Err(self.0.into())
        }
    }

    #[test]
    fn writes_pass_through_and_nothing_fails() {
        let mut out = Vec::new();
        let mut watched = Watched::new(&mut out);
        assert!(watched.write_all(b"hi").is_ok());
        assert!(watched.finish().is_none());
        assert_eq!(out, b"hi");
    }

    #[test]
    fn a_failed_write_is_kept() {
        let mut full = Failing(io::ErrorKind::StorageFull);
        let mut watched = Watched::new(&mut full);
        assert!(watched.write_all(b"hi").is_err());
        let failed = watched.finish().map(|e| e.kind());
        assert_eq!(failed, Some(io::ErrorKind::StorageFull));
    }

    #[test]
    fn a_broken_pipe_is_not_a_failure() {
        let mut closed = Failing(io::ErrorKind::BrokenPipe);
        let mut watched = Watched::new(&mut closed);
        assert!(watched.write_all(b"hi").is_err());
        assert!(watched.finish().is_none());
    }
}
