//! The runtime source each program is linked with, written to a temp file:
//! [`super::RUNTIME_SOURCE`], then [`TLS_SOURCE`] for a program that loads
//! `std.tls` (v17, milestone 112, linking `-lssl -lcrypto`) and
//! [`CRYPTO_SOURCE`] for one that loads `std.crypto` (milestone 113,
//! `-lcrypto`).

use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

/// `runtime/tls.c`: `std.tls` over OpenSSL, only for programs that use it.
pub const TLS_SOURCE: &str = include_str!("../../runtime/tls.c");

/// Every function codegen calls in [`TLS_SOURCE`].
pub const TLS_SYMBOLS: [&str; 9] = [
    "lugha_rt_tls_connect",
    "lugha_rt_tls_listen",
    "lugha_rt_tls_accept",
    "lugha_rt_tls_read",
    "lugha_rt_tls_write",
    "lugha_rt_tls_fd",
    "lugha_rt_tls_close",
    "lugha_rt_tls_close_listener",
    "lugha_rt_tls_listener_fd",
];

/// `runtime/crypto.c`: `std.crypto` over OpenSSL, only for programs that use it.
pub const CRYPTO_SOURCE: &str = include_str!("../../runtime/crypto.c");

/// Every function codegen calls in [`CRYPTO_SOURCE`].
pub const CRYPTO_SYMBOLS: [&str; 10] = [
    "lugha_rt_crypto_sha256",
    "lugha_rt_crypto_sha256_new",
    "lugha_rt_crypto_sha256_update",
    "lugha_rt_crypto_sha256_finish",
    "lugha_rt_crypto_seed",
    "lugha_rt_crypto_ed25519_public",
    "lugha_rt_crypto_ed25519_sign",
    "lugha_rt_crypto_ed25519_verify",
    "lugha_rt_crypto_random",
    "lugha_rt_crypto_equal",
];

/// True if `cc`'s complaint is OpenSSL's absence: its headers or libraries.
pub fn missing_openssl(stderr: &str) -> bool {
    stderr.contains("openssl/") || stderr.contains("-lssl") || stderr.contains("-lcrypto")
}

/// The runtime source in a private temp file, removed on drop.
pub(super) struct RuntimeFile(pub PathBuf);

impl RuntimeFile {
    /// Writes the runtime, with TLS's part when `tls` and crypto's when `crypto`.
    pub(super) fn write(tls: bool, crypto: bool) -> io::Result<Self> {
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let n = NEXT.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join(format!("lughac-rt-{}-{n}.c", std::process::id()));
        // create_new: fail rather than reuse a file someone planted under that name.
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)?;
        io::Write::write_all(&mut file, super::RUNTIME_SOURCE.as_bytes())?;
        if tls {
            io::Write::write_all(&mut file, TLS_SOURCE.as_bytes())?;
        }
        if crypto {
            io::Write::write_all(&mut file, CRYPTO_SOURCE.as_bytes())?;
        }
        Ok(RuntimeFile(path))
    }
}

impl Drop for RuntimeFile {
    fn drop(&mut self) {
        // Best effort: a leftover temp file is harmless and must not hide the result.
        let _ = std::fs::remove_file(&self.0);
    }
}

#[cfg(test)]
mod tests {
    use super::{
        CRYPTO_SOURCE, CRYPTO_SYMBOLS, RuntimeFile, TLS_SOURCE, TLS_SYMBOLS, missing_openssl,
    };

    #[test]
    fn the_tls_part_defines_its_symbols_and_only_tls_programs_get_it() {
        for name in TLS_SYMBOLS {
            assert!(
                TLS_SOURCE.contains(&format!("{name}(")),
                "tls.c lacks {name}"
            );
        }
        assert!(!super::super::RUNTIME_SOURCE.contains("openssl"));
        for name in CRYPTO_SYMBOLS {
            assert!(
                CRYPTO_SOURCE.contains(&format!("{name}(")),
                "crypto.c lacks {name}"
            );
        }
        let read = |tls, crypto| {
            let file = RuntimeFile::write(tls, crypto).unwrap();
            std::fs::read_to_string(&file.0).unwrap()
        };
        assert!(!read(false, false).contains("openssl"));
        let tls = read(true, false);
        assert!(tls.contains("#include <openssl/ssl.h>") && !tls.contains("openssl/rand.h"));
        let crypto = read(false, true);
        assert!(crypto.contains("#include <openssl/rand.h>") && !crypto.contains("openssl/ssl.h"));
    }

    #[test]
    fn openssl_missing_is_recognised() {
        assert!(missing_openssl(
            "fatal error: openssl/ssl.h: No such file or directory"
        ));
        assert!(missing_openssl(
            "/usr/bin/ld: cannot find -lssl: No such file"
        ));
        assert!(!missing_openssl("undefined reference to `foo'"));
    }

    #[test]
    fn only_a_tls_plan_links_openssl() {
        use std::path::Path;
        let mut plan = super::super::Plan::default();
        let has_ssl = |plan: &super::super::Plan| {
            super::super::args(&[], &[], Path::new("rt.c"), plan, Path::new("out"))
                .iter()
                .any(|a| a == "-lssl")
        };
        assert!(!has_ssl(&plan));
        plan.crypto = Some(crate::span::Span::new(0, 1));
        let args = super::super::args(&[], &[], Path::new("rt.c"), &plan, Path::new("out"));
        assert!(args.iter().any(|a| a == "-lcrypto") && !has_ssl(&plan));
        plan.tls = Some(crate::span::Span::new(0, 1));
        assert!(has_ssl(&plan));
        let d = super::super::LinkError::Tls {
            span: crate::span::Span::new(0, 1),
        }
        .diagnostic();
        assert_eq!(d.map(|d| d.code), Some("E0332"));
    }
}
