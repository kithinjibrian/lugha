## FEATURE: `crypto.hkdf_sha256` — HKDF-SHA256 key derivation (RFC 5869) in `std.crypto`; milestone 120 (v17).

**Status:** approved and built 2026-10-18 (session 287), reviewed by the user and merged; proposed in session 286, from the user's brief: Rapidkoi derives each app's Ed25519 seed from one master secret — `hkdf_sha256(master, salt, "rapidkoi identity v1" ‖ app identity, 32)` — so one backup restores every app's key. Crypto: the user reviews it before it merges (security rule).

## OBJECTIVE
```lugha
import std.crypto;

let info = "rapidkoi identity v1".to_bytes().concat(app_id.to_bytes());
let seed = crypto.hkdf_sha256(master, salt, info, 32)?;   // Result<u8[], CryptoError>
let public = crypto.ed25519_public(seed)?;
```

## CONTEXT
- `std.crypto` (milestone 113, PRP-145): Lugha in `src/std/crypto.la` over `$os.crypto_*` built-ins (`check/float_ops.rs` `os_signature`, `codegen/os.rs` → `codegen/net.rs`), C in `runtime/crypto.c` (OpenSSL 3, `-lcrypto`, appended only for programs loading `std.crypto`); `RUNTIME_SYMBOLS` in `src/link/mod.rs`. Failures are `CryptoError.BadLength(what, wanted, given)` or `OpenSsl(reason)`; the C side wipes its copies with `OPENSSL_cleanse`.
- OpenSSL 3's `EVP_KDF_fetch(NULL, "HKDF", NULL)` with `OSSL_KDF_PARAM_DIGEST` "SHA256", `KEY`, `SALT`, `INFO` and the default mode (extract then expand) is RFC 5869 exactly.
- Related: spec v17 `std.crypto` (its table and "Not included" line), `tests/crypto.rs`.

## DECISIONS (draft decisions 245–251)
245. **`hkdf_sha256(key: u8[], salt: u8[], info: u8[], length: i64): Result<u8[], CryptoError>`**: RFC 5869 extract then expand with SHA-256; `length` output bytes.
246. **`length` from 1 to 8160 (255 × 32)**; otherwise `Err(BadLength("HKDF length (from 1 to)", 8160, length))`, checked in Lugha before any C call (as `random_bytes` does).
247. **Empty inputs**: an empty `salt` means RFC 5869's default (32 zero bytes — OpenSSL does this when the salt is empty); an empty `info` is allowed; an empty `key` is allowed too (RFC 5869 doesn't forbid it; the caller's choice).
248. **C side** (`lugha_rt_crypto_hkdf` in `runtime/crypto.c`): one `EVP_KDF_CTX`, freed on every path; output derived into a C buffer, copied into a collected array, then wiped with `OPENSSL_cleanse`; OpenSSL's own copies of the parameters are cleared when its context is freed. A failing OpenSSL call is `Err(OpenSsl(reason))` from its error queue, never a panic.
249. **Not**: HKDF's separate extract/expand steps, other digests, HMAC on its own, password hashing.
250. **Docs**: the spec's `std.crypto` table (a row; "key derivation" leaves the "Not included" line, which then names password hashing), the milestones table (120), the "Changes since v16" list; CHANGELOG.
251. **Success**: milestone 120, tag `m120` after the user's review.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `runtime/crypto.c`: `lugha_rt_crypto_hkdf(key, salt, info, length, out, error)`; `RUNTIME_SYMBOLS`.
- `$os.crypto_hkdf` in the checker and codegen (`Result<u8[], string>`).
- `src/std/crypto.la`: `hkdf_sha256` with doc comments.
### Must NOT Do
- Decision 249; `unsafe` Rust; new crates.
### New dependencies
- none (OpenSSL's `libcrypto` is already linked for `std.crypto`).

## BUILT (session 287)
- As planned. An empty key, salt or info goes to OpenSSL as a non-NULL pointer of length 0 (a NULL key would be "missing"); the output buffer is OpenSSL's (`OPENSSL_malloc`), wiped and freed with `OPENSSL_clear_free` after the copy. Tests: `tests/crypto.rs` `hkdf_sha256_matches_rfc_5869_and_checks_lengths`.

## ERROR HANDLING REQUIREMENTS
- `BadLength` for a length outside 1–8160; `OpenSsl(reason)` if OpenSSL fails; nothing panics.

## SECURITY CONSIDERATIONS
- Secrets: the C output buffer is wiped; OpenSSL clears its context on free. The derived bytes handed to Lugha live in collected memory, which Lugha can't wipe (as for `ed25519_keypair`; stated in the spec already).
- No new process, file or network access.

## TESTS TO WRITE
- [ ] `tests/crypto.rs`: RFC 5869 A.1, A.2 and A.3 (OKM byte for byte; A.3 is the empty salt and empty info case); an empty salt with a non-empty info equals an explicit 32-zero-byte salt; lengths 1, 32, 42 and 8160 give that many bytes (a shorter output is the longer one's prefix); 0 and 8161 are `BadLength` with the message `HKDF length (from 1 to): wanted 8160 bytes, got 0`.

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] The tests pass; narrow tests (`crypto`, `lazy_std`, `lsp_docs`, `--lib` for the `$os` signature), fmt, clippy; CHANGELOG; the user's review before merging.

## VALIDATION
- `cargo test --test crypto --test lazy_std --test lsp_docs`, fmt, clippy
