## FEATURE: `std.crypto` — SHA-256, Ed25519, secure random bytes and constant-time comparison over the system's OpenSSL 3; milestone 113 (v17).

**Status:** proposed 2026-10-18 (session 272), from the user's brief (session 272). **Needs the user's security review before it merges** (cryptography).

## OBJECTIVE
```lugha
import std.crypto;

let id = crypto.sha256(bundle);                        // 32 bytes

let mut h = crypto.Sha256.new();                       // a 32 MB file in pieces
for chunk of chunks { h.update(chunk); }
let digest = h.finish();

let (secret, public) = crypto.ed25519_keypair()?;      // 32-byte seed, 32-byte key
let sig = crypto.ed25519_sign(secret, manifest)?;      // 64 bytes
if crypto.ed25519_verify(public, manifest, sig) { … } // false for anything wrong
let nonce = crypto.random_bytes(16)?;
crypto.equal(digest, expected);                        // constant time
```
Rapidkoi signs app bundles with Ed25519 and names files by SHA-256 (its packaging meeting of 2026-10-18 deferred signing and the crypto library; this replaces the `lugha-sodium` idea with the OpenSSL that `std.tls` already links). Programs never see an OpenSSL object.

## CONTEXT
- `std.tls` (milestone 112, PRP-144) binds OpenSSL 3 this way: a C fragment (`runtime/tls.c`) appended to the runtime only for programs that load `std.tls` (`src/link/runtime.rs`, `Plan.tls`, `-lssl -lcrypto`, E0332 when OpenSSL is missing), reached through `$os.tls_*` built-ins (`check/float_ops.rs` `os_signature`, `codegen/net.rs`) from a Lugha std module (`src/std/tls.la`). `std.crypto` follows the same pattern: `runtime/crypto.c`, `$os.crypto_*`, `src/std/crypto.la`. Every OpenSSL call is C code compiled against the system headers, so each signature is checked by `cc` against `/usr/include/openssl` (the functions below were checked by hand too).
- Related: `src/link/{mod,runtime}.rs`, `src/driver/pipeline.rs` (`tls_import`), `src/std/tls.la`, spec standard-library section, E0332's page.

## DECISIONS (draft decisions 181–197)
181. **Module** `std.crypto`; OpenSSL linked only into programs that load it (or `std.tls`): `Plan.tls` becomes `Plan.openssl` (the import's span for E0332) with which C parts to append (`tls.c`, `crypto.c`, either or both). E0332 is reworded to "this program needs OpenSSL, which isn't installed" (its code and page stay; the page names `std.crypto` too).
182. **`sha256(bytes: u8[]): u8[]`**: the 32-byte digest (`EVP_Digest` with `EVP_sha256()`).
183. **Streaming** (the shape the brief left open): `crypto.Sha256.new(): Sha256`, `h.update(bytes: u8[])`, `h.finish(): u8[]`.
     - The running state is an `EVP_MD_CTX` in a small collected object with a Boehm finalizer (`GC_register_finalizer`) that frees it: an abandoned hasher leaks nothing, and nothing needs closing.
     - `finish` gives the digest of everything updated so far and starts the hasher over (so it can hash the next input).
     - Copies of a `Sha256` share one running hash (as `Ref` copies share a box): documented.
184. **SHA-256 never fails on input.** OpenSSL failing inside a digest (out of memory, a broken install) panics with OpenSSL's reason, as running out of memory does elsewhere; no input can cause it.
185. **Ed25519** (RFC 8032), keys as raw bytes: a 32-byte secret seed, a 32-byte public key, a 64-byte signature.
     - `ed25519_keypair(): Result<(u8[], u8[]), CryptoError>`: a seed from `RAND_priv_bytes`, its public key derived as below.
     - `ed25519_public(secret: u8[]): Result<u8[], CryptoError>`: `EVP_PKEY_new_raw_private_key(EVP_PKEY_ED25519, NULL, seed, 32)`, then `EVP_PKEY_get_raw_public_key`.
     - `ed25519_sign(secret: u8[], message: u8[]): Result<u8[], CryptoError>`: `EVP_DigestSignInit(ctx, NULL, NULL, NULL, pkey)` and the one-shot `EVP_DigestSign` (NULL digest: Ed25519 hashes internally).
     - `ed25519_verify(public: u8[], message: u8[], signature: u8[]): bool`: `EVP_PKEY_new_raw_public_key`, `EVP_DigestVerifyInit`, `EVP_DigestVerify` == 1. False for a wrong length (checked before OpenSSL sees it), a malformed key, a bad signature or any OpenSSL failure; never `Err`, never a panic.
186. **`random_bytes(n: i64): Result<u8[], CryptoError>`**: `RAND_bytes`; `n` from 0 to 1048576, else `BadLength`.
187. **`equal(a: u8[], b: u8[]): bool`**: `CRYPTO_memcmp` over the bytes; false at once for different lengths (only the length can be learnt from the time).
188. **`CryptoError`**: `enum CryptoError { BadLength(string, i64, i64), OpenSsl(string) }` — what had the wrong length, the length wanted, the length given (`BadLength("secret key", 32, 31)`); OpenSSL's reason from `ERR_get_error`/`ERR_error_string_n`. `e.message(): string` reads it as text.
189. **Nothing panics on input** (184's internal failure aside). Lengths are checked in Lugha before any C call; the C side checks them again.
190. **Cleanup on every path:** every `EVP_PKEY`, `EVP_MD_CTX` is freed on success and failure; the OpenSSL error queue is read or cleared (`ERR_clear_error`) before returning.
191. **Secrets:** the C side wipes its own copies with `OPENSSL_cleanse` — the generated seed's stack buffer; OpenSSL clears its key object when freed. The secret seed handed to Lugha, and any copy a program makes, live in collected memory, which Lugha can't wipe yet: the spec and the module's docs say exactly that and no more.
192. **No new dependency:** OpenSSL 3 (`libcrypto`), already linked for `std.tls`.
193. **Built-ins** (bundled sources only): `$os.crypto_sha256`, `crypto_sha256_new`, `crypto_sha256_update`, `crypto_sha256_finish`, `crypto_seed`, `crypto_ed25519_public`, `crypto_ed25519_sign`, `crypto_ed25519_verify`, `crypto_random`, `crypto_equal`; runtime `lugha_rt_crypto_*` in `runtime/crypto.c`.
194. **Docs:** the spec's standard-library section (`std.crypto`), the milestones table (113), the "Changes since v16" list, E0332's row and page, CHANGELOG, `CLAUDE.md`'s tree.
195. **Not** (Must NOT Do): encryption (AES, ChaCha20), key exchange (X25519), key derivation and password hashing (HKDF, PBKDF2, scrypt, Argon2), certificates, any algorithm but SHA-256 and Ed25519, HMAC; any change to `std.tls` beyond sharing its OpenSSL linking.
196. **Testing against standards:** the vectors below are checked bit for bit.
197. **Success:** milestone 113, tag `m113`, after the user's security review.

## IMPLEMENTATION REQUIREMENTS
### Must Do
- `runtime/crypto.c` (its own `#include <openssl/evp.h>`, `<openssl/rand.h>`, `<openssl/crypto.h>`, `<openssl/err.h>`), the OpenSSL calls in decisions 182–187 exactly as the headers declare them:
  - `EVP_MD_CTX *EVP_MD_CTX_new(void)`, `void EVP_MD_CTX_free(EVP_MD_CTX *)`, `int EVP_DigestInit_ex(EVP_MD_CTX *, const EVP_MD *, ENGINE *)`, `int EVP_DigestUpdate(EVP_MD_CTX *, const void *, size_t)`, `int EVP_DigestFinal_ex(EVP_MD_CTX *, unsigned char *, unsigned int *)`, `int EVP_Digest(const void *, size_t, unsigned char *, unsigned int *, const EVP_MD *, ENGINE *)`, `const EVP_MD *EVP_sha256(void)`
  - `EVP_PKEY *EVP_PKEY_new_raw_private_key(int, ENGINE *, const unsigned char *, size_t)`, `EVP_PKEY *EVP_PKEY_new_raw_public_key(int, ENGINE *, const unsigned char *, size_t)`, `int EVP_PKEY_get_raw_public_key(const EVP_PKEY *, unsigned char *, size_t *)`, `void EVP_PKEY_free(EVP_PKEY *)`, `EVP_PKEY_ED25519` (`NID_ED25519`)
  - `int EVP_DigestSignInit(EVP_MD_CTX *, EVP_PKEY_CTX **, const EVP_MD *, ENGINE *, EVP_PKEY *)`, `int EVP_DigestSign(EVP_MD_CTX *, unsigned char *, size_t *, const unsigned char *, size_t)`, `int EVP_DigestVerifyInit(…same…)`, `int EVP_DigestVerify(EVP_MD_CTX *, const unsigned char *, size_t, const unsigned char *, size_t)`
  - `int RAND_bytes(unsigned char *, int)`, `int RAND_priv_bytes(unsigned char *, int)`, `int CRYPTO_memcmp(const void *, const void *, size_t)`, `void OPENSSL_cleanse(void *, size_t)`, `unsigned long ERR_get_error(void)`, `void ERR_error_string_n(unsigned long, char *, size_t)`, `void ERR_clear_error(void)`
  - `GC_register_finalizer` (Boehm) for the hasher's context.
- Link: `Plan.openssl` with the parts to append; `runtime.rs` appends `crypto.c`; the driver's detection covers `std.crypto`; E0332 reworded.
- Checker/codegen: `$os.crypto_*` signatures and lowering (as `$os.tls_*`).
- `src/std/crypto.la` with doc comments (the secrets caveat on the module).
### Must NOT Do
- Decision 195; exposing any OpenSSL object or pointer to programs; claiming Lugha-side secrets are wiped.
### New dependencies
- none (OpenSSL 3's `libcrypto`, already linked by `std.tls`).

## ERROR HANDLING REQUIREMENTS
- `CryptoError.BadLength(what, wanted, given)`: a secret seed or public key not 32 bytes (sign, public), `random_bytes` outside 0 … 1048576.
- `CryptoError.OpenSsl(reason)`: OpenSSL failing in keypair, public, sign, random.
- `ed25519_verify` and `equal` return `false`, never an error.
- Build: E0332 when OpenSSL is missing.

## SECURITY CONSIDERATIONS
- Ed25519 via OpenSSL's EVP one-shot interface only; no hand-written primitives.
- Randomness only from OpenSSL's CSPRNG (`RAND_priv_bytes` for seeds, `RAND_bytes` for `random_bytes`).
- Comparison of secrets in constant time (`equal`); `ed25519_verify` uses OpenSSL's own comparison.
- C-side secret copies wiped (`OPENSSL_cleanse`); OpenSSL's key objects freed (which clears them); Lugha-side copies documented as not wiped.
- Every OpenSSL object freed and the error queue cleared on every path; no input reaches a panic.
- Needs the user's security review before merging.

## TESTS TO WRITE
- [ ] SHA-256 (FIPS 180-4 / NIST examples): `""` → `e3b0c442…b855`; `"abc"` → `ba7816bf…15ad`; the 448-bit `"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"` → `248d6a61…06c1`; one million `"a"` → `cdc76e5c…c7c0`; streaming equals one-shot when the input is split at every offset of a short message and at chunk sizes 1, 63, 64, 65 and 4096 over the million `"a"`; `finish` restarts the hasher; an abandoned hasher is collected (no leak checked by a loop of 100 000 hashers in bounded memory).
- [ ] Ed25519 (RFC 8032 §7.1, TEST 1, 2, 3 and TEST SHA(abc)): the public key from each secret; `sign` gives the exact signature; `verify` accepts it; `verify` rejects a changed message, a changed signature byte, the wrong public key, and wrong lengths of the key (31, 33, 0) and of the signature (63, 65, 0); a malformed public key (not on the curve) is `false`.
- [ ] `sign`/`public` with a 31- or 33-byte secret → `BadLength("secret key", 32, n)`.
- [ ] `keypair`: two calls give different seeds and keys; the public key equals `ed25519_public(secret)`; sign/verify round trip.
- [ ] `random_bytes`: lengths 0, 1, 32, 1048576; two 32-byte draws differ; 1048577 and -1 → `BadLength`.
- [ ] `equal`: equal, one byte different, different lengths, both empty.
- [ ] Link: a `std.crypto` program gets `crypto.c` and `-lcrypto` (and not `tls.c` unless it loads `std.tls`); a program with neither gets no OpenSSL (unit tests on the plan and runtime file).
- [ ] `explain` (E0332's page), `programs` (E0312's list gains `std.crypto`).

## ROLLBACK PLAN
- `main`; revert the merge.

## ACCEPTANCE CRITERIA
- [ ] Every vector matches; narrow tests (`crypto`, `tls`, link and lib units, `explain`, `programs`), fmt, clippy; CHANGELOG; the user's security review before merging.

## VALIDATION
- `cargo test --test crypto --test tls --test explain --test programs`, `cargo test --lib -- link`, fmt, clippy
