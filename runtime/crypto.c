/*
 * crypto.c — `std.crypto`'s built-ins (v17, milestone 113): SHA-256,
 * Ed25519, random bytes, constant-time comparison and HKDF (milestone 120), through the system's
 * OpenSSL 3. Appended to the runtime only for programs that load
 * `std.crypto` (they link -lcrypto); every other program never sees it.
 *
 * Programs never see an OpenSSL object: digests, keys and signatures cross
 * as byte arrays, a running hash as a collected object whose finalizer frees
 * its `EVP_MD_CTX`. Every `EVP_PKEY` and `EVP_MD_CTX` is freed, and
 * OpenSSL's error queue read or cleared, on every path. Lengths are checked
 * here too, though `std.crypto` checks them first.
 *
 * Secrets: the seed this file makes is wiped from its stack buffer with
 * `OPENSSL_cleanse`, derived keys are wiped from their buffer, and OpenSSL clears its key objects when they are freed.
 * The bytes handed to Lugha live in collected memory, which isn't wiped.
 */
#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/core_names.h>
#include <openssl/evp.h>
#include <openssl/kdf.h>
#include <openssl/rand.h>

enum {
    CRYPTO_KEY = 32,
    CRYPTO_SIGNATURE = 64,
    CRYPTO_DIGEST = 32,
    CRYPTO_MAX_RANDOM = 1048576,
    CRYPTO_MAX_HKDF = 255 * CRYPTO_DIGEST,
};

/* OpenSSL's reason for the last failure (the queue then cleared), or `fallback`. */
static int32_t crypto_error(LughaString **error, const char *fallback) {
    unsigned long code = ERR_get_error();
    ERR_clear_error();
    if (code == 0) {
        return net_error(error, fallback);
    }
    char text[256];
    ERR_error_string_n(code, text, sizeof text);
    return net_error(error, text);
}

/* SHA-256 failing on any input is impossible; OpenSSL failing inside it
 * (memory, a broken install) ends the program, as running out of memory does. */
static void crypto_panic(void) {
    unsigned long code = ERR_get_error();
    ERR_clear_error();
    char text[300];
    char reason[256] = "unknown reason";
    if (code != 0) {
        ERR_error_string_n(code, reason, sizeof reason);
    }
    snprintf(text, sizeof text, "panic: OpenSSL failed in SHA-256: %s\n", reason);
    die(text);
}

/* `$os.crypto_sha256(bytes)`: the 32-byte digest. */
LughaArray *lugha_rt_crypto_sha256(const LughaArray *data) {
    unsigned char md[CRYPTO_DIGEST];
    unsigned int n = 0;
    if (EVP_Digest(data->elems, (size_t)data->len, md, &n, EVP_sha256(), NULL) != 1 ||
        n != CRYPTO_DIGEST) {
        crypto_panic();
    }
    return bytes_of(md, CRYPTO_DIGEST);
}

/* A running SHA-256, in collected memory; its context is OpenSSL's. */
typedef struct {
    EVP_MD_CTX *ctx;
} CryptoHasher;

/* The collector's finalizer: the hasher is unreachable, so its context goes. */
static void crypto_hasher_free(void *obj, void *data) {
    (void)data;
    CryptoHasher *h = obj;
    EVP_MD_CTX_free(h->ctx);
    h->ctx = NULL;
}

static void crypto_hasher_start(CryptoHasher *h) {
    if (EVP_DigestInit_ex(h->ctx, EVP_sha256(), NULL) != 1) {
        crypto_panic();
    }
}

/* `$os.crypto_sha256_new()`: an empty running hash. */
void *lugha_rt_crypto_sha256_new(void) {
    CryptoHasher *h = GC_MALLOC(sizeof *h);
    if (h == NULL) {
        die("panic: out of memory\n");
    }
    h->ctx = EVP_MD_CTX_new();
    if (h->ctx == NULL) {
        crypto_panic();
    }
    GC_register_finalizer(h, crypto_hasher_free, NULL, NULL, NULL);
    crypto_hasher_start(h);
    return h;
}

/* `$os.crypto_sha256_update(hasher, bytes)`. */
void lugha_rt_crypto_sha256_update(void *hasher, const LughaArray *data) {
    CryptoHasher *h = hasher;
    if (EVP_DigestUpdate(h->ctx, data->elems, (size_t)data->len) != 1) {
        crypto_panic();
    }
}

/* `$os.crypto_sha256_finish(hasher)`: the digest so far; the hasher starts over. */
LughaArray *lugha_rt_crypto_sha256_finish(void *hasher) {
    CryptoHasher *h = hasher;
    unsigned char md[CRYPTO_DIGEST];
    unsigned int n = 0;
    if (EVP_DigestFinal_ex(h->ctx, md, &n) != 1 || n != CRYPTO_DIGEST) {
        crypto_panic();
    }
    crypto_hasher_start(h);
    return bytes_of(md, CRYPTO_DIGEST);
}

/* `$os.crypto_seed()`: a 32-byte Ed25519 secret seed from OpenSSL's CSPRNG
 * for private values; the stack copy is wiped. */
int32_t lugha_rt_crypto_seed(LughaArray **out, LughaString **error) {
    unsigned char seed[CRYPTO_KEY];
    if (RAND_priv_bytes(seed, CRYPTO_KEY) != 1) {
        OPENSSL_cleanse(seed, sizeof seed);
        return crypto_error(error, "no randomness available");
    }
    *out = bytes_of(seed, CRYPTO_KEY);
    OPENSSL_cleanse(seed, sizeof seed);
    return 1;
}

/* The Ed25519 key of a 32-byte secret seed; NULL (with OpenSSL's reason
 * queued) when OpenSSL refuses it. */
static EVP_PKEY *crypto_secret(const LughaArray *secret) {
    return EVP_PKEY_new_raw_private_key(EVP_PKEY_ED25519, NULL, secret->elems, CRYPTO_KEY);
}

/* `$os.crypto_ed25519_public(secret)`: the 32-byte public key of a seed. */
int32_t lugha_rt_crypto_ed25519_public(const LughaArray *secret, LughaArray **out,
                                       LughaString **error) {
    if (secret->len != CRYPTO_KEY) {
        return net_error(error, "secret key must be 32 bytes");
    }
    ERR_clear_error();
    EVP_PKEY *key = crypto_secret(secret);
    if (key == NULL) {
        return crypto_error(error, "can't read the secret key");
    }
    unsigned char pub[CRYPTO_KEY];
    size_t len = sizeof pub;
    int ok = EVP_PKEY_get_raw_public_key(key, pub, &len) == 1 && len == CRYPTO_KEY;
    EVP_PKEY_free(key);
    if (!ok) {
        return crypto_error(error, "can't derive the public key");
    }
    *out = bytes_of(pub, CRYPTO_KEY);
    return 1;
}

/* `$os.crypto_ed25519_sign(secret, message)`: the 64-byte signature
 * (Ed25519 hashes the message itself: no digest is named). */
int32_t lugha_rt_crypto_ed25519_sign(const LughaArray *secret, const LughaArray *message,
                                     LughaArray **out, LughaString **error) {
    if (secret->len != CRYPTO_KEY) {
        return net_error(error, "secret key must be 32 bytes");
    }
    ERR_clear_error();
    EVP_PKEY *key = crypto_secret(secret);
    if (key == NULL) {
        return crypto_error(error, "can't read the secret key");
    }
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    unsigned char sig[CRYPTO_SIGNATURE];
    size_t len = sizeof sig;
    int ok = ctx != NULL && EVP_DigestSignInit(ctx, NULL, NULL, NULL, key) == 1 &&
             EVP_DigestSign(ctx, sig, &len, message->elems, (size_t)message->len) == 1 &&
             len == CRYPTO_SIGNATURE;
    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(key);
    if (!ok) {
        return crypto_error(error, "signing failed");
    }
    *out = bytes_of(sig, CRYPTO_SIGNATURE);
    return 1;
}

/* `$os.crypto_ed25519_verify(public, message, signature)`: 1 for a valid
 * signature, 0 for anything else — wrong lengths, a malformed key, a bad
 * signature, OpenSSL failing. Never an error. */
int64_t lugha_rt_crypto_ed25519_verify(const LughaArray *pub, const LughaArray *message,
                                       const LughaArray *signature) {
    if (pub->len != CRYPTO_KEY || signature->len != CRYPTO_SIGNATURE) {
        return 0;
    }
    ERR_clear_error();
    EVP_PKEY *key = EVP_PKEY_new_raw_public_key(EVP_PKEY_ED25519, NULL, pub->elems, CRYPTO_KEY);
    EVP_MD_CTX *ctx = key == NULL ? NULL : EVP_MD_CTX_new();
    int valid = ctx != NULL && EVP_DigestVerifyInit(ctx, NULL, NULL, NULL, key) == 1 &&
                EVP_DigestVerify(ctx, signature->elems, CRYPTO_SIGNATURE, message->elems,
                                 (size_t)message->len) == 1;
    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(key);
    ERR_clear_error();
    return valid;
}

/* `$os.crypto_random(n)`: `n` bytes (0 to 1 MiB) from OpenSSL's CSPRNG. */
int32_t lugha_rt_crypto_random(int64_t n, LughaArray **out, LughaString **error) {
    if (n < 0 || n > CRYPTO_MAX_RANDOM) {
        return net_error(error, "random byte count must be from 0 to 1048576");
    }
    unsigned char *buffer = lugha_rt_alloc(n > 0 ? n : 1);
    if (RAND_bytes(buffer, (int)n) != 1) {
        return crypto_error(error, "no randomness available");
    }
    *out = bytes_of(buffer, n);
    return 1;
}

/* `$os.crypto_equal(a, b)`: 1 when the arrays hold the same bytes, compared
 * in time that depends only on the length; 0 at once for different lengths. */
int64_t lugha_rt_crypto_equal(const LughaArray *a, const LughaArray *b) {
    if (a->len != b->len) {
        return 0;
    }
    return CRYPTO_memcmp(a->elems, b->elems, (size_t)a->len) == 0;
}

/* An array's bytes as an OpenSSL octet-string parameter. An empty array
 * still gets a non-NULL pointer: OpenSSL treats a NULL key as missing. */
static OSSL_PARAM crypto_octets(const char *name, const LughaArray *bytes) {
    static unsigned char none[1];
    void *data = bytes->len > 0 ? bytes->elems : none;
    return OSSL_PARAM_construct_octet_string(name, data, (size_t)bytes->len);
}

/* `$os.crypto_hkdf(key, salt, info, length)`: RFC 5869's extract then
 * expand with SHA-256 (OpenSSL's default HKDF mode), `length` bytes (1 to
 * 8160). An empty salt is RFC 5869's default, a hash-length of zeros. */
int32_t lugha_rt_crypto_hkdf(const LughaArray *key, const LughaArray *salt,
                             const LughaArray *info, int64_t length, LughaArray **out,
                             LughaString **error) {
    if (length < 1 || length > CRYPTO_MAX_HKDF) {
        return net_error(error, "HKDF length must be from 1 to 8160");
    }
    ERR_clear_error();
    EVP_KDF *kdf = EVP_KDF_fetch(NULL, "HKDF", NULL);
    EVP_KDF_CTX *ctx = kdf == NULL ? NULL : EVP_KDF_CTX_new(kdf);
    // The context holds its own reference to the algorithm.
    EVP_KDF_free(kdf);
    // OpenSSL's buffer rather than the collector's, so it can be wiped
    // (`OPENSSL_clear_free`) once copied out.
    unsigned char *okm = ctx == NULL ? NULL : OPENSSL_malloc((size_t)length);
    OSSL_PARAM params[] = {
        OSSL_PARAM_construct_utf8_string(OSSL_KDF_PARAM_DIGEST, "SHA256", 0),
        crypto_octets(OSSL_KDF_PARAM_KEY, key),
        crypto_octets(OSSL_KDF_PARAM_SALT, salt),
        crypto_octets(OSSL_KDF_PARAM_INFO, info),
        OSSL_PARAM_construct_end(),
    };
    int ok = okm != NULL && EVP_KDF_derive(ctx, okm, (size_t)length, params) == 1;
    // Freeing the context clears OpenSSL's copies of the key, salt and info.
    EVP_KDF_CTX_free(ctx);
    if (ok) {
        *out = bytes_of(okm, length);
    }
    OPENSSL_clear_free(okm, (size_t)length);
    return ok ? 1 : crypto_error(error, "key derivation failed");
}
