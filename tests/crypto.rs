//! `std.crypto` end to end (v17, milestone 113, PRP-145): the FIPS 180-4
//! SHA-256 examples and the RFC 8032 §7.1 Ed25519 vectors, bit for bit,
//! through Lugha programs; random bytes, constant-time equality, every
//! rejection, and abandoned hashers freed by the collector.

use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Hex helpers every program here uses.
const HEX: &str = "import std.crypto;

fun hex(bytes: u8[]): string {
    let digits = \"0123456789abcdef\";
    let mut out = \"\";
    for b of bytes {
        let hi = (b / 16) as i64;
        let lo = (b % 16) as i64;
        out = out + digits.slice(hi, hi + 1) + digits.slice(lo, lo + 1);
    }
    out
}

fun nibble(c: u8): u8 = if c >= 97 { c - 87 } else { c - 48 };

fun unhex(s: string): u8[] {
    let raw = s.to_bytes();
    let mut out: u8[] = [];
    let mut i = 0;
    while i + 1 < raw.len {
        out.push(nibble(raw[i]) * 16 + nibble(raw[i + 1]));
        i += 2;
    }
    out
}
";

/// Runs `main` (with the helpers) and returns its stdout, expecting exit 0.
fn run(main: &str) -> String {
    static NEXT: AtomicUsize = AtomicUsize::new(0);
    let n = NEXT.fetch_add(1, Ordering::Relaxed);
    let dir: PathBuf =
        std::env::temp_dir().join(format!("lugha-crypto-{}-{n}", std::process::id()));
    // A leftover from an earlier run would change what the test sees.
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("main.la"), format!("{HEX}\n{main}")).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_lughac"))
        .args(["run", "main.la"])
        .current_dir(&dir)
        .env("HOME", &dir)
        .output()
        .expect("lughac runs");
    // Best effort: a leftover temp dir must not mask the real failure.
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn sha256_matches_the_fips_examples_streamed_or_not() {
    let out = run("fun main() {
    println(hex(crypto.sha256([])));
    println(hex(crypto.sha256(\"abc\".to_bytes())));
    let long = \"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq\".to_bytes();
    println(hex(crypto.sha256(long)));
    let million = [97 as u8; 1000000];
    let whole = crypto.sha256(million);
    println(hex(whole));
    // Streaming equals one-shot at every split of a short message…
    let mut every = true;
    for cut in 0..long.len + 1 {
        let h = crypto.Sha256.new();
        let mut first: u8[] = [];
        let mut second: u8[] = [];
        for i in 0..long.len {
            if i < cut { first.push(long[i]); } else { second.push(long[i]); }
        }
        h.update(first);
        h.update(second);
        every = every && crypto.equal(h.finish(), crypto.sha256(long));
    }
    println(every);
    // …and at block-edge chunk sizes over the million `a`.
    for size of [1, 63, 64, 65, 4096] {
        let h = crypto.Sha256.new();
        let chunk = [97 as u8; size];
        let mut fed = 0;
        while fed + size <= 1000000 {
            h.update(chunk);
            fed += size;
        }
        h.update([97 as u8; 1000000 - fed]);
        println(crypto.equal(h.finish(), whole));
    }
    // `finish` starts the hash over.
    let h = crypto.Sha256.new();
    h.update(\"abc\".to_bytes());
    let _ = h.finish();
    println(hex(h.finish()));
}
");
    assert_eq!(
        out,
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\n\
         ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\n\
         248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1\n\
         cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0\n\
         true\ntrue\ntrue\ntrue\ntrue\ntrue\n\
         e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\n"
    );
}

/// RFC 8032 §7.1: TEST 1, 2, 3 and TEST SHA(abc) — secret, public, message, signature.
const VECTORS: [(&str, &str, &str, &str); 4] = [
    (
        "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60",
        "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a",
        "",
        "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b",
    ),
    (
        "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb",
        "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c",
        "72",
        "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00",
    ),
    (
        "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7",
        "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025",
        "af82",
        "6291d657deec24024827e69c3abe01a30ce548a284743a445e3680d7db5ac3ac18ff9b538d16f290ae67f760984dc6594a7c15e9716ed28dc027beceea1ec40a",
    ),
    (
        "833fe62409237b9d62ec77587520911e9a759cec1d19755b7da901b96dca3d42",
        "ec172b93ad5e563bf4932c70e1245034c35467ef2efd4d64ebf819683467e2bf",
        "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f",
        "dc2a4459e7369633a52b1bf277839a00201009a3efbf3ecb69bea2186c26b58909351fc9ac90b3ecfdfbc7c66431e0303dca179c138ac17ad9bef1177331a704",
    ),
];

#[test]
fn ed25519_matches_rfc_8032_and_rejects_everything_wrong() {
    for (secret, public, message, signature) in VECTORS {
        let out = run(&format!(
            "fun flip(xs: u8[], at: i64): u8[] {{
    let mut ys = xs;
    ys[at] = ys[at] ^ 1;
    ys
}}

fun cut(xs: u8[], n: i64): u8[] {{
    let mut ys: u8[] = [];
    for i in 0..n {{ ys.push(xs[i]); }}
    ys
}}

fun main() {{
    let secret = unhex(\"{secret}\");
    let key = unhex(\"{public}\");
    let message = unhex(\"{message}\");
    let sig = unhex(\"{signature}\");
    println(hex(crypto.ed25519_public(secret).unwrap()));
    println(hex(crypto.ed25519_sign(secret, message).unwrap()));
    println(crypto.ed25519_verify(key, message, sig));
    let mut longer = message;
    longer.push(0);
    println(crypto.ed25519_verify(key, longer, sig));
    println(crypto.ed25519_verify(key, message, flip(sig, 0)));
    println(crypto.ed25519_verify(key, message, flip(sig, 63)));
    println(crypto.ed25519_verify(flip(key, 0), message, sig));
    let mut long_key = key;
    long_key.push(0);
    let mut long_sig = sig;
    long_sig.push(0);
    println(crypto.ed25519_verify(cut(key, 31), message, sig) || crypto.ed25519_verify(long_key, message, sig) || crypto.ed25519_verify([], message, sig));
    println(crypto.ed25519_verify(key, message, cut(sig, 63)) || crypto.ed25519_verify(key, message, long_sig) || crypto.ed25519_verify(key, message, []));
    println(crypto.ed25519_verify([255 as u8; 32], message, sig));
    println(crypto.ed25519_sign(cut(secret, 31), message));
    let mut long_secret = secret;
    long_secret.push(0);
    println(crypto.ed25519_public(long_secret).map_err(fun(e: crypto.CryptoError): string = e.message()));
}}
"
        ));
        assert_eq!(
            out,
            format!(
                "{public}\n{signature}\ntrue\nfalse\nfalse\nfalse\nfalse\nfalse\nfalse\nfalse\n\
                 Err(BadLength(\"secret key\", 32, 31))\nErr(\"secret key: wanted 32 bytes, got 33\")\n"
            ),
            "{secret}"
        );
    }
}

#[test]
fn keypairs_random_bytes_and_equality() {
    let out = run("fun main() {
    let (s1, k1) = crypto.ed25519_keypair().unwrap();
    let (s2, k2) = crypto.ed25519_keypair().unwrap();
    println(\"{s1.len} {k1.len} {!crypto.equal(s1, s2)} {!crypto.equal(k1, k2)}\");
    println(crypto.equal(crypto.ed25519_public(s1).unwrap(), k1));
    let sig = crypto.ed25519_sign(s1, \"hello\".to_bytes()).unwrap();
    println(\"{sig.len} {crypto.ed25519_verify(k1, \"hello\".to_bytes(), sig)} {crypto.ed25519_verify(k2, \"hello\".to_bytes(), sig)}\");
    for n of [0, 1, 32, 1048576] {
        print(crypto.random_bytes(n).unwrap().len);
        print(\" \");
    }
    println();
    println(!crypto.equal(crypto.random_bytes(32).unwrap(), crypto.random_bytes(32).unwrap()));
    println(crypto.random_bytes(1048577));
    println(crypto.random_bytes(-1).is_err());
    println(\"{crypto.equal([1, 2, 3], [1, 2, 3])} {crypto.equal([1, 2, 3], [1, 2, 4])} {crypto.equal([1, 2], [1, 2, 3])} {crypto.equal([], [])}\");
}
");
    assert_eq!(
        out,
        "32 32 true true\ntrue\n64 true false\n0 1 32 1048576 \ntrue\n\
         Err(BadLength(\"random bytes (at most)\", 1048576, 1048577))\ntrue\ntrue false false true\n"
    );
}

#[test]
fn abandoned_hashers_are_freed() {
    // Each hasher holds an OpenSSL context outside the collector; without
    // the finalizer, 300 000 of them would hold well over 50 MB.
    let out = run("fun rss_kb(): i64 {
    let status = read_file(\"/proc/self/status\").unwrap_or(\"\");
    for line of status.lines() {
        if line.starts_with(\"VmRSS:\") {
            let digits = line.slice(6, line.len).trim().split(\" \")[0];
            return parse_i64(digits).unwrap_or(0);
        }
    }
    0
}

fun main() {
    for _ in 0..300000 {
        let h = crypto.Sha256.new();
        h.update([1, 2, 3]);
    }
    println(rss_kb() < 50000);
}
");
    assert_eq!(out, "true\n");
}

#[test]
fn hkdf_sha256_matches_rfc_5869_and_checks_lengths() {
    let out = run("fun span(from: i64, to: i64): u8[] {
    let mut out: u8[] = [];
    for i in from..to {
        out.push(i as u8);
    }
    out
}

fun derive(key: u8[], salt: u8[], info: u8[], length: i64): string {
    match crypto.hkdf_sha256(key, salt, info, length) {
        Ok(okm) => hex(okm),
        Err(e) => e.message(),
    }
}

fun main() {
    let ikm = [11 as u8; 22];
    println(derive(ikm, span(0, 13), span(240, 250), 42));
    println(derive(span(0, 80), span(96, 176), span(176, 256), 82));
    println(derive(ikm, [], [], 42));
    println(derive(ikm, [], span(240, 250), 32) == derive(ikm, [0 as u8; 32], span(240, 250), 32));
    let longest = crypto.hkdf_sha256(ikm, [], [], 8160).unwrap();
    for n of [1, 32, 42, 8160] {
        let okm = crypto.hkdf_sha256(ikm, [], [], n).unwrap();
        println(\"{okm.len} {hex(okm) == hex(longest).slice(0, 2 * n)}\");
    }
    println(derive(ikm, [], [], 0));
    println(derive(ikm, [], [], 8161));
}
");
    assert_eq!(
        out,
        "3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865\n\
         b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c\
         59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71\
         cc30c58179ec3e87c14c01d5c1f3434f1d87\n\
         8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8\n\
         true\n1 true\n32 true\n42 true\n8160 true\n\
         HKDF length (from 1 to): wanted 8160 bytes, got 0\n\
         HKDF length (from 1 to): wanted 8160 bytes, got 8161\n"
    );
}
