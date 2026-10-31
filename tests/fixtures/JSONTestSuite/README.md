# JSONTestSuite fixtures

The `test_parsing/` cases of [JSONTestSuite](https://github.com/nst/JSONTestSuite),
commit **1ef36fa01286573e846ac449e8683f8833c5b26a** (2024-11-22), copied
unedited. JSONTestSuite is MIT-licensed; its licence is `LICENSE` beside this
file.

`tests/json.rs` runs them against `std.json` (milestone 137, PRP-169): every
`y_` case must parse, every `n_` case must be rejected, and every `i_` case has
its outcome pinned there. To refresh them, copy `test_parsing/` from a newer
commit and update the commit above.
