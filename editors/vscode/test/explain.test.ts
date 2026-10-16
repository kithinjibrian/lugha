import { test } from "node:test";
import assert from "node:assert/strict";
import { codeOf, explainArgs, explainFailure } from "../src/explain";

test("a lugha-explain path names one code, and nothing else gets through", () => {
    assert.equal(codeOf("E0501.md"), "E0501");
    assert.equal(codeOf("W0103.md"), "W0103");
    assert.equal(codeOf("e0501.md"), undefined);
    assert.equal(codeOf("E0501; rm -rf /.md"), undefined);
    assert.equal(codeOf("../E0501.md"), undefined);
    assert.equal(codeOf("E05011.md"), undefined);
    assert.equal(codeOf(""), undefined);
});

test("explaining runs `lugha.path explain CODE`", () => {
    assert.deepEqual(explainArgs("E0501"), ["explain", "E0501"]);
});

test("a failure names the executable and the milestone it needs", () => {
    assert.equal(
        explainFailure("lughac", "E0501"),
        "# E0501\n\n`lughac explain E0501` didn't run (`lughac`): set `lugha.path` to a lughac from milestone 86 or later.\n",
    );
});
