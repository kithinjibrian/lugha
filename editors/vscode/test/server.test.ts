import { test } from "node:test";
import assert from "node:assert/strict";
import { serverCommand, startFailure } from "../src/server";

test("the server is `lugha.path lsp`", () => {
    assert.deepEqual(serverCommand("/opt/lugha/lughac"), { command: "/opt/lugha/lughac", args: ["lsp"] });
});

test("an empty or missing path means lughac on PATH", () => {
    assert.deepEqual(serverCommand(undefined), { command: "lughac", args: ["lsp"] });
    assert.deepEqual(serverCommand("  "), { command: "lughac", args: ["lsp"] });
});

test("a start failure names the executable and the milestone it needs", () => {
    assert.equal(
        startFailure("lughac"),
        "`lughac lsp` didn't start (`lughac`): set `lugha.path` to a lughac from milestone 70 or later.",
    );
});
