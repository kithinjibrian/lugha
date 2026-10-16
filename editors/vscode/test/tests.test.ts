import { test } from "node:test";
import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";
import { listArgs, outcome, parseListed, parseResult, runArgs, testsStartFailure } from "../src/tests";

test("a listed test is read from its JSON line, and garbage is not", () => {
    assert.deepEqual(parseListed('{"test":"src.geo.area","file":"src/geo.la","line":5,"col":5}'), {
        name: "src.geo.area",
        file: "src/geo.la",
        line: 5,
        col: 5,
    });
    assert.equal(parseListed("test geo.area geo.la:5:5"), undefined);
    assert.equal(parseListed('{"test":3}'), undefined);
    assert.equal(parseListed(""), undefined);
});

test("results are read, the summary is not a result", () => {
    const line =
        '{"test":"geo.doubles","file":"geo.la","line":11,"status":"failed","message":"assert_eq failed: left 4, right 5","at":{"file":"geo.la","line":13,"col":5},"output":"doubling 2\\n","ms":4}';
    assert.deepEqual(parseResult(line), {
        name: "geo.doubles",
        file: "geo.la",
        line: 11,
        status: "failed",
        message: "assert_eq failed: left 4, right 5",
        at: { file: "geo.la", line: 13, col: 5 },
        output: "doubling 2\n",
        ms: 4,
    });
    assert.equal(parseResult('{"summary":{"passed":1,"failed":1}}'), undefined);
    assert.equal(parseResult('{"test":"a","file":"a.la","line":1,"status":"maybe","output":"","ms":1}'), undefined);
});

test("arguments: list, one test, a file's tests, everything", () => {
    assert.deepEqual(listArgs(), ["test", "--list", "--json"]);
    assert.deepEqual(runArgs(["geo.doubles"], ["geo.la"]), ["test", "--json", "--exact", "--filter", "geo.doubles", "geo.la"]);
    assert.deepEqual(runArgs(undefined, ["geo.la"]), ["test", "--json", "geo.la"]);
    assert.deepEqual(runArgs(undefined, []), ["test", "--json"]);
});

test("a result becomes a status, a message at its zero-based place, and output", () => {
    const failed = outcome({
        name: "geo.doubles",
        file: "geo.la",
        line: 11,
        status: "failed",
        message: "assert_eq failed: left 4, right 5",
        at: { file: "geo.la", line: 13, col: 5 },
        output: "doubling 2\n",
        ms: 4,
    });
    assert.deepEqual(failed, {
        passed: false,
        skipped: false,
        message: "assert_eq failed: left 4, right 5",
        at: { file: "geo.la", line: 12, character: 4 },
        output: "doubling 2\r\n",
        ms: 4,
    });
    const timedOut = outcome({ name: "m.loops", file: "m.la", line: 1, status: "timed_out", message: "timed out after 10 s", output: "", ms: 10000 });
    assert.deepEqual(timedOut, { passed: false, skipped: false, message: "timed out after 10 s", at: { file: "m.la", line: 0, character: 0 }, output: "", ms: 10000 });
    assert.equal(outcome({ name: "m.a", file: "m.la", line: 2, status: "passed", output: "", ms: 1 }).passed, true);
    // `skip(reason)` (lughac milestone 97): skipped, not failed.
    const skipped = parseResult('{"test":"m.screen","file":"m.la","line":3,"status":"skipped","message":"no display","output":"","ms":2}');
    assert.equal(skipped?.status, "skipped");
    const shownSkip = outcome(skipped!);
    assert.equal(shownSkip.skipped, true);
    assert.equal(shownSkip.passed, false);
    assert.equal(shownSkip.message, "no display");
});

test("a start failure names the executable and the milestone it needs", () => {
    assert.equal(
        testsStartFailure("lughac"),
        "`lughac test` didn't run (`lughac`): set `lugha.path` to a lughac from milestone 85 or later.",
    );
});

test("the manifest says tests never run in untrusted folders", () => {
    const manifest = JSON.parse(fs.readFileSync(path.resolve(__dirname, "../../package.json"), "utf8"));
    assert.match(manifest.capabilities.untrustedWorkspaces.description, /Tests are neither listed nor run/);
});
