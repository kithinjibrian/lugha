// These run the real compiler named by $LUGHAC; CI builds it first.
import { test } from "node:test";
import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { parseOutput } from "../src/diagnostics";
import { Runner } from "../src/runner";

const lughac = process.env.LUGHAC ? path.resolve(process.env.LUGHAC) : undefined;
const skip = lughac ? false : "set LUGHAC to a built lughac to run these";
const fixtures = path.resolve(__dirname, "../../test/fixtures");
const read = (file: string) => fs.readFileSync(file, "utf8");

test("a clean program checks with no diagnostics", { skip }, async () => {
    const result = await new Runner(lughac!).run(path.join(fixtures, "clean/main.la"));
    assert.equal(result.kind, "ok");
    assert.equal(result.kind === "ok" && result.stderr, "");
});

test("an error in an imported module lands in that module", { skip }, async () => {
    const main = path.join(fixtures, "imports/main.la");
    const result = await new Runner(lughac!).run(main);
    assert.equal(result.kind, "ok");
    if (result.kind !== "ok") return;
    assert.equal(result.exitCode, 1);
    const { diagnostics } = parseOutput(result.stderr, main, read);
    assert.equal(diagnostics.length, 1);
    assert.equal(diagnostics[0].file, path.join(fixtures, "imports/util.la"));
    assert.equal(diagnostics[0].code, "E0403");
    // `return s;` after `"é"`: byte column 37, UTF-16 character 35.
    assert.deepEqual(diagnostics[0].range.start, { line: 1, character: 35 });
});

test("a module checked alone reports E0303", { skip }, async () => {
    const util = path.join(fixtures, "imports/util.la");
    const result = await new Runner(lughac!).run(util);
    assert.equal(result.kind, "ok");
    if (result.kind !== "ok") return;
    const codes = parseOutput(result.stderr, util, read).diagnostics.map((d) => d.code);
    assert.deepEqual(codes.sort(), ["E0303", "E0403"]);
});

test("checks are offline: a git package not in the cache is E0325 and git never runs", { skip }, async () => {
    const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "lugha-vscode-"));
    try {
        const bin = path.join(scratch, "bin");
        const marker = path.join(scratch, "git-ran");
        fs.mkdirSync(bin);
        fs.writeFileSync(path.join(bin, "git"), `#!/bin/sh\ntouch '${marker}'\nexit 1\n`, { mode: 0o755 });
        const env = {
            ...process.env,
            PATH: `${bin}${path.delimiter}${process.env.PATH ?? ""}`,
            LUGHA_CACHE: path.join(scratch, "cache"),
        };
        const main = path.join(fixtures, "git/main.la");
        const result = await new Runner(lughac!, { env }).run(main);
        assert.equal(result.kind, "ok");
        if (result.kind !== "ok") return;
        const codes = parseOutput(result.stderr, main, read).diagnostics.map((d) => d.code);
        assert.ok(codes.includes("E0325"), result.stderr);
        assert.ok(!fs.existsSync(marker), "git ran");
        assert.ok(!fs.existsSync(path.join(fixtures, "git/lugha.lock")));
    } finally {
        fs.rmSync(scratch, { recursive: true, force: true });
    }
});

test("a missing executable is notFound", async () => {
    const result = await new Runner("/nonexistent/lughac").run(path.join(fixtures, "clean/main.la"));
    assert.deepEqual(result, { kind: "notFound" });
});

test("exit 2 is a failure carrying stderr", { skip }, async () => {
    const result = await new Runner(lughac!).run(path.join(fixtures, "no-such-file.la"));
    assert.equal(result.kind, "failed");
    if (result.kind !== "failed") return;
    assert.equal(result.exitCode, 2);
    assert.match(result.stderr, /cannot read/);
});

test("a run that outlives its timeout fails as timed out", async () => {
    const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "lugha-vscode-"));
    try {
        const slow = path.join(scratch, "slow");
        fs.writeFileSync(slow, "#!/bin/sh\nsleep 5\n", { mode: 0o755 });
        const result = await new Runner(slow, { timeoutMs: 100 }).run(path.join(scratch, "main.la"));
        assert.equal(result.kind, "failed");
        assert.equal(result.kind === "failed" && result.timedOut, true);
    } finally {
        fs.rmSync(scratch, { recursive: true, force: true });
    }
});

test("a second run for the same target cancels the first", async () => {
    const scratch = fs.mkdtempSync(path.join(os.tmpdir(), "lugha-vscode-"));
    try {
        // Sleeps on the first call only, so the second finishes at once.
        const script = path.join(scratch, "fake");
        const once = path.join(scratch, "once");
        fs.writeFileSync(script, `#!/bin/sh\nif [ ! -e '${once}' ]; then touch '${once}'; sleep 5; fi\nexit 0\n`, {
            mode: 0o755,
        });
        const runner = new Runner(script);
        const main = path.join(scratch, "main.la");
        const first = runner.run(main);
        // Let the first process start before replacing it.
        await new Promise((resolve) => setTimeout(resolve, 200));
        const second = runner.run(main);
        assert.deepEqual(await first, { kind: "cancelled" });
        assert.equal((await second).kind, "ok");
    } finally {
        fs.rmSync(scratch, { recursive: true, force: true });
    }
});
