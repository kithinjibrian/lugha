import { test } from "node:test";
import assert from "node:assert/strict";
import * as path from "node:path";
import { dedupe, parseOutput, utf16Position } from "../src/diagnostics";

const target = "/w/app/main.la";
const files: Record<string, string> = {
    "/w/app/main.la": "fun main(): i32 {\n    let x: i32 = 1;\n    let y: i32 = x + 2.5;\n    return y;\n}\n",
    "/w/app/util.la": "fun f(): i32 { let s = \"é👋\"; return s; }\n",
};
const read = (file: string) => files[file];

// The spec's E0401 example (§9), as `lughac` prints it.
const e0401 =
    '{"severity":"error","code":"E0401","message":"float literal where i32 expected","file":"/w/app/main.la",' +
    '"span":{"start":{"line":3,"col":22,"offset":59},"end":{"line":3,"col":25,"offset":62}},' +
    '"label":"expected i32",' +
    '"labels":[{"span":{"start":{"line":3,"col":18,"offset":55},"end":{"line":3,"col":19,"offset":56}},"message":"this operand is i32"}],' +
    '"help":null}';

test("an error line becomes a diagnostic with its code, range, label and related label", () => {
    const { diagnostics, skipped } = parseOutput(e0401 + "\n", target, read);
    assert.deepEqual(skipped, []);
    assert.equal(diagnostics.length, 1);
    const d = diagnostics[0];
    assert.equal(d.file, "/w/app/main.la");
    assert.equal(d.severity, "error");
    assert.equal(d.code, "E0401");
    assert.equal(d.message, "float literal where i32 expected\nexpected i32");
    assert.deepEqual(d.range, { start: { line: 2, character: 21 }, end: { line: 2, character: 24 } });
    assert.deepEqual(d.related, [
        {
            file: "/w/app/main.la",
            range: { start: { line: 2, character: 17 }, end: { line: 2, character: 18 } },
            message: "this operand is i32",
        },
    ]);
});

test("columns count UTF-16 units, not bytes", () => {
    // `é` is 2 bytes and 1 unit, `👋` 4 bytes and 2 units: `s` after them is
    // at byte 40 but character 37.
    const text = files["/w/app/util.la"];
    const offset = Buffer.byteLength(text.slice(0, text.lastIndexOf("s;")));
    assert.equal(offset, 40);
    assert.deepEqual(utf16Position(text, offset), { line: 0, character: 37 });
    assert.deepEqual(utf16Position("ab\ncd", 4), { line: 1, character: 1 });
});

test("help goes on its own line and warnings keep their severity", () => {
    const line = JSON.stringify({
        severity: "warning",
        code: "W0101",
        message: "unreachable code",
        file: "/w/app/main.la",
        span: { start: { line: 4, col: 5, offset: 68 }, end: { line: 4, col: 14, offset: 77 } },
        label: null,
        labels: [],
        help: "remove it",
    });
    const [d] = parseOutput(line, target, read).diagnostics;
    assert.equal(d.severity, "warning");
    assert.equal(d.code, "W0101");
    assert.equal(d.message, "unreachable code\nhelp: remove it");
});

test("a relative file resolves against the target's folder, an absolute one is kept", () => {
    const span = { start: { line: 1, col: 1, offset: 0 }, end: { line: 1, col: 4, offset: 3 } };
    const relative = JSON.stringify({ severity: "error", code: "E0403", message: "m", file: "util.la", span, labels: [] });
    const absolute = JSON.stringify({ severity: "error", code: "E0403", message: "m", file: "/elsewhere/x.la", span, labels: [] });
    const { diagnostics } = parseOutput(relative + "\n" + absolute, target, read);
    assert.equal(diagnostics[0].file, path.resolve("/w/app/util.la"));
    assert.equal(diagnostics[1].file, "/elsewhere/x.la");
});

test("a label naming its own file points there", () => {
    const span = { start: { line: 1, col: 1, offset: 0 }, end: { line: 1, col: 4, offset: 3 } };
    const line = JSON.stringify({
        severity: "error",
        code: "E0402",
        message: "m",
        file: "main.la",
        span,
        labels: [{ span, message: "declared here", file: "util.la" }],
    });
    const [d] = parseOutput(line, target, read).diagnostics;
    assert.equal(d.related[0].file, "/w/app/util.la");
});

test("a file that can't be read falls back to the JSON line and column", () => {
    const line = JSON.stringify({
        severity: "error",
        code: "E0403",
        message: "m",
        file: "gone.la",
        span: { start: { line: 3, col: 7, offset: 40 }, end: { line: 3, col: 9, offset: 42 } },
        labels: [],
    });
    const [d] = parseOutput(line, target, read).diagnostics;
    assert.deepEqual(d.range, { start: { line: 2, character: 6 }, end: { line: 2, character: 8 } });
});

test("lines that aren't diagnostics are skipped with a reason", () => {
    const span = { start: { line: 1, col: 1, offset: 0 }, end: { line: 1, col: 2, offset: 1 } };
    const std = JSON.stringify({ severity: "error", code: "E0403", message: "m", file: "<std>/math.la", span, labels: [] });
    const output = ["warning: something odd", '{"severity":"error"}', std, ""].join("\n");
    const { diagnostics, skipped } = parseOutput(output, target, read);
    assert.deepEqual(diagnostics, []);
    assert.deepEqual(
        skipped.map((s) => s.reason),
        ["not JSON", "missing `severity` or `message`", "inside a std module"],
    );
});

test("an internal error without a span lands on the target's first line and is logged", () => {
    const line =
        '{"severity":"error","code":null,"message":"cannot read `x.la`","file":"x.la","span":null,"label":null,"labels":[],"help":null}';
    const { diagnostics, skipped } = parseOutput(line, target, read);
    assert.equal(diagnostics.length, 1);
    assert.equal(diagnostics[0].file, target);
    assert.equal(diagnostics[0].code, null);
    assert.deepEqual(diagnostics[0].range, { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } });
    assert.deepEqual(skipped.map((s) => s.reason), ["no location; shown at the top of the file"]);
});

test("the same diagnostic reported by two checks is kept once", () => {
    const [d] = parseOutput(e0401, target, read).diagnostics;
    assert.deepEqual(dedupe([d, { ...d }, { ...d, message: "other" }]).length, 2);
});
