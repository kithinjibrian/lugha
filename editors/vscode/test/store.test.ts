import { test } from "node:test";
import assert from "node:assert/strict";
import type { LughaDiagnostic } from "../src/diagnostics";
import { DiagnosticStore } from "../src/store";

function diag(file: string, message: string): LughaDiagnostic {
    const at = { line: 0, character: 0 };
    return { file, range: { start: at, end: at }, severity: "error", code: "E0403", message, related: [] };
}

test("a check's diagnostics are shown per file", () => {
    const store = new DiagnosticStore();
    const changed = store.update("/a/main.la", [diag("/a/main.la", "x"), diag("/a/util.la", "y")]);
    assert.deepEqual(changed.sort(), ["/a/main.la", "/a/util.la"]);
    assert.deepEqual(store.forFile("/a/util.la").map((d) => d.message), ["y"]);
});

test("a new result clears the files the target's last run reported", () => {
    const store = new DiagnosticStore();
    store.update("/a/main.la", [diag("/a/util.la", "y")]);
    const changed = store.update("/a/main.la", []);
    assert.deepEqual(changed, ["/a/util.la"]);
    assert.deepEqual(store.forFile("/a/util.la"), []);
});

test("a file reported by two targets keeps the other's diagnostics, once", () => {
    const store = new DiagnosticStore();
    store.update("/a/main.la", [diag("/a/util.la", "y")]);
    store.update("/a/demo.la", [diag("/a/util.la", "y"), diag("/a/util.la", "z")]);
    assert.deepEqual(store.forFile("/a/util.la").map((d) => d.message), ["y", "z"]);
    store.update("/a/demo.la", []);
    assert.deepEqual(store.forFile("/a/util.la").map((d) => d.message), ["y"]);
});

test("a deleted file loses its diagnostics from every target", () => {
    const store = new DiagnosticStore();
    store.update("/a/main.la", [diag("/a/util.la", "y")]);
    store.update("/a/demo.la", [diag("/a/util.la", "z")]);
    store.removeFile("/a/util.la");
    assert.deepEqual(store.forFile("/a/util.la"), []);
    assert.deepEqual(store.update("/a/main.la", []), []);
});
