import { test } from "node:test";
import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";

const root = path.resolve(__dirname, "../..");

test("the snippet file is registered and holds the chosen prefixes", () => {
    const manifest = JSON.parse(fs.readFileSync(path.join(root, "package.json"), "utf8"));
    const entry = manifest.contributes.snippets[0];
    assert.equal(entry.language, "lugha");
    const snippets = JSON.parse(fs.readFileSync(path.join(root, entry.path), "utf8"));
    const prefixes = Object.values(snippets).map((s) => (s as { prefix: string }).prefix);
    for (const p of ["fun", "main", "struct", "enum", "impl", "interface", "import", "doc", "if", "ifelse", "while", "forof", "forin", "iflet", "letelse", "match"]) {
        assert.ok(prefixes.includes(p), `no ${p} snippet`);
    }
});
