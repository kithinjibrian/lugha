import { test } from "node:test";
import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";

const root = path.resolve(__dirname, "../..");

test("the extension is Lugha's default formatter, and format-on-save is left to the user", () => {
    const manifest = JSON.parse(fs.readFileSync(path.join(root, "package.json"), "utf8"));
    const defaults = manifest.contributes.configurationDefaults["[lugha]"];
    assert.equal(defaults["editor.defaultFormatter"], `${manifest.publisher}.${manifest.name}`);
    assert.equal(defaults["editor.formatOnSave"], undefined);
});
