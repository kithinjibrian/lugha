// Guards against grammar drift: every keyword in the newest spec's §2 must be
// highlighted, so a milestone adding one fails here until the grammar has it.
import { test } from "node:test";
import assert from "node:assert/strict";
import * as fs from "node:fs";
import * as path from "node:path";

const root = path.resolve(__dirname, "../..");
const specs = path.resolve(root, "../../docs/specs");

function newestSpec(): string {
    const versions = fs
        .readdirSync(specs)
        .map((name) => /^Language v(\d+) Specification\.md$/.exec(name))
        .filter((m): m is RegExpExecArray => m !== null)
        .map((m) => Number(m[1]));
    assert.ok(versions.length > 0, `no specification in ${specs}`);
    return path.join(specs, `Language v${Math.max(...versions)} Specification.md`);
}

function specKeywords(): string[] {
    const text = fs.readFileSync(newestSpec(), "utf8");
    const block = /\*\*Keywords\.\*\*[\s\S]*?```\n([\s\S]*?)```/.exec(text);
    assert.ok(block, "no keyword block in §2");
    return block[1].split(/\s+/).filter((w) => w.length > 0);
}

interface Rule {
    match?: string;
    name?: string;
    captures?: Record<string, { name?: string }>;
    patterns?: Rule[];
}

/** Collects every `match` rule with the scopes it can give a whole match. */
function matchRules(rule: Rule, out: { regex: RegExp; scopes: string[] }[]): void {
    if (rule.match) {
        const scopes = [rule.name, ...Object.values(rule.captures ?? {}).map((c) => c.name)];
        out.push({ regex: new RegExp(`^(?:${rule.match})$`), scopes: scopes.filter((s): s is string => !!s) });
    }
    for (const inner of rule.patterns ?? []) matchRules(inner, out);
}

test("every keyword in the spec is highlighted as a keyword, type or constant", () => {
    const grammar = JSON.parse(fs.readFileSync(path.join(root, "syntaxes/lugha.tmLanguage.json"), "utf8"));
    const rules: { regex: RegExp; scopes: string[] }[] = [];
    for (const rule of Object.values(grammar.repository as Record<string, Rule>)) matchRules(rule, rules);
    const keywordScope = /^(keyword|storage|constant\.language|variable\.language)\./;
    const keywords = specKeywords();
    assert.ok(keywords.includes("fun") && keywords.includes("char"), "keyword block not parsed");
    const missing = keywords.filter(
        (k) => !rules.some((r) => r.regex.test(k) && r.scopes.some((s) => keywordScope.test(s))),
    );
    assert.deepEqual(missing, []);
});
