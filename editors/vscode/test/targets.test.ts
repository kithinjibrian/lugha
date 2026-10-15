import { test } from "node:test";
import assert from "node:assert/strict";
import { hasMain, planTargets } from "../src/targets";

test("hasMain finds a main function at the start of a line", () => {
    assert.ok(hasMain("fun main() {\n}\n"));
    assert.ok(hasMain("import x;\n\nfun main(): i32 {\n    return 0;\n}\n"));
    assert.ok(hasMain("public fun main() { }"));
    assert.ok(hasMain("    fun  main ()"));
});

test("hasMain ignores comments, other names and strings", () => {
    assert.ok(!hasMain("// fun main() {}\n"));
    assert.ok(!hasMain("fun mainly() {}\n"));
    assert.ok(!hasMain('fun f() { let s = "fun main()"; }\n'));
    assert.ok(!hasMain(""));
});

const folder = "/w";
const present = new Set(["/w/src/main.la", "/w/examples/demo.la"]);
const exists = (p: string) => present.has(p);

test("a file with main is checked itself", () => {
    const plan = planTargets("/w/src/main.la", "fun main() {}", folder, ["examples/demo.la"], exists);
    assert.deepEqual(plan, { targets: [{ file: "/w/src/main.la", dropCodes: [] }], rejected: [] });
});

test("a module is checked through the configured entries", () => {
    const plan = planTargets("/w/src/util.la", "fun f() {}", folder, ["src/main.la", "examples/demo.la"], exists);
    assert.deepEqual(plan, {
        targets: [
            { file: "/w/src/main.la", dropCodes: [] },
            { file: "/w/examples/demo.la", dropCodes: [] },
        ],
        rejected: [],
    });
});

test("a module without entries is checked alone, without E0303", () => {
    const alone = { targets: [{ file: "/w/src/util.la", dropCodes: ["E0303"] }], rejected: [] };
    assert.deepEqual(planTargets("/w/src/util.la", "fun f() {}", folder, [], exists), alone);
    // A file outside any workspace folder has no entries to use.
    assert.deepEqual(planTargets("/w/src/util.la", "fun f() {}", undefined, ["src/main.la"], exists), alone);
});

test("bad entries are rejected with a reason and the rest still run", () => {
    const entries = ["/abs/main.la", "../outside.la", "src/main.txt", "src/missing.la", "src/main.la"];
    const plan = planTargets("/w/src/util.la", "fun f() {}", folder, entries, exists);
    assert.deepEqual(plan.targets, [{ file: "/w/src/main.la", dropCodes: [] }]);
    assert.deepEqual(plan.rejected, [
        { entry: "/abs/main.la", reason: "not a relative path" },
        { entry: "../outside.la", reason: "outside the workspace folder" },
        { entry: "src/main.txt", reason: "not a .la file" },
        { entry: "src/missing.la", reason: "not found" },
    ]);
});

test("when every entry is rejected the module is checked alone", () => {
    const plan = planTargets("/w/src/util.la", "fun f() {}", folder, ["src/missing.la"], exists);
    assert.deepEqual(plan.targets, [{ file: "/w/src/util.la", dropCodes: ["E0303"] }]);
    assert.equal(plan.rejected.length, 1);
});
