/**
 * Tests: reading `lughac test --list --json` and `--json` lines and building
 * its arguments (PRP-115), kept apart from `vscode` so it is tested with
 * `node:test` alone. Positions from lughac are 1-based; VS Code's are 0-based.
 */

/** A test as `lughac test --list --json` names it. */
export interface ListedTest {
    name: string;
    file: string;
    line: number;
    col: number;
}

/** A finished test as `lughac test --json` reports it. */
export interface TestResult {
    name: string;
    file: string;
    line: number;
    status: "passed" | "failed" | "timed_out" | "skipped";
    message?: string;
    at?: { file: string; line: number; col: number };
    output: string;
    ms: number;
}

/** What the Testing view shows for a result. */
export interface Outcome {
    passed: boolean;
    /** Ended through `skip(reason)` (lughac milestone 97); `message` is the reason. */
    skipped: boolean;
    message?: string;
    /** Where to show the message: the panic's place, else the test's line. */
    at?: { file: string; line: number; character: number };
    /** The test's output with `\r\n` line ends, as a test run's output wants. */
    output: string;
    ms: number;
}

function object(line: string): Record<string, unknown> | undefined {
    try {
        const value: unknown = JSON.parse(line);
        return typeof value === "object" && value !== null ? (value as Record<string, unknown>) : undefined;
    } catch {
        return undefined;
    }
}

const isNumber = (v: unknown): v is number => typeof v === "number";
const isString = (v: unknown): v is string => typeof v === "string";

/** One `--list --json` line, or `undefined` for anything else. */
export function parseListed(line: string): ListedTest | undefined {
    const o = object(line);
    if (!o || !isString(o.test) || !isString(o.file) || !isNumber(o.line) || !isNumber(o.col)) return undefined;
    return { name: o.test, file: o.file, line: o.line, col: o.col };
}

/** One `--json` result line, or `undefined` for the summary or anything else. */
export function parseResult(line: string): TestResult | undefined {
    const o = object(line);
    if (!o || !isString(o.test) || !isString(o.file) || !isNumber(o.line) || !isString(o.output) || !isNumber(o.ms)) {
        return undefined;
    }
    if (o.status !== "passed" && o.status !== "failed" && o.status !== "timed_out" && o.status !== "skipped") {
        return undefined;
    }
    const result: TestResult = { name: o.test, file: o.file, line: o.line, status: o.status, output: o.output, ms: o.ms };
    if (isString(o.message)) result.message = o.message;
    const at = o.at as Record<string, unknown> | undefined;
    if (at && isString(at.file) && isNumber(at.line) && isNumber(at.col)) {
        result.at = { file: at.file, line: at.line, col: at.col };
    }
    return result;
}

/** The arguments listing a folder's tests. */
export function listArgs(): string[] {
    return ["test", "--list", "--json"];
}

/** The arguments running `names` exactly (all tests when `undefined`) in `files` (the folder when empty). */
export function runArgs(names: string[] | undefined, files: string[]): string[] {
    const filters = names ? ["--exact", ...names.flatMap((n) => ["--filter", n])] : [];
    return ["test", "--json", ...filters, ...files];
}

/** What to show for `result`. */
export function outcome(result: TestResult): Outcome {
    const at = result.at
        ? { file: result.at.file, line: result.at.line - 1, character: result.at.col - 1 }
        : { file: result.file, line: result.line - 1, character: 0 };
    const shown: Outcome = {
        passed: result.status === "passed",
        skipped: result.status === "skipped",
        output: result.output.replace(/\r?\n/g, "\r\n"),
        ms: result.ms,
    };
    if (shown.skipped) {
        shown.message = result.message;
        return shown;
    }
    if (!shown.passed) {
        shown.message = result.message ?? "failed";
        shown.at = at;
    }
    return shown;
}

/** The message shown when `command test` can't run: missing, or a lughac from before milestone 85. */
export function testsStartFailure(command: string): string {
    return `\`lughac test\` didn't run (\`${command}\`): set \`lugha.path\` to a lughac from milestone 85 or later.`;
}
