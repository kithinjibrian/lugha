/**
 * Testing: the workspace's `@test` functions in VS Code's Testing view, with
 * a ▶ beside each (PRP-115). Tests are listed with `lughac test --list
 * --json` and run with `lughac test --json`, started without a shell from
 * the workspace folder, only when asked, and never in an untrusted folder:
 * running a test runs that folder's code. Parsing lives in `tests.ts`.
 */
import { execFile, spawn } from "node:child_process";
import * as path from "node:path";
import * as vscode from "vscode";
import { listArgs, outcome, parseListed, parseResult, runArgs, testsStartFailure } from "./tests";

/** Creates the "Lugha" test controller; `lughac` gives the executable to start. */
export function registerTests(output: vscode.LogOutputChannel, lughac: () => string): vscode.Disposable {
    const controller = vscode.tests.createTestController("lugha", "Lugha");
    const refresh = () => {
        for (const folder of vscode.workspace.workspaceFolders ?? []) void list(controller, folder, lughac(), output);
    };
    controller.resolveHandler = async () => refresh();
    controller.refreshHandler = async () => refresh();
    const watcher = vscode.workspace.createFileSystemWatcher("**/*.la");
    // A burst of saves (a rename, a branch switch) lists once.
    let timer: NodeJS.Timeout | undefined;
    const later = () => {
        clearTimeout(timer);
        timer = setTimeout(refresh, 300);
    };
    watcher.onDidChange(later);
    watcher.onDidCreate(later);
    watcher.onDidDelete(later);
    const profile = controller.createRunProfile(
        "Run",
        vscode.TestRunProfileKind.Run,
        (request, token) => run(controller, request, token, lughac(), output),
        true,
        undefined,
        true,
    );
    refresh();
    return vscode.Disposable.from(controller, watcher, profile, { dispose: () => clearTimeout(timer) });
}

/** Replaces `folder`'s items with what `lughac test --list --json` names. */
async function list(
    controller: vscode.TestController,
    folder: vscode.WorkspaceFolder,
    command: string,
    output: vscode.LogOutputChannel,
): Promise<void> {
    const stdout = await new Promise<string | undefined>((resolve) => {
        execFile(command, listArgs(), { cwd: folder.uri.fsPath, maxBuffer: 16 << 20 }, (error, out) => {
            if (error && !out) {
                output.warn(`${testsStartFailure(command)}\n${error.message}`);
                resolve(undefined);
            } else resolve(out);
        });
    });
    if (stdout === undefined) return;
    const files = new Map<string, vscode.TestItem>();
    for (const line of stdout.split("\n")) {
        const listed = parseListed(line);
        if (!listed) {
            if (line.trim() !== "") output.info(`lughac test --list: ignored ${line}`);
            continue;
        }
        const uri = vscode.Uri.file(path.join(folder.uri.fsPath, listed.file));
        let file = files.get(listed.file);
        if (!file) {
            file = controller.createTestItem(uri.toString(), listed.file, uri);
            files.set(listed.file, file);
        }
        const short = listed.name.slice(listed.name.lastIndexOf(".") + 1);
        const item = controller.createTestItem(listed.name, short, uri);
        const start = new vscode.Position(listed.line - 1, listed.col - 1);
        item.range = new vscode.Range(start, start.translate(0, short.length));
        file.children.add(item);
    }
    // Only this folder's files are replaced; other folders keep theirs.
    const prefix = folder.uri.toString();
    const kept: vscode.TestItem[] = [];
    controller.items.forEach((item) => {
        if (!item.id.startsWith(prefix)) kept.push(item);
    });
    controller.items.replace([...kept, ...files.values()]);
}

/** Runs what `request` asks for, then again on every save if it is continuous. */
async function run(
    controller: vscode.TestController,
    request: vscode.TestRunRequest,
    token: vscode.CancellationToken,
    command: string,
    output: vscode.LogOutputChannel,
): Promise<void> {
    await runOnce(controller, request, token, command, output);
    if (!request.continuous) return;
    const saved = vscode.workspace.onDidSaveTextDocument((doc) => {
        if (doc.languageId === "lugha" && !token.isCancellationRequested) {
            void runOnce(controller, request, token, command, output);
        }
    });
    await new Promise<void>((resolve) => token.onCancellationRequested(() => resolve()));
    saved.dispose();
}

/** One run of the requested tests, folder by folder. */
async function runOnce(
    controller: vscode.TestController,
    request: vscode.TestRunRequest,
    token: vscode.CancellationToken,
    command: string,
    output: vscode.LogOutputChannel,
): Promise<void> {
    const testRun = controller.createTestRun(request);
    const excluded = new Set((request.exclude ?? []).map((i) => i.id));
    const roots: vscode.TestItem[] = [];
    if (request.include) roots.push(...request.include);
    else controller.items.forEach((i) => roots.push(i));
    // Tests by name, and which were asked for one by one.
    const tests = new Map<string, vscode.TestItem>();
    for (const root of roots) {
        const leaves: vscode.TestItem[] = [];
        if (root.children.size === 0) leaves.push(root);
        else root.children.forEach((c) => leaves.push(c));
        for (const leaf of leaves) if (!excluded.has(leaf.id) && !excluded.has(root.id)) tests.set(leaf.id, leaf);
    }
    for (const folder of vscode.workspace.workspaceFolders ?? []) {
        const mine = [...tests.values()].filter((t) => t.uri && vscode.workspace.getWorkspaceFolder(t.uri) === folder);
        if (mine.length === 0 || token.isCancellationRequested) continue;
        const files = [...new Set(mine.map((t) => path.relative(folder.uri.fsPath, t.uri!.fsPath)))];
        for (const t of mine) testRun.started(t);
        await runFolder(folder, mine, files, testRun, token, command, output);
    }
    testRun.end();
}

/** Runs `tests` (in `files`) of one folder, reporting each as lughac finishes it. */
function runFolder(
    folder: vscode.WorkspaceFolder,
    tests: vscode.TestItem[],
    files: string[],
    testRun: vscode.TestRun,
    token: vscode.CancellationToken,
    command: string,
    output: vscode.LogOutputChannel,
): Promise<void> {
    return new Promise((resolve) => {
        const byName = new Map(tests.map((t) => [t.id, t]));
        const child = spawn(command, runArgs([...byName.keys()], files), { cwd: folder.uri.fsPath, shell: false });
        const cancel = token.onCancellationRequested(() => child.kill());
        let pending = "";
        let stderr = "";
        child.stdout.setEncoding("utf8");
        child.stderr.setEncoding("utf8");
        child.stdout.on("data", (chunk: string) => {
            pending += chunk;
            const lines = pending.split("\n");
            pending = lines.pop() ?? "";
            for (const line of lines) report(line);
        });
        child.stderr.on("data", (chunk: string) => (stderr += chunk));
        const report = (line: string) => {
            const result = parseResult(line);
            const item = result && byName.get(result.name);
            if (!result || !item) return;
            byName.delete(result.name);
            const shown = outcome(result);
            if (shown.output) testRun.appendOutput(shown.output, undefined, item);
            if (shown.passed) {
                testRun.passed(item, shown.ms);
                return;
            }
            const message = new vscode.TestMessage(shown.message ?? "failed");
            if (shown.at) {
                const uri = vscode.Uri.file(path.join(folder.uri.fsPath, shown.at.file));
                message.location = new vscode.Location(uri, new vscode.Position(shown.at.line, shown.at.character));
            }
            testRun.failed(item, message, shown.ms);
        };
        let finished = false;
        const finish = (problem?: string) => {
            // `error` and then `close` may both arrive.
            if (finished) return;
            finished = true;
            cancel.dispose();
            if (pending) report(pending);
            // Tests lughac never reported: their file didn't compile, or the run stopped.
            const why = problem ?? (token.isCancellationRequested ? undefined : "didn't run: the file doesn't compile (see Problems)");
            for (const item of byName.values()) {
                if (why) testRun.errored(item, new vscode.TestMessage(why));
                else testRun.skipped(item);
            }
            if (stderr.trim() !== "") output.info(`lughac test:\n${stderr}`);
            resolve();
        };
        child.on("error", (error) => {
            output.error(`${testsStartFailure(command)}\n${error.message}`);
            void vscode.window.showErrorMessage(testsStartFailure(command));
            finish(testsStartFailure(command));
        });
        child.on("close", () => finish());
    });
}
