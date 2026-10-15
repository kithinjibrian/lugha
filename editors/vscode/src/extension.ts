/**
 * The extension: checks `.la` files with `lughac` when they are opened or
 * saved and shows the results as diagnostics.
 *
 * The only file importing `vscode`; the logic it wires together lives in
 * `targets`, `runner`, `diagnostics` and `store`, which are tested alone.
 */
import * as fs from "node:fs";
import * as vscode from "vscode";
import { parseOutput, type LughaDiagnostic } from "./diagnostics";
import { Runner } from "./runner";
import { DiagnosticStore } from "./store";
import { planTargets, type Target } from "./targets";

/** Starts the extension when a Lugha file is first opened. */
export function activate(context: vscode.ExtensionContext): void {
    const checker = new Checker();
    context.subscriptions.push(
        checker,
        vscode.commands.registerCommand("lugha.showOutput", () => checker.showOutput()),
        vscode.workspace.onDidOpenTextDocument((doc) => checker.checkDocument(doc)),
        vscode.workspace.onDidSaveTextDocument((doc) => checker.checkDocument(doc)),
        vscode.workspace.onDidDeleteFiles((event) => event.files.forEach((uri) => checker.forget(uri))),
        vscode.workspace.onDidChangeConfiguration((event) => {
            if (event.affectsConfiguration("lugha.path")) checker.resetRunner();
        }),
    );
    vscode.workspace.textDocuments.forEach((doc) => checker.checkDocument(doc));
}

/** Nothing to do: the checker is disposed through the subscriptions. */
export function deactivate(): void {}

class Checker implements vscode.Disposable {
    private readonly collection = vscode.languages.createDiagnosticCollection("lugha");
    private readonly output = vscode.window.createOutputChannel("Lugha");
    private readonly status = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left);
    private readonly store = new DiagnosticStore();
    private runner = newRunner();
    // Once per session: a missing compiler would otherwise notify on every save.
    private toldNotFound = false;

    constructor() {
        this.status.text = "$(warning) lughac failed";
        this.status.tooltip = "Show the Lugha output";
        this.status.command = "lugha.showOutput";
    }

    checkDocument(doc: vscode.TextDocument): void {
        if (doc.languageId !== "lugha" || doc.uri.scheme !== "file") return;
        const folder = vscode.workspace.getWorkspaceFolder(doc.uri);
        const entries = vscode.workspace.getConfiguration("lugha", doc.uri).get<string[]>("entries", []);
        const plan = planTargets(doc.uri.fsPath, doc.getText(), folder?.uri.fsPath, entries, fs.existsSync);
        for (const { entry, reason } of plan.rejected) this.log(`lugha.entries: skipped \`${entry}\`: ${reason}`);
        for (const target of plan.targets) {
            this.check(target).catch((error) => this.log(`internal error checking ${target.file}: ${error}`));
        }
    }

    forget(uri: vscode.Uri): void {
        this.store.removeFile(uri.fsPath);
        this.collection.delete(uri);
    }

    resetRunner(): void {
        this.runner.dispose();
        this.runner = newRunner();
        this.toldNotFound = false;
    }

    dispose(): void {
        this.runner.dispose();
        this.collection.dispose();
        this.output.dispose();
        this.status.dispose();
    }

    private async check(target: Target): Promise<void> {
        const result = await this.runner.run(target.file);
        switch (result.kind) {
            case "cancelled":
                return;
            case "notFound":
                return this.notFound();
            case "failed": {
                const why = result.timedOut
                    ? "timed out"
                    : result.signal
                      ? `killed by ${result.signal}`
                      : `exit ${result.exitCode}`;
                this.log(`lughac check ${target.file}: ${why}\n${result.stderr}`);
                // Earlier diagnostics stay: a crash says nothing about the program.
                this.showFailure(true);
                return;
            }
            case "ok": {
                this.showFailure(false);
                const read = (file: string) => {
                    try {
                        return fs.readFileSync(file, "utf8");
                    } catch {
                        return undefined;
                    }
                };
                const parsed = parseOutput(result.stderr, target.file, read);
                for (const { line, reason } of parsed.skipped) this.log(`${reason}: ${line}`);
                const shown = parsed.diagnostics.filter((d) => d.code === null || !target.dropCodes.includes(d.code));
                for (const file of this.store.update(target.file, shown)) {
                    const uri = vscode.Uri.file(file);
                    this.collection.set(uri, this.store.forFile(file).map(toVscode));
                }
            }
        }
    }

    private notFound(): void {
        if (this.toldNotFound) return;
        this.toldNotFound = true;
        const exe = vscode.workspace.getConfiguration("lugha").get<string>("path", "lughac");
        this.log(`lughac not found: \`${exe}\``);
        const open = "Set lugha.path";
        vscode.window.showErrorMessage(`lughac not found (\`${exe}\`) — set \`lugha.path\`.`, open).then((choice) => {
            if (choice === open) vscode.commands.executeCommand("workbench.action.openSettings", "lugha.path");
        });
    }

    private showFailure(failed: boolean): void {
        if (failed) this.status.show();
        else this.status.hide();
    }

    private log(message: string): void {
        this.output.appendLine(message);
    }

    showOutput(): void {
        this.output.show(true);
    }
}

function newRunner(): Runner {
    return new Runner(vscode.workspace.getConfiguration("lugha").get<string>("path", "lughac"));
}

function toVscode(d: LughaDiagnostic): vscode.Diagnostic {
    const range = (r: LughaDiagnostic["range"]) =>
        new vscode.Range(r.start.line, r.start.character, r.end.line, r.end.character);
    const severity = d.severity === "warning" ? vscode.DiagnosticSeverity.Warning : vscode.DiagnosticSeverity.Error;
    const diagnostic = new vscode.Diagnostic(range(d.range), d.message, severity);
    diagnostic.source = "lughac";
    if (d.code !== null) diagnostic.code = d.code;
    diagnostic.relatedInformation = d.related.map(
        (r) => new vscode.DiagnosticRelatedInformation(new vscode.Location(vscode.Uri.file(r.file), range(r.range)), r.message),
    );
    return diagnostic;
}
