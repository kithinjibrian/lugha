/**
 * The extension: starts the language server `lughac lsp` for `.la` files,
 * which publishes `lughac check`'s diagnostics as you type (PRP-100), and,
 * in a trusted workspace, the Testing view's `lughac test` (PRP-115,
 * `testing.ts`).
 *
 * With `testing.ts`, the only files importing `vscode`; highlighting and
 * editing come from the grammar and the language configuration.
 */
import * as vscode from "vscode";
import { LanguageClient, type LanguageClientOptions, type ServerOptions } from "vscode-languageclient/node";
import { execFile } from "node:child_process";
import { codeOf, explainArgs, explainFailure } from "./explain";
import { serverCommand, startFailure } from "./server";
import { registerTests } from "./testing";

let client: LanguageClient | undefined;

/** Starts the client when a Lugha file is first opened, and restarts it when `lugha.path` changes. */
export async function activate(context: vscode.ExtensionContext): Promise<void> {
    // A log channel: the client writes its own trace and the server's stderr there.
    const output = vscode.window.createOutputChannel("Lugha", { log: true });
    context.subscriptions.push(
        output,
        vscode.commands.registerCommand("lugha.showOutput", () => output.show(true)),
        vscode.workspace.onDidChangeConfiguration(async (event) => {
            if (event.affectsConfiguration("lugha.path")) {
                await stop();
                await start(output);
            }
        }),
    );
    const lughac = () => serverCommand(vscode.workspace.getConfiguration("lugha").get<string>("path")).command;
    // Diagnostics link their code here (milestone 86). Explaining runs no
    // workspace code, so it works in untrusted folders too.
    context.subscriptions.push(
        vscode.workspace.registerTextDocumentContentProvider("lugha-explain", {
            provideTextDocumentContent: (uri) => explanation(lughac(), uri.path),
        }),
        vscode.commands.registerCommand("lugha.explain", async (given?: string) => {
            const code = given ?? (await vscode.window.showInputBox({ prompt: "Error or warning code", placeHolder: "E0501" }));
            const named = code ? codeOf(`${code.trim().toUpperCase()}.md`) : undefined;
            if (!named) return;
            const doc = await vscode.workspace.openTextDocument(vscode.Uri.parse(`lugha-explain:${named}.md`));
            await vscode.window.showTextDocument(doc, { preview: true });
        }),
    );
    // Tests run the folder's code: only once VS Code trusts it (decision 90).
    if (vscode.workspace.isTrusted) {
        context.subscriptions.push(registerTests(output, lughac));
    } else {
        context.subscriptions.push(
            vscode.workspace.onDidGrantWorkspaceTrust(() => context.subscriptions.push(registerTests(output, lughac))),
        );
    }
    await start(output);
}

/** Stops the server with the extension. */
export async function deactivate(): Promise<void> {
    await stop();
}

async function start(output: vscode.LogOutputChannel): Promise<void> {
    const config = vscode.workspace.getConfiguration("lugha");
    const server = serverCommand(config.get<string>("path"));
    const serverOptions: ServerOptions = { run: server, debug: server };
    const clientOptions: LanguageClientOptions = {
        // Unsaved `untitled:` buffers have no folder to resolve imports from.
        documentSelector: [{ language: "lugha", scheme: "file" }],
        // Sends `workspace/didChangeConfiguration` with `{ lugha: { … } }`
        // when settings change, so `lugha.entries` takes effect at once.
        synchronize: { configurationSection: "lugha" },
        initializationOptions: { entries: config.get<string[]>("entries", []) },
        outputChannel: output,
    };
    const started = new LanguageClient("lugha", "Lugha", serverOptions, clientOptions);
    try {
        await started.start();
        client = started;
    } catch (error) {
        output.appendLine(`${startFailure(server.command)}\n${error}`);
        const open = "Set lugha.path";
        const choice = await vscode.window.showErrorMessage(startFailure(server.command), open);
        if (choice === open) await vscode.commands.executeCommand("workbench.action.openSettings", "lugha.path");
    }
}

async function stop(): Promise<void> {
    const running = client;
    client = undefined;
    // A client that never started can't be stopped; nothing is left to clean up.
    await running?.stop().catch(() => undefined);
}

/** The text of `lughac explain` for the code a `lugha-explain:` path names. */
function explanation(command: string, path: string): Promise<string> {
    const code = codeOf(path);
    if (!code) return Promise.resolve(`# ${path}\n\nNot a Lugha error code.\n`);
    return new Promise((resolve) => {
        execFile(command, explainArgs(code), (error, stdout) => {
            resolve(error ? explainFailure(command, code) : stdout);
        });
    });
}
