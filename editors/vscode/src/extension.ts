/**
 * The extension: starts the language server `lughac lsp` for `.la` files,
 * which publishes `lughac check`'s diagnostics as you type (PRP-100).
 *
 * The only file importing `vscode`; highlighting and editing come from the
 * grammar and the language configuration, not from here.
 */
import * as vscode from "vscode";
import { LanguageClient, type LanguageClientOptions, type ServerOptions } from "vscode-languageclient/node";
import { serverCommand, startFailure } from "./server";

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
