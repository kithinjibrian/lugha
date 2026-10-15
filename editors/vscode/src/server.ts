/**
 * Server: how the extension starts `lughac lsp`, kept apart from `vscode`
 * so it is tested with `node:test` alone.
 */

/** The command and arguments that start the language server. */
export interface ServerCommand {
    command: string;
    args: string[];
}

/** Returns the server command for the `lugha.path` setting (`lughac` when unset or blank). */
export function serverCommand(path: string | undefined): ServerCommand {
    const command = path && path.trim() !== "" ? path : "lughac";
    return { command, args: ["lsp"] };
}

/** Returns the notification shown when `command lsp` can't start: missing, or a lughac from before milestone 70. */
export function startFailure(command: string): string {
    return `\`lughac lsp\` didn't start (\`${command}\`): set \`lugha.path\` to a lughac from milestone 70 or later.`;
}
