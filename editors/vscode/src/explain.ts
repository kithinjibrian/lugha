/**
 * Explain: the documents behind each diagnostic's code link,
 * `lugha-explain:E0501.md`, served from `lughac explain E0501` (PRP-116);
 * kept apart from `vscode` so it is tested with `node:test` alone.
 */

/** The code a `lugha-explain:` path names (`E0501.md`), or `undefined` for anything else. */
export function codeOf(path: string): string | undefined {
    // Only a code reaches `lughac`'s arguments: nothing else is accepted.
    const match = /^([EW][0-9]{4})\.md$/.exec(path);
    return match ? match[1] : undefined;
}

/** The arguments explaining `code`. */
export function explainArgs(code: string): string[] {
    return ["explain", code];
}

/** The document shown when `lughac explain` can't run: missing, or a lughac from before milestone 86. */
export function explainFailure(command: string, code: string): string {
    return `# ${code}\n\n\`lughac explain ${code}\` didn't run (\`${command}\`): set \`lugha.path\` to a lughac from milestone 86 or later.\n`;
}
