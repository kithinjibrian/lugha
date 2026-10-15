/**
 * Targets: which files to pass to `lughac check` when a file is opened or
 * saved.
 *
 * `lughac` has no default entry point (spec §11), so a module file reaches
 * its main files only through the `lugha.entries` setting.
 */
import * as path from "node:path";

/** A file to check, and codes to hide from its results. */
export interface Target {
    file: string;
    /** E0303 when a module is checked alone: it has no `main` by design. */
    dropCodes: string[];
}

/** A `lugha.entries` value that can't be used, and why. */
export interface Rejected {
    entry: string;
    reason: string;
}

/** The files to check for one saved file. */
export interface Plan {
    targets: Target[];
    rejected: Rejected[];
}

// At the start of a line, so `// fun main` and strings don't count.
const MAIN = /^[ \t]*(?:public[ \t]+)?fun[ \t]+main[ \t]*\(/m;

/** Returns whether `text` declares `fun main`. */
export function hasMain(text: string): boolean {
    return MAIN.test(text);
}

/**
 * Plans the checks for `file` with contents `text`.
 *
 * A file with `main` checks itself. A module checks the valid `entries` of
 * its workspace `folder` (relative `.la` paths inside it that `exists`); with
 * none, it checks itself with E0303 dropped. Invalid entries come back in
 * `rejected`.
 */
export function planTargets(
    file: string,
    text: string,
    folder: string | undefined,
    entries: readonly string[],
    exists: (file: string) => boolean,
): Plan {
    if (hasMain(text)) return { targets: [{ file, dropCodes: [] }], rejected: [] };
    const targets: Target[] = [];
    const rejected: Rejected[] = [];
    if (folder !== undefined) {
        for (const entry of entries) {
            const reason = invalid(entry, folder, exists);
            if (reason) rejected.push({ entry, reason });
            else targets.push({ file: path.resolve(folder, entry), dropCodes: [] });
        }
    }
    if (targets.length === 0) targets.push({ file, dropCodes: ["E0303"] });
    return { targets, rejected };
}

function invalid(entry: string, folder: string, exists: (file: string) => boolean): string | undefined {
    if (path.isAbsolute(entry)) return "not a relative path";
    const resolved = path.resolve(folder, entry);
    const inside = path.relative(folder, resolved);
    // The setting may come from a cloned repository, so it can't name files
    // outside the folder it belongs to.
    if (inside.startsWith("..") || path.isAbsolute(inside)) return "outside the workspace folder";
    if (!entry.endsWith(".la")) return "not a .la file";
    if (!exists(resolved)) return "not found";
    return undefined;
}
