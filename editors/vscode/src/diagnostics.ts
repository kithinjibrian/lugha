/**
 * Diagnostics: turns `lughac check --diagnostics=json` output into plain
 * records with UTF-16 ranges and absolute files.
 *
 * Doesn't import `vscode`, so it is tested with `node:test` alone.
 */
import * as path from "node:path";

/** A zero-based line and UTF-16 column, as VS Code counts them. */
export interface Position {
    line: number;
    character: number;
}

/** A start and end position in one file. */
export interface Range {
    start: Position;
    end: Position;
}

/** A secondary label of a diagnostic, possibly in another file. */
export interface Related {
    file: string;
    range: Range;
    message: string;
}

/** One `lughac` diagnostic, ready to show. */
export interface LughaDiagnostic {
    file: string;
    range: Range;
    severity: "error" | "warning";
    /** `E0403`, `W0101`…; null for an internal error. */
    code: string | null;
    /** The message, then the label and `help: …` on their own lines. */
    message: string;
    related: Related[];
}

/** An output line not shown as a diagnostic, and why. */
export interface Skipped {
    line: string;
    reason: string;
}

/** What one run's output yields. */
export interface Parsed {
    diagnostics: LughaDiagnostic[];
    skipped: Skipped[];
}

/** Reads a file's text, or returns undefined when it can't be read. */
export type ReadText = (file: string) => string | undefined;

interface JsonPoint {
    line: number;
    col: number;
    offset: number;
}

interface JsonSpan {
    start: JsonPoint;
    end: JsonPoint;
}

/**
 * Parses `lughac`'s JSON lines from a check of `target`.
 *
 * A relative `file` is resolved against the target's folder, where `lughac`
 * names imported modules from. Ranges are recomputed from byte offsets using
 * `read`, because `lughac` columns count bytes. Never throws: a line that
 * isn't a diagnostic is returned in `skipped` with its reason.
 */
export function parseOutput(output: string, target: string, read: ReadText): Parsed {
    const diagnostics: LughaDiagnostic[] = [];
    const skipped: Skipped[] = [];
    const texts = new Map<string, string | undefined>();
    const text = (file: string) => {
        if (!texts.has(file)) texts.set(file, read(file));
        return texts.get(file);
    };
    const folder = path.dirname(target);
    for (const line of output.split("\n")) {
        if (line.trim() === "") continue;
        let json: Record<string, unknown>;
        try {
            json = JSON.parse(line);
        } catch {
            skipped.push({ line, reason: "not JSON" });
            continue;
        }
        if (typeof json !== "object" || json === null || typeof json.severity !== "string" || typeof json.message !== "string") {
            skipped.push({ line, reason: "missing `severity` or `message`" });
            continue;
        }
        const named = typeof json.file === "string" ? json.file : target;
        if (named.startsWith("<std>")) {
            skipped.push({ line, reason: "inside a std module" });
            continue;
        }
        const span = json.span as JsonSpan | null | undefined;
        if (!span) {
            // An internal error (exit 2 usually) has no location; showing it
            // at the top of the checked file keeps it visible.
            skipped.push({ line, reason: "no location; shown at the top of the file" });
        }
        const file = resolve(folder, named);
        const zero = { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } };
        const related: Related[] = [];
        for (const label of Array.isArray(json.labels) ? json.labels : []) {
            if (!label || typeof label.message !== "string" || !label.span) continue;
            const at = typeof label.file === "string" ? resolve(folder, label.file) : file;
            related.push({ file: at, range: range(label.span, text(at)), message: label.message });
        }
        diagnostics.push({
            file: span ? file : target,
            range: span ? range(span, text(file)) : zero,
            severity: json.severity === "warning" ? "warning" : "error",
            code: typeof json.code === "string" ? json.code : null,
            message: [
                json.message,
                typeof json.label === "string" ? json.label : undefined,
                typeof json.help === "string" ? `help: ${json.help}` : undefined,
            ]
                .filter((part) => part !== undefined)
                .join("\n"),
            related,
        });
    }
    return { diagnostics, skipped };
}

/** Returns the UTF-16 position of a byte offset in `text`. */
export function utf16Position(text: string, byteOffset: number): Position {
    const before = Buffer.from(text, "utf8").subarray(0, byteOffset).toString("utf8");
    const lineStart = before.lastIndexOf("\n") + 1;
    return { line: before.split("\n").length - 1, character: before.length - lineStart };
}

/** Removes repeats: the same file, range, code and message. */
export function dedupe(diagnostics: LughaDiagnostic[]): LughaDiagnostic[] {
    const seen = new Set<string>();
    return diagnostics.filter((d) => {
        const key = JSON.stringify([d.file, d.range, d.code, d.message]);
        if (seen.has(key)) return false;
        seen.add(key);
        return true;
    });
}

function resolve(folder: string, file: string): string {
    return path.isAbsolute(file) ? file : path.resolve(folder, file);
}

function range(span: JsonSpan, text: string | undefined): Range {
    if (text === undefined) {
        // The file can't be read (deleted since the check): `lughac`'s own
        // line is right, and its byte column is the best column there is.
        const point = (p: JsonPoint) => ({ line: p.line - 1, character: p.col - 1 });
        return { start: point(span.start), end: point(span.end) };
    }
    return { start: utf16Position(text, span.start.offset), end: utf16Position(text, span.end.offset) };
}
