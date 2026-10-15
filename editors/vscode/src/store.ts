/**
 * Store: which check reported what, so a new result replaces only that
 * check's diagnostics, even in a module several entries import.
 */
import { dedupe, type LughaDiagnostic } from "./diagnostics";

/** Diagnostics grouped by the target whose check reported them. */
export class DiagnosticStore {
    private readonly byTarget = new Map<string, Map<string, LughaDiagnostic[]>>();

    /**
     * Replaces `target`'s diagnostics with `diagnostics`, and returns every
     * file whose shown diagnostics may have changed: those it reported
     * before and those it reports now.
     */
    update(target: string, diagnostics: LughaDiagnostic[]): string[] {
        const changed = new Set(this.byTarget.get(target)?.keys());
        const files = new Map<string, LughaDiagnostic[]>();
        for (const d of diagnostics) {
            files.set(d.file, [...(files.get(d.file) ?? []), d]);
            changed.add(d.file);
        }
        this.byTarget.set(target, files);
        return [...changed];
    }

    /** Returns `file`'s diagnostics from every target, each once. */
    forFile(file: string): LughaDiagnostic[] {
        const all: LughaDiagnostic[] = [];
        for (const files of this.byTarget.values()) all.push(...(files.get(file) ?? []));
        return dedupe(all);
    }

    /** Forgets `file` everywhere, for a deleted file. */
    removeFile(file: string): void {
        for (const files of this.byTarget.values()) files.delete(file);
        this.byTarget.delete(file);
    }
}
