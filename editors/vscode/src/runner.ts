/**
 * Runner: starts `lughac check --offline --diagnostics=json` on one file,
 * one run per file at a time.
 *
 * `execFile` with an argument array, never a shell, so no file name is ever
 * interpreted. `--offline` keeps opening or saving a file from running `git`.
 */
import { execFile, type ChildProcess } from "node:child_process";
import * as path from "node:path";

/** How a run ended. Process failures are results, never exceptions. */
export type RunResult =
    /** Exit 0 (warnings at most) or 1 (program errors): `stderr` holds the JSON lines. */
    | { kind: "ok"; exitCode: 0 | 1; stderr: string }
    /** The executable doesn't exist. */
    | { kind: "notFound" }
    /** Exit 2 (bad usage or a compiler bug), another code, a signal or the timeout. */
    | { kind: "failed"; exitCode: number | null; signal: string | null; timedOut: boolean; stderr: string }
    /** A newer run of the same file replaced this one. */
    | { kind: "cancelled" };

/** Options for a [`Runner`]. */
export interface RunnerOptions {
    /** Defaults to 30 seconds. */
    timeoutMs?: number;
    /** The environment for `lughac`; defaults to this process's. */
    env?: NodeJS.ProcessEnv;
}

// `lughac` prints one line per diagnostic; this is far beyond any real program.
const MAX_OUTPUT = 16 * 1024 * 1024;

/** Runs `lughac check` for files, replacing a file's older run with a newer one. */
export class Runner {
    private readonly running = new Map<string, { child: ChildProcess; cancel: () => void }>();

    /** Creates a runner for the executable `lughac` (a name on PATH or a path). */
    constructor(
        private readonly lughac: string,
        private readonly options: RunnerOptions = {},
    ) {}

    /** Checks `file` (absolute), cancelling a run of the same file still going. */
    run(file: string): Promise<RunResult> {
        this.running.get(file)?.cancel();
        return new Promise((resolve) => {
            let settled = false;
            const finish = (result: RunResult) => {
                if (settled) return;
                settled = true;
                clearTimeout(timer);
                if (this.running.get(file)?.child === child) this.running.delete(file);
                resolve(result);
            };
            let timedOut = false;
            const child = execFile(
                this.lughac,
                ["check", file, "--offline", "--diagnostics=json"],
                { cwd: path.dirname(file), env: this.options.env ?? process.env, maxBuffer: MAX_OUTPUT },
                (error, _stdout, stderr) => {
                    const code = error ? error.code : 0;
                    if (code === "ENOENT") return finish({ kind: "notFound" });
                    if (!timedOut && (code === 0 || code === 1)) return finish({ kind: "ok", exitCode: code, stderr });
                    finish({
                        kind: "failed",
                        exitCode: typeof code === "number" ? code : null,
                        signal: error?.signal ?? null,
                        timedOut,
                        stderr,
                    });
                },
            );
            const timer = setTimeout(() => {
                timedOut = true;
                child.kill();
            }, this.options.timeoutMs ?? 30_000);
            this.running.set(file, {
                child,
                cancel: () => {
                    // Resolve first, so the killed process's callback is ignored.
                    finish({ kind: "cancelled" });
                    child.kill();
                },
            });
        });
    }

    /** Kills every running check, when the extension stops. */
    dispose(): void {
        for (const { cancel } of [...this.running.values()]) cancel();
    }
}
