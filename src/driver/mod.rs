//! Driver — the `lughac` command line (spec §9).
//!
//! Parses arguments, runs the pipeline for `build`, `run` and `check`, prints
//! the bundled spec for `spec`, starts a project for `init` and serves the
//! language server for `lsp` (v16), prints
//! diagnostics (human or JSON) and chooses the exit code: 0 success, 1 the
//! program has errors, 2 bad usage or an internal error.
//!
//! Depends on: lexer, parser, codegen, link, diagnostic.

mod build;
mod explain;
mod fmt;
mod guard;
mod init;
mod json;
mod lsp;
mod pipeline;
mod render;
mod source;
mod test;
mod test_json;
mod update;

use std::ffi::OsString;
use std::io::Write;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::{Args, Parser, Subcommand, ValueEnum};

use self::pipeline::Failure;
use self::render::Report;
use self::source::Source;
use crate::codegen::OptLevel;
use crate::diagnostic::Diagnostic;
use crate::modules::fetch::Mode;

/// Runs `lughac` with the process's arguments and standard streams.
pub fn main() -> ExitCode {
    guard::install_hook();
    let args: Vec<OsString> = std::env::args_os().collect();
    // Where to report a crash: the format and file the command asked for.
    let (format, file) = match Cli::try_parse_from(&args) {
        Ok(cli) => (cli.command.format(), cli.command.file()),
        Err(_) => (Format::Human, None),
    };
    let code = guard::guarded(format, file.as_deref(), &mut std::io::stderr(), || {
        run(args, &mut std::io::stdout(), &mut std::io::stderr())
    });
    ExitCode::from(code)
}

/// Runs `lughac` with `args` (including the program name), returning the exit code.
///
/// `lughac run` lets the compiled program inherit the process's real
/// standard streams; everything lughac itself prints goes to `stdout` and
/// `stderr`.
pub fn run<I, T>(args: I, stdout: &mut dyn Write, stderr: &mut dyn Write) -> u8
where
    I: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    let cli = match Cli::try_parse_from(args) {
        Ok(cli) => cli,
        Err(error) => {
            // `--help` and `--version` go to stdout with exit 0; usage errors to stderr with exit 2.
            let out: &mut dyn Write = if error.use_stderr() { stderr } else { stdout };
            let _ = write!(out, "{}", error.render());
            return u8::try_from(error.exit_code()).unwrap_or(2);
        }
    };
    let format = cli.command.format();
    let mut out = Output {
        stdout,
        stderr,
        format,
    };
    let result = match &cli.command {
        Command::Build {
            file,
            output,
            emit,
            options,
        } => build::build(
            &mut out,
            file,
            output.as_deref(),
            *emit,
            (options.opt, options.mode()),
        ),
        Command::Run {
            file,
            options,
            args,
        } => pipeline::front(file, options.mode()).and_then(|front| {
            out.diagnostics(&front.source, &front.warnings);
            pipeline::run(&front, options.opt, args)
        }),
        Command::Check { file, offline, .. } => {
            pipeline::front(file, offline_mode(*offline)).map(|front| {
                out.diagnostics(&front.source, &front.warnings);
                0
            })
        }
        Command::Update { file, .. } => update::update(file).map(|(source, warnings)| {
            out.diagnostics(&source, &warnings);
            0
        }),
        Command::Fmt { paths, check } => Ok(fmt::fmt(paths, *check, &mut out)),
        Command::Test {
            paths,
            filter,
            exact,
            timeout,
            list,
            json,
        } => Ok(test::test(
            &test::Request {
                paths,
                filters: filter,
                exact: *exact,
                timeout: std::time::Duration::from_secs(*timeout),
                list: *list,
                json: *json,
            },
            &mut out,
        )),
        Command::Explain { code } => Ok(explain::run(code.as_deref(), out.stdout, out.stderr)),
        Command::Spec => Ok(spec(out.stdout, out.stderr)),
        Command::Lsp => Ok(lsp::serve(out.stderr)),
        Command::Init { folder, lib, name } => Ok(init::init(
            &init::Request {
                folder: folder.as_deref(),
                lib: *lib,
                name: name.as_deref(),
            },
            out.stdout,
            out.stderr,
        )),
    };
    match result {
        Ok(code) => code,
        Err(Failure::Program(source, diagnostics)) => {
            out.diagnostics(&source, &diagnostics);
            1
        }
        Err(Failure::Internal(source, report)) => {
            out.report(&source, &report);
            2
        }
    }
}

#[derive(Parser)]
#[command(name = "lughac", version, about = "The Lugha compiler")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile and link an executable
    Build {
        /// Source file
        file: PathBuf,
        /// Output path [default: the file name without its extension]
        #[arg(short = 'o', value_name = "OUT")]
        output: Option<PathBuf>,
        /// Print one intermediate stage to stdout and stop
        #[arg(long, value_enum)]
        emit: Option<Emit>,
        #[command(flatten)]
        options: Options,
    },
    /// Build to a temporary file and run it
    Run {
        /// Source file
        file: PathBuf,
        #[command(flatten)]
        options: Options,
        /// Arguments for the program, after `--` (v3: read with `args()`)
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Lex, parse and type-check only, reporting errors
    Check {
        /// Source file
        file: PathBuf,
        /// Diagnostic format
        #[arg(long, value_enum, default_value = "human")]
        diagnostics: Format,
        /// Run no `git`: every git package must be locked and cached (v14)
        #[arg(long)]
        offline: bool,
    },
    /// Fetch every git package's newest matching tag and rewrite `lugha.lock`
    Update {
        /// Source file
        file: PathBuf,
        /// Diagnostic format
        #[arg(long, value_enum, default_value = "human")]
        diagnostics: Format,
    },
    /// Format .la files in place in the one canonical style (v16)
    Fmt {
        /// Files or folders (every .la file under them) [default: the current folder]
        paths: Vec<PathBuf>,
        /// Write nothing; list the files that would change and exit 1 if any
        #[arg(long)]
        check: bool,
    },
    /// Run the `@test` functions in .la files, each in its own process (v16)
    Test {
        /// Files or folders (every .la file under them) [default: the current folder]
        paths: Vec<PathBuf>,
        /// Run only the tests whose `module.name` contains TEXT (repeatable)
        #[arg(long, value_name = "TEXT")]
        filter: Vec<String>,
        /// Each --filter must equal a whole test name
        #[arg(long)]
        exact: bool,
        /// Fail a test that runs longer than SECS seconds
        #[arg(long, value_name = "SECS", default_value_t = 10, value_parser = clap::value_parser!(u64).range(1..))]
        timeout: u64,
        /// List the tests and where they are; build and run nothing
        #[arg(long)]
        list: bool,
        /// One JSON object per line, for tools such as editors
        #[arg(long)]
        json: bool,
    },
    /// Explain an error or warning code, or list every code (v16)
    Explain {
        /// The code, such as E0501 [default: list every code]
        code: Option<String>,
    },
    /// Print the LLM-ready spec bundled with this compiler version
    Spec,
    /// Serve the Language Server Protocol on stdin/stdout, for editors (v16)
    Lsp,
    /// Start a program (or with --lib a library) in a folder; never overwrites (v16)
    Init {
        /// The folder [default: the current one]
        folder: Option<PathBuf>,
        /// Make a library: src/<name>.la and examples/hello.la
        #[arg(long)]
        lib: bool,
        /// The package name [default: the folder's name]
        #[arg(long, value_name = "NAME")]
        name: Option<String>,
    },
}

/// `lughac spec`: the embedded spec, byte for byte (spec §9).
fn spec(stdout: &mut dyn Write, stderr: &mut dyn Write) -> u8 {
    match stdout
        .write_all(crate::SPEC.as_bytes())
        .and_then(|()| stdout.flush())
    {
        Ok(()) => 0,
        // A reader that stops early (`lughac spec | head`) is not a failure.
        Err(error) if error.kind() == std::io::ErrorKind::BrokenPipe => 0,
        Err(error) => {
            // Nowhere better to report it; if stderr fails too, the exit code still says so.
            let _ = writeln!(stderr, "error: cannot write the spec: {error}");
            2
        }
    }
}

impl Command {
    fn format(&self) -> Format {
        match self {
            Command::Build { options, .. } | Command::Run { options, .. } => options.diagnostics,
            Command::Check { diagnostics, .. } | Command::Update { diagnostics, .. } => {
                *diagnostics
            }
            Command::Spec
            | Command::Init { .. }
            | Command::Lsp
            | Command::Fmt { .. }
            | Command::Test { .. }
            | Command::Explain { .. } => Format::Human,
        }
    }

    /// The source file the command reads, as given.
    fn file(&self) -> Option<String> {
        match self {
            Command::Build { file, .. }
            | Command::Run { file, .. }
            | Command::Check { file, .. }
            | Command::Update { file, .. } => Some(file.display().to_string()),
            Command::Spec
            | Command::Init { .. }
            | Command::Lsp
            | Command::Fmt { .. }
            | Command::Test { .. }
            | Command::Explain { .. } => None,
        }
    }
}

#[derive(Args)]
struct Options {
    /// Diagnostic format
    #[arg(long, value_enum, default_value = "human")]
    diagnostics: Format,
    /// Optimisation level: 0 or 2
    #[arg(short = 'O', value_name = "LEVEL", default_value = "0", value_parser = opt_level)]
    opt: OptLevel,
    /// Run no `git`: every git package must be locked and cached (v14)
    #[arg(long)]
    offline: bool,
}

impl Options {
    fn mode(&self) -> Mode {
        offline_mode(self.offline)
    }
}

/// How a build may fetch git packages: never with `--offline`.
fn offline_mode(offline: bool) -> Mode {
    Mode {
        offline,
        update: false,
    }
}

#[derive(Clone, Copy, ValueEnum)]
enum Format {
    Human,
    Json,
}

#[derive(Clone, Copy, ValueEnum)]
enum Emit {
    Tokens,
    Ast,
    Ir,
}

fn opt_level(level: &str) -> Result<OptLevel, String> {
    match level {
        "0" => Ok(OptLevel::O0),
        "2" => Ok(OptLevel::O2),
        _ => Err("expected 0 or 2".to_string()),
    }
}

/// Where lughac's own output goes. Write errors (e.g. a closed pipe) are
/// ignored: the exit code reports the compile result, not the terminal.
struct Output<'a> {
    stdout: &'a mut dyn Write,
    stderr: &'a mut dyn Write,
    format: Format,
}

impl Output<'_> {
    fn print(&mut self, text: &str) {
        let _ = self.stdout.write_all(text.as_bytes());
    }

    /// One of lughac's own errors, not tied to a source.
    fn error(&mut self, message: &str) {
        let _ = writeln!(self.stderr, "error: {message}");
    }

    fn diagnostics(&mut self, source: &Source, diagnostics: &[Diagnostic]) {
        for diagnostic in diagnostics {
            self.report(source, &Report::from(diagnostic));
        }
        // People read the human format; tools read JSON, which has the codes.
        if matches!(self.format, Format::Human)
            && let Some(hint) = explain::hint(diagnostics)
        {
            let _ = self.stderr.write_all(hint.as_bytes());
        }
    }

    fn report(&mut self, source: &Source, report: &Report) {
        let text = match self.format {
            Format::Human => render::human(report, source),
            Format::Json => json::line(report, source),
        };
        let _ = self.stderr.write_all(text.as_bytes());
    }
}
