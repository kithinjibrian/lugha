//! Driver — the `lughac` command line (spec §9).
//!
//! Parses arguments, runs the pipeline for `build`, `run` and `check`, prints
//! diagnostics (human or JSON) and chooses the exit code: 0 success, 1 the
//! program has errors, 2 bad usage or an internal error.
//!
//! Depends on: lexer, parser, codegen, link, diagnostic.

mod json;
mod pipeline;
mod render;
mod source;

use std::ffi::OsString;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::{Args, Parser, Subcommand, ValueEnum};

use self::pipeline::{Failure, Front};
use self::render::Report;
use self::source::{Source, line_col};
use crate::codegen::OptLevel;
use crate::diagnostic::Diagnostic;
use crate::parser::sexp;

/// Runs `lughac` with the process's arguments and standard streams.
pub fn main() -> ExitCode {
    let code = run(
        std::env::args_os(),
        &mut std::io::stdout(),
        &mut std::io::stderr(),
    );
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
        } => build(&mut out, file, output.as_deref(), *emit, options.opt),
        Command::Run { file, options } => pipeline::front(file).and_then(|front| {
            out.diagnostics(&front.source, &front.warnings);
            pipeline::run(&front, options.opt)
        }),
        Command::Check { file, .. } => pipeline::front(file).map(|front| {
            out.diagnostics(&front.source, &front.warnings);
            0
        }),
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
    },
    /// Lex, parse and type-check only, reporting errors
    Check {
        /// Source file
        file: PathBuf,
        /// Diagnostic format
        #[arg(long, value_enum, default_value = "human")]
        diagnostics: Format,
    },
}

impl Command {
    fn format(&self) -> Format {
        match self {
            Command::Build { options, .. } | Command::Run { options, .. } => options.diagnostics,
            Command::Check { diagnostics, .. } => *diagnostics,
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

fn build(
    out: &mut Output,
    file: &Path,
    output: Option<&Path>,
    emit: Option<Emit>,
    opt: OptLevel,
) -> Result<u8, Failure> {
    if let Some(Emit::Tokens) = emit {
        let source = pipeline::load(file)?;
        let (tokens, warnings) = pipeline::tokens(&source)?;
        out.diagnostics(&source, &warnings);
        for token in tokens
            .iter()
            .filter(|t| t.kind != crate::lexer::TokenKind::Eof)
        {
            let (line, col) = line_col(&source.text, token.span.start);
            out.print(&format!("{line}:{col} {:?}\n", token.kind));
        }
        return Ok(0);
    }
    // `--emit=ast` shows the tree even when it doesn't type-check.
    let front: Front = if let Some(Emit::Ast) = emit {
        pipeline::parsed(file)?
    } else {
        pipeline::front(file)?
    };
    out.diagnostics(&front.source, &front.warnings);
    match emit {
        Some(Emit::Ast) => out.print(&format!("{}\n", sexp::program(&front.program))),
        Some(Emit::Ir) => out.print(&pipeline::ir(&front)?),
        Some(Emit::Tokens) => unreachable!("handled above"),
        None => {
            let exe = output.map_or_else(|| pipeline::default_output(file), Path::to_path_buf);
            pipeline::build(&front, opt, &exe)?;
        }
    }
    Ok(0)
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

    fn diagnostics(&mut self, source: &Source, diagnostics: &[Diagnostic]) {
        for diagnostic in diagnostics {
            self.report(source, &Report::from(diagnostic));
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
