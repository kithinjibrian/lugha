//! The `build` command: compiling to an executable, or printing a stage
//! with `--emit=tokens|ast|ir`, for every module (PRP-033).

use std::path::Path;

use super::pipeline::{self, Failure, Front};
use super::source::line_col;
use super::{Emit, Output};
use crate::codegen::OptLevel;
use crate::modules::fetch::Mode;
use crate::parser::sexp;

pub(super) fn build(
    out: &mut Output,
    file: &Path,
    output: Option<&Path>,
    emit: Option<Emit>,
    (opt, mode): (OptLevel, Mode),
) -> Result<u8, Failure> {
    if let Some(Emit::Tokens) = emit {
        return tokens(out, file, mode);
    }
    // `--emit=ast` shows the tree even when it doesn't type-check.
    let front: Front = if let Some(Emit::Ast) = emit {
        pipeline::parsed(file, mode)?
    } else {
        pipeline::front(file, mode)?
    };
    out.diagnostics(&front.source, &front.warnings);
    match emit {
        Some(Emit::Ast) => {
            for module in &front.modules {
                out.print(&header(&front.modules, &module.path));
                let program = module.program.as_ref().expect("every module parsed");
                out.print(&format!("{}\n", sexp::program(program)));
            }
        }
        Some(Emit::Ir) => out.print(&pipeline::ir(&front)?),
        Some(Emit::Tokens) => unreachable!("handled above"),
        None => {
            let exe = output.map_or_else(|| pipeline::default_output(file), Path::to_path_buf);
            pipeline::build(&front, opt, &exe)?;
        }
    }
    Ok(0)
}

/// `--emit=tokens`: each module's tokens, with lines and columns in its own
/// file. Only lexing matters, so a module that doesn't parse still shows.
fn tokens(out: &mut Output, file: &Path, mode: Mode) -> Result<u8, Failure> {
    let (source, loaded) = pipeline::loaded(file, mode)?;
    let (lexing, other): (Vec<_>, Vec<_>) = loaded
        .diagnostics
        .into_iter()
        .partition(|d| d.code.starts_with("E01"));
    if !lexing.is_empty() {
        return Err(Failure::Program(source, lexing));
    }
    out.diagnostics(
        &source,
        &other
            .into_iter()
            .filter(|d| d.severity == crate::diagnostic::Severity::Warning)
            .collect::<Vec<_>>(),
    );
    for module in &loaded.modules {
        out.print(&header(&loaded.modules, &module.path));
        let (tokens, _) = crate::lexer::lex(&module.text).expect("it lexed while loading");
        for token in tokens
            .iter()
            .filter(|t| t.kind != crate::lexer::TokenKind::Eof)
        {
            let (line, col) = line_col(&module.text, token.span.start);
            out.print(&format!("{line}:{col} {:?}\n", token.kind));
        }
    }
    Ok(0)
}

/// `; module <name>` before each module's output when there is more than
/// one module (`main` for the main file); nothing for a single file.
fn header(modules: &[crate::modules::Module], path: &[String]) -> String {
    match (modules.len(), path) {
        (1, _) => String::new(),
        (_, []) => "; module main\n".to_string(),
        (_, path) => format!("; module {}\n", path.join(".")),
    }
}
