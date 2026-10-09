//! Codegen — lowers a parsed program to LLVM IR and native object files
//! (spec §9 stages 4–5).
//!
//! Milestone 2 subset so far (CLAUDE.md rule 9): one `main` with locals,
//! blocks, `if`, loops and boolean operators; every integer is `i64`. Anything else is
//! `CodegenError::Unsupported`, naming the milestone that adds it. Does not
//! link — see `crate::link`.
//!
//! Depends on: ast, span, inkwell (LLVM 21).

mod control;
mod expr;
mod function;
mod lower;
mod scope;
mod stmt;
mod value;

use std::path::Path;

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};

use crate::ast::Program;
use crate::span::Span;

/// Optimisation level for the LLVM pass pipeline.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    /// `default<O0>` — the `lughac` default.
    O0,
    /// `default<O2>`.
    O2,
}

/// Why codegen could not produce output. None of these are the user's
/// mistake, so the driver reports them with exit code 2.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum CodegenError {
    /// Valid syntax that a later milestone will compile.
    #[error("not implemented yet: {what} (milestone {milestone})")]
    Unsupported {
        what: &'static str,
        milestone: u8,
        span: Span,
    },
    /// The generated module is invalid — a compiler bug.
    #[error("generated LLVM IR failed verification (compiler bug): {0}")]
    Verify(String),
    /// LLVM could not set up the target, run passes, or write the object file.
    #[error("could not emit object code: {0}")]
    Emit(String),
}

/// Lowers `program` to verified LLVM IR text.
///
/// # Errors
///
/// [`CodegenError::Unsupported`] for constructs beyond the current milestone;
/// [`CodegenError::Verify`] if the generated IR is invalid.
pub fn emit_ir(program: &Program) -> Result<String, CodegenError> {
    let context = Context::create();
    let module = lower::lower(&context, program)?;
    Ok(module.print_to_string().to_string())
}

/// Lowers `program`, optimises it at `opt`, and writes a native object file to `path`.
///
/// # Errors
///
/// As [`emit_ir`], plus [`CodegenError::Emit`] if LLVM can't target the host
/// or write the file.
pub fn emit_object(program: &Program, opt: OptLevel, path: &Path) -> Result<(), CodegenError> {
    let context = Context::create();
    let module = lower::lower(&context, program)?;
    let machine = host_machine(opt)?;
    module.set_triple(&machine.get_triple());
    module.set_data_layout(&machine.get_target_data().get_data_layout());
    let pipeline = match opt {
        OptLevel::O0 => "default<O0>",
        OptLevel::O2 => "default<O2>",
    };
    let emit_error = |e: inkwell::support::LLVMString| CodegenError::Emit(e.to_string());
    module
        .run_passes(pipeline, &machine, PassBuilderOptions::create())
        .map_err(emit_error)?;
    machine
        .write_to_file(&module, FileType::Object, path)
        .map_err(emit_error)
}

/// A target machine for the host, producing position-independent code
/// because Ubuntu's `cc` links PIE executables by default.
fn host_machine(opt: OptLevel) -> Result<TargetMachine, CodegenError> {
    Target::initialize_native(&InitializationConfig::default()).map_err(CodegenError::Emit)?;
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).map_err(|e| CodegenError::Emit(e.to_string()))?;
    let level = match opt {
        OptLevel::O0 => OptimizationLevel::None,
        OptLevel::O2 => OptimizationLevel::Default,
    };
    let cpu = TargetMachine::get_host_cpu_name().to_string();
    let features = TargetMachine::get_host_cpu_features().to_string();
    target
        .create_target_machine(
            &triple,
            &cpu,
            &features,
            level,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or_else(|| CodegenError::Emit(format!("LLVM has no target machine for {triple}")))
}

#[cfg(test)]
pub(crate) mod test_util {
    use super::{CodegenError, emit_ir};
    use crate::{lexer, parser};

    fn program(src: &str) -> crate::ast::Program {
        let (tokens, _) = lexer::lex(src).expect("test source lexes");
        parser::parse(&tokens).expect("test source parses").0
    }

    /// The IR for `src`. Panics if codegen fails.
    pub fn ir(src: &str) -> String {
        emit_ir(&program(src)).unwrap_or_else(|e| panic!("{src:?}: {e}"))
    }

    /// `(what, milestone, spanned source)` of the `Unsupported` error for `src`.
    pub fn unsupported(src: &str) -> (&'static str, u8, &str) {
        match emit_ir(&program(src)) {
            Err(CodegenError::Unsupported {
                what,
                milestone,
                span,
            }) => (what, milestone, &src[span.start..span.end]),
            other => panic!("{src:?}: expected Unsupported, got {other:?}"),
        }
    }
}
