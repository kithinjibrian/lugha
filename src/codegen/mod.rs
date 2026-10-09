//! Codegen — lowers a checked program to LLVM IR and native object files
//! (spec §9 stages 4–5).
//!
//! Every type comes from the checker's table (CLAUDE.md rule 4): `i32`, `i64`,
//! `u8`, `f64` and `bool` lower per spec §4. Until milestone 4 adds panics,
//! integer arithmetic wraps and `/ %` trap on a bad divisor. Milestone 4/5
//! constructs are `CodegenError::Unsupported`. Does not link — see
//! `crate::link`.
//!
//! Depends on: ast, check (types), span, inkwell (LLVM 21).

mod arith;
mod cast;
mod control;
mod expr;
mod function;
mod heap;
mod lower;
mod runtime;
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
use crate::check::Checked;

use crate::span::Span;
pub use runtime::RUNTIME_SYMBOLS;

/// The source file being compiled, for panic locations (spec §5).
#[derive(Debug, Clone, Copy)]
pub struct SourceInfo<'a> {
    /// The path as given on the command line.
    pub name: &'a str,
    /// The file's text.
    pub text: &'a str,
}

impl SourceInfo<'_> {
    /// 1-based line and byte column of `offset` (spec §9).
    pub(crate) fn line_col(&self, offset: usize) -> (usize, usize) {
        let before = &self.text.as_bytes()[..offset.min(self.text.len())];
        let line_start = before
            .iter()
            .rposition(|&b| b == b'\n')
            .map_or(0, |i| i + 1);
        (
            before.iter().filter(|&&b| b == b'\n').count() + 1,
            before.len() - line_start + 1,
        )
    }
}

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

/// Lowers a checked `program` to verified LLVM IR text.
///
/// # Errors
///
/// [`CodegenError::Unsupported`] for constructs beyond the current milestone;
/// [`CodegenError::Verify`] if the generated IR is invalid.
pub fn emit_ir(
    program: &Program,
    checked: &Checked,
    source: &SourceInfo,
) -> Result<String, CodegenError> {
    let context = Context::create();
    let module = lower::lower(&context, program, checked, source)?;
    Ok(module.print_to_string().to_string())
}

/// Lowers a checked `program`, optimises it at `opt`, and writes a native object file to `path`.
///
/// # Errors
///
/// As [`emit_ir`], plus [`CodegenError::Emit`] if LLVM can't target the host
/// or write the file.
pub fn emit_object(
    program: &Program,
    checked: &Checked,
    source: &SourceInfo,
    opt: OptLevel,
    path: &Path,
) -> Result<(), CodegenError> {
    let context = Context::create();
    let module = lower::lower(&context, program, checked, source)?;
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
    use super::emit_ir;
    use crate::{check, lexer, parser};

    /// The IR for `src`, which must lex, parse, type-check and lower.
    pub fn ir(src: &str) -> String {
        let (tokens, _) = lexer::lex(src).expect("test source lexes");
        let (program, _) = parser::parse(&tokens).expect("test source parses");
        let (checked, _) = check::check(&program).unwrap_or_else(|e| panic!("{src:?}: {e:?}"));
        let source = super::SourceInfo {
            name: "test.la",
            text: src,
        };
        emit_ir(&program, &checked, &source).unwrap_or_else(|e| panic!("{src:?}: {e}"))
    }
}
