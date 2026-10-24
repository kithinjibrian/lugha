//! Codegen — lowers a checked program to LLVM IR and native object files
//! (spec §9 stages 4–5).
//!
//! Every type comes from the checker's table (CLAUDE.md rule 4) and lowers
//! per spec §4 and §7. Does not link — see `crate::link`.
//!
//! Depends on: ast, check (types), span, inkwell (LLVM 21).

mod actors;
mod arith;
mod array;
mod assertions;
mod bits;
mod c_abi;
mod c_abi_arm;
mod c_arrays;
mod c_calls;
mod callbacks;
mod cast;
mod channels;
mod closures;
mod concat;
mod conditional;
mod construct;
mod consts;
mod control;
mod copy;
mod cow;
mod declare;
mod defaults;
mod dispatch;
mod enums;
mod equality;
mod expr;
mod externs;
mod float_ops;
mod format;
mod frames;
mod fun_values;
mod function;
mod growth;
mod hashing;
mod heap;
mod hoist;
mod hoist_scan;
mod includes;
mod input;
mod instances;
mod int_checked;
mod int_methods;
mod interfaces;
mod io;
mod iterators;
mod lower;
mod maps;
mod matching;
mod methods;
mod net;
mod operators;
mod or_patterns;
mod ordering;
mod os;
mod out_params;
mod patterns;
mod places;
mod rebuild;
mod refs;
mod results;
mod runtime;
mod scope;
mod show;
mod show_iface;
mod split;
mod split_leaves;
mod split_params;
mod sret;
mod stmt;
mod structs;
mod swap;
mod targets;
mod tasks;
mod test_main;
mod tuples;
mod unwrap;
mod upcast;
mod value;

pub use self::targets::Target;

use std::path::Path;

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target as LlvmTarget, TargetMachine,
    TargetTriple,
};

use crate::ast::Program;
use crate::check::{Checked, Unit};

/// The source file being compiled, for panic locations (spec §5).
#[derive(Debug, Clone, Copy)]
pub struct SourceInfo<'a> {
    /// The path as given on the command line.
    pub name: &'a str,
    /// The file's text.
    pub text: &'a str,
}

/// One module's source for panic locations: its name, where it starts in
/// the program's one offset space, and its text (PRP-034).
#[derive(Debug, Clone, Copy)]
pub struct ModuleSource<'a> {
    /// The name diagnostics use for the file.
    pub name: &'a str,
    /// The offset of its first byte.
    pub base: usize,
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

/// The x86-64 CPU level the program's code is built for (v17, milestone
/// 130). Only LLVM's code for the program follows it; the runtime and
/// `[link]` C files keep `cc`'s default, which is the baseline too.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Cpu {
    /// `x86-64`: every 64-bit Intel and AMD CPU. The default, so a binary
    /// runs on machines other than the one that built it.
    #[default]
    X86_64,
    /// `x86-64-v2`: adds SSE4.2 and `popcnt` (CPUs from about 2009).
    V2,
    /// `x86-64-v3`: adds AVX2 and FMA (about 2015).
    V3,
    /// `x86-64-v4`: adds AVX-512.
    V4,
    /// This machine's own CPU and features; the binary may not run elsewhere.
    Native,
}

/// Why codegen could not produce output. None of these are the user's
/// mistake, so the driver reports them with exit code 2.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum CodegenError {
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
/// [`CodegenError::Verify`] if the generated IR is invalid.
pub fn emit_ir(
    program: &Program,
    checked: &Checked,
    source: &SourceInfo,
) -> Result<String, CodegenError> {
    emit_ir_modules(
        &[Unit {
            path: &[],
            program,
            imports: &[],
            package: None,
            origin: None,
        }],
        checked,
        &[single(source)],
    )
}

/// Lowers a checked program of several modules (the main file first) to
/// verified LLVM IR text.
///
/// # Errors
///
/// As [`emit_ir`].
pub fn emit_ir_modules(
    units: &[Unit],
    checked: &Checked,
    sources: &[ModuleSource],
) -> Result<String, CodegenError> {
    emit_ir_for(units, checked, sources, Target::default())
}

/// [`emit_ir_modules`] for `target` (v17, milestone 134): extern structs
/// cross to C by that target's rules.
///
/// # Errors
///
/// As [`emit_ir`].
pub fn emit_ir_for(
    units: &[Unit],
    checked: &Checked,
    sources: &[ModuleSource],
    target: Target,
) -> Result<String, CodegenError> {
    let context = Context::create();
    let module = lower::lower(&context, units, checked, (sources, target))?;
    Ok(module.print_to_string().to_string())
}

/// A one-file program's source at offset 0.
fn single<'a>(source: &SourceInfo<'a>) -> ModuleSource<'a> {
    ModuleSource {
        name: source.name,
        base: 0,
        text: source.text,
    }
}

/// Lowers a checked `program`, optimises it at `opt` for `cpu` on `target`,
/// and writes a native object file to `path`.
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
    (cpu, target): (Cpu, Target),
    path: &Path,
) -> Result<(), CodegenError> {
    let units = [Unit {
        path: &[],
        program,
        imports: &[],
        package: None,
        origin: None,
    }];
    emit_object_modules(&units, checked, &[single(source)], opt, (cpu, target), path)
}

/// [`emit_object`] for a program of several modules, the main file first.
///
/// # Errors
///
/// As [`emit_object`].
pub fn emit_object_modules(
    units: &[Unit],
    checked: &Checked,
    sources: &[ModuleSource],
    opt: OptLevel,
    (cpu, target): (Cpu, Target),
    path: &Path,
) -> Result<(), CodegenError> {
    let context = Context::create();
    let module = lower::lower(&context, units, checked, (sources, target))?;
    let machine = target_machine(opt, cpu, target)?;
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

/// A target machine for `target` at `cpu`'s level (x86-64; ARM builds for
/// its baseline, ARMv8-A), producing position-independent code because
/// Ubuntu's `cc` links PIE executables by default.
fn target_machine(opt: OptLevel, cpu: Cpu, target: Target) -> Result<TargetMachine, CodegenError> {
    let config = InitializationConfig::default();
    match target {
        Target::X86_64Linux => LlvmTarget::initialize_x86(&config),
        Target::Aarch64Linux => LlvmTarget::initialize_aarch64(&config),
    }
    let triple = TargetTriple::create(target.triple());
    let llvm = LlvmTarget::from_triple(&triple).map_err(|e| CodegenError::Emit(e.to_string()))?;
    let level = match opt {
        OptLevel::O0 => OptimizationLevel::None,
        OptLevel::O2 => OptimizationLevel::Default,
    };
    // A level is a CPU name whose features LLVM knows; only `native` adds
    // the host's own, as every build did before milestone 130.
    let (cpu, features) = match (target, cpu) {
        // The driver refuses `--cpu` for ARM, so only the default reaches here.
        (Target::Aarch64Linux, _) => ("generic".to_string(), String::new()),
        (_, Cpu::X86_64) => ("x86-64".to_string(), String::new()),
        (_, Cpu::V2) => ("x86-64-v2".to_string(), String::new()),
        (_, Cpu::V3) => ("x86-64-v3".to_string(), String::new()),
        (_, Cpu::V4) => ("x86-64-v4".to_string(), String::new()),
        (_, Cpu::Native) => (
            TargetMachine::get_host_cpu_name().to_string(),
            TargetMachine::get_host_cpu_features().to_string(),
        ),
    };
    let machine = llvm
        .create_target_machine(
            &triple,
            &cpu,
            &features,
            level,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or_else(|| CodegenError::Emit(format!("LLVM has no target machine for {triple}")))?;
    // Codegen's scalar sizes (`split_leaves::byte_size`) assume 8-byte pointers.
    assert_eq!(
        machine.get_target_data().get_pointer_byte_size(None),
        8,
        "every target lughac knows is LP64"
    );
    Ok(machine)
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

    /// The IR for `src` lowered for `target` (v17, milestone 134).
    pub fn ir_for(src: &str, target: super::Target) -> String {
        let (tokens, _) = lexer::lex(src).expect("test source lexes");
        let (program, _) = parser::parse(&tokens).expect("test source parses");
        let (checked, _) = check::check(&program).unwrap_or_else(|e| panic!("{src:?}: {e:?}"));
        let unit = check::Unit {
            path: &[],
            program: &program,
            imports: &[],
            package: None,
            origin: None,
        };
        let source = super::ModuleSource {
            name: "test.la",
            base: 0,
            text: src,
        };
        super::emit_ir_for(&[unit], &checked, &[source], target)
            .unwrap_or_else(|e| panic!("{src:?}: {e}"))
    }

    /// The IR of `src` built for `lughac test` (milestone 84).
    pub fn test_ir(src: &str) -> String {
        let (tokens, _) = lexer::lex(src).expect("test source lexes");
        let (program, _) = parser::parse(&tokens).expect("test source parses");
        let unit = check::Unit {
            path: &[],
            program: &program,
            imports: &[],
            package: None,
            origin: None,
        };
        let (checked, _) = check::check_tests(&[unit]).unwrap_or_else(|e| panic!("{src:?}: {e:?}"));
        let source = super::SourceInfo {
            name: "test.la",
            text: src,
        };
        emit_ir(&program, &checked, &source).unwrap_or_else(|e| panic!("{src:?}: {e}"))
    }
}

#[cfg(test)]
mod tests {
    use super::{Cpu, OptLevel, Target, TargetMachine, target_machine};

    #[test]
    fn each_level_is_its_llvm_cpu_with_no_extra_features() {
        for (cpu, name) in [
            (Cpu::default(), "x86-64"),
            (Cpu::V2, "x86-64-v2"),
            (Cpu::V3, "x86-64-v3"),
            (Cpu::V4, "x86-64-v4"),
        ] {
            let machine =
                target_machine(OptLevel::O2, cpu, Target::default()).expect("x86-64 is a target");
            assert_eq!(machine.get_cpu().to_string(), name);
            assert_eq!(machine.get_feature_string().to_string_lossy(), "");
        }
    }

    #[test]
    fn native_is_the_hosts_cpu_and_features() {
        let machine = target_machine(OptLevel::O0, Cpu::Native, Target::default())
            .expect("the host is a target");
        assert_eq!(
            machine.get_cpu().to_string(),
            TargetMachine::get_host_cpu_name().to_string()
        );
        assert_eq!(
            machine.get_feature_string().to_string_lossy(),
            TargetMachine::get_host_cpu_features().to_string()
        );
    }

    #[test]
    fn each_target_has_its_triple_and_arm_builds_for_generic_v17() {
        assert_eq!(Target::default(), Target::X86_64Linux);
        for target in [Target::X86_64Linux, Target::Aarch64Linux] {
            let machine = target_machine(OptLevel::O2, Cpu::default(), target).expect("a target");
            assert_eq!(
                machine.get_triple().as_str().to_string_lossy(),
                target.triple()
            );
        }
        let arm = target_machine(OptLevel::O0, Cpu::default(), Target::Aarch64Linux).unwrap();
        assert_eq!(arm.get_cpu().to_string(), "generic");
    }
}
