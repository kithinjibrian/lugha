use std::path::Path;
use std::process::Command;
use crate::codegen::Codegen;

pub fn write_bitcode_to_file(codegen: &Codegen, path: &Path) -> bool {
    codegen
        .module
        .write_bitcode_to_path(path)
}

pub fn run_llc(input: &str, output: &str) -> Result<(), String> {
    let status = Command::new("llc")
        .args(["-filetype=obj", "-o", output, input])
        .status()
        .map_err(|e| format!("Failed to run llc: {e}"))?;

    if status.success() {
        Ok(())
    } else {
        Err("llc failed".into())
    }
}

pub fn run_clang(input: &str, output: &str) -> Result<(), String> {
    let status = Command::new("clang")
        .args([input, "-o", output])
        .status()
        .map_err(|e| format!("Failed to run clang: {e}"))?;

    if status.success() {
        Ok(())
    } else {
        Err("clang failed".into())
    }
}
