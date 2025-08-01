mod error;
mod parser;
mod codegen;

use inkwell::context::Context;

use codegen::Codegen;
use codegen::jit::{ run_jit };
use codegen::aot::{run_clang, run_llc, write_bitcode_to_file};

use clap::{Parser, ValueEnum};
use std::path::{Path, PathBuf};
use tempfile::NamedTempFile;

use std::fs;

#[derive(Parser)]
#[command(name = "lugha")]
#[command(about = "Lugha's compiler", long_about = None)]
struct Cli {
    /// Path to input source file
    #[arg()]
    input: PathBuf,

    /// What to emit: bc, obj, or exe
    #[arg(long, default_value = "none", value_enum)]
    emit: Emit,

    // Jit ot Aot
    #[arg(long, default_value = "jit", value_enum)]
    backend: Backend,

    #[arg(long)]
    pub link: bool,

   #[arg(long)]
    pub keep_obj: bool,

    #[arg(long, default_value = "0")]
    pub opt_level: u32,

    /// Output file
    #[arg(short, long)]
    output: Option<String>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Backend {
    Jit,
    Aot,
    None
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Emit {
    Bc,
    Obj,
    Ll,
    None
}

fn main() {
    let cli = Cli::parse();

    let source = std::fs::read_to_string(&cli.input).expect("Failed to read source");
    let span = crate::error::Span::new(&source);
    let stmts = match crate::parser::parser::parse(span) {
        Ok(s) => s,
        Err(e) => return eprintln!("{e}"),
    };

    let output_base = cli.output.clone().unwrap_or_else(|| {
        cli.input
            .file_stem()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string()
    });

    let context = Context::create();
    let mut codegen = Codegen::new(&context, "main_module");
    codegen.compile(&stmts);

    match cli.backend {
        Backend::None => {
            
        },
        Backend::Jit => {
            let result = run_jit(&codegen);
            println!("Program returned: {}", result);
        }
        Backend::Aot => {
            let needs_bitcode = matches!(cli.emit, Emit::Bc | Emit::Obj) || cli.link;

            let mut bitcode_path: Option<PathBuf> = None;

            if needs_bitcode {
                // Create an unnamed temporary file
                let temp = NamedTempFile::new().expect("Failed to create temp file");
                let raw_path = temp.path().to_path_buf();

                // Give it a .bc extension so clang recognizes it
                let extended_path = raw_path.with_extension("bc");
                fs::copy(&raw_path, &extended_path).expect("Failed to prepare bitcode file");

                let success = write_bitcode_to_file(&codegen, &extended_path);
                if !success {
                    eprintln!("Failed to write bitcode");
                    std::process::exit(1);
                }

                bitcode_path = Some(extended_path);
            }

            if cli.link {
                if let Some(ref path) = bitcode_path {
                    println!("Linking with clang...");
                    run_clang(path.to_str().unwrap(), &output_base).unwrap_or_else(|e| {
                        eprintln!("{e}");
                        std::process::exit(1);
                    });

                    if !cli.keep_obj {
                        println!("Cleaning up temporary bitcode file: {}", path.display());
                        fs::remove_file(path).ok();
                    }
                }
            }

            match cli.emit {
                Emit::Ll => {
                    let ll_str = codegen.module.print_to_string().to_string();
                    let ll_path = format!("{output_base}.ll");
                    if Path::new(&ll_path).exists() {
                        println!("Warning: overwriting existing file {}", ll_path);
                    }
                    std::fs::write(&ll_path, ll_str).expect("Failed to write .ll file");
                    println!("LLVM IR written to {}", ll_path);
                }
                Emit::Obj => {
                    if let Some(ref path) = bitcode_path {
                        let obj_path = format!("{output_base}.o");
                        println!("Generating object file with llc...");
                        run_llc(path.to_str().unwrap(), &obj_path).unwrap_or_else(|e| {
                            eprintln!("{e}");
                            std::process::exit(1);
                        });

                        if !cli.keep_obj {
                            println!("Cleaning up temporary bitcode file: {}", path.display());
                            fs::remove_file(path).ok();
                        }
                    }
                }
                Emit::Bc => {
                    if let Some(ref path) = bitcode_path {
                        let out_path = format!("{output_base}.bc");
                        println!("Saving bitcode to {}", out_path);
                        fs::copy(path, &out_path).expect("Failed to copy bitcode file");
                    }
                }
                Emit::None => {
                    println!("No output emitted (--emit none)");
                }
            }
        }
    }
}

