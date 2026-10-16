//! `lughac init` (v16 draft decisions 9–11, milestone 69): starts a program
//! or a library in a folder.
//!
//! Never overwrites: an existing `lugha.toml` stops it before anything is
//! written, and any other file that exists is kept. Runs no `git` and
//! never prompts.

use std::io::{self, Write};
use std::path::{Component, Path, PathBuf};

use thiserror::Error;

use crate::lexer::token::keyword;

/// What `lughac init` was asked to make.
pub(super) struct Request<'a> {
    /// The folder as given; `None` is the current one.
    pub folder: Option<&'a Path>,
    /// A library instead of a program.
    pub lib: bool,
    /// `--name`; otherwise the folder's name.
    pub name: Option<&'a str>,
}

/// Why `lughac init` stopped. Every one is exit 2: bad usage or the system.
#[derive(Debug, Error)]
enum InitError {
    #[error("`{name}` isn't a valid package name")]
    InvalidName { name: String, help: String },
    #[error("{0} already has a lugha.toml")]
    HasManifest(String),
    #[error("cannot create `{path}`: {source}")]
    Io { path: String, source: io::Error },
    #[error("cannot find the current folder: {0}")]
    CurrentDir(io::Error),
}

/// Runs `lughac init`, printing a line per file and a next step to `stdout`,
/// or the error to `stderr`. Returns the exit code: 0, or 2 on any error.
pub(super) fn init(request: &Request, stdout: &mut dyn Write, stderr: &mut dyn Write) -> u8 {
    match create(request, stdout) {
        Ok(()) => 0,
        Err(error) => {
            // lughac's own output: a closed stderr leaves only the exit code to say it.
            let _ = writeln!(stderr, "error: {error}");
            if let InitError::InvalidName { help, .. } = &error {
                let _ = writeln!(stderr, "  = help: {help}");
            }
            2
        }
    }
}

fn create(request: &Request, stdout: &mut dyn Write) -> Result<(), InitError> {
    let folder = request.folder.unwrap_or(Path::new(""));
    let name = match request.name {
        Some(name) => name.to_string(),
        None => folder_name(folder)?,
    };
    if !valid(&name) {
        return Err(InitError::InvalidName {
            help: help(request, &name),
            name,
        });
    }
    // The manifest is what makes a folder a package: finding one means this
    // folder is already set up, so nothing at all is written.
    if folder.join("lugha.toml").exists() {
        return Err(InitError::HasManifest(match request.folder {
            Some(f) => format!("`{}`", f.display()),
            None => "this folder".to_string(),
        }));
    }
    // A library's entry (v17, milestone 99): `import {name};` reaches it.
    let main = if request.lib {
        "src/lib.la".to_string()
    } else {
        "src/main.la".to_string()
    };
    let mut files = vec![("lugha.toml".to_string(), manifest(&name))];
    if request.lib {
        files.push((main, library(&name)));
        files.push(("examples/hello.la".to_string(), example(&name)));
        files.push((".gitignore".to_string(), gitignore("hello")));
    } else {
        files.push((main, program(&name)));
        files.push((".gitignore".to_string(), gitignore("main")));
    }
    for (rel, text) in &files {
        let path = folder.join(rel);
        let shown = path.display().to_string();
        let io = |source| InitError::Io {
            path: shown.clone(),
            source,
        };
        let parent = path.parent().unwrap_or(folder);
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent).map_err(|source| InitError::Io {
                path: parent.display().to_string(),
                source,
            })?;
        }
        // `create_new` fails on an existing file instead of truncating it,
        // so a file that appears between a check and the write is kept too.
        let line = match std::fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
        {
            Ok(mut file) => {
                file.write_all(text.as_bytes()).map_err(io)?;
                format!("created {shown}")
            }
            Err(error) if error.kind() == io::ErrorKind::AlreadyExists => {
                format!("kept    {shown} (already exists)")
            }
            Err(error) => return Err(io(error)),
        };
        // A closed stdout doesn't undo the files; the exit code stays 0.
        let _ = writeln!(stdout, "{line}");
    }
    let (what, file) = if request.lib {
        ("run the example with", folder.join("examples/hello.la"))
    } else {
        ("run it with", folder.join("src/main.la"))
    };
    let _ = writeln!(stdout, "{what}: lughac run {}", file.display());
    Ok(())
}

/// The last component of `folder` once `.` and `..` are resolved: `init .`
/// in `~/oyster` names the package `oyster`.
fn folder_name(folder: &Path) -> Result<String, InitError> {
    let cwd = std::env::current_dir().map_err(InitError::CurrentDir)?;
    let mut path = PathBuf::new();
    for part in cwd.join(folder).components() {
        match part {
            Component::CurDir => {}
            Component::ParentDir => {
                path.pop();
            }
            other => path.push(other),
        }
    }
    Ok(path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default())
}

/// A package name is an identifier that is neither a keyword nor `std`
/// (spec §6: `std` is reserved), since programs write it in `import`.
fn valid(name: &str) -> bool {
    let mut chars = name.chars();
    let starts = chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_');
    starts
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        && keyword(name).is_none()
        && name != "std"
}

/// Suggests `--name` with the nearest identifier, when there is a valid one.
fn help(request: &Request, name: &str) -> String {
    let mut fixed: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect();
    if fixed.starts_with(|c: char| c.is_ascii_digit()) {
        fixed.insert(0, '_');
    }
    if !valid(&fixed) {
        return "a package name is an identifier that isn't a keyword or `std`".to_string();
    }
    let mut command = "lughac init".to_string();
    if let Some(folder) = request.folder {
        command.push_str(&format!(" {}", folder.display()));
    }
    if request.lib {
        command.push_str(" --lib");
    }
    format!("try `{command} --name {fixed}`")
}

fn manifest(name: &str) -> String {
    format!(
        "# The package manifest (Lugha spec §6).\n\
         [package]\n\
         name = \"{name}\"\n\
         version = \"0.1.0\"\n\
         source = \"src\"\n"
    )
}

fn program(name: &str) -> String {
    format!(
        "// {name}: a Lugha program. Run it with `lughac run src/main.la`.\n\
         \n\
         fun main() {{\n    println(\"Hello from {name}!\");\n}}\n"
    )
}

fn library(name: &str) -> String {
    format!(
        "/**\n\
         \x20* {name}: a Lugha library. This is its entry, `import {name};`;\n\
         \x20* its other modules live beside it in `src/`.\n\
         \x20*/\n\
         \n\
         // Returns a greeting from this library.\n\
         public fun greet(who: string): string = \"Hello, {{who}}, from {name}!\";\n"
    )
}

fn example(name: &str) -> String {
    format!(
        "// An example using {name}. Run it with `lughac run examples/hello.la`.\n\
         import {name};\n\
         \n\
         fun main() {{\n    println({name}.greet(\"world\"));\n}}\n"
    )
}

/// `lughac build` writes the executable to the current folder, named after
/// the file it builds.
fn gitignore(executable: &str) -> String {
    format!("# Executables from `lughac build`\n/{executable}\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_are_identifiers_but_not_keywords_or_std() {
        assert!(valid("oyster"));
        assert!(valid("_x2"));
        assert!(!valid(""));
        assert!(!valid("my-shell"));
        assert!(!valid("2d"));
        assert!(!valid("fun"));
        assert!(!valid("i64"));
        assert!(!valid("std"));
        assert!(!valid("café"));
    }

    #[test]
    fn help_suggests_the_nearest_identifier() {
        let request = |folder: Option<&'static str>, lib| Request {
            folder: folder.map(Path::new),
            lib,
            name: None,
        };
        assert_eq!(
            help(&request(Some("my-shell"), false), "my-shell"),
            "try `lughac init my-shell --name my_shell`"
        );
        assert_eq!(
            help(&request(None, true), "2d"),
            "try `lughac init --lib --name _2d`"
        );
        assert_eq!(
            help(&request(None, false), "fun"),
            "a package name is an identifier that isn't a keyword or `std`"
        );
    }

    #[test]
    fn the_folder_name_resolves_dots() {
        let cwd = std::env::current_dir().unwrap();
        let last = cwd.file_name().unwrap().to_string_lossy().into_owned();
        assert_eq!(folder_name(Path::new("")).unwrap(), last);
        assert_eq!(folder_name(Path::new(".")).unwrap(), last);
        assert_eq!(folder_name(Path::new("a/..")).unwrap(), last);
        assert_eq!(folder_name(Path::new("a/./b")).unwrap(), "b");
    }
}
