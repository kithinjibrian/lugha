//! The C compiler lughac runs (v17, milestone 132, PRP-164): `cc`, or the
//! words of `$LUGHA_CC` — a program and its first arguments, split on
//! spaces and run from an argument list, never through a shell, as
//! `$PKG_CONFIG` is. One per build, so every compile, the link and the
//! object cache's key agree.

use std::ffi::OsString;
use std::process::Command;

use super::LinkError;

/// A C compiler: a program and the arguments that go before lughac's own.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Compiler {
    program: OsString,
    args: Vec<OsString>,
    /// Set when the words came from `$LUGHA_CC`, for messages.
    from_env: bool,
}

impl Default for Compiler {
    /// `cc`, with no arguments: what lughac ran before milestone 132.
    fn default() -> Compiler {
        Compiler {
            program: OsString::from("cc"),
            args: Vec::new(),
            from_env: false,
        }
    }
}

impl Compiler {
    /// The compiler `$LUGHA_CC` names, or `cc` when it is unset or blank.
    ///
    /// # Errors
    ///
    /// [`LinkError::BadCompiler`] when `$LUGHA_CC` isn't UTF-8, since its
    /// words couldn't be split reliably.
    pub fn from_env() -> Result<Compiler, LinkError> {
        Compiler::from_env_or("cc")
    }

    /// The compiler `$LUGHA_CC` names, or the program `fallback` when it is
    /// unset or blank: a target's own compiler (`aarch64-linux-gnu-gcc`,
    /// v17, milestone 133).
    ///
    /// # Errors
    ///
    /// As [`Compiler::from_env`].
    pub fn from_env_or(fallback: &str) -> Result<Compiler, LinkError> {
        choose(std::env::var_os("LUGHA_CC").as_deref(), fallback)
    }

    /// The compiler `text` names: its first space-separated word is the
    /// program, the rest its first arguments; blank text is `cc`. Quotes,
    /// `$`, `;` and the like are ordinary characters, as no shell runs it.
    pub fn from_words(text: &str) -> Compiler {
        let mut words = text
            .split(' ')
            .filter(|w| !w.is_empty())
            .map(OsString::from);
        match words.next() {
            None => Compiler::default(),
            Some(program) => Compiler {
                program,
                args: words.collect(),
                from_env: true,
            },
        }
    }

    /// A command running the compiler, its first arguments already added.
    pub fn command(&self) -> Command {
        let mut command = Command::new(&self.program);
        command.args(&self.args);
        command
    }

    /// The words, for the object cache's key: two commands printing the
    /// same `--version`, such as `zig cc` for two targets, still differ.
    pub fn words(&self) -> String {
        let mut words = vec![self.program.to_string_lossy().into_owned()];
        words.extend(self.args.iter().map(|a| a.to_string_lossy().into_owned()));
        words.join(" ")
    }

    /// The compiler as messages name it: `` `cc` ``, or
    /// `` `zig cc` (from $LUGHA_CC) ``.
    pub fn name(&self) -> String {
        match self.from_env {
            true => format!("`{}` (from $LUGHA_CC)", self.words()),
            false => format!("`{}`", self.words()),
        }
    }
}

/// The compiler a `$LUGHA_CC` of `value` names, or `fallback` when it is
/// unset or blank.
fn choose(value: Option<&std::ffi::OsStr>, fallback: &str) -> Result<Compiler, LinkError> {
    let fallback = || Compiler {
        program: OsString::from(fallback),
        ..Compiler::default()
    };
    match value.map(|v| v.to_str()) {
        None => Ok(fallback()),
        Some(Some(text)) if text.trim_matches(' ').is_empty() => Ok(fallback()),
        Some(Some(text)) => Ok(Compiler::from_words(text)),
        Some(None) => Err(LinkError::BadCompiler),
    }
}

#[cfg(test)]
mod tests {
    use super::{Compiler, choose};
    use std::ffi::OsStr;

    #[test]
    fn a_target_s_compiler_is_the_fallback_and_lugha_cc_wins_v17() {
        let arm = "aarch64-linux-gnu-gcc";
        assert_eq!(choose(None, arm).unwrap().words(), arm);
        assert_eq!(choose(Some(OsStr::new("  ")), arm).unwrap().words(), arm);
        assert_eq!(choose(None, arm).unwrap().name(), "`aarch64-linux-gnu-gcc`");
        let zig = choose(Some(OsStr::new("zig cc -target aarch64-linux-gnu")), arm).unwrap();
        assert_eq!(zig.words(), "zig cc -target aarch64-linux-gnu");
        assert_eq!(choose(None, "cc").unwrap(), Compiler::default());
    }

    #[test]
    fn a_lugha_cc_that_isnt_utf8_is_refused_v17() {
        use std::os::unix::ffi::OsStrExt;
        let bad = OsStr::from_bytes(b"cc\xff");
        assert!(matches!(
            choose(Some(bad), "cc"),
            Err(super::LinkError::BadCompiler)
        ));
    }

    fn parts(text: &str) -> (String, Vec<String>) {
        let c = Compiler::from_words(text);
        let program = c.program.to_string_lossy().into_owned();
        let args = c
            .args
            .iter()
            .map(|a| a.to_string_lossy().into_owned())
            .collect();
        (program, args)
    }

    #[test]
    fn blank_text_is_cc() {
        for text in ["", "   "] {
            assert_eq!(Compiler::from_words(text), Compiler::default(), "{text:?}");
        }
        assert_eq!(parts(""), ("cc".to_string(), vec![]));
        assert_eq!(Compiler::default().name(), "`cc`");
    }

    #[test]
    fn the_first_word_is_the_program_and_the_rest_come_first() {
        assert_eq!(parts("clang"), ("clang".to_string(), vec![]));
        assert_eq!(
            parts("zig  cc -target  x86_64-linux-musl"),
            (
                "zig".to_string(),
                vec!["cc".into(), "-target".into(), "x86_64-linux-musl".into()]
            )
        );
        let c = Compiler::from_words("zig cc");
        assert_eq!(c.name(), "`zig cc` (from $LUGHA_CC)");
        assert_eq!(c.words(), "zig cc");
        let command = c.command();
        assert_eq!(command.get_program(), "zig");
        assert_eq!(command.get_args().collect::<Vec<_>>(), ["cc"]);
    }

    #[test]
    fn shell_characters_are_ordinary() {
        assert_eq!(parts("a;b"), ("a;b".to_string(), vec![]));
        assert_eq!(
            parts("cc $(x) \"q\" ~/y"),
            (
                "cc".to_string(),
                vec!["$(x)".into(), "\"q\"".into(), "~/y".into()]
            )
        );
    }
}
