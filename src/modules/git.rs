//! Running `git` for downloaded packages (v14 draft §6, milestone 50).
//! Only `https://` and `file://` URLs without user info are accepted, and
//! `git` always runs from an argument list, never a shell, with prompts,
//! hooks, symlinks, submodules and every other protocol turned off. The
//! `Git` trait lets the loader's tests use a fake.

use std::ffi::OsString;
use std::path::Path;
use std::process::Command;

use super::version::Version;

/// The `git` commands the loader needs.
pub trait Git {
    /// The output of `git ls-remote --tags --refs -- url`, or why it failed.
    ///
    /// # Errors
    ///
    /// `git`'s message, or that `git` couldn't be run.
    fn ls_remote(&mut self, url: &str) -> Result<String, String>;
    /// Clones `url` at `tag` into the empty folder `dir`.
    ///
    /// # Errors
    ///
    /// `git`'s message, or that `git` couldn't be run.
    fn clone_tag(&mut self, url: &str, tag: &str, dir: &Path) -> Result<(), String>;
    /// The commit `dir`'s `HEAD` points at, as `git rev-parse` prints it.
    ///
    /// # Errors
    ///
    /// `git`'s message, or that `git` couldn't be run.
    fn head(&mut self, dir: &Path) -> Result<String, String>;
}

/// Why `url` can't be fetched, if it can't: only `https://` and `file://`,
/// with no user info (no credentials in a manifest) and no whitespace or
/// control characters.
pub fn check_url(url: &str) -> Result<(), String> {
    if url.chars().any(|c| c.is_whitespace() || c.is_control()) {
        return Err("a `git` URL may not contain spaces or control characters".into());
    }
    if let Some(rest) = url.strip_prefix("https://") {
        let authority = rest.split('/').next().unwrap_or("");
        if authority.contains('@') {
            return Err("a `git` URL may not hold a user name or password; \
                        let git's credential helper supply them"
                .into());
        }
        if authority.is_empty() {
            return Err("a `git` URL needs a host: `https://host/path`".into());
        }
        return Ok(());
    }
    if url
        .strip_prefix("file://")
        .is_some_and(|p| p.starts_with('/'))
    {
        return Ok(());
    }
    Err("a `git` URL must start `https://` or `file:///`".into())
}

/// The `vMAJOR.MINOR.PATCH` tags in `ls-remote` output, as versions;
/// every other ref, and peeled `^{}` lines, are ignored.
pub fn tags(output: &str) -> Vec<Version> {
    output
        .lines()
        .filter_map(|line| line.split_once('\t'))
        .filter_map(|(_, name)| name.strip_prefix("refs/tags/v"))
        .filter(|name| !name.ends_with("^{}"))
        .filter_map(|name| Version::parse(name).ok())
        .collect()
}

/// Settings passed as `-c` to every `git` command.
pub const CONFIG: [&str; 4] = [
    "core.hooksPath=/dev/null",
    "core.fsmonitor=false",
    "core.symlinks=false",
    "submodule.recurse=false",
];

/// Variables set for every `git` command.
pub const ENV: [(&str, &str); 4] = [
    ("GIT_TERMINAL_PROMPT", "0"),
    ("GIT_ALLOW_PROTOCOL", "https:file"),
    ("GIT_HTTP_LOW_SPEED_LIMIT", "1000"),
    ("GIT_HTTP_LOW_SPEED_TIME", "30"),
];

/// The arguments of one `git` command: the `-c` settings, then `command`.
pub fn arguments(command: &[OsString]) -> Vec<OsString> {
    let mut args = Vec::new();
    for setting in CONFIG {
        args.push(OsString::from("-c"));
        args.push(OsString::from(setting));
    }
    args.extend(command.iter().cloned());
    args
}

/// The system's `git`, found on `PATH`.
pub struct SystemGit;

impl SystemGit {
    fn run(&mut self, command: &[OsString]) -> Result<String, String> {
        let output = Command::new("git")
            .args(arguments(command))
            .envs(ENV)
            .output()
            .map_err(|e| {
                format!("`git` could not be run ({e}); install git to use git packages")
            })?;
        if output.status.success() {
            return Ok(String::from_utf8_lossy(&output.stdout).into_owned());
        }
        let stderr = String::from_utf8_lossy(&output.stderr);
        let lines: Vec<&str> = stderr.lines().take(10).collect();
        Err(format!("`git` failed: {}", lines.join("\n")))
    }
}

impl Git for SystemGit {
    fn ls_remote(&mut self, url: &str) -> Result<String, String> {
        self.run(&os(&["ls-remote", "--tags", "--refs", "--", url]))
    }

    fn clone_tag(&mut self, url: &str, tag: &str, dir: &Path) -> Result<(), String> {
        let mut command = os(&[
            "clone",
            "--quiet",
            "--depth",
            "1",
            "--branch",
            tag,
            "--no-recurse-submodules",
            "--",
            url,
        ]);
        command.push(dir.as_os_str().to_owned());
        self.run(&command).map(|_| ())
    }

    fn head(&mut self, dir: &Path) -> Result<String, String> {
        let mut command = vec![OsString::from("-C"), dir.as_os_str().to_owned()];
        command.extend(os(&["rev-parse", "HEAD"]));
        self.run(&command).map(|out| out.trim().to_string())
    }
}

fn os(args: &[&str]) -> Vec<OsString> {
    args.iter().map(OsString::from).collect()
}

#[cfg(test)]
mod tests {
    use super::{arguments, check_url, os, tags};

    #[test]
    fn only_https_and_file_urls_without_user_info() {
        for good in [
            "https://x.org/geo.git",
            "file:///srv/geo",
            "https://x.org:8443/a/b",
        ] {
            assert_eq!(check_url(good), Ok(()), "{good}");
        }
        for bad in [
            "http://x.org/g",
            "ssh://x.org/g",
            "git://x.org/g",
            "git@x.org:g",
            "ext::sh -c touch",
            "file://relative",
            "https://me:token@x.org/g",
            "https://me@x.org/g",
            "https:///g",
            "https://x.org/a b",
            "https://x.org/\u{7}",
            "-uhttps://x.org",
        ] {
            assert!(check_url(bad).is_err(), "{bad}");
        }
    }

    #[test]
    fn only_version_tags_are_read() {
        let out = "aaa\trefs/tags/v1.0.0\nbbb\trefs/tags/v1.2.3\nccc\trefs/tags/v1.2.3^{}\n\
                   ddd\trefs/tags/latest\neee\trefs/heads/main\nfff\trefs/tags/v2\n";
        let found: Vec<String> = tags(out).iter().map(ToString::to_string).collect();
        assert_eq!(found, ["1.0.0", "1.2.3"]);
    }

    #[test]
    fn every_command_turns_hooks_symlinks_and_submodules_off() {
        let args = arguments(&os(&["ls-remote", "--", "https://x.org/g"]));
        let text: Vec<String> = args.iter().map(|a| a.to_string_lossy().into()).collect();
        assert_eq!(
            text,
            [
                "-c",
                "core.hooksPath=/dev/null",
                "-c",
                "core.fsmonitor=false",
                "-c",
                "core.symlinks=false",
                "-c",
                "submodule.recurse=false",
                "ls-remote",
                "--",
                "https://x.org/g"
            ]
        );
        assert!(super::ENV.contains(&("GIT_TERMINAL_PROMPT", "0")));
        assert!(super::ENV.contains(&("GIT_ALLOW_PROTOCOL", "https:file")));
    }
}
