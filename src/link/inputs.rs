//! What the `[link]` tables of a build add to it (v16, milestone 87,
//! PRP-117): library folders and libraries, in order and each once, and the
//! C files to compile, each with its own package's header folders and
//! `pkg-config --cflags`.
//!
//! Order: the tables come program first, then packages as they were
//! loaded, so a library comes before the libraries it depends on, as a
//! static link needs. Within a table, its own lists come before what
//! `pkg-config` gives.

use std::ffi::OsString;
use std::path::PathBuf;

use super::pkg_config::{Flag, Query};
use crate::modules::link_table::{LinkKey, PackageLink};
use crate::span::Span;

/// One C file to compile, with the flags its package gives it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CFile {
    /// The file, under its package's folder.
    pub file: PathBuf,
    /// `-I` for each `include` folder, then `pkg-config --cflags`.
    pub flags: Vec<OsString>,
    /// Its `c` line, for E0330.
    pub span: Span,
}

/// Why a plan couldn't be made: `pkg-config` failed or gave a refused flag
/// (E0329), at the `pkg-config` line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlanError {
    /// The `pkg-config` line.
    pub span: Span,
    /// What went wrong.
    pub message: String,
}

/// What a build's `[link]` tables add to its link.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Plan {
    /// `-L` folders, each once, in order.
    search: Vec<PathBuf>,
    /// `-l` libraries, each once, in order.
    libs: Vec<String>,
    /// Whether some `pkg-config --libs` asked for `-pthread`.
    pthread: bool,
    /// C files to compile and link.
    pub c_files: Vec<CFile>,
    /// The first `[link]` line, for E0331; `None` when no table lists
    /// anything, and a link failure is lughac's own.
    pub at: Option<Span>,
}

/// Runs `pkg-config` for some names: [`super::pkg_config::run`], or a fake in tests.
pub type PkgConfig<'a> = dyn FnMut(&[String], Query) -> Result<Vec<Flag>, String> + 'a;

impl Plan {
    /// The plan for `links`, asking `pkg_config` for each table's names.
    ///
    /// # Errors
    ///
    /// A [`PlanError`] at the first `pkg-config` line whose run fails.
    pub fn new(links: &[PackageLink], pkg_config: &mut PkgConfig<'_>) -> Result<Plan, PlanError> {
        let mut plan = Plan::default();
        for link in links {
            let table = &link.table;
            let listing = table.lines.iter().find(|l| !l.values.is_empty());
            plan.at = plan.at.or_else(|| listing.map(|l| l.span));
            let names = table.values(LinkKey::PkgConfig);
            let (mut libs, mut cflags) = (Vec::new(), Vec::new());
            if let Some(line) = table
                .line_of(LinkKey::PkgConfig)
                .filter(|_| !names.is_empty())
            {
                let fail = |message| PlanError {
                    span: line.span,
                    message,
                };
                libs = pkg_config(names, Query::Libs).map_err(fail)?;
                if !table.values(LinkKey::C).is_empty() {
                    cflags = pkg_config(names, Query::Cflags).map_err(fail)?;
                }
            }
            for folder in table.values(LinkKey::Paths) {
                plan.add_search(link.folder.join(folder));
            }
            for name in table.values(LinkKey::Libs) {
                plan.add_lib(name);
            }
            for flag in libs {
                match flag {
                    Flag::Search(folder) => plan.add_search(PathBuf::from(folder)),
                    Flag::Lib(name) => plan.add_lib(&name),
                    Flag::Pthread => plan.pthread = true,
                    // `flags` keeps only the three above for `--libs`.
                    Flag::Include(_) | Flag::Define(_) => {}
                }
            }
            let mut flags: Vec<OsString> = table
                .values(LinkKey::Include)
                .iter()
                .map(|folder| {
                    let mut arg = OsString::from("-I");
                    arg.push(link.folder.join(folder));
                    arg
                })
                .collect();
            flags.extend(cflags.iter().map(|f| OsString::from(f.arg())));
            if let Some(line) = table.line_of(LinkKey::C) {
                for file in &line.values {
                    plan.c_files.push(CFile {
                        file: link.folder.join(file),
                        flags: flags.clone(),
                        span: line.span,
                    });
                }
            }
        }
        Ok(plan)
    }

    /// True when no table lists anything: the link is lughac's own.
    pub fn is_empty(&self) -> bool {
        self.at.is_none()
    }

    /// The arguments the plan adds after `-lgc -lm`: `-L` folders first,
    /// then libraries, then `-pthread`.
    pub fn link_args(&self) -> Vec<OsString> {
        let search = self.search.iter().map(|folder| {
            let mut arg = OsString::from("-L");
            arg.push(folder);
            arg
        });
        let libs = self
            .libs
            .iter()
            .map(|name| OsString::from(format!("-l{name}")));
        let pthread = self.pthread.then(|| OsString::from("-pthread"));
        search.chain(libs).chain(pthread).collect()
    }

    fn add_search(&mut self, folder: PathBuf) {
        if !self.search.contains(&folder) {
            self.search.push(folder);
        }
    }

    fn add_lib(&mut self, name: &str) {
        if !self.libs.iter().any(|l| l == name) {
            self.libs.push(name.to_string());
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::OsString;
    use std::path::PathBuf;

    use super::Plan;
    use crate::link::pkg_config::{Flag, Query};
    use crate::modules::link_table::{LinkKey, LinkLine, LinkTable, PackageLink};
    use crate::span::Span;

    fn package(
        name: Option<&str>,
        folder: &str,
        lines: &[(LinkKey, &[&str], usize)],
    ) -> PackageLink {
        let lines = lines
            .iter()
            .map(|(key, values, at)| LinkLine {
                key: *key,
                values: values.iter().map(|v| v.to_string()).collect(),
                span: Span::new(*at, *at + 1),
            })
            .collect();
        PackageLink {
            package: name.map(str::to_string),
            folder: PathBuf::from(folder),
            table: LinkTable { lines },
        }
    }

    fn strings(args: &[OsString]) -> Vec<String> {
        args.iter()
            .map(|a| a.to_string_lossy().into_owned())
            .collect()
    }

    fn no_pkg_config(_: &[String], _: Query) -> Result<Vec<Flag>, String> {
        panic!("no `pkg-config` names were listed")
    }

    #[test]
    fn nothing_listed_is_an_empty_plan() {
        let plan = Plan::new(&[], &mut no_pkg_config).unwrap();
        assert!(plan.is_empty());
        assert!(plan.link_args().is_empty());
        assert!(plan.c_files.is_empty());
        assert_eq!(plan.at, None);
    }

    #[test]
    fn folders_then_libraries_in_order_each_once() {
        let links = [
            package(
                None,
                "game",
                &[
                    (LinkKey::Libs, &["raylib", "m"], 10),
                    (LinkKey::Paths, &["lib"], 20),
                ],
            ),
            package(
                Some("geo"),
                "pkgs/geo",
                &[
                    (LinkKey::Libs, &["geos", "m"], 30),
                    (LinkKey::Paths, &["lib", "lib2"], 40),
                ],
            ),
        ];
        let plan = Plan::new(&links, &mut no_pkg_config).unwrap();
        assert!(!plan.is_empty());
        assert_eq!(
            strings(&plan.link_args()),
            [
                "-Lgame/lib",
                "-Lpkgs/geo/lib",
                "-Lpkgs/geo/lib2",
                "-lraylib",
                "-lm",
                "-lgeos"
            ]
        );
        assert_eq!(plan.at, Some(Span::new(10, 11)));
    }

    #[test]
    fn pkg_config_flags_follow_the_tables_own() {
        let links = [package(
            None,
            ".",
            &[
                (LinkKey::PkgConfig, &["gtk4", "gl"], 5),
                (LinkKey::Libs, &["m"], 9),
            ],
        )];
        let mut asked = Vec::new();
        let mut pkg = |names: &[String], query: Query| {
            asked.push((names.to_vec(), query));
            Ok(vec![
                Flag::Search("/opt/gtk/lib".into()),
                Flag::Lib("gtk-4".into()),
                Flag::Lib("m".into()),
                Flag::Pthread,
            ])
        };
        let plan = Plan::new(&links, &mut pkg).unwrap();
        // No C files, so no `--cflags`.
        assert_eq!(
            asked,
            [(vec!["gtk4".to_string(), "gl".to_string()], Query::Libs)]
        );
        assert_eq!(
            strings(&plan.link_args()),
            ["-L/opt/gtk/lib", "-lm", "-lgtk-4", "-pthread"]
        );
    }

    #[test]
    fn c_files_get_their_own_packages_headers() {
        let links = [
            package(
                None,
                "game",
                &[
                    (LinkKey::C, &["main_shim.c"], 1),
                    (LinkKey::Include, &["inc"], 2),
                ],
            ),
            package(
                Some("ray"),
                "ray",
                &[
                    (LinkKey::C, &["shim/a.c", "shim/b.c"], 3),
                    (LinkKey::PkgConfig, &["raylib"], 4),
                ],
            ),
        ];
        let mut pkg = |_: &[String], query: Query| {
            Ok(match query {
                Query::Libs => vec![Flag::Lib("raylib".into())],
                Query::Cflags => vec![
                    Flag::Include("/usr/include/ray".into()),
                    Flag::Define("R=1".into()),
                ],
            })
        };
        let plan = Plan::new(&links, &mut pkg).unwrap();
        let files: Vec<(PathBuf, Vec<String>, Span)> = plan
            .c_files
            .iter()
            .map(|c| (c.file.clone(), strings(&c.flags), c.span))
            .collect();
        assert_eq!(
            files,
            [
                (
                    PathBuf::from("game/main_shim.c"),
                    vec!["-Igame/inc".to_string()],
                    Span::new(1, 2)
                ),
                (
                    PathBuf::from("ray/shim/a.c"),
                    vec!["-I/usr/include/ray".to_string(), "-DR=1".to_string()],
                    Span::new(3, 4)
                ),
                (
                    PathBuf::from("ray/shim/b.c"),
                    vec!["-I/usr/include/ray".to_string(), "-DR=1".to_string()],
                    Span::new(3, 4)
                ),
            ]
        );
        assert_eq!(strings(&plan.link_args()), ["-lraylib"]);
    }

    #[test]
    fn a_pkg_config_failure_points_at_its_line() {
        let links = [package(None, ".", &[(LinkKey::PkgConfig, &["gtk4"], 7)])];
        let mut pkg = |_: &[String], _: Query| Err("Package gtk4 was not found".to_string());
        let err = Plan::new(&links, &mut pkg).unwrap_err();
        assert_eq!(err.span, Span::new(7, 8));
        assert!(
            err.message.contains("gtk4 was not found"),
            "{}",
            err.message
        );
    }
}
