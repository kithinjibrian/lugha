//! Checking a program of several modules (v2 draft §5, §6; PRP-034): every
//! item is keyed by its qualified name (`geometry.Vec2`; a main-file item by
//! its bare name), each module sees its own items unqualified and imported
//! modules through `m.` (`lookup.rs`).

mod lookup;

use std::collections::HashMap;

use super::{Checked, Checker, Instance, Type, liveness};
use crate::ast::Program;
use crate::diagnostic::{Diagnostic, Severity};

/// One module to check: its path segments (empty for the main file) and its tree.
#[derive(Debug, Clone, Copy)]
pub struct Unit<'a> {
    pub path: &'a [String],
    pub program: &'a Program,
    /// Each import's resolved module path (v8, `Module::imports`); empty
    /// means the paths as written, as for a single file.
    pub imports: &'a [Vec<String>],
    /// The module's package (v8, `Module::package`); `None` for the root.
    pub package: Option<&'a str>,
    /// Its file and package folder, for `include_bytes`/`include_str`
    /// (v17, milestone 128); `None` for bundled and in-memory sources.
    pub origin: Option<Origin<'a>>,
}

/// Where a module's file is, and the folder its includes must stay in.
#[derive(Debug, Clone, Copy)]
pub struct Origin<'a> {
    /// The module's `.la` file: include paths are relative to its folder.
    pub file: &'a std::path::Path,
    /// Its package's folder (`Module::folder`).
    pub folder: &'a std::path::Path,
}

impl<'a> Origin<'a> {
    /// The origin of a loaded module; `None` for one with no package folder.
    pub fn of(module: &'a crate::modules::Module) -> Option<Origin<'a>> {
        let folder = module.folder.as_deref()?;
        Some(Origin {
            file: &module.file,
            folder,
        })
    }
}

/// Type-checks a whole program of modules, the main file first.
///
/// # Errors
///
/// Every diagnostic found (errors and warnings) when there is at least one error.
pub fn check_modules(units: &[Unit]) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
    check_with(units, None)
}

/// [`check_modules`] for `lughac test` (v16, milestone 84): the main file
/// needs no `main`, and [`Checked::tests`] lists its tests.
///
/// # Errors
///
/// As [`check_modules`].
pub fn check_tests(units: &[Unit]) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
    check_test_roots(units, 1)
}

/// [`check_tests`] for a program of several roots, the first `roots`
/// units (v17, milestone 124): every root's tests are listed, in order.
///
/// # Errors
///
/// As [`check_modules`].
pub fn check_test_roots(
    units: &[Unit],
    roots: usize,
) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
    check_with(units, Some(roots))
}

/// Checks `units`; with `tests`, lists the tests of that many first units.
fn check_with(
    units: &[Unit],
    tests: Option<usize>,
) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
    let (checker, monos, layouts, used) = run_with(units, tests.is_some());
    if checker
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        return Err(checker.diagnostics);
    }
    let (structs, enums) = checker.tables(&layouts);
    let interfaces = checker.interface_methods();
    // Every expression of a valid program has been visited; `Error` is a safe filler.
    let types = checker
        .types
        .into_iter()
        .map(|t| t.unwrap_or(Type::Error))
        .collect();
    let moves = units
        .iter()
        .flat_map(|u| liveness::moves(u.program, &checker.mentions))
        .collect();
    Ok((
        Checked {
            types,
            structs,
            enums,
            moves,
            callees: checker.callees,
            conversions: checker.conversions,
            try_conversions: checker.try_conversions,
            default_args: checker.default_args,
            iterations: checker.iterations,
            derives: checker.derives,
            derived: checker.derived,
            interfaces,
            annotations: checker.annotations,
            instances: checker.instances,
            monos,
            used,
            captures: checker.captures,
            uses_tasks: checker.uses_tasks,
            c_params: checker.c_params,
            variant_values: checker.variant_values,
            consts: checker
                .const_order
                .iter()
                .map(|k| (k.clone(), checker.consts[k].ty.clone()))
                .collect(),
            const_uses: checker.const_uses,
            includes: checker.includes,
            tests: tests.map(|roots| {
                // Qualified, so a package's module keeps its tests' keys (v17).
                units
                    .iter()
                    .take(roots)
                    .flat_map(|u| {
                        let tests = super::test_fns::tests_of(u.program);
                        tests
                            .into_iter()
                            .map(|t| qualify(u.path, &t))
                            .collect::<Vec<_>>()
                    })
                    .collect()
            }),
        },
        checker.diagnostics,
    ))
}

/// Every checking pass over `units`, before codegen's tables are built:
/// shared by `check_modules` and the language server's `check_partial`.
pub(in crate::check) fn run(units: &[Unit]) -> (Checker, Vec<Instance>, Vec<Type>, Vec<Type>) {
    run_with(units, false)
}

fn run_with(units: &[Unit], tests: bool) -> (Checker, Vec<Instance>, Vec<Type>, Vec<Type>) {
    // Ids are shared across modules (PRP-034), so the last module's count covers all.
    let count = units
        .iter()
        .map(|u| u.program.expr_count)
        .max()
        .unwrap_or(0);
    let mut checker = Checker::new(count as usize);
    checker.test_mode = tests;
    checker.modules(units);
    let monos = checker.monomorphize();
    checker.instance_literals(&monos);
    let (layouts, used) = checker.type_instances(&monos);
    (checker, monos, layouts, used)
}

/// The key of item `name` in module `path`: `a.b.name`, or `name` in the main file.
pub fn qualify(path: &[String], name: &str) -> String {
    if path.is_empty() {
        name.to_string()
    } else {
        format!("{}.{name}", path.join("."))
    }
}

/// A key as a symbol: `a.b.f` becomes `a/b/f`. Methods add `.name`, so
/// function `c` of module `a.b` (`a/b/c`) can't collide with method `c` of
/// type `b` in module `a` (`a/b.c`; PRP-034).
pub fn symbol(key: &str) -> String {
    key.replace('.', "/")
}

impl Checker {
    /// Every pass of spec §6 over every module in turn, then every body.
    fn modules(&mut self, units: &[Unit]) {
        type Pass = fn(&mut Checker, &Program);
        let passes: [Pass; 13] = [
            Checker::collect_handles,
            Checker::collect_interfaces,
            Checker::collect_structs,
            Checker::collect_enums,
            Checker::interface_params,
            Checker::resolve_structs,
            Checker::resolve_enums,
            Checker::resolve_interfaces,
            Checker::collect,
            Checker::collect_consts,
            Checker::collect_methods,
            Checker::collect_derives,
            Checker::check_actors,
        ];
        // Named imports first, so every pass resolves through them (v11).
        for unit in units {
            self.enter(unit);
            self.named_imports(unit);
            self.glob_imports(unit, units);
        }
        for pass in passes {
            for unit in units {
                self.enter(unit);
                pass(self, unit.program);
            }
        }
        for unit in units {
            self.enter(unit);
            self.visibility_rules(unit.program);
            self.check_named_imports(unit);
            self.settle_globs();
            self.test_shapes(unit.program);
        }
        if let Some((main, others)) = units.split_first() {
            self.enter(main);
            // `lughac test` runs tests, never `main` (milestone 84).
            if !self.test_mode {
                self.check_main(main.program);
            }
            for unit in others {
                self.enter(unit);
                self.main_belongs_in_the_main_file(unit.program);
            }
        }
        for unit in units {
            self.enter(unit);
            self.const_values(unit.program);
            self.bodies(unit.program);
        }
        self.order_consts();
    }

    /// Makes `unit` the module whose names are being resolved.
    fn enter(&mut self, unit: &Unit) {
        self.module = unit.path.to_vec();
        self.package = unit.package.map(str::to_string);
        self.origin = unit
            .origin
            .map(|o| (o.file.to_path_buf(), o.folder.to_path_buf()));
        self.imports = unit
            .program
            .imports
            .iter()
            .enumerate()
            .map(|(n, i)| {
                let written = || i.path.iter().map(|s| s.name.clone()).collect();
                let path: Vec<String> = unit.imports.get(n).cloned().unwrap_or_else(written);
                for segment in i.path.iter().chain(&i.alias) {
                    self.hover.module(segment.span, &path);
                }
                (i.bound().name.clone(), path)
            })
            .collect::<HashMap<_, _>>();
    }

    /// The key of `name` declared in the current module.
    pub(super) fn key(&self, name: &str) -> String {
        // A named import or re-export means the item it names (v11).
        self.follow(qualify(&self.module, name))
    }

    /// The module imported as `name` here, unless a local shadows it.
    pub(super) fn imported(&self, name: &str) -> Option<Vec<String>> {
        self.module_path(name).map(|(path, _)| path)
    }
}

#[cfg(test)]
pub(crate) mod test_util {
    use std::path::Path;

    use super::{Unit, check_modules};
    use crate::check::Checked;
    use crate::diagnostic::Diagnostic;
    use crate::modules::{Loaded, load};

    /// Loads `files` (`main.la` first) from memory.
    pub fn loaded(files: &[(&str, &str)]) -> Loaded {
        // A folder exists when a file is under it (v8 manifests probe folders).
        let mut read = |path: &Path| match files.iter().find(|(name, _)| Path::new(name) == path) {
            Some((_, src)) => Ok(src.as_bytes().to_vec()),
            None if files
                .iter()
                .any(|(name, _)| Path::new(name).starts_with(path)) =>
            {
                Err(std::io::Error::from(std::io::ErrorKind::IsADirectory))
            }
            None => Err(std::io::Error::from(std::io::ErrorKind::NotFound)),
        };
        let loaded = load(
            Path::new("main.la"),
            &mut read,
            crate::modules::layout::LoadEnv::plain(),
        )
        .expect("files are readable");
        assert!(loaded.diagnostics.is_empty(), "{:?}", loaded.diagnostics);
        loaded
    }

    /// Checks every module of `loaded`.
    pub fn check(loaded: &Loaded) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
        let units: Vec<Unit> = loaded
            .modules
            .iter()
            .map(|m| Unit {
                path: &m.path,
                program: m.program.as_ref().expect("parsed"),
                imports: &m.imports,
                package: m.package.as_deref(),
                origin: super::Origin::of(m),
            })
            .collect();
        check_modules(&units)
    }

    /// `(code, the text its span covers)` for every diagnostic of `files`.
    pub fn errors(files: &[(&str, &str)]) -> Vec<(&'static str, String)> {
        let loaded = loaded(files);
        let diagnostics = match check(&loaded) {
            Ok((_, warnings)) => warnings,
            Err(diagnostics) => diagnostics,
        };
        diagnostics
            .iter()
            .map(|d| {
                let m = loaded
                    .modules
                    .iter()
                    .rev()
                    .find(|m| m.base <= d.span.start)
                    .expect("a module holds every span");
                (
                    d.code,
                    m.text[d.span.start - m.base..d.span.end - m.base].to_string(),
                )
            })
            .collect()
    }

    /// The codes of checking `files` with the prelude, as a program is
    /// checked (v15: `std.math` needs the prelude's `Float`).
    pub fn prelude_codes(files: &[(&str, &str)]) -> Vec<&'static str> {
        let mut loaded = loaded(files);
        let mut errors = Vec::new();
        crate::modules::with_prelude(&mut loaded.modules, &mut errors);
        assert!(errors.is_empty(), "{errors:?}");
        match check(&loaded) {
            Ok((_, warnings)) => warnings.iter().map(|d| d.code).collect(),
            Err(diagnostics) => diagnostics.iter().map(|d| d.code).collect(),
        }
    }

    /// Checks `files`, which must have no errors.
    pub fn ok(files: &[(&str, &str)]) -> Checked {
        check(&loaded(files)).unwrap_or_else(|d| panic!("{d:?}")).0
    }
}

#[cfg(test)]
mod tests {
    use super::qualify;

    #[test]
    fn keys_are_qualified_outside_the_main_file() {
        assert_eq!(qualify(&[], "Point"), "Point");
        assert_eq!(
            qualify(&["shapes".into(), "circle".into()], "Circle"),
            "shapes.circle.Circle"
        );
    }
}

#[cfg(test)]
mod root_tests {
    use std::path::{Path, PathBuf};

    use super::{Unit, check_test_roots};
    use crate::modules::layout::LoadEnv;

    #[test]
    fn every_roots_tests_are_listed_by_their_modules_v17() {
        let files = [
            (
                "a.la",
                "fun helper(): i64 = 1;\n@test fun works() { assert(helper() == 1); }\n",
            ),
            (
                "b.la",
                "fun helper(): i64 = 2;\n@test fun works() {}\n@test fun more() {}\n",
            ),
        ];
        let mut read = |path: &Path| match files.iter().find(|(n, _)| Path::new(n) == path) {
            Some((_, src)) => Ok(src.as_bytes().to_vec()),
            None => Err(std::io::Error::from(std::io::ErrorKind::NotFound)),
        };
        let roots = [PathBuf::from("a.la"), PathBuf::from("b.la")];
        let env = LoadEnv {
            as_module: true,
            ..LoadEnv::plain()
        };
        let loaded = crate::modules::load_all(&roots, &mut read, env).expect("readable");
        let units: Vec<Unit> = loaded
            .modules
            .iter()
            .map(|m| Unit {
                path: &m.path,
                program: m.program.as_ref().expect("parsed"),
                imports: &m.imports,
                package: m.package.as_deref(),
                origin: super::Origin::of(m),
            })
            .collect();
        let (checked, _) = check_test_roots(&units, 2).unwrap_or_else(|e| panic!("{e:?}"));
        assert_eq!(checked.tests.unwrap(), ["a.works", "b.works", "b.more"]);
    }
}
