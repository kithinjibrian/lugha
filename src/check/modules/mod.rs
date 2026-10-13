//! Checking a program of several modules (v2 draft §5, §6; PRP-034): every
//! item is keyed by its qualified name (`geometry.Vec2`; a main-file item by
//! its bare name), each module sees its own items unqualified and imported
//! modules through `m.` (`lookup.rs`).

mod lookup;

use std::collections::HashMap;

use super::{Checked, Checker, Type, liveness};
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
}

/// Type-checks a whole program of modules, the main file first.
///
/// # Errors
///
/// Every diagnostic found (errors and warnings) when there is at least one error.
pub fn check_modules(units: &[Unit]) -> Result<(Checked, Vec<Diagnostic>), Vec<Diagnostic>> {
    // Ids are shared across modules (PRP-034), so the last module's count covers all.
    let count = units
        .iter()
        .map(|u| u.program.expr_count)
        .max()
        .unwrap_or(0);
    let mut checker = Checker::new(count as usize);
    checker.modules(units);
    let monos = checker.monomorphize();
    let (layouts, used) = checker.type_instances(&monos);
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
            interfaces,
            annotations: checker.annotations,
            instances: checker.instances,
            monos,
            used,
            captures: checker.captures,
        },
        checker.diagnostics,
    ))
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
        let passes: [Pass; 9] = [
            Checker::collect_interfaces,
            Checker::collect_structs,
            Checker::collect_enums,
            Checker::interface_params,
            Checker::resolve_structs,
            Checker::resolve_enums,
            Checker::resolve_interfaces,
            Checker::collect,
            Checker::collect_methods,
        ];
        for pass in passes {
            for unit in units {
                self.enter(unit);
                pass(self, unit.program);
            }
        }
        for unit in units {
            self.enter(unit);
            self.visibility_rules(unit.program);
        }
        if let Some((main, others)) = units.split_first() {
            self.enter(main);
            self.check_main(main.program);
            for unit in others {
                self.enter(unit);
                self.main_belongs_in_the_main_file(unit.program);
            }
        }
        for unit in units {
            self.enter(unit);
            self.bodies(unit.program);
        }
    }

    /// Makes `unit` the module whose names are being resolved.
    fn enter(&mut self, unit: &Unit) {
        self.module = unit.path.to_vec();
        self.imports = unit
            .program
            .imports
            .iter()
            .enumerate()
            .map(|(n, i)| {
                let written = || i.path.iter().map(|s| s.name.clone()).collect();
                let path: Vec<String> = unit.imports.get(n).cloned().unwrap_or_else(written);
                let last = i.path.last().expect("an import has a path").name.clone();
                (last, path)
            })
            .collect::<HashMap<_, _>>();
    }

    /// The key of `name` declared in the current module.
    pub(super) fn key(&self, name: &str) -> String {
        qualify(&self.module, name)
    }

    /// The module imported as `name` here, unless a local shadows it.
    pub(super) fn imported(&self, name: &str) -> Option<Vec<String>> {
        if self.local(name).is_some() {
            return None;
        }
        self.imports.get(name).cloned()
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
        let mut read = |path: &Path| {
            files
                .iter()
                .find(|(name, _)| Path::new(name) == path)
                .map(|(_, src)| src.as_bytes().to_vec())
                .ok_or_else(|| std::io::Error::from(std::io::ErrorKind::NotFound))
        };
        let loaded = load(Path::new("main.la"), &mut read).expect("files are readable");
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
