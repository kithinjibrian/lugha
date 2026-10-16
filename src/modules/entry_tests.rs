//! Package entry modules (v17, milestone 99, PRP-131): `import N;` loads the
//! package's `lib.la` as the module `N`.

use super::tests::{codes, run};

const GEO: &[u8] = b"[packages]\ngeo = \"libs/geo\"\n";

fn paths(files: &[(&str, &[u8])]) -> (Vec<String>, Vec<(&'static str, String)>) {
    let loaded = run(files).unwrap();
    let paths = loaded.modules.iter().map(|m| m.path.join(".")).collect();
    let codes = codes(&loaded).into_iter().collect();
    (paths, codes)
}

#[test]
fn import_n_loads_lib_la_as_the_module_n() {
    let (paths, codes) = paths(&[
        ("main.la", b"import geo;\nfun main() {}\n"),
        ("lugha.toml", GEO),
        ("libs/geo/lib.la", b"public import shapes;\n"),
        ("libs/geo/shapes.la", b"public fun area(): i64 = 1;\n"),
    ]);
    assert_eq!(codes, []);
    assert_eq!(paths, ["", "geo", "geo.shapes"]);
}

#[test]
fn the_entry_is_in_the_source_folder() {
    let (paths, codes) = paths(&[
        ("main.la", b"import geo as g;\nfun main() {}\n"),
        ("lugha.toml", GEO),
        (
            "libs/geo/lugha.toml",
            b"[package]\nname = \"geo\"\nsource = \"src\"\n",
        ),
        ("libs/geo/src/lib.la", b"public fun one(): i64 = 1;\n"),
    ]);
    assert_eq!(codes, []);
    assert_eq!(paths, ["", "geo"]);
}

#[test]
fn without_lib_la_import_n_is_e0312_with_help() {
    let loaded = run(&[
        ("main.la", b"import geo;\nfun main() {}\n"),
        ("lugha.toml", GEO),
        ("libs/geo/shapes.la", b"fun f() {}\n"),
    ])
    .unwrap();
    assert_eq!(codes(&loaded), [("E0312", "geo".to_string())]);
    let help = loaded.diagnostics[0].help.clone().unwrap_or_default();
    assert!(
        help.contains("lib.la") && help.contains("import geo.<module>;"),
        "{help}"
    );
}

#[test]
fn the_entry_has_one_name() {
    let loaded = run(&[
        ("main.la", b"import geo.lib;\nfun main() {}\n"),
        ("lugha.toml", GEO),
        ("libs/geo/lib.la", b"fun f() {}\n"),
    ])
    .unwrap();
    assert_eq!(codes(&loaded), [("E0312", "geo.lib".to_string())]);
    let help = loaded.diagnostics[0].help.clone().unwrap_or_default();
    assert!(help.contains("import geo;"), "{help}");
    // A `lib/` folder's modules are ordinary.
    let (paths, codes) = paths(&[
        ("main.la", b"import geo.lib.x;\nfun main() {}\n"),
        ("lugha.toml", GEO),
        ("libs/geo/lib/x.la", b"fun f() {}\n"),
    ]);
    assert_eq!(codes, []);
    assert_eq!(paths, ["", "geo.lib.x"]);
}

#[test]
fn std_has_no_entry() {
    let (_, codes) = paths(&[("main.la", b"import std;\nfun main() {}\n")]);
    assert_eq!(codes, [("E0312", "std".to_string())]);
}
