//! Pure code for constants (v17, milestone 147, PRP-178): a constant's value
//! may run any code that never reaches outside the program. Each body
//! records what it does outside (`effects`) and the constants it reads
//! (`reads`); after every body, each constant's value is followed through
//! the calls it reaches, at their real type arguments.

use std::collections::{HashMap, HashSet};

use super::instances::{Instance, MAX_DEPTH, Use};
use super::{Checker, errors};
use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// Something a body does outside the program.
#[derive(Debug, Clone)]
pub(super) struct Effect {
    /// What does it, as written (`read_file`), or empty when only the
    /// function holding it is worth naming (`$os` inside std).
    pub name: String,
    /// What it does, finishing "`f` …": `reads files`.
    pub reason: &'static str,
    pub span: Span,
}

/// The constants a value reads, each with the functions on the way to it
/// (empty when the value reads it itself).
pub(super) type Reads = Vec<(String, Vec<String>)>;

/// The longest path E0445 shows in full.
const SHOWN: usize = 6;

/// The owner a constant's value is checked under, so its calls, effects and
/// reads are recorded like a body's.
pub(super) fn const_owner(key: &str) -> String {
    format!("$const.{key}")
}

/// Why intrinsic `name` reaches outside the program, or `None` when it doesn't.
fn intrinsic_reason(name: &str) -> Option<&'static str> {
    Some(match name {
        "print" | "println" | "eprint" | "eprintln" | "flush" => "prints",
        "read_line" | "read_stdin_bytes" => "reads input",
        "read_file" | "read_bytes" => "reads files",
        "write_file" | "append_file" | "write_bytes" | "append_bytes" => "writes files",
        "args" => "reads the command line",
        "exit" => "ends the program",
        "c_read" | "c_write" => "uses C memory",
        _ => return None,
    })
}

/// A key as people write it: `time.now_ms`, `Map.insert`.
fn shown(key: &str) -> String {
    let key = key.strip_prefix("$prelude.").unwrap_or(key);
    key.strip_prefix("std.").unwrap_or(key).to_string()
}

/// "`a` calls `b`, which calls `c`, which reads files", shortened past [`SHOWN`].
fn path_text(names: &[String], reason: &str) -> String {
    let mut names: Vec<String> = names.iter().map(|n| format!("`{n}`")).collect();
    if names.len() > SHOWN {
        let last = names.pop().expect("longer than SHOWN");
        names.truncate(SHOWN - 1);
        names.push("…".to_string());
        names.push(last);
    }
    match names.split_first() {
        None => format!("it {reason}"),
        Some((first, [])) => format!("{first} {reason}"),
        Some((first, rest)) => {
            let calls: Vec<String> = rest.iter().map(|n| format!("calls {n}")).collect();
            format!("{first} {}, which {reason}", calls.join(", which "))
        }
    }
}

impl Checker {
    /// Records intrinsic `name`, called at `span`, if it reaches outside the program.
    pub(super) fn intrinsic_effect(&mut self, name: &str, span: Span) {
        if let Some(reason) = intrinsic_reason(name) {
            self.effect(name, reason, span);
        }
    }

    /// Records that the body being checked does something outside the program.
    pub(super) fn effect(&mut self, name: &str, reason: &'static str, span: Span) {
        let effect = Effect {
            name: name.to_string(),
            reason,
            span,
        };
        self.effects
            .entry(self.owner.clone())
            .or_default()
            .push(effect);
    }

    /// Records that the body being checked reads constant `key`.
    pub(super) fn read_const(&mut self, key: &str) {
        let reads = self.reads.entry(self.owner.clone()).or_default();
        if !reads.iter().any(|k| k == key) {
            reads.push(key.to_string());
        }
    }

    /// Every recorded call, by the body it is in.
    pub(super) fn calls_by_owner(&self) -> HashMap<&str, Vec<&Use>> {
        let mut index: HashMap<&str, Vec<&Use>> = HashMap::new();
        for u in self.uses.iter().chain(&self.calls) {
            index.entry(u.owner.as_str()).or_default().push(u);
        }
        index
    }

    /// Follows constant `key`'s value through every call it reaches: E0445
    /// for the first effect on the way, and every constant read on the way.
    pub(super) fn reach_const(
        &self,
        key: &str,
        index: &HashMap<&str, Vec<&Use>>,
    ) -> (Option<Diagnostic>, Reads) {
        let root = Instance {
            key: const_owner(key),
            args: Vec::new(),
        };
        // Each instance with the one that called it and where, and its depth.
        let mut nodes: Vec<(Instance, Option<usize>, Span, usize)> =
            vec![(root.clone(), None, Span::new(0, 0), 0)];
        let mut seen: HashSet<Instance> = HashSet::from([root]);
        let mut reads: Reads = Vec::new();
        let mut first: Option<(usize, Effect)> = None;
        let mut next = 0;
        while next < nodes.len() {
            let (instance, _, at, depth) = nodes[next].clone();
            let here = next;
            next += 1;
            if self
                .functions
                .get(&instance.key)
                .is_some_and(|f| f.is_extern)
            {
                let effect = Effect {
                    name: String::new(),
                    reason: "calls C",
                    span: at,
                };
                first.get_or_insert((here, effect));
                continue;
            }
            if let Some(effect) = self.effects.get(&instance.key).and_then(|e| e.first()) {
                first.get_or_insert((here, effect.clone()));
            }
            for read in self.reads.get(&instance.key).into_iter().flatten() {
                if !reads.iter().any(|(k, _)| k == read) {
                    reads.push((read.clone(), names(&nodes, here)));
                }
            }
            // E0420 is monomorphization's to report; purity stops looking.
            if depth >= MAX_DEPTH {
                continue;
            }
            let generics = self.params_of(&instance.key);
            for u in index.get(instance.key.as_str()).into_iter().flatten() {
                for target in self.targets(u, &generics, &instance.args, true) {
                    if seen.insert(target.clone()) {
                        nodes.push((target, Some(here), u.span, depth + 1));
                    }
                }
            }
        }
        let diagnostic = first.map(|(at, effect)| {
            let mut path = names(&nodes, at);
            if !effect.name.is_empty() {
                path.push(effect.name.clone());
            }
            // The value's own part that leads there: the call, or the effect itself.
            let span = step_from_root(&nodes, at).unwrap_or(effect.span);
            errors::impure(&path_text(&path, effect.reason), span)
        });
        (diagnostic, reads)
    }
}

/// The functions from a constant's value to node `at`, as written.
fn names(nodes: &[(Instance, Option<usize>, Span, usize)], mut at: usize) -> Vec<String> {
    let mut path = Vec::new();
    while let Some(parent) = nodes[at].1 {
        path.push(shown(&nodes[at].0.key));
        at = parent;
    }
    path.reverse();
    path
}

/// Where the constant's value makes the call leading to node `at`, unless
/// `at` is the value itself.
fn step_from_root(nodes: &[(Instance, Option<usize>, Span, usize)], mut at: usize) -> Option<Span> {
    let mut step = None;
    while let Some(parent) = nodes[at].1 {
        step = Some(nodes[at].2);
        at = parent;
    }
    step
}

/// How a cycle of constants reads: `A -> B -> A`, or with the functions on
/// the way when there are any: `A calls `f`, which reads A`.
pub(super) fn cycle_text(steps: &[(&str, &[String])], last: &str) -> String {
    if steps.iter().all(|(_, via)| via.is_empty()) {
        let mut names: Vec<&str> = steps.iter().map(|(n, _)| *n).collect();
        names.push(last);
        return names.join(" -> ");
    }
    let mut text = String::new();
    for (i, (name, via)) in steps.iter().enumerate() {
        if i == 0 {
            text.push_str(name);
        }
        for f in via.iter() {
            text.push_str(&format!(" calls `{f}`, which"));
        }
        let next = steps.get(i + 1).map_or(last, |(n, _)| *n);
        text.push_str(&format!(" reads {next}"));
        if i + 1 < steps.len() {
            text.push_str(", which");
        }
    }
    text
}

#[cfg(test)]
mod tests {
    use super::{cycle_text, path_text};
    use crate::check::modules::test_util::{errors, ok, prelude_codes};
    use crate::check::test_util::{diagnostics, errors as one_file_errors};

    /// The help of the first diagnostic of a one-file program.
    fn help(src: &str) -> String {
        let d = diagnostics(src);
        d[0].help.clone().unwrap_or_default()
    }

    #[test]
    fn paths_read_as_sentences() {
        let names = |xs: &[&str]| xs.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        assert_eq!(
            path_text(&names(&["read_file"]), "reads files"),
            "`read_file` reads files"
        );
        assert_eq!(
            path_text(&names(&["load", "read_file"]), "reads files"),
            "`load` calls `read_file`, which reads files"
        );
        let long = names(&["a", "b", "c", "d", "e", "f", "g", "h"]);
        assert_eq!(
            path_text(&long, "prints"),
            "`a` calls `b`, which calls `c`, which calls `d`, which calls `e`, which calls …, which calls `h`, which prints"
        );
        let f = names(&["f"]);
        assert_eq!(cycle_text(&[("A", &[]), ("B", &[])], "A"), "A -> B -> A");
        assert_eq!(cycle_text(&[("A", &f)], "A"), "A calls `f`, which reads A");
    }

    #[test]
    fn pure_code_is_accepted() {
        ok(&[(
            "main.la",
            "fun square(x: i64): i64 = x * x;\n\
             fun table(n: i64): i64[] { let mut t: i64[] = []; let mut i = 0; while i < n { t.push(square(i)); i += 1; } t }\n\
             fun fact(n: i64): i64 = if n < 2 { 1 } else { n * fact(n - 1) };\n\
             fun counted(): i64 { let r = Ref.new(0); r.set(r.get() + 1); r.get() }\n\
             fun checked(x: i64): i64 { if x < 0 { panic(\"negative\"); } x }\n\
             fun apply(f: fun(i64): i64, x: i64): i64 = f(x);\n\
             const T: i64[] = table(4);\n\
             const F: i64 = fact(5);\n\
             const C: i64 = counted() + checked(1);\n\
             const M: string = if F > 100 { \"big\" } else { \"small\" };\n\
             const S: string = \"{F} {M}\";\n\
             const D: i64 = apply(fun(x: i64): i64 = x * 2, 3) + { let y = 2; y };\n\
             fun main() {}",
        )]);
    }

    #[test]
    fn each_effect_is_e0445_with_its_reason() {
        for (body, reason) in [
            ("println(\"hi\"); 1", "`f` calls `println`, which prints"),
            (
                "match read_file(\"x\") { Ok(s) => s.len, Err(_) => 0 }",
                "`f` calls `read_file`, which reads files",
            ),
            (
                "args().len",
                "`f` calls `args`, which reads the command line",
            ),
            (
                "let t = spawn g(); t.join()",
                "`f` calls `spawn`, which starts a task",
            ),
            (
                "let c: Chan<i64> = Chan.new(); 1",
                "`f` calls `Chan.new`, which makes a channel",
            ),
            ("abs(1) as i64", "`f` calls `abs`, which calls C"),
        ] {
            let src = format!(
                "extern fun abs(x: i32): i32;\nfun g(): i64 = 1;\nfun f(): i64 {{ {body} }}\nconst A: i64 = f();\nfun main() {{}}"
            );
            assert_eq!(one_file_errors(&src), [("E0445", "f()")], "{body}");
            assert!(help(&src).starts_with(reason), "{body}: {}", help(&src));
        }
        // An effect in the value itself, and one two calls deep.
        let src =
            "const A: i64 = match read_file(\"x\") { Ok(s) => s.len, Err(_) => 0 };\nfun main() {}";
        assert_eq!(one_file_errors(src)[0].0, "E0445");
        assert!(
            help(src).starts_with("`read_file` reads files"),
            "{}",
            help(src)
        );
        let src = "fun a(): i64 = b();\nfun b(): i64 { print(1); 1 }\nconst A: i64 = a() + 1;\nfun main() {}";
        assert_eq!(one_file_errors(src), [("E0445", "a()")]);
        assert!(
            help(src).starts_with("`a` calls `b`, which calls `print`, which prints"),
            "{}",
            help(src)
        );
    }

    #[test]
    fn effects_are_found_through_values_and_interfaces() {
        // A function passed as a value.
        let src = "fun loud(x: i64): i64 { println(x); x }\n\
                   fun apply(f: fun(i64): i64): i64 = f(1);\n\
                   const A: i64 = apply(loud);\nfun main() {}";
        assert_eq!(one_file_errors(src)[0].0, "E0445");
        assert!(help(src).contains("`loud`"), "{}", help(src));
        // An interface value whose type's method prints.
        let src = "interface Size { fun size(self): i64; }\n\
                   struct Loud {}\nimpl Loud { fun size(self): i64 { println(1); 1 } }\n\
                   fun measure(s: Size): i64 = s.size();\n\
                   const A: i64 = measure(Loud {});\nfun main() {}";
        assert_eq!(one_file_errors(src)[0].0, "E0445");
    }

    #[test]
    fn generics_are_judged_at_their_real_types() {
        let lib = "interface Measure { fun size(self): i64; }\n\
                   struct Quiet {}\nimpl Quiet { fun size(self): i64 = 1; }\n\
                   struct Loud {}\nimpl Loud { fun size(self): i64 { println(2); 2 } }\n\
                   fun total<T: Measure>(x: T): i64 = x.size() + 1;\n";
        ok(&[(
            "main.la",
            &format!("{lib}const A: i64 = total(Quiet {{}});\nfun main() {{}}"),
        )]);
        let src = format!("{lib}const B: i64 = total(Loud {{}});\nfun main() {{}}");
        assert_eq!(one_file_errors(&src)[0].0, "E0445");
        assert!(
            help(&src)
                .starts_with("`total` calls `Loud.size`, which calls `println`, which prints"),
            "{}",
            help(&src)
        );
    }

    #[test]
    fn a_types_own_show_and_eq_are_followed() {
        // Interpolation calls `show` without naming it.
        let src = "struct P { x: i64 }\n\
                   impl P { fun show(self): string { println(1); \"p\" } }\n\
                   const L: string = \"at {P { x: 0 }}\";\nfun main() {}";
        assert_eq!(one_file_errors(src)[0].0, "E0445");
        assert!(help(src).contains("`P.show`"), "{}", help(src));
        // `==` calls `eq` without naming it (with the prelude's `Eq`).
        let eq = "struct P { x: i64 }\n\
                  impl P {\n\
                      fun eq(self, other: P): bool { println(1); self.x == other.x }\n\
                      fun hash(self, h: u64): u64 = h;\n\
                  }\n";
        let codes = prelude_codes(&[(
            "main.la",
            &format!("{eq}const B: bool = P {{ x: 1 }} == P {{ x: 1 }};\nfun main() {{}}"),
        )]);
        assert_eq!(codes, ["E0445"]);
        // A quiet `show` stays pure.
        ok(&[(
            "main.la",
            "struct P { x: i64 }\nimpl P { fun show(self): string = \"p\"; }\n\
             const L: string = \"at {P { x: 0 }}\";\nfun main() {}",
        )]);
    }

    #[test]
    fn std_reaches_the_system_through_os() {
        let found = errors(&[(
            "main.la",
            "import std.time;\nconst A: i64 = time.now_ms();\nfun main() {}",
        )]);
        // Without the prelude std has errors of its own; the constant's comes last.
        let last = found.last().expect("E0445");
        assert_eq!(
            (last.0, last.1.as_str()),
            ("E0445", "time.now_ms()"),
            "{found:?}"
        );
    }
}
