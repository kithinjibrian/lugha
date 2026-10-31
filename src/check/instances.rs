use std::collections::HashSet;

use super::generics::{Generic, subst};
use super::printing::PRINT;
use super::{Checker, Type, errors};
use crate::ast::Expr;
use crate::diagnostic::Severity;
use crate::span::Span;

/// The start of every prelude item's key.
const PRELUDE_KEY: &str = "$prelude.";

/// The start of every std item's key (v15).
const STD_KEY: &str = "std.";

/// Whether `key` is an item compiled only when reached: the prelude's
/// (PRP-043, v10) and, since milestone 60, std's.
pub(super) fn lazy_key(key: &str) -> bool {
    key.starts_with(PRELUDE_KEY) || key.starts_with(STD_KEY)
}

/// How deep instances may nest before it is E0420 (v2 draft §4, §12).
pub(super) const MAX_DEPTH: usize = 64;

/// A generic function or method at concrete type arguments: the `impl`'s,
/// then the method's own.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instance {
    /// The function's key (`max`, `a.b.max`) or the method's (`Stack.push`).
    pub key: String,
    pub args: Vec<Type>,
}

/// A generic call in some body, with its arguments as checked there
/// (they may mention that body's own parameters).
#[derive(Debug, Clone)]
pub(super) struct Use {
    /// The key of the body the call is in.
    owner: String,
    callee: Instance,
    /// For a call through a bound (milestone 12), the type parameter; the
    /// callee is then just the method's name until the parameter is known.
    through: Option<String>,
    /// A derive block's field intrinsic (v17, milestone 141): the callee is
    /// `f` at `Self`, reached at each part's type once `Self` is known.
    per_field: bool,
    span: Span,
}

impl Checker {
    /// Records that the body being checked calls generic `key` at `args`.
    /// A prelude or std function is recorded even when it isn't generic, so
    /// it is compiled only if user code reaches it (PRP-043, v15).
    pub(super) fn used(&mut self, call: &Expr, key: &str, args: &[Type]) {
        if args.is_empty() && !lazy_key(key) {
            return;
        }
        let callee = Instance {
            key: key.to_string(),
            args: args.to_vec(),
        };
        self.uses.push(Use {
            owner: self.owner.clone(),
            callee,
            through: None,
            per_field: false,
            span: call.span,
        });
    }

    /// Records that a field intrinsic calls generic `key` once per part of
    /// the derive block's `Self` (v17, milestone 141).
    pub(super) fn field_used(&mut self, call: &Expr, key: &str) {
        self.uses.push(Use {
            owner: self.owner.clone(),
            callee: Instance {
                key: key.to_string(),
                args: vec![Type::Param(super::SELF.to_string())],
            },
            through: None,
            per_field: true,
            span: call.span,
        });
    }

    /// Records a call of `name` through a bound of type parameter `param`:
    /// each instance resolves it to the concrete type's method.
    pub(super) fn bound_used(&mut self, call: &Expr, param: &str, name: &str) {
        let callee = Instance {
            key: name.to_string(),
            args: Vec::new(),
        };
        self.uses.push(Use {
            owner: self.owner.clone(),
            callee,
            through: Some(param.to_string()),
            per_field: false,
            span: call.span,
        });
    }

    /// The instance a call of `name` through `param` reaches when the
    /// parameters `generics` are `args`: `None` unless the concrete type's
    /// method is generic (a non-generic one is compiled anyway).
    fn bound_target(
        &self,
        generics: &[Generic],
        args: &[Type],
        param: &str,
        name: &str,
    ) -> Option<Instance> {
        let concrete = subst(&Type::Param(param.to_string()), generics, args);
        // A derivable interface's item comes from the block covering the
        // type (v17, milestone 141).
        let derived = generics.iter().find(|g| g.name == param).and_then(|g| {
            // `Eq` brings `$Hash` with it (v17, milestone 143).
            let implied = super::derive::implies_hash(&g.bounds).then(super::derive::hash_bound);
            g.bounds
                .interfaces
                .iter()
                .cloned()
                .chain(implied)
                .find(|a| {
                    let declares = self
                        .interfaces
                        .get(&a.key)
                        .is_some_and(|i| i.items.iter().any(|r| r.name == name));
                    declares && self.derives.derivable(&a.key)
                })
        });
        // A struct's or enum's own item wins (v17, milestone 142).
        if let Some(interface) = derived
            && let Some(target) = self
                .derives
                .target(&interface.key, name, &concrete, |k| self.holds_union(k))
        {
            return Some(target);
        }
        // `a + b` through `T: Add` at an array joins them (v17, milestone 122).
        if name == "add"
            && let Some(target) = super::concat::concat_target(&concrete)
        {
            return Some(target);
        }
        if let Some(default) = self.default_target(generics, args, param, name, &concrete) {
            return Some(default);
        }
        let (Type::Struct(owner, targs) | Type::Enum(owner, targs)) = &concrete else {
            return None;
        };
        let m = self.methods.get(owner)?.get(name)?;
        // A prelude type's methods compile only when reached, generic or not (v10).
        let listed = !m.generics.is_empty() || lazy_key(&m.key);
        listed.then(|| Instance {
            key: m.key.clone(),
            args: targs.clone(),
        })
    }

    /// Every instance reachable from the non-generic bodies, in discovery
    /// order; E0420 past [`MAX_DEPTH`]. Runs only on a program without errors.
    pub(super) fn monomorphize(&mut self) -> Vec<Instance> {
        if self
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error)
        {
            return Vec::new();
        }
        let mut roots: Vec<&str> = Vec::new();
        for u in &self.uses {
            // Prelude and std bodies are reached from user code, never roots
            // themselves (v15, milestone 60).
            if !roots.contains(&u.owner.as_str())
                && self.params_of(&u.owner).is_empty()
                && !lazy_key(&u.owner)
            {
                roots.push(&u.owner);
            }
        }
        // Each instance with its depth and the instance that asked for it.
        let mut found: Vec<(Instance, usize, Option<usize>)> = Vec::new();
        let mut seen: HashSet<Instance> = HashSet::new();
        let mut queue: Vec<(String, Vec<Type>, usize, Option<usize>)> = roots
            .iter()
            .map(|r| (r.to_string(), Vec::new(), 0, None))
            .collect();
        let mut next = 0;
        while next < queue.len() {
            let (owner, args, depth, at) = queue[next].clone();
            next += 1;
            let generics = self.params_of(&owner);
            for u in self.uses.iter().filter(|u| u.owner == owner) {
                let callees = match &u.through {
                    // A field intrinsic reaches `f` at each part's type (v17).
                    None if u.per_field => {
                        let me = subst(&u.callee.args[0], &generics, &args);
                        self.part_types(&me)
                            .into_iter()
                            .map(|t| Instance {
                                key: u.callee.key.clone(),
                                args: vec![t],
                            })
                            .collect()
                    }
                    // A print site reaches the `show` instances of its type (v12).
                    None if u.callee.key == PRINT => {
                        self.show_instances(&subst(&u.callee.args[0], &generics, &args))
                    }
                    // An `==` reaches the `same` of each map and set it compares (v17).
                    None if u.callee.key == super::maps::EQUAL => {
                        self.same_instances(&subst(&u.callee.args[0], &generics, &args))
                    }
                    None => vec![Instance {
                        key: u.callee.key.clone(),
                        args: u
                            .callee
                            .args
                            .iter()
                            .map(|a| subst(a, &generics, &args))
                            .collect(),
                    }],
                    // `<` through `T: Ord` at a tuple reaches its elements'
                    // `cmp` (v15, milestone 59).
                    Some(param) if u.callee.key == "cmp" => {
                        let concrete = subst(&Type::Param(param.clone()), &generics, &args);
                        self.cmp_targets(&concrete)
                    }
                    Some(param) => {
                        match self.bound_target(&generics, &args, param, &u.callee.key) {
                            Some(instance) => vec![instance],
                            None => continue,
                        }
                    }
                };
                for callee in callees {
                    if !seen.insert(callee.clone()) {
                        continue;
                    }
                    if depth + 1 > MAX_DEPTH {
                        let chain = chain(&found, at);
                        let d = errors::too_deep(&chain, u.span);
                        self.report(d);
                        return Vec::new();
                    }
                    queue.push((
                        callee.key.clone(),
                        callee.args.clone(),
                        depth + 1,
                        Some(found.len()),
                    ));
                    found.push((callee, depth + 1, at));
                }
            }
        }
        found.into_iter().map(|(i, _, _)| i).collect()
    }

    /// The type parameters of the function or method with key `key`.
    pub(super) fn params_of(&self, key: &str) -> Vec<Generic> {
        if let Some(f) = self.functions.get(key) {
            return f.generics.clone();
        }
        key.rsplit_once('.')
            .and_then(|(owner, name)| self.methods.get(owner)?.get(name))
            .map(|m| m.generics.clone())
            .unwrap_or_default()
    }
}

/// `f<i64> -> f<i64[]> -> f<i64[][]> -> …`: the first instances on the way
/// to the one at index `last`.
fn chain(found: &[(Instance, usize, Option<usize>)], mut last: Option<usize>) -> String {
    let mut path = Vec::new();
    while let Some(i) = last {
        path.push(&found[i].0);
        last = found[i].2;
    }
    let shown: Vec<String> = path.iter().rev().take(3).map(|i| show(i)).collect();
    format!("{} -> …", shown.join(" -> "))
}

/// An instance as written: `f<i64>`.
pub(super) fn show(i: &Instance) -> String {
    let args: Vec<String> = i.args.iter().map(Type::to_string).collect();
    format!("{}<{}>", i.key, args.join(", "))
}

#[cfg(test)]
mod tests {
    use crate::check::Type;
    use crate::check::test_util::{diagnostics, errors, ok};

    /// `key<args>` of every instance, in discovery order.
    fn monos(src: &str) -> Vec<String> {
        ok(src)
            .monos
            .iter()
            .map(|m| {
                let args: Vec<String> = m.args.iter().map(Type::to_string).collect();
                format!("{}<{}>", m.key, args.join(", "))
            })
            .collect()
    }

    const MAX: &str = "fun max<T: Ord>(a: T, b: T): T = if a > b { a } else { b };\n";

    #[test]
    fn one_instance_per_set_of_arguments() {
        let src = format!(
            "{MAX}fun main() {{ let a = max(3, 7); let b = max(\"a\", \"b\"); let c = max(1, 2); }}"
        );
        assert_eq!(monos(&src), ["max<i64>", "max<string>"]);
        assert_eq!(
            monos(&format!("{MAX}fun main() {{}}")),
            Vec::<String>::new()
        );
    }

    #[test]
    fn generic_bodies_reach_further_instances() {
        let src = "fun first<T>(xs: T[]): Option<T> = if xs.len > 0 { Some(xs[0]) } else { None };\n\
            fun largest<T: Ord>(xs: T[]): Option<T> = first(xs);\n\
            fun main() { let l = largest([\"a\"]); }";
        assert_eq!(monos(src), ["largest<string>", "first<string>"]);
    }

    #[test]
    fn methods_take_the_impls_arguments_then_their_own() {
        let src = "struct Stack<T> { items: T[] }\n\
            impl Stack<T> {\n\
                fun new(): Stack<T> = Stack { items: [] };\n\
                fun push(mut self, x: T) { self.items.push(x); }\n\
                fun tag<U>(self, u: U): U = u;\n\
            }\n\
            fun main() { let mut s: Stack<string> = Stack.new(); s.push(\"a\"); let t = s.tag(1.5); }";
        assert_eq!(
            monos(src),
            [
                "Stack.new<string>",
                "Stack.push<string>",
                "Stack.tag<string, f64>"
            ]
        );
    }

    #[test]
    fn recursion_at_the_same_arguments_adds_nothing() {
        let src = "enum List<T> { Nil, Cons(T, List<T>) }\n\
            fun count<T>(l: List<T>): i64 = match l { Nil => 0, Cons(_, rest) => 1 + count(rest) };\n\
            fun main() { let l: List<bool> = List.Cons(true, List.Nil); let n = count(l); }";
        assert_eq!(monos(src), ["count<bool>"]);
    }

    #[test]
    fn instantiation_more_than_64_deep_is_e0420() {
        let src = "fun f<T>(x: T) { f([x]); }\nfun main() { f(1); }";
        assert_eq!(errors(src), [("E0420", "f([x])")]);
        let d = diagnostics(src);
        let help = d[0].help.as_deref().expect("E0420 shows the chain");
        assert!(
            help.starts_with("f<i64> -> f<i64[]> -> f<i64[][]> -> "),
            "{help}"
        );
    }

    #[test]
    fn calls_through_bounds_reach_generic_methods() {
        let src = "interface Sized { fun size(self): i64; }\n\
            struct Stack<T> { items: T[] }\n\
            impl Stack<T> { fun size(self): i64 = self.items.len; }\n\
            fun count<T: Sized>(x: T): i64 = x.size();\n\
            fun main() { let n = count(Stack { items: [true] }); }";
        assert_eq!(monos(src), ["count<Stack<bool>>", "Stack.size<bool>"]);
    }

    #[test]
    fn main_may_not_be_generic() {
        assert_eq!(errors("fun main<T>() {}"), [("E0304", "main")]);
    }
}
