//! Exhaustiveness and unreachable arms over nested patterns: the pattern
//! matrix algorithm from Maranget, "Warnings for pattern matching" (2007).
//!
//! A row is one arm's pattern; columns are the values still to match, with
//! their types. Specializing by a constructor keeps the rows that can start
//! with it and opens its payloads as new columns.

use super::patterns::{Ctor, Pat};
use super::{Checker, Type};

/// Stop listing missing cases after this many, so hostile input stays cheap.
const LIMIT: usize = 100;

type Row = Vec<Pat>;

/// A constructor and its payload types.
type Sig = Vec<(Ctor, Vec<Type>)>;

/// The checker's knowledge of each type's constructors.
pub(super) struct Space<'c> {
    checker: &'c Checker,
}

impl Checker {
    pub(super) fn space(&self) -> Space<'_> {
        Space { checker: self }
    }
}

impl Space<'_> {
    /// Every constructor of `ty`, when there are finitely many.
    fn signature(&self, ty: &Type) -> Option<Sig> {
        if *ty == Type::Bool {
            return Some(vec![
                (Ctor::Bool(true), vec![]),
                (Ctor::Bool(false), vec![]),
            ]);
        }
        let variants = self.checker.variants_of(ty)?;
        let numbered = variants.into_iter().enumerate();
        Some(
            numbered
                .map(|(i, (name, p))| (Ctor::Variant(i, name), p))
                .collect(),
        )
    }

    /// Payload types of `ctor` in `ty`; literals have none.
    fn payloads(&self, ty: &Type, ctor: &Ctor) -> Vec<Type> {
        match ctor {
            Ctor::Variant(i, _) => self
                .checker
                .variants_of(ty)
                .and_then(|v| v.into_iter().nth(*i))
                .map(|(_, p)| p)
                .unwrap_or_default(),
            _ => Vec::new(),
        }
    }

    /// True if `row` matches a value that no row in `rows` matches.
    pub(super) fn useful(&self, rows: &[Row], row: &[Pat], tys: &[Type]) -> bool {
        let Some((head, rest)) = row.split_first() else {
            return rows.is_empty();
        };
        let (ty, rest_tys) = tys.split_first().expect("a type per column");
        match head {
            Pat::Ctor(c, args) => {
                let sub = self.payloads(ty, c);
                let q = [args.as_slice(), rest].concat();
                self.useful(
                    &specialize(rows, c, sub.len()),
                    &q,
                    &[&sub, rest_tys].concat(),
                )
            }
            Pat::Wild => match self.signature(ty) {
                // Every constructor appears: `_` is useful if it is useful under one of them.
                Some(sig) if sig.iter().all(|(c, _)| heads(rows).contains(c)) => {
                    sig.iter().any(|(c, sub)| {
                        let q = [vec![Pat::Wild; sub.len()].as_slice(), rest].concat();
                        self.useful(
                            &specialize(rows, c, sub.len()),
                            &q,
                            &[sub, rest_tys].concat(),
                        )
                    })
                }
                _ => self.useful(&default(rows), rest, rest_tys),
            },
        }
    }

    /// Value vectors that no row matches, written with `_` wherever any value
    /// does; at most `LIMIT` of them.
    pub(super) fn missing(&self, rows: &[Row], tys: &[Type]) -> Vec<Row> {
        let Some((ty, rest_tys)) = tys.split_first() else {
            return if rows.is_empty() {
                vec![vec![]]
            } else {
                Vec::new()
            };
        };
        let mut out = Vec::new();
        let Some(sig) = self.signature(ty) else {
            // Integers and strings: no literal list is ever complete.
            for w in self.missing(&default(rows), rest_tys) {
                out.push([vec![Pat::Wild], w].concat());
            }
            return out;
        };
        let used = heads(rows);
        let mut unused_rest: Option<Vec<Row>> = None;
        for (c, sub) in sig {
            let found = if used.contains(&c) {
                // Look inside: some payload combination may be missing.
                let inner = self.missing(
                    &specialize(rows, &c, sub.len()),
                    &[sub.as_slice(), rest_tys].concat(),
                );
                inner
                    .into_iter()
                    .map(|w| {
                        let (args, rest) = w.split_at(sub.len());
                        [vec![Pat::Ctor(c.clone(), args.to_vec())], rest.to_vec()].concat()
                    })
                    .collect()
            } else {
                // No arm starts with `c`: only `_` rows can match it.
                let rest =
                    unused_rest.get_or_insert_with(|| self.missing(&default(rows), rest_tys));
                let head = Pat::Ctor(c.clone(), vec![Pat::Wild; sub.len()]);
                rest.iter()
                    .map(|w| [vec![head.clone()], w.clone()].concat())
                    .collect::<Vec<_>>()
            };
            for w in found {
                if out.len() >= LIMIT {
                    return out;
                }
                out.push(w);
            }
        }
        out
    }
}

/// The constructors heading the rows' first column.
fn heads(rows: &[Row]) -> Vec<Ctor> {
    rows.iter()
        .filter_map(|row| match row.first() {
            Some(Pat::Ctor(c, _)) => Some(c.clone()),
            _ => None,
        })
        .collect()
}

/// The rows that can start with `ctor`, with its payloads opened as columns.
fn specialize(rows: &[Row], ctor: &Ctor, arity: usize) -> Vec<Row> {
    rows.iter()
        .filter_map(|row| {
            let (head, rest) = row.split_first()?;
            let opened = match head {
                Pat::Ctor(c, args) if c == ctor => args.clone(),
                Pat::Ctor(..) => return None,
                Pat::Wild => vec![Pat::Wild; arity],
            };
            Some([opened.as_slice(), rest].concat())
        })
        .collect()
}

/// The rows starting with `_`, without that column.
fn default(rows: &[Row]) -> Vec<Row> {
    rows.iter()
        .filter(|row| row.first() == Some(&Pat::Wild))
        .map(|row| row[1..].to_vec())
        .collect()
}

/// A pattern in source syntax. At the top level a variant whose payloads are
/// all `_` prints as its bare name (`Rect`), as v1's E0417 did.
pub(super) fn show(pat: &Pat, top: bool) -> String {
    match pat {
        Pat::Wild => "_".into(),
        Pat::Ctor(Ctor::Variant(_, name), args)
            if args.is_empty() || (top && args.iter().all(|a| *a == Pat::Wild)) =>
        {
            name.clone()
        }
        Pat::Ctor(Ctor::Variant(_, name), args) => {
            let args: Vec<String> = args.iter().map(|a| show(a, false)).collect();
            format!("{name}({})", args.join(", "))
        }
        Pat::Ctor(Ctor::Bool(b), _) => b.to_string(),
        Pat::Ctor(Ctor::Int(value, negative), _) => {
            format!("{}{value}", if *negative { "-" } else { "" })
        }
        Pat::Ctor(Ctor::Str(text), _) => format!("{text:?}"),
        Pat::Ctor(Ctor::Type(ty), _) => format!("_: {ty}"),
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{diagnostics, errors, ok, warnings};

    const SHAPE: &str = "enum Shape { Circle(f64), Rect(f64, f64), Empty }\nenum B { P(bool, bool) }\nenum Many { A, B, C, D, E }\n";

    fn main(body: &str) -> String {
        format!(
            "{SHAPE}fun main() {{ let s: Option<Shape> = None; let b = B.P(true, false); let m = Many.A; {body} }}"
        )
    }

    #[test]
    fn complete_nested_matches_pass() {
        ok(&main(
            "let a = match s { Some(Circle(_)) => 1, Some(Rect(_, _)) => 2, Some(Empty) => 3, None => 4 };",
        ));
        ok(&main(
            "let a = match b { P(true, _) => 1, P(false, _) => 2 };",
        ));
    }

    #[test]
    fn missing_cases_are_listed_as_patterns() {
        let d = diagnostics(&main(
            "let a = match s { Some(Circle(_)) => 1, Some(Empty) => 3 };",
        ));
        assert_eq!(
            (d[0].code, d[0].message.as_str()),
            (
                "E0417",
                "non-exhaustive match: `None`, `Some(Rect(_, _))` not covered"
            )
        );
        let d = diagnostics(&main("let a = match m { A => 1 };"));
        assert_eq!(
            d[0].message,
            "non-exhaustive match: `B`, `C`, `D` and 1 more not covered"
        );
        // A guarded arm never counts.
        assert_eq!(
            errors(&main(
                "let a = match b { P(true, _) => 1, P(false, x) if x => 2 };"
            )),
            [("E0417", "match b")]
        );
        // Literals inside variants never complete a match without a catch-all.
        assert_eq!(
            errors(&main(
                "let a = match s { Some(Circle(_)) => 1, None => 2, Some(Rect(_, _)) => 3, Some(Empty) => 4 }; let p = Some(1); let q = match p { Some(1) => 1, None => 0 };"
            )),
            [("E0417", "match p")]
        );
    }

    #[test]
    fn unreachable_arms_include_combined_coverage() {
        assert_eq!(
            warnings(&main(
                "let a = match s { Some(_) => 1, None => 2, Some(Empty) => 3 };"
            )),
            [("W0102", "Some(Empty)")]
        );
        assert_eq!(
            warnings(&main(
                "let a = match b { P(true, _) => 1, P(_, true) => 2, P(false, false) => 3, P(_, _) => 4 };"
            )),
            [("W0102", "P(_, _)")]
        );
        // After a guarded arm, the same pattern is still reachable.
        ok(&main(
            "let a = match b { P(x, _) if x => 1, P(_, _) => 2 };",
        ));
    }
}
