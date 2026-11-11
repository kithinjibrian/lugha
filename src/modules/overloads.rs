//! Several arithmetic methods on one type. `add`, `sub`, `mul`, `div` and
//! `rem` may be declared more than once on a type, each with a different
//! right-hand type, and an operator picks one by its right operand. So
//! that the rest of the compiler keeps one method per name, every one but
//! the first is renamed here to its name and right-hand type as written,
//! `mul(f64)`: no name a program writes contains `(`. The one that keeps the
//! plain name is the one taking the type itself (`Self`, `V`, `V<T>`), so
//! `T: Mul` and the interfaces see it unchanged, or else the first.
//!
//! An `impl` of a type that isn't the module's own (`impl f64`, `impl
//! shapes.W`) holds only arithmetic methods for the module's types, keyed
//! by their right-hand type: every one is renamed, with the module's path
//! (`mul(V)@geo`), so two modules' `f64 * V` never share a name.

use std::collections::HashMap;

use crate::ast::{FunDecl, Item, Program};

/// The number types an `impl` may name, for arithmetic with them on the left.
pub const NUMBERS: [&str; 10] = [
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64",
];

/// The methods an operator calls and that may be declared more than once.
pub const ARITHMETIC: [&str; 5] = ["add", "sub", "mul", "div", "rem"];

/// Renames all but one of each type's same-named arithmetic methods in
/// `program`, whose source is `text`, starting at byte `base`; `module`
/// is its path, joined with `.`.
pub(crate) fn name_overloads(program: &mut Program, text: &str, base: usize, module: &str) {
    // Each `impl` block's type, as written, with the blocks' methods by name.
    let mut found: HashMap<(String, &str), Vec<(usize, usize)>> = HashMap::new();
    for (i, item) in program.items.iter().enumerate() {
        let Item::Impl(block) = item else { continue };
        let owner = match &block.module {
            Some(m) => format!("{}.{}", m.name, block.ty.name),
            None => block.ty.name.clone(),
        };
        for (j, method) in block.methods.iter().enumerate() {
            if let Some(name) = ARITHMETIC.iter().find(|n| **n == method.name.name) {
                found.entry((owner.clone(), name)).or_default().push((i, j));
            }
        }
    }
    let written = |m: &FunDecl| {
        let ty = m.params.first().map(|p| p.ty.span)?;
        let at = ty.start.checked_sub(base)?..ty.end.checked_sub(base)?;
        let source = text.get(at)?;
        Some(source.split_whitespace().collect::<String>())
    };
    let tag = if module.is_empty() {
        String::new()
    } else {
        format!("@{module}")
    };
    for ((owner, name), places) in found {
        let foreign = places.iter().any(|(i, _)| match &program.items[*i] {
            Item::Impl(block) => {
                block.module.is_some() || NUMBERS.contains(&block.ty.name.as_str())
            }
            _ => false,
        });
        if places.len() < 2 && !foreign {
            continue;
        }
        let method = |(i, j): (usize, usize)| match &program.items[i] {
            Item::Impl(block) => &block.methods[j],
            _ => unreachable!("collected from an `impl`"),
        };
        let bare = owner.rsplit('.').next().unwrap_or(&owner);
        let on_itself = |m: &FunDecl| {
            shaped(m)
                && written(m)
                    .is_some_and(|t| t == "Self" || t == bare || t.starts_with(&format!("{bare}<")))
        };
        // A foreign type's methods are all named by their right-hand type.
        let kept = match foreign {
            true => None,
            false => Some(
                places
                    .iter()
                    .copied()
                    .find(|p| on_itself(method(*p)))
                    .unwrap_or(places[0]),
            ),
        };
        // A method without the operator's shape keeps its name, so the
        // checker reports it as a second method of that name.
        let renamed: Vec<((usize, usize), String)> = places
            .iter()
            .copied()
            .filter(|p| Some(*p) != kept && shaped(method(*p)))
            .filter_map(|p| {
                let suffix = if foreign { tag.as_str() } else { "" };
                Some((p, format!("{name}({}){suffix}", written(method(p))?)))
            })
            .collect();
        for ((i, j), new) in renamed {
            if let Item::Impl(block) = &mut program.items[i] {
                block.methods[j].name.name = new;
            }
        }
    }
}

/// The operator shape: a plain `self`, one parameter and a result.
fn shaped(m: &FunDecl) -> bool {
    m.receiver.as_ref().is_some_and(|r| !r.mutable) && m.params.len() == 1 && m.ret.is_some()
}

/// The name a program wrote for `name`, which may be an overload's
/// `mul(f64)`.
pub fn written_name(name: &str) -> &str {
    name.split('(').next().unwrap_or(name)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn names(src: &str) -> Vec<String> {
        let (tokens, _) = crate::lexer::lex(src).expect("lexes");
        let (mut program, _) = crate::parser::parse(&tokens).expect("parses");
        name_overloads(&mut program, src, 0, "geo");
        program
            .items
            .iter()
            .filter_map(|i| match i {
                Item::Impl(b) => Some(b.methods.iter().map(|m| m.name.name.clone())),
                _ => None,
            })
            .flatten()
            .collect()
    }

    #[test]
    fn all_but_the_one_on_the_type_itself_are_renamed() {
        let src = "struct V { x: f64 }\nimpl V {\n\
                   fun mul(self, k: f64): V = self;\n\
                   fun mul(self, w: V): V = w;\n\
                   fun add(self, w: V): V = w;\n}\n\
                   impl V { fun mul(self, n: u8 []): V = self; }";
        assert_eq!(names(src), ["mul(f64)", "mul", "add", "mul(u8[])"]);
    }

    #[test]
    fn without_one_on_the_type_the_first_keeps_its_name() {
        let src = "struct V { x: f64 }\nimpl V {\n\
                   fun mul(self, k: f64): V = self;\n\
                   fun mul(self, k: u8): V = self;\n}";
        assert_eq!(names(src), ["mul", "mul(u8)"]);
    }

    #[test]
    fn a_foreign_type_s_methods_are_all_named_by_type_and_module() {
        let src = "struct V { x: f64 }\nimpl f64 { fun mul(self, v: V): V = v; }\n\
                   impl shapes.W { fun add(self, v: V): V = v; fun mul(self, v: V[]): V = v[0]; }";
        assert_eq!(names(src), ["mul(V)@geo", "add(V)@geo", "mul(V[])@geo"]);
    }

    #[test]
    fn other_shapes_and_other_names_keep_their_names() {
        let src = "struct S { n: i64 }\nimpl S {\n\
                   fun add(mut self, x: i64) { }\n\
                   fun add(self, o: S): S = o;\n\
                   fun scale(self, k: f64): S = self;\n}";
        assert_eq!(names(src), ["add", "add", "scale"]);
    }
}
