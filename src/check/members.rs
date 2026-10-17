//! What can follow a `.` (v16 draft decision 28): the members of the
//! program's types, the built-in methods and the prelude helpers, for the
//! language server's completion. Built from the checker's own tables, and
//! built-ins only as the checker's resolvers accept them, so completion
//! never offers a name a call would reject.

use std::collections::HashMap;

use super::generics::{Generic, subst};
use super::member_names::{CANDIDATES, MORE_CANDIDATES};
use super::{Checker, Partial, Type, builtins, helpers, iter_helpers};
use crate::modules::PRELUDE_PATH;
use crate::span::Span;

/// What a member is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberKind {
    /// `p.x`.
    Field,
    /// `p.len()`: called on a value.
    Method,
    /// `Stack.new()`: called on the type.
    Associated,
    /// `Shape.Circle`.
    Variant,
}

/// How to describe a member.
#[derive(Debug, Clone, PartialEq)]
pub enum Detail {
    /// A field's type.
    Type(Type),
    /// A function's or method's declaration name: its header is in the source.
    Decl(Span),
    /// Text as it is: a variant's payloads, a signature with no source.
    Text(String),
}

/// One member of a type.
#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub name: String,
    pub kind: MemberKind,
    pub detail: Detail,
    /// Its declaration's name, for its doc comment (v16, milestone 77);
    /// `None` for a built-in.
    pub decl: Option<Span>,
}

/// A struct's, enum's or interface's members.
#[derive(Debug, Clone)]
pub struct TypeMembers {
    /// The type's name in its declaration.
    pub decl: Span,
    /// Its type parameters, substituted by a value's type arguments.
    pub(crate) generics: Vec<Generic>,
    pub members: Vec<Member>,
}

impl Checker {
    /// The members of every struct, enum and interface, by key.
    pub(super) fn type_members(&self) -> HashMap<String, TypeMembers> {
        let mut out = HashMap::new();
        for (key, info) in &self.structs {
            let mut members: Vec<Member> = info
                .fields
                .iter()
                .map(|(name, ty)| Member {
                    name: name.clone(),
                    kind: MemberKind::Field,
                    detail: Detail::Type(ty.clone()),
                    decl: self.hover.fields.get(&(key.clone(), name.clone())).copied(),
                })
                .collect();
            members.extend(self.methods_of(key));
            out.insert(
                key.clone(),
                TypeMembers {
                    decl: info.span,
                    generics: info.generics.clone(),
                    members,
                },
            );
        }
        for (key, info) in &self.enums {
            let mut members: Vec<Member> = info
                .variants
                .iter()
                .map(|(name, payloads)| Member {
                    name: name.clone(),
                    kind: MemberKind::Variant,
                    detail: Detail::Text(variant_text(name, payloads)),
                    decl: self
                        .hover
                        .variants
                        .get(&(key.clone(), name.clone()))
                        .copied(),
                })
                .collect();
            members.extend(self.methods_of(key));
            out.insert(
                key.clone(),
                TypeMembers {
                    decl: info.span,
                    generics: info.generics.clone(),
                    members,
                },
            );
        }
        for (key, info) in &self.interfaces {
            let members = info
                .items
                .iter()
                .map(|item| Member {
                    name: item.name.clone(),
                    kind: if item.receiver.is_some() {
                        MemberKind::Method
                    } else {
                        MemberKind::Associated
                    },
                    detail: Detail::Text(signature(
                        &item.name,
                        item.receiver,
                        &item.params,
                        &item.ret,
                    )),
                    decl: Some(item.span),
                })
                .collect();
            out.insert(
                key.clone(),
                TypeMembers {
                    decl: info.span,
                    generics: info.generics.clone(),
                    members,
                },
            );
        }
        out
    }

    fn methods_of(&self, key: &str) -> Vec<Member> {
        let mut methods: Vec<Member> = self
            .methods
            .get(key)
            .into_iter()
            .flatten()
            .map(|(name, m)| Member {
                name: name.clone(),
                kind: if m.receiver.is_some() {
                    MemberKind::Method
                } else {
                    MemberKind::Associated
                },
                detail: Detail::Decl(m.span),
                decl: Some(m.span),
            })
            .collect();
        methods.sort_by(|a, b| a.name.cmp(&b.name));
        methods
    }

    /// Every prelude function's declaration name, by its name.
    pub(super) fn prelude_functions(&self) -> HashMap<String, Span> {
        let prefix = format!("{PRELUDE_PATH}.");
        self.functions
            .iter()
            .filter_map(|(key, sig)| Some((key.strip_prefix(&prefix)?.to_string(), sig.span)))
            .collect()
    }
}

/// What can follow `value.` for a value of type `ty`: its fields and
/// methods, the built-in methods and the prelude helpers that apply, and
/// for an iterator the iterator helpers.
pub fn value_members(partial: &Partial, ty: &Type) -> Vec<Member> {
    if let Type::Actor(key) = ty {
        return super::actors::handle_members(partial, key);
    }
    let mut out = Vec::new();
    let own = match ty {
        Type::Struct(key, args) | Type::Enum(key, args) | Type::Interface(key, args) => {
            partial.members.get(key).map(|t| (t, args))
        }
        _ => None,
    };
    if let Some((t, args)) = own {
        for m in t
            .members
            .iter()
            .filter(|m| matches!(m.kind, MemberKind::Field | MemberKind::Method))
            // A `$` name is the prelude's own (v17 maps): no program can write it.
            .filter(|m| !m.name.starts_with('$'))
        {
            let mut m = m.clone();
            if let Detail::Type(field) = &m.detail
                && args.len() == t.generics.len()
            {
                m.detail = Detail::Type(subst(field, &t.generics, args));
            }
            out.push(m);
        }
    }
    if matches!(ty, Type::Array(_) | Type::String) {
        out.push(Member {
            name: "len".into(),
            kind: MemberKind::Field,
            detail: Detail::Type(Type::I64),
            decl: None,
        });
    }
    for name in CANDIDATES.iter().chain(&MORE_CANDIDATES) {
        if out.iter().any(|m| m.name == *name) {
            continue;
        }
        if let Some(m) = builtins::builtin(ty, name) {
            let text = signature(name, None, &m.params, &m.ret);
            out.push(Member {
                name: name.to_string(),
                kind: MemberKind::Method,
                detail: Detail::Text(text),
                decl: None,
            });
        } else if let Some(function) = helpers::helper_function(ty, name) {
            out.push(helper(partial, name, &function));
        }
    }
    let iterates = out
        .iter()
        .any(|m| m.name == "next" && m.kind == MemberKind::Method);
    if iterates {
        for name in iter_helpers::HELPERS {
            if !out.iter().any(|m| m.name == name) {
                let function = if name == "sum" {
                    "iter_sum".to_string()
                } else {
                    format!("iter_{name}")
                };
                out.push(helper(partial, name, &function));
            }
        }
    }
    out
}

/// What can follow `Type.`: an enum's variants, a type's associated functions.
pub fn type_members(partial: &Partial, key: &str) -> Vec<Member> {
    partial
        .members
        .get(key)
        .map(|t| {
            t.members
                .iter()
                .filter(|m| matches!(m.kind, MemberKind::Variant | MemberKind::Associated))
                .filter(|m| !m.name.starts_with('$'))
                .cloned()
                .collect()
        })
        .unwrap_or_default()
}

fn helper(partial: &Partial, name: &str, function: &str) -> Member {
    let detail = match partial.prelude.get(function) {
        Some(span) => Detail::Decl(*span),
        None => Detail::Text(format!("fun {name}(…)")),
    };
    Member {
        name: name.to_string(),
        kind: MemberKind::Method,
        detail,
        decl: partial.prelude.get(function).copied(),
    }
}

fn variant_text(name: &str, payloads: &[Type]) -> String {
    if payloads.is_empty() {
        return name.to_string();
    }
    let payloads: Vec<String> = payloads.iter().map(Type::to_string).collect();
    format!("{name}({})", payloads.join(", "))
}

/// `fun name(self, T1, T2): R` from types alone, for members without source.
pub(super) fn signature(name: &str, receiver: Option<bool>, params: &[Type], ret: &Type) -> String {
    let mut parts: Vec<String> = match receiver {
        Some(true) => vec!["mut self".into()],
        Some(false) => vec!["self".into()],
        None => Vec::new(),
    };
    parts.extend(params.iter().map(Type::to_string));
    match ret {
        Type::Unit => format!("fun {name}({})", parts.join(", ")),
        ret => format!("fun {name}({}): {ret}", parts.join(", ")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::check::test_util;

    const SRC: &str = "struct P<T> { x: T, n: i64 }
impl P<T> { fun len(self): i64 = self.n; fun make(x: T): P<T> = P { x: x, n: 0 }; }
enum Shape { Circle(f64), Dot }
impl Shape { fun unit(): Shape = Shape.Dot; }
interface Area { fun area(self): f64; }
fun main() { }
";

    fn names(members: &[Member]) -> Vec<String> {
        let mut names: Vec<String> = members.iter().map(|m| m.name.clone()).collect();
        names.sort();
        names
    }

    #[test]
    fn a_struct_value_has_its_fields_with_arguments_and_its_methods() {
        let partial = test_util::partial(SRC);
        let ty = Type::Struct("P".into(), vec![Type::F64]);
        let members = value_members(&partial, &ty);
        assert_eq!(names(&members), ["len", "n", "x"]);
        let x = members.iter().find(|m| m.name == "x").unwrap();
        assert_eq!(x.detail, Detail::Type(Type::F64));
        assert!(matches!(
            members.iter().find(|m| m.name == "len").unwrap().detail,
            Detail::Decl(_)
        ));
        assert_eq!(names(&type_members(&partial, "P")), ["make"]);
    }

    #[test]
    fn an_enum_type_has_its_variants_and_associated_functions() {
        let partial = test_util::partial(SRC);
        let members = type_members(&partial, "Shape");
        assert_eq!(names(&members), ["Circle", "Dot", "unit"]);
        let circle = members.iter().find(|m| m.name == "Circle").unwrap();
        assert_eq!(circle.detail, Detail::Text("Circle(f64)".into()));
    }

    #[test]
    fn an_interface_value_has_its_methods() {
        let partial = test_util::partial(SRC);
        let members = value_members(&partial, &Type::Interface("Area".into(), vec![]));
        assert_eq!(
            members,
            [Member {
                name: "area".into(),
                kind: MemberKind::Method,
                detail: Detail::Text("fun area(self): f64".into()),
                decl: members[0].decl,
            }]
        );
        let at = members[0].decl.expect("its declaration");
        assert_eq!(&SRC[at.start..at.end], "area");
    }

    #[test]
    fn arrays_strings_and_options_have_their_built_ins_and_helpers() {
        let partial = test_util::partial(SRC);
        let array = names(&value_members(&partial, &Type::Array(Box::new(Type::I64))));
        for name in [
            "len", "push", "pop", "clear", "map", "filter", "iter", "sort",
        ] {
            assert!(array.contains(&name.to_string()), "{name} in {array:?}");
        }
        assert!(
            !array.contains(&"join".to_string()),
            "join is for string arrays"
        );
        let strings = names(&value_members(&partial, &Type::String));
        for name in ["len", "slice", "split", "trim", "chars", "to_bytes"] {
            assert!(strings.contains(&name.to_string()), "{name} in {strings:?}");
        }
        let option = names(&value_members(&partial, &Type::Option(Box::new(Type::I64))));
        assert!(
            option.contains(&"unwrap_or".to_string()) && option.contains(&"is_some".to_string())
        );
        let int = names(&value_members(&partial, &Type::I64));
        assert!(int.contains(&"checked_add".to_string()) && !int.contains(&"push".to_string()));
        assert!(!int.contains(&"to_bits".to_string()));
        let float = names(&value_members(&partial, &Type::F64));
        assert!(float.contains(&"to_bits".to_string()), "{float:?}");
    }

    #[test]
    fn iterators_have_the_iterator_helpers() {
        let partial = test_util::partial(SRC);
        let iter = Type::Struct("$prelude.ArrayIter".into(), vec![Type::I64]);
        let members = names(&value_members(&partial, &iter));
        for name in ["next", "map", "filter", "collect", "sum", "zip"] {
            assert!(members.contains(&name.to_string()), "{name} in {members:?}");
        }
    }

    #[test]
    fn tasks_and_channels_have_their_methods_v17() {
        let partial = test_util::partial(SRC);
        let chan = names(&value_members(&partial, &Type::Chan(Box::new(Type::I64))));
        for name in ["send", "recv", "try_recv", "close", "is"] {
            assert!(chan.contains(&name.to_string()), "{name} in {chan:?}");
        }
        let task = names(&value_members(&partial, &Type::Task(Box::new(Type::I64))));
        assert_eq!(task, ["join"]);
    }

    #[test]
    fn every_candidate_name_belongs_to_some_receiver() {
        let receivers = [
            Type::Array(Box::new(Type::I64)),
            Type::Array(Box::new(Type::String)),
            Type::String,
            Type::Char,
            Type::I64,
            Type::F64,
            Type::Option(Box::new(Type::I64)),
            Type::Enum(
                super::super::results::RESULT.into(),
                vec![Type::I64, Type::String],
            ),
            Type::Ref(Box::new(Type::I64)),
            Type::Task(Box::new(Type::I64)),
            Type::Chan(Box::new(Type::I64)),
        ];
        for name in CANDIDATES.iter().chain(&MORE_CANDIDATES) {
            let resolves = receivers.iter().any(|r| {
                builtins::builtin(r, name).is_some() || helpers::helper_function(r, name).is_some()
            });
            assert!(
                resolves,
                "{name} resolves for no receiver: a stale candidate"
            );
        }
    }
}
