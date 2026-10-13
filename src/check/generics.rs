use std::collections::HashMap;

use super::{Checker, Type, errors};
use crate::ast::{AppliedType, Ident, Item, TypeParam};
use crate::span::Span;

/// What a type parameter's bounds allow: `==` with `Eq`, `<` with `Ord`,
/// and the items of each interface (by key, milestone 12).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct Bounds {
    pub eq: bool,
    pub ord: bool,
    pub interfaces: Vec<String>,
}

/// A declared type parameter: its name and bounds.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Generic {
    pub name: String,
    pub bounds: Bounds,
}

/// `ty` with each parameter named in `generics` replaced by the matching
/// type in `args`; other types, including other parameters, unchanged.
pub(super) fn subst(ty: &Type, generics: &[Generic], args: &[Type]) -> Type {
    let map: HashMap<&str, &Type> = generics.iter().map(|g| g.name.as_str()).zip(args).collect();
    replace(ty, &map)
}

/// True if `ty` mentions a type parameter.
pub(super) fn has_param(ty: &Type) -> bool {
    match ty {
        Type::Param(_) => true,
        Type::Array(inner) | Type::Option(inner) => has_param(inner),
        Type::Struct(_, args) | Type::Enum(_, args) => args.iter().any(has_param),
        Type::Fun(params, ret) => params.iter().any(has_param) || has_param(ret),
        _ => false,
    }
}

/// The parameters as types: `[T, U]` for `<T, U>`.
pub(super) fn params(generics: &[Generic]) -> Vec<Type> {
    generics
        .iter()
        .map(|g| Type::Param(g.name.clone()))
        .collect()
}

/// A declaration's parameters under the names an `impl` gives them
/// (`impl Stack<U>`); bounds stay those of the declaration.
pub(super) fn renamed(declared: &[Generic], names: &[Ident]) -> Vec<Generic> {
    let named = declared.iter().zip(names);
    named
        .map(|(g, n)| Generic {
            name: n.name.clone(),
            bounds: g.bounds.clone(),
        })
        .collect()
}

/// A struct or enum type with its type arguments replaced by `args`.
pub(super) fn with_args(ty: &Type, args: Vec<Type>) -> Type {
    match ty {
        Type::Struct(k, _) => Type::Struct(k.clone(), args),
        Type::Enum(k, _) => Type::Enum(k.clone(), args),
        other => other.clone(),
    }
}

/// Replaces each parameter named in `map`, in one pass: a caller's `T`
/// standing in for a callee's `T` is never replaced again.
pub(super) fn replace(ty: &Type, map: &HashMap<&str, &Type>) -> Type {
    match ty {
        Type::Param(name) => map
            .get(name.as_str())
            .map_or_else(|| ty.clone(), |t| (*t).clone()),
        Type::Array(inner) => Type::Array(Box::new(replace(inner, map))),
        Type::Option(inner) => Type::Option(Box::new(replace(inner, map))),
        Type::Fun(params, ret) => Type::Fun(
            params.iter().map(|p| replace(p, map)).collect(),
            Box::new(replace(ret, map)),
        ),
        Type::Struct(k, args) => {
            Type::Struct(k.clone(), args.iter().map(|a| replace(a, map)).collect())
        }
        Type::Enum(k, args) => {
            Type::Enum(k.clone(), args.iter().map(|a| replace(a, map)).collect())
        }
        other => other.clone(),
    }
}

impl Checker {
    /// Type parameters as written, before any checks: names and the
    /// bounds that name `Eq`, `Ord` or an interface (pass 1 needs them
    /// before every type is known; interfaces are collected first).
    pub(super) fn generics(&self, params: &[TypeParam]) -> Vec<Generic> {
        params
            .iter()
            .map(|p| Generic {
                name: p.name.name.clone(),
                bounds: self.bounds_of(p),
            })
            .collect()
    }

    /// A function's parameters, now in scope for its signature (checked:
    /// E0315, E0316); an extern has none.
    pub(super) fn enter_params(&mut self, item: &Item) -> Vec<Generic> {
        let Item::Fun(f) = item else {
            self.type_params.clear();
            return Vec::new();
        };
        self.check_params(&f.type_params, &[]);
        self.type_params = self.generics(&f.type_params);
        self.type_params.clone()
    }

    /// E0315 for an unknown bound and E0316 for a bad parameter name: a
    /// duplicate, a name like a type or module, or one of `outer`'s.
    pub(super) fn check_params(&mut self, params: &[TypeParam], outer: &[Generic]) {
        for (i, p) in params.iter().enumerate() {
            self.check_bounds(p);
            let repeated = params[..i].iter().any(|q| q.name.name == p.name.name)
                || outer.iter().any(|g| g.name == p.name.name);
            if repeated || self.is_type_name(&p.name.name) {
                self.report(errors::bad_type_param(&p.name.name, p.name.span));
            }
        }
    }

    /// The parameters of the struct or enum with key `key`.
    pub(super) fn generics_of(&self, key: &str) -> Vec<Generic> {
        self.structs
            .get(key)
            .map(|s| s.generics.clone())
            .or_else(|| self.enums.get(key).map(|e| e.generics.clone()))
            .unwrap_or_default()
    }

    /// A written name in a type position: a parameter in scope, or a type
    /// (E0305 if unknown, E0418 if generic but written without arguments).
    pub(super) fn resolve_named(&mut self, name: &str, span: Span) -> Type {
        // A default method's hidden `Self` can't be written (v5: out of scope).
        if name != super::defaults::SELF && self.type_params.iter().any(|g| g.name == name) {
            return Type::Param(name.to_string());
        }
        let ident = Ident {
            name: name.to_string(),
            span,
        };
        if let Some(ty) = self.interface_type(None, &ident, span) {
            return ty;
        }
        let Some(key) = self.type_key(None, &ident, span) else {
            return Type::Error;
        };
        let generics = self.generics_of(&key);
        if !generics.is_empty() {
            self.report(errors::type_args(name, generics.len(), 0, span));
            return Type::Error;
        }
        self.named_type(&key).unwrap_or(Type::Error)
    }

    /// `m.T`, `T<args>` or `m.T<args>` (E0418 for the wrong count, E0419
    /// for an argument that doesn't meet its bound).
    pub(super) fn resolve_applied(&mut self, applied: &AppliedType, span: Span) -> Type {
        if let Some(ty) = self.interface_type(applied.module.as_ref(), &applied.name, span) {
            return self.no_type_args(ty, applied, span);
        }
        let Some(key) = self.type_key(applied.module.as_ref(), &applied.name, span) else {
            return Type::Error;
        };
        let generics = self.generics_of(&key);
        let args: Vec<Type> = applied.args.iter().map(|a| self.resolve(a)).collect();
        if args.len() != generics.len() {
            let d = errors::type_args(&applied.name.name, generics.len(), args.len(), span);
            self.report(d);
            return Type::Error;
        }
        if !self.meets_bounds(&generics, &args, span) {
            return Type::Error;
        }
        match self.named_type(&key) {
            Some(Type::Struct(k, _)) => Type::Struct(k, args),
            Some(Type::Enum(k, _)) => Type::Enum(k, args),
            _ => Type::Error,
        }
    }

    /// The type of field `name` of struct `key` with type arguments `args`.
    pub(super) fn field_type(&self, key: &str, args: &[Type], name: &str) -> Option<Type> {
        let declared = self.struct_field(key, name)?;
        Some(subst(&declared, &self.generics_of(key), args))
    }

    /// The variants of enum `key` with its payloads at `args`.
    pub(super) fn enum_variants(
        &self,
        key: &str,
        args: &[Type],
    ) -> Option<Vec<(String, Vec<Type>)>> {
        let info = self.enums.get(key)?;
        let variants = info
            .variants
            .iter()
            .map(|(v, ps)| {
                (
                    v.clone(),
                    ps.iter().map(|p| subst(p, &info.generics, args)).collect(),
                )
            })
            .collect();
        Some(variants)
    }
}

#[cfg(test)]
mod tests {
    use crate::check::test_util::{errors, ok};

    #[test]
    fn declarations_take_parameters_and_bounds() {
        ok(
            "fun max<T: Ord>(a: T, b: T): T = if a > b { a } else { b };\n\
            struct Pair<A, B> { a: A, b: B }\n\
            enum Tree<T: Eq> { Leaf, Node(T, Tree<T>) }\n\
            struct Stack<T> { items: T[] }\n\
            impl Stack<T> {\n\
                fun new(): Stack<T> = Stack { items: [] };\n\
                fun push(mut self, x: T) { self.items.push(x); }\n\
                fun pair<U>(self, u: U): Pair<Stack<T>, U> = Pair { a: self, b: u };\n\
            }\n\
            fun main() {}",
        );
    }

    #[test]
    fn bad_bounds_and_parameter_names() {
        assert_eq!(
            errors("fun f<T: Hash>(x: T) {}\nfun main() {}"),
            [("E0315", "Hash")]
        );
        assert_eq!(errors("fun f<T, T>() {}\nfun main() {}"), [("E0316", "T")]);
        assert_eq!(
            errors("struct P {}\nfun f<P>() {}\nfun main() {}"),
            [("E0316", "P")]
        );
        assert_eq!(
            errors("struct S<T> { x: T }\nimpl S<T> { fun f<T>(self) {} }\nfun main() {}"),
            [("E0316", "T")]
        );
        assert_eq!(errors("struct Eq {}\nfun main() {}"), [("E0302", "Eq")]);
    }

    #[test]
    fn type_arguments_must_match_the_declaration() {
        let pair = "struct Pair<A, B> { a: A, b: B }\nstruct Stack<T> { items: T[] }\n";
        assert_eq!(
            errors(&format!("{pair}fun f(p: Pair<i64>) {{}}\nfun main() {{}}")),
            [("E0418", "Pair<i64>")]
        );
        assert_eq!(
            errors(&format!(
                "{pair}fun f(s: Stack<i64, i64>) {{}}\nfun main() {{}}"
            )),
            [("E0418", "Stack<i64, i64>")]
        );
        assert_eq!(
            errors(&format!("{pair}fun f(s: Stack) {{}}\nfun main() {{}}")),
            [("E0418", "Stack")]
        );
        assert_eq!(
            errors(&format!("{pair}impl Stack<T, U> {{}}\nfun main() {{}}")),
            [("E0418", "Stack")]
        );
    }
}
