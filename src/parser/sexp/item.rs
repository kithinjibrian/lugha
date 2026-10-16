//! S-expressions for imports, items, functions and types, including the v2
//! headers: `pub`, type parameters and qualified names (v2 draft §3).

use super::block;
use crate::ast::{
    Bound, FunDecl, Ident, InterfaceItem, Item, Param, Program, Type, TypeKind, TypeParam,
    Visibility, VisibilityKind,
};

/// Prints every import, then every item, one per line.
///
/// # Examples
///
/// ```
/// let (tokens, _) = lugha::lexer::lex("fun f(x: i64): i64 = x;").unwrap();
/// let (program, _) = lugha::parser::parse(&tokens).unwrap();
/// assert_eq!(lugha::parser::sexp::program(&program), "(fun f ((x i64)) i64 (block x))");
/// ```
pub fn program(program: &Program) -> String {
    let imports = program.imports.iter().map(|i| {
        let path: Vec<&str> = i.path.iter().map(|s| s.name.as_str()).collect();
        let public = if i.public.is_some() { "(public " } else { "(" };
        let items: Vec<String> = i
            .items
            .iter()
            .map(|s| match &s.alias {
                Some(a) => format!("(as {} {})", s.name.name, a.name),
                None => s.name.name.clone(),
            })
            .collect();
        // `(import a b (items X y))` for named items (v11), `(as c)` (v14).
        let items = if items.is_empty() {
            String::new()
        } else {
            format!(" (items {})", items.join(" "))
        };
        let alias = i
            .alias
            .as_ref()
            .map_or(String::new(), |a| format!(" (as {})", a.name));
        format!("{public}import {}{alias}{items})", path.join(" "))
    });
    imports
        .chain(program.items.iter().map(item))
        .collect::<Vec<_>>()
        .join("\n")
}

/// `private `, `public `, or nothing (v2 draft §6).
fn visibility(v: Option<Visibility>) -> &'static str {
    match v.map(|v| v.kind) {
        Some(VisibilityKind::Private) => "private ",
        Some(VisibilityKind::Public) => "public ",
        None => "",
    }
}

/// ` (type-params T (U Eq Ord))`, or nothing when there are none.
fn type_params(params: &[TypeParam]) -> String {
    if params.is_empty() {
        return String::new();
    }
    let list: Vec<String> = params
        .iter()
        .map(|p| match p.bounds.as_slice() {
            [] => p.name.name.clone(),
            bounds => {
                let bounds: Vec<String> = bounds.iter().map(bound).collect();
                format!("({} {})", p.name.name, bounds.join(" "))
            }
        })
        .collect();
    format!(" (type-params {})", list.join(" "))
}

/// `Shape`, `m.Shape`, or with arguments `(Source T)` (v6).
fn bound(b: &Bound) -> String {
    let name = qualified(&b.module, &b.name);
    if b.args.is_empty() {
        return name;
    }
    let args: Vec<String> = b.args.iter().map(ty).collect();
    format!("({name} {})", args.join(" "))
}

/// `name`, or `module.name` (v2 draft §3).
pub(super) fn qualified(module: &Option<Ident>, name: &Ident) -> String {
    match module {
        Some(m) => format!("{}.{}", m.name, name.name),
        None => name.name.clone(),
    }
}

fn item(item: &Item) -> String {
    match item {
        Item::Fun(f) => fun(f),
        Item::Extern(e) => format!(
            "({}extern {}{} {} {})",
            visibility(e.visibility),
            e.name.name,
            e.symbol
                .as_ref()
                .map_or(String::new(), |s| format!(" {:?}", s.name)),
            params(&e.params),
            ret(&e.ret)
        ),
        Item::ExternType(t) => format!("({}extern-type {})", visibility(t.visibility), t.name.name),
        Item::Struct(s) => {
            let fields: String = s
                .fields
                .iter()
                .map(|f| {
                    format!(
                        " ({}{} {})",
                        visibility(f.visibility),
                        f.name.name,
                        ty(&f.ty)
                    )
                })
                .collect();
            let (p, tp) = (visibility(s.visibility), type_params(&s.type_params));
            let kind = if s.c_layout.is_some() {
                "extern-struct"
            } else if s.actor.is_some() {
                "actor"
            } else {
                "struct"
            };
            format!("({p}{kind} {}{tp}{fields})", s.name.name)
        }
        Item::Enum(e) => {
            let variants: String = e
                .variants
                .iter()
                .map(|v| match v.payloads.as_slice() {
                    [] => format!(" {}", v.name.name),
                    payloads => {
                        let types: Vec<String> = payloads.iter().map(ty).collect();
                        format!(" ({} {})", v.name.name, types.join(" "))
                    }
                })
                .collect();
            let (p, tp) = (visibility(e.visibility), type_params(&e.type_params));
            format!("({p}enum {}{tp}{variants})", e.name.name)
        }
        Item::Impl(i) => {
            let methods: String = i.methods.iter().map(|m| format!(" {}", fun(m))).collect();
            let params = type_params(&i.params);
            let kind = if i.actor { "actor-methods" } else { "impl" };
            format!("({kind} {}{params}{methods})", qualified(&i.module, &i.ty))
        }
        Item::Interface(i) => {
            let items: String = i
                .items
                .iter()
                .map(|m| format!(" {}", m.default.as_ref().map_or_else(|| signature(m), fun)))
                .collect();
            format!(
                "({}interface {}{}{items})",
                visibility(i.visibility),
                i.name.name,
                type_params(&i.type_params)
            )
        }
    }
}

/// `(fun name (receiver params…) ret)`: an interface's required item.
fn signature(f: &InterfaceItem) -> String {
    let mut list: Vec<String> = f
        .receiver
        .map(|r| if r.mutable { "(mut self)" } else { "self" }.to_string())
        .into_iter()
        .collect();
    list.extend(f.params.iter().map(param));
    format!("(fun {} ({}) {})", f.name.name, list.join(" "), ret(&f.ret))
}

/// `(fun name (receiver params…) ret body)`.
fn fun(f: &FunDecl) -> String {
    let mut list: Vec<String> = f
        .receiver
        .map(|r| if r.mutable { "(mut self)" } else { "self" }.to_string())
        .into_iter()
        .collect();
    list.extend(f.params.iter().map(param));
    format!(
        "({}{}fun {}{} ({}) {} {})",
        if f.test.is_some() { "@test " } else { "" },
        visibility(f.visibility),
        f.name.name,
        type_params(&f.type_params),
        list.join(" "),
        ret(&f.ret),
        block(&f.body)
    )
}

fn params(params: &[Param]) -> String {
    format!(
        "({})",
        params.iter().map(param).collect::<Vec<_>>().join(" ")
    )
}

fn param(param: &Param) -> String {
    format!("({} {})", param.name.name, ty(&param.ty))
}

fn ret(ret: &Option<Type>) -> String {
    ret.as_ref().map_or_else(|| "void".to_string(), ty)
}

pub(super) fn ty(ty: &Type) -> String {
    match &ty.kind {
        TypeKind::I32 => "i32".into(),
        TypeKind::I64 => "i64".into(),
        TypeKind::U8 => "u8".into(),
        TypeKind::F64 => "f64".into(),
        TypeKind::Bool => "bool".into(),
        TypeKind::String => "string".into(),
        TypeKind::U32 => "u32".into(),
        TypeKind::U64 => "u64".into(),
        TypeKind::F32 => "f32".into(),
        TypeKind::I8 => "i8".into(),
        TypeKind::I16 => "i16".into(),
        TypeKind::U16 => "u16".into(),
        TypeKind::Char => "char".into(),
        TypeKind::Named(name) => name.clone(),
        TypeKind::Array(element) => format!("{}[]", self::ty(element)),
        TypeKind::Option(inner) => format!("(Option {})", self::ty(inner)),
        TypeKind::Tuple(elements) => {
            let shown: Vec<String> = elements.iter().map(self::ty).collect();
            format!("(tuple {})", shown.join(" "))
        }
        TypeKind::Fun(params, ret) => {
            let params: Vec<String> = params.iter().map(self::ty).collect();
            let ret = ret.as_deref().map_or_else(|| "void".to_string(), self::ty);
            format!("(fun-type ({}) {ret})", params.join(" "))
        }
        TypeKind::Applied(applied) => {
            let args: String = applied
                .args
                .iter()
                .map(|a| format!(" {}", self::ty(a)))
                .collect();
            format!(
                "(applied {}{args})",
                qualified(&applied.module, &applied.name)
            )
        }
    }
}
