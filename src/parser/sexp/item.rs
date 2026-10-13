//! S-expressions for imports, items, functions and types, including the v2
//! headers: `pub`, type parameters and qualified names (v2 draft §3).

use super::block;
use crate::ast::{
    FunDecl, Ident, InterfaceItem, Item, Param, Program, Type, TypeKind, TypeParam, Visibility,
    VisibilityKind,
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
        format!("(import {})", path.join(" "))
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
                let bounds: Vec<String> = bounds
                    .iter()
                    .map(|b| qualified(&b.module, &b.name))
                    .collect();
                format!("({} {})", p.name.name, bounds.join(" "))
            }
        })
        .collect();
    format!(" (type-params {})", list.join(" "))
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
            "({}extern {} {} {})",
            visibility(e.visibility),
            e.name.name,
            params(&e.params),
            ret(&e.ret)
        ),
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
            format!("({p}struct {}{tp}{fields})", s.name.name)
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
            let params = if i.params.is_empty() {
                String::new()
            } else {
                let names: Vec<&str> = i.params.iter().map(|p| p.name.as_str()).collect();
                format!(" (type-params {})", names.join(" "))
            };
            format!("(impl {}{params}{methods})", qualified(&i.module, &i.ty))
        }
        Item::Interface(i) => {
            let items: String = i
                .items
                .iter()
                .map(|m| format!(" {}", signature(m)))
                .collect();
            format!(
                "({}interface {}{items})",
                visibility(i.visibility),
                i.name.name
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
        "({}fun {}{} ({}) {} {})",
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
        TypeKind::Named(name) => name.clone(),
        TypeKind::Array(element) => format!("{}[]", self::ty(element)),
        TypeKind::Option(inner) => format!("(Option {})", self::ty(inner)),
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
