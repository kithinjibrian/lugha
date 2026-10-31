//! Generic instances (v2 draft §9 "Monomorphization", PRP-036): each
//! generic function and method in `Checked::monos` is declared and lowered
//! once per set of type arguments, from its one AST body, with every type
//! the checker recorded made concrete by substitution — never inferred
//! (architecture rule 4).

use std::collections::HashMap;

use inkwell::module::Linkage;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};

use super::CodegenError;
use super::function::Signature;
use super::lower::Lowerer;
use super::methods::Method;
use super::value::llvm_type;
use crate::ast::{DeriveDecl, DeriveTarget, Expr, FunDecl, ImplDecl, InterfaceDecl, Item};
use crate::check::{Derives, Instance, SELF, Type, Unit, qualify, symbol};

/// Where an instance's code comes from.
struct Source<'a> {
    f: &'a FunDecl,
    /// The `impl` of a method, `None` for a function.
    owner: Option<(&'a ImplDecl, String)>,
    /// The interface of a default method (v5), by key: its type arguments
    /// are the receiver's type, then the interface's own (v6).
    interface: Option<(String, &'a InterfaceDecl)>,
    /// The derive block of a derived item (v17, milestone 141), with its
    /// hidden owner: its type arguments are the block's parameters.
    derive: Option<(&'a DeriveDecl, String)>,
}

/// An instance's linkage: the prelude's and std's (derive blocks
/// included) are only ever called from this module, so they are internal
/// and LLVM inlines them as it did the per-type helpers they replaced (v17,
/// milestone 144); a program's own stay external.
fn linkage(key: &str) -> Linkage {
    if crate::check::lazy_key(key) {
        Linkage::Internal
    } else {
        Linkage::External
    }
}

/// `key<args>`: how an instance is named in `Lowerer::monos`.
pub(super) fn instance_name(key: &str, args: &[Type]) -> String {
    // A prelude function without type parameters is listed with no arguments (PRP-043).
    if args.is_empty() {
        return key.to_string();
    }
    format!("{key}<{}>", spelled(args))
}

/// Type arguments as they print, comma-separated.
fn spelled(args: &[Type]) -> String {
    args.iter()
        .map(Type::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

impl<'ctx> Lowerer<'ctx> {
    /// `ty` with the current instance's type parameters replaced.
    pub(super) fn concrete(&self, ty: &Type) -> Type {
        if self.subst.is_empty() {
            ty.clone()
        } else {
            ty.substitute(&self.subst)
        }
    }

    /// The instance a generic call calls, made concrete: `None` for a call
    /// of something that isn't generic.
    pub(super) fn instance_of(&self, call: &Expr, key: &str) -> Option<Method<'ctx>> {
        let args = self.instances.get(&call.id)?;
        let args: Vec<Type> = args.iter().map(|a| self.concrete(a)).collect();
        let name = instance_name(key, &args);
        let method = self.monos.get(&name);
        Some(
            method
                .unwrap_or_else(|| panic!("checked: `{name}` is listed"))
                .clone(),
        )
    }

    /// What a call of `key` calls when it is an instance: a generic
    /// instance, or a prelude function compiled because something reaches it.
    pub(super) fn prelude_target(&self, call: &Expr, key: &str) -> Option<Method<'ctx>> {
        self.instance_of(call, key)
            .or_else(|| self.monos.get(key).cloned())
    }

    /// Declares every instance: `lugha_fn_max<i64>`, `lugha_fn_Stack<string>.push`.
    pub(super) fn declare_instances(&mut self, units: &[Unit], monos: &[Instance]) {
        for mono in monos {
            let source = find(units, &mono.key, &self.derives);
            self.enter(&source, &mono.args);
            let f = source.f;
            let params: Vec<Type> = f
                .params
                .iter()
                .map(|p| self.annotation_type(&p.ty))
                .collect();
            let ret = f.ret.as_ref().map(|t| self.annotation_type(t));
            let ptr = self.context.ptr_type(Default::default());
            let mut types: Vec<BasicMetadataTypeEnum> =
                f.receiver.iter().map(|_| ptr.into()).collect();
            types.extend(params.iter().map(|ty| self.param_type(ty)));
            let fn_type = match &ret {
                Some(ty) => llvm_type(self.context, ty).fn_type(&types, false),
                None => self.context.void_type().fn_type(&types, false),
            };
            let name = symbol_of(&source, mono);
            let function = self
                .module
                .add_function(&name, fn_type, Some(linkage(&mono.key)));
            let signature = Signature {
                function,
                params,
                ret,
                is_extern: false,
                c_type: None,
                c_sig: None,
            };
            let receiver = f.receiver.map(|r| r.mutable);
            let method = Method {
                signature,
                receiver,
            };
            self.monos
                .insert(instance_name(&mono.key, &mono.args), method);
            self.subst.clear();
        }
    }

    /// Lowers every instance's body under its substitution.
    pub(super) fn define_instances(
        &mut self,
        units: &[Unit],
        monos: &[Instance],
    ) -> Result<(), CodegenError> {
        for mono in monos {
            let source = find(units, &mono.key, &self.derives);
            let method = self.monos[&instance_name(&mono.key, &mono.args)].clone();
            self.enter(&source, &mono.args);
            let receiver = match (&source.owner, method.receiver) {
                (Some((i, owner)), Some(_)) => {
                    let args = mono.args[..i.params.len()].to_vec();
                    Some(owner_type(units, owner, args))
                }
                _ if source.interface.is_some() => mono.args.first().cloned(),
                (_, Some(_)) if let Some((_, owner)) = &source.derive => {
                    self.derives.self_type(owner, &mono.args)
                }
                _ => None,
            };
            self.define_with(source.f, &method.signature, receiver)?;
            self.subst.clear();
        }
        Ok(())
    }

    /// Makes `args` the types of `source`'s parameters: the `impl`'s, then its own.
    fn enter(&mut self, source: &Source, args: &[Type]) {
        let outer = source
            .owner
            .iter()
            .flat_map(|(i, _)| i.params.iter().map(|p| &p.name));
        let names = outer.chain(source.f.type_params.iter().map(|p| &p.name));
        let hidden = source.interface.iter().flat_map(|(_, i)| {
            let own = i.type_params.iter().map(|p| p.name.name.clone());
            std::iter::once(SELF.to_string()).chain(own)
        });
        // A derive block's parameters: its own, or `Self` (v17, milestone 141).
        let derived = source.derive.iter().flat_map(|(d, _)| match d.target {
            DeriveTarget::Type(_) => d.type_params.iter().map(|p| p.name.name.clone()).collect(),
            _ => vec![SELF.to_string()],
        });
        let hidden = hidden.chain(derived);
        self.subst = hidden
            .chain(names.map(|n| n.name.clone()))
            .zip(args.iter().cloned())
            .collect::<HashMap<_, _>>();
    }
}

/// The declaration of function or method `key`.
fn find<'a>(units: &[Unit<'a>], key: &str, derives: &Derives) -> Source<'a> {
    for unit in units {
        for item in &unit.program.items {
            match item {
                Item::Derive(d) => {
                    let Some(owner) = derives.owner_at(d.span) else {
                        continue;
                    };
                    let found = d
                        .methods
                        .iter()
                        .find(|f| format!("{owner}.{}", f.name.name) == key);
                    if let Some(f) = found {
                        return Source {
                            f,
                            owner: None,
                            interface: None,
                            derive: Some((d, owner.to_string())),
                        };
                    }
                }
                Item::Fun(f) if qualify(unit.path, &f.name.name) == key => {
                    return Source {
                        f,
                        owner: None,
                        interface: None,
                        derive: None,
                    };
                }
                Item::Interface(i) => {
                    let owner = qualify(unit.path, &i.name.name);
                    let defaults = i.items.iter().filter_map(|it| it.default.as_ref());
                    if let Some(f) = defaults
                        .into_iter()
                        .find(|f| format!("{owner}.{}", f.name.name) == key)
                    {
                        return Source {
                            f,
                            owner: None,
                            interface: Some((owner, i)),
                            derive: None,
                        };
                    }
                }
                Item::Impl(i) => {
                    let owner = qualify(unit.path, &i.ty.name);
                    let found = i
                        .methods
                        .iter()
                        .find(|f| format!("{owner}.{}", f.name.name) == key);
                    if let Some(f) = found {
                        return Source {
                            f,
                            owner: Some((i, owner)),
                            interface: None,
                            derive: None,
                        };
                    }
                }
                _ => {}
            }
        }
    }
    unreachable!("checked: instance `{key}` has a declaration")
}

/// The symbol of `mono`: the type arguments go on the function, or for a
/// method on its type and then its own (`lugha_fn_Stack<string>.tag<f64>`).
fn symbol_of(source: &Source, mono: &Instance) -> String {
    // `lugha_fn_std/$json_value/Json@array.$to_json<i64>` (v17, milestone 141).
    if let Some((_, owner)) = &source.derive {
        let args = if mono.args.is_empty() {
            String::new()
        } else {
            format!("<{}>", symbol(&spelled(&mono.args)))
        };
        return format!("lugha_fn_{}.{}{args}", symbol(owner), source.f.name.name);
    }
    match &source.owner {
        // `lugha_fn_Error.report<ParseError>` (v5 draft §9).
        // `lugha_fn_Source<i64>.skip<Count>` for a generic interface (v6).
        None if let Some((interface, _)) = &source.interface => {
            let (receiver, own) = mono.args.split_at(1);
            format!(
                "lugha_fn_{}.{}<{}>",
                symbol(&instance_name(interface, own)),
                source.f.name.name,
                symbol(&spelled(receiver))
            )
        }
        None => format!("lugha_fn_{}", symbol(&instance_name(&mono.key, &mono.args))),
        Some((i, owner)) => {
            let (outer, own) = mono.args.split_at(i.params.len());
            let owner = if outer.is_empty() {
                owner.clone()
            } else {
                instance_name(owner, outer)
            };
            let own = if own.is_empty() {
                String::new()
            } else {
                format!("<{}>", symbol(&spelled(own)))
            };
            format!("lugha_fn_{}.{}{own}", symbol(&owner), source.f.name.name)
        }
    }
}

/// The type an `impl` of `owner` is for, at `args`.
fn owner_type(units: &[Unit], owner: &str, args: Vec<Type>) -> Type {
    let is_enum = units.iter().any(|u| {
        u.program
            .items
            .iter()
            .any(|item| matches!(item, Item::Enum(e) if qualify(u.path, &e.name.name) == owner))
    });
    if is_enum {
        Type::Enum(owner.to_string(), args)
    } else {
        Type::Struct(owner.to_string(), args)
    }
}
