//! Operator overloading in codegen. An operator whose left operand is a struct or enum calls its
//! method (`lugha_fn_Vec2.add`), receiver first like any `self` method; an
//! interface value dispatches through its table. Everything else (numbers,
//! strings, and so every instance of a `T: Num` body at a number type)
//! keeps the built-in operator, so `add<i64>` is a plain checked `add`.

use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, ValueKind};

use super::CodegenError;
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::methods::{Method, key};
use super::value::Value;
use crate::ast::{Expr, ExprKind, Ident};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// `lhs op rhs` (or `-lhs`) as a call of method `name`, when `lhs`'s
    /// type is a struct, enum or interface value; `None` otherwise.
    pub(super) fn operator(
        &mut self,
        expr: &Expr,
        name: &str,
        lhs: &Expr,
        rhs: Option<&Expr>,
    ) -> Result<Option<BasicValueEnum<'ctx>>, CodegenError> {
        let ty = self.ty(lhs);
        let args: Vec<Expr> = rhs.into_iter().cloned().collect();
        match &ty {
            Type::Struct(..) | Type::Enum(..) => {
                let current = self.get(lhs, &ty)?;
                self.operator_on(expr, (current, &ty), name, &args)
                    .map(Some)
            }
            // Through the interface's table: the method call an operator stands for.
            Type::Interface(..) => {
                let method = Ident {
                    name: name.to_string(),
                    span: expr.span,
                };
                let callee = Expr {
                    id: expr.id,
                    span: expr.span,
                    kind: ExprKind::Field(Box::new(lhs.clone()), method),
                };
                match self.method_call(expr, &callee, &args)? {
                    Some(Value::Val(v)) => Ok(Some(v)),
                    _ => unreachable!("an operator method returns a value"),
                }
            }
            _ => Ok(None),
        }
    }

    /// Calls `ty`'s operator method `name` on `current` with `args`, for
    /// the operator (or compound assignment) whose checker id is `call`'s.
    pub(super) fn operator_on(
        &mut self,
        call: &Expr,
        (current, ty): (BasicValueEnum<'ctx>, &Type),
        name: &str,
        args: &[Expr],
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        // A type with several `mul`s: the one the checker chose.
        let chosen = match (self.overloads.get(&call.id), ty, args) {
            (Some(chosen), _, _) => Some(chosen.clone()),
            // Through a bound: the concrete type is only known now.
            (None, Type::Struct(owner, _) | Type::Enum(owner, _), [operand]) => {
                Some(self.overload_for(owner, name, &self.ty(operand)))
            }
            _ => None,
        };
        let name = chosen.as_deref().unwrap_or(name);
        let method = self.operator_method(call, ty, name);
        // A `self` receiver is a pointer to a copy, as for any method call.
        let slot = self.entry_alloca(ty, "self");
        self.builder.build_store(slot, current).expect(POSITIONED);
        let mut values: Vec<BasicMetadataValueEnum> = vec![slot.into()];
        values.extend(self.arguments(args, &method.signature.params, false)?);
        let site = self
            .builder
            .build_call(method.signature.function, &values, name)
            .expect(POSITIONED);
        match site.try_as_basic_value() {
            ValueKind::Basic(result) => Ok(result),
            ValueKind::Instruction(_) => unreachable!("an operator method returns a value"),
        }
    }

    /// For a type with several arithmetic methods `name` (`mul`,
    /// `mul(f64)`, …), the one taking `operand`; `name` itself otherwise.
    /// Used where the checker chose through a bound, so the concrete type
    /// is only known here.
    pub(super) fn overload_for(&self, owner: &str, name: &str, operand: &Type) -> String {
        if !crate::modules::overloads::ARITHMETIC.contains(&name) {
            return name.to_string();
        }
        let prefix = format!("{}(", key(owner, name));
        self.methods
            .iter()
            .filter(|(k, _)| **k == key(owner, name) || k.starts_with(&prefix))
            .find(|(_, m)| m.signature.params.first() == Some(operand))
            .map_or_else(
                || name.to_string(),
                |(k, _)| k[owner.len() + 1..].to_string(),
            )
    }

    /// The method `name` of struct or enum `ty`: an instance for a generic
    /// type, else the declared method or a listed prelude one.
    fn operator_method(&self, call: &Expr, ty: &Type, name: &str) -> Method<'ctx> {
        let (Type::Struct(owner, args) | Type::Enum(owner, args)) = ty else {
            unreachable!("operators call methods of structs and enums")
        };
        let k = key(owner, name);
        let by_type = || {
            (!args.is_empty())
                .then(|| self.monos.get(&instance_name(&k, args)).cloned())
                .flatten()
        };
        self.instance_of(call, &k)
            .or_else(by_type)
            .or_else(|| self.methods.get(&k).cloned())
            .or_else(|| self.monos.get(&k).cloned())
            .unwrap_or_else(|| panic!("checked: `{k}` is an operator method"))
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    const VEC2: &str = "struct Vec2 { x: f64, y: f64 }\nimpl Vec2 {\n\
        fun add(self, o: Vec2): Vec2 = Vec2 { x: self.x + o.x, y: self.y + o.y };\n\
        fun neg(self): Vec2 = Vec2 { x: -self.x, y: -self.y };\n}\n";

    #[test]
    fn user_types_call_their_operator_methods() {
        let ir = ir(&format!(
            "{VEC2}fun main() {{ let mut a = Vec2 {{ x: 1.0, y: 2.0 }}; let b = a + -a; a += b; }}"
        ));
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        assert_eq!(
            main.matches("call void @lugha_fn_Vec2.add(ptr sret(%Vec2)")
                .count(),
            2,
            "{ir}"
        );
        assert!(main.contains("@lugha_fn_Vec2.neg("), "{ir}");
    }

    #[test]
    fn brackets_on_user_types_call_index_and_set_index() {
        let ir = ir("struct B { xs: i64[] }\nimpl B {\n\
            fun index(self, i: i64): i64 = self.xs[i];\n\
            fun set_index(mut self, i: i64, v: i64) { self.xs[i] = v; }\n}\n\
            fun main() { let mut b = B { xs: [1, 2] }; b[0] = b[1]; }");
        let main = &ir[ir.find("define void @lugha_fn_main").unwrap()..];
        assert!(main.contains("call i64 @lugha_fn_B.index("), "{ir}");
        assert!(main.contains("call void @lugha_fn_B.set_index("), "{ir}");
    }
}
