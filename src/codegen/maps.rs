//! Maps and sets in codegen (v17, milestone 101): a `#{ … }` literal is a
//! call of the prelude's `$map_of(keys, values)` or `$set_of(keys)` instance
//! the checker recorded at the literal's id.

use inkwell::IntPredicate;
use inkwell::values::{BasicValueEnum, ValueKind};

use super::CodegenError;
use super::lower::{Lowerer, POSITIONED};
use super::value::Value;
use crate::ast::{Expr, ExprKind, HashLiteral, Ident};
use crate::check::Type;

impl<'ctx> Lowerer<'ctx> {
    /// True if `e` is a map (the prelude's `Map<K, V>`).
    pub(super) fn is_map(&self, e: &Expr) -> bool {
        matches!(self.ty(e), Type::Struct(k, _) if k == "$prelude.Map")
    }

    /// `m[k]`: the `Ok` of the map's `$entry`, or a panic at `at` (the `[`)
    /// with its `Err`'s message, `no key "z" in the map`.
    pub(super) fn map_entry(
        &mut self,
        expr: &Expr,
        base: &Expr,
        key: &Expr,
        at: usize,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let Some(Value::Val(found)) =
            self.map_method(expr, base, "$entry", std::slice::from_ref(key))?
        else {
            unreachable!("`$entry` returns a `Result`")
        };
        let Type::Struct(_, args) = self.ty(base) else {
            unreachable!("checked: a map")
        };
        let result = Type::Enum(
            "$prelude.Result".to_string(),
            vec![args[1].clone(), Type::String],
        );
        let slot = self.entry_alloca(&result, "entry");
        self.builder.build_store(slot, found).expect(POSITIONED);
        let (ok, payloads) = self.variant(&result, "Ok");
        let tag = self.tag(&result, slot);
        let ok = self.context.i32_type().const_int(u64::from(ok), false);
        let has = self
            .builder
            .build_int_compare(IntPredicate::EQ, tag, ok, "has.key")
            .expect(POSITIONED);
        let (pass, fail) = (self.append("key.found"), self.append("key.missing"));
        self.builder
            .build_conditional_branch(has, pass, fail)
            .expect(POSITIONED);
        self.builder.position_at_end(fail);
        let (_, errors) = self.variant(&result, "Err");
        let message = self.load_payload(&result, slot, &errors, 0, &errors[0]);
        self.call_panic(message, at);
        self.builder.position_at_end(pass);
        Ok(self.load_payload(&result, slot, &payloads, 0, &payloads[0]))
    }

    /// `base.name(args)`, the method the checker recorded at `at`'s id: a
    /// map entry's read (`at`) or write (`insert`, a `mut self` receiver).
    pub(super) fn map_method(
        &mut self,
        at: &Expr,
        base: &Expr,
        name: &str,
        args: &[Expr],
    ) -> Result<Option<Value<'ctx>>, CodegenError> {
        let method = Ident {
            name: name.to_string(),
            span: at.span,
        };
        let callee = Expr {
            id: at.id,
            span: at.span,
            kind: ExprKind::Field(Box::new(base.clone()), method),
        };
        self.method_call(at, &callee, args)
    }

    /// `#{ … }`: its instance of `$map_of` or `$set_of`, called on its arrays.
    pub(super) fn hashed(
        &mut self,
        expr: &Expr,
        lit: &HashLiteral,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError> {
        let key = self.callees[&expr.id].clone();
        let instance = self
            .prelude_target(expr, &key)
            .expect("checked: a literal is an instance of `$map_of` or `$set_of`");
        let args = match key.ends_with("$set_of") {
            true => vec![lit.keys.clone()],
            false => vec![lit.keys.clone(), lit.values.clone()],
        };
        let values = self.arguments(&args, &instance.signature.params, false)?;
        let site = self
            .builder
            .build_call(instance.signature.function, &values, "hashed")
            .expect(POSITIONED);
        match site.try_as_basic_value() {
            ValueKind::Basic(result) => Ok(result),
            ValueKind::Instruction(_) => unreachable!("`$map_of` and `$set_of` return a value"),
        }
    }
}
