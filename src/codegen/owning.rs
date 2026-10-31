//! Handing over arguments (v17, milestone 149, PRP-180): a function that
//! keeps a parameter (stores it on its last use) is compiled twice. Its
//! ordinary instance, `lugha_fn_<symbol>`, marks what it stores, as every
//! function did before, and is what function values, interfaces, derive
//! blocks, printing and every other internal path call. Its owning instance,
//! `lugha_fn_<symbol>.$own`, takes its kept parameters as its own: their
//! last-use stores move. Only direct calls reach it, marking each kept
//! argument the caller still needs and handing over the rest unmarked.

use inkwell::values::{BasicMetadataValueEnum, FunctionValue};

use super::CodegenError;
use super::function::Signature;
use super::lower::{Lowerer, POSITIONED};
use super::value::is_aggregate;
use crate::ast::{Expr, ExprId};
use crate::check::Type;

/// A keeping function's owning instance and which slots it keeps
/// (receiver first for a method).
#[derive(Debug, Clone)]
pub(super) struct Owned<'ctx> {
    pub function: FunctionValue<'ctx>,
    pub kept: Vec<bool>,
}

impl<'ctx> Lowerer<'ctx> {
    /// Declares `function`'s owning instance when the checker found that
    /// declaration `key` keeps a parameter holding arrays at these types.
    pub(super) fn declare_owning(
        &mut self,
        key: &str,
        function: FunctionValue<'ctx>,
        params: &[Type],
        receiver: bool,
    ) {
        let Some(found) = self.keeps.get(key) else {
            return;
        };
        // A receiver's type isn't known here: kept means kept. A parameter
        // without arrays gains nothing from being owned.
        let kept: Vec<bool> = found
            .iter()
            .enumerate()
            .map(|(slot, &k)| {
                k && match slot.checked_sub(usize::from(receiver)) {
                    None => true,
                    Some(i) => params.get(i).is_some_and(|t| t.is_deep(&self.structs)),
                }
            })
            .collect();
        if !kept.iter().any(|&k| k) {
            return;
        }
        let name = format!("{}.$own", function.get_name().to_string_lossy());
        let linkage = function.get_linkage();
        let own = self
            .module
            .add_function(&name, function.get_type(), Some(linkage));
        self.owning_instances.insert(
            function,
            Owned {
                function: own,
                kept,
            },
        );
    }

    /// The owning instance to define after `signature`'s ordinary body.
    pub(super) fn owning_of(&self, signature: &Signature<'ctx>) -> Option<Signature<'ctx>> {
        let owned = self.owning_instances.get(&signature.function)?;
        Some(Signature {
            function: owned.function,
            ..signature.clone()
        })
    }

    /// True if the value at `id`, read from a place, is stored without a
    /// copy: a dead local's, or a kept parameter's in an owning instance.
    pub(super) fn moved(&self, id: ExprId) -> bool {
        self.moves.contains(&id) || (self.owning && self.param_moves.contains(&id))
    }

    /// True if the argument at `id` is handed over unmarked to a kept
    /// parameter: nothing reads it again.
    pub(super) fn handed(&self, id: ExprId) -> bool {
        self.hands.contains(&id) || (self.owning && self.param_hands.contains(&id))
    }

    /// `arg`'s value for a kept parameter: as is when handed over,
    /// otherwise marked shared, as the callee's own store would have.
    pub(super) fn protect(
        &mut self,
        arg: &Expr,
        value: inkwell::values::BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        if self.handed(arg.id) {
            value
        } else {
            self.deep_copy(value, ty)
        }
    }

    /// Lowers a direct call's arguments for owning instance `owned` (its
    /// slots after any receiver): kept ones protected, the rest as for
    /// any call (`arguments`).
    pub(super) fn owned_arguments(
        &mut self,
        args: &[Expr],
        params: &[Type],
        kept: &[bool],
    ) -> Result<Vec<BasicMetadataValueEnum<'ctx>>, CodegenError> {
        let mut values: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());
        for (i, (arg, ty)) in args.iter().zip(params).enumerate() {
            let value = self.get(arg, ty)?;
            let value = if kept.get(i).copied().unwrap_or(false) {
                self.protect(arg, value, ty)
            } else {
                value
            };
            let value = if is_aggregate(ty) {
                // Passed as a pointer to caller storage (spec §4, §9).
                let slot = self.entry_alloca(ty, "arg");
                self.builder.build_store(slot, value).expect(POSITIONED);
                slot.into()
            } else {
                value
            };
            values.push(value.into());
        }
        Ok(values)
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::test_util::ir;

    const BAG: &str = "struct Bag { items: i64[] }\n\
        fun insert(b: Bag, k: i64): Bag { let mut out = b; out.items.push(k); out }\n\
        fun total(b: Bag): i64 = b.items.len;\n";

    /// The body of function `name` in `ir`.
    fn body<'a>(ir: &'a str, name: &str) -> &'a str {
        let at = ir
            .lines()
            .find(|l| l.starts_with("define ") && l.contains(&format!(" @{name}(")))
            .expect(name);
        let rest = &ir[ir.find(at).expect("a line of ir")..];
        &rest[..rest.find("\n}").expect("a body ends")]
    }

    #[test]
    fn a_keeping_function_has_an_owning_instance_and_direct_calls_use_it() {
        let ir = ir(&format!(
            "{BAG}fun main() {{ let mut bag = Bag {{ items: [] }}; bag = insert(bag, 1); let more = insert(bag, 2); println(total(bag) + total(more)); }}"
        ));
        assert!(ir.contains("define void @lugha_fn_insert("), "{ir}");
        assert!(
            ir.contains("define void @\"lugha_fn_insert.$own\"("),
            "{ir}"
        );
        // A function that only reads its parameter has no second instance.
        assert!(!ir.contains("lugha_fn_total.$own"), "{ir}");
        let main = body(&ir, "lugha_fn_main");
        assert_eq!(
            main.matches("call void @\"lugha_fn_insert.$own\"").count(),
            2,
            "{main}"
        );
        // Handed over once (no copy), protected once (`bag` is read after).
        assert_eq!(main.matches("@lugha_copy_3Bag").count(), 1, "{main}");
        // The owning instance moves its parameter; the ordinary one copies it.
        assert!(!body(&ir, "\"lugha_fn_insert.$own\"").contains("lugha_copy_"));
        assert!(body(&ir, "lugha_fn_insert").contains("lugha_copy_3Bag"));
    }

    #[test]
    fn a_function_value_calls_the_ordinary_instance() {
        let ir = ir(&format!(
            "{BAG}fun main() {{ let f = insert; let b = f(Bag {{ items: [] }}, 1); println(total(b)); }}"
        ));
        let wrapper = body(&ir, "lugha_ref_insert");
        assert!(wrapper.contains("call void @lugha_fn_insert("), "{wrapper}");
        assert!(!wrapper.contains("$own"), "{wrapper}");
    }
}
