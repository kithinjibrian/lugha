//! Iterator loops in codegen (v6 draft §5, §9; milestone 24): `for x of e`
//! keeps a copy of `e` in a hidden slot and calls its `next` until `None`,
//! directly for a concrete type and through the table for an interface value.

use inkwell::IntPredicate;
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, ValueKind};

use super::CodegenError;
use super::control::Loop;
use super::instances::instance_name;
use super::interfaces::SLOTS;
use super::lower::{Lowerer, POSITIONED};
use super::methods::key;
use super::scope::Local;
use super::value::llvm_type;
use crate::ast::{Block, Expr, Ident};
use crate::check::{Iteration, Type};

impl<'ctx> Lowerer<'ctx> {
    /// `for var of iter { body }` where `iter` isn't an array: `iter` is
    /// stored once (copied from a place, moved on last use), then each
    /// `Some` of its `next` runs the body with `var` bound to the payload.
    pub(super) fn iterator_loop(
        &mut self,
        var: &Ident,
        iter: &Expr,
        body: &Block,
        in_place: bool,
    ) -> Result<(), CodegenError> {
        let ty = self.ty(iter);
        let info = self
            .iterations
            .get(&iter.id)
            .cloned()
            .expect("checked: a loop over a non-array is an iterator loop");
        let element = self.concrete(&info.element);
        let option = Type::Option(Box::new(element.clone()));
        // `of mut` (v13) steps the place itself; otherwise a hidden copy.
        let slot = if in_place {
            self.place_address(iter)?.0
        } else {
            let value = self.value_for_store(iter, &ty)?;
            let slot = self.entry_alloca(&ty, "iter");
            self.builder.build_store(slot, value).expect(POSITIONED);
            slot
        };
        let (next, body_block, exit) = (
            self.append("iter.next"),
            self.append("iter.body"),
            self.append("iter.end"),
        );
        self.branch(next);

        self.builder.position_at_end(next);
        let result = self.call_next(&ty, &info, slot, &option);
        let stepped = self.entry_alloca(&option, "stepped");
        self.builder.build_store(stepped, result).expect(POSITIONED);
        let tag = self.tag(&option, stepped);
        let (some, payloads) = self.variant(&option, "Some");
        let is_some = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.context.i32_type().const_int(u64::from(some), false),
                "is.some",
            )
            .expect(POSITIONED);
        self.builder
            .build_conditional_branch(is_some, body_block, exit)
            .expect(POSITIONED);

        self.builder.position_at_end(body_block);
        self.scopes.push();
        let item = self.load_payload(&option, stepped, &payloads, 0, &element);
        let local = self.entry_alloca(&element, &var.name);
        self.builder.build_store(local, item).expect(POSITIONED);
        // The payload belongs to this iteration alone, like a moved value.
        self.scopes.declare(
            &var.name,
            Local {
                ptr: local,
                ty: element,
                borrowed: false,
            },
        );
        self.loops.push(Loop { next, exit });
        self.block(body)?;
        self.loops.pop();
        self.scopes.pop();
        self.branch(next);

        self.builder.position_at_end(exit);
        Ok(())
    }

    /// `next` called on the iterator in `slot`, giving an `option`.
    fn call_next(
        &mut self,
        ty: &Type,
        info: &Iteration,
        slot: PointerValue<'ctx>,
        option: &Type,
    ) -> BasicValueEnum<'ctx> {
        let site = match ty {
            Type::Interface(interface, _) => {
                let ptr = self.context.ptr_type(Default::default());
                let pair = self
                    .builder
                    .build_load(llvm_type(self.context, ty), slot, "pair")
                    .expect(POSITIONED)
                    .into_struct_value();
                let (table, boxed) = self.parts(pair);
                let methods = &self.interfaces[interface].methods;
                let index = methods.iter().position(|m| m.name == "next");
                let index = index.expect("checked: the interface declares `next`");
                let index = u32::try_from(index).expect("method count fits in u32") + SLOTS;
                let code = self.slot(table, index);
                let fn_type = llvm_type(self.context, option).fn_type(&[ptr.into()], false);
                self.builder
                    .build_indirect_call(fn_type, code, &[boxed.into()], "next")
            }
            _ => {
                let function = self.next_function(ty, info);
                self.builder.build_call(function, &[slot.into()], "next")
            }
        };
        match site.expect(POSITIONED).try_as_basic_value() {
            ValueKind::Basic(result) => result,
            ValueKind::Instruction(_) => unreachable!("`next` returns an `Option`"),
        }
    }

    /// The `next` of concrete type `ty`: its own (an instance for a generic
    /// type), else the default of the bound it came through.
    fn next_function(&self, ty: &Type, info: &Iteration) -> FunctionValue<'ctx> {
        let own = match ty {
            Type::Struct(owner, args) | Type::Enum(owner, args) => {
                let k = key(owner, "next");
                match args.is_empty() {
                    true => self.methods.get(&k).or_else(|| self.monos.get(&k)),
                    false => self.monos.get(&instance_name(&k, args)),
                }
            }
            _ => None,
        };
        let default = || {
            let (interface, args) = info.default.as_ref()?;
            let args: Vec<Type> = std::iter::once(ty.clone())
                .chain(args.iter().map(|a| self.concrete(a)))
                .collect();
            self.monos
                .get(&instance_name(&format!("{interface}.next"), &args))
        };
        own.or_else(default)
            .expect("checked: an iterated type's `next` is compiled")
            .signature
            .function
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::interfaces::tests::body;
    use crate::codegen::test_util::ir;

    const COUNT: &str = "struct Count { n: i64 }\n\
        impl Count { fun next(mut self): Option<i64> = None; }\n\
        interface Source<T> { fun next(mut self): Option<T>; }\n";

    #[test]
    fn a_loop_calls_next_on_its_own_copy() {
        let ir = ir(&format!(
            "{COUNT}fun main() {{ let c = Count {{ n: 1 }}; for n of c {{ println(n); }} }}"
        ));
        let main = body(&ir, "main");
        assert!(
            main.contains("call %\"Option<i64>\" @lugha_fn_Count.next(ptr %iter"),
            "{main}"
        );
    }

    #[test]
    fn a_loop_over_an_interface_value_calls_through_its_table() {
        let ir = ir(&format!(
            "{COUNT}fun main() {{ let s: Source<i64> = Count {{ n: 1 }}; for n of s {{ println(n); }} }}"
        ));
        let main = body(&ir, "main");
        assert!(main.contains("call %\"Option<i64>\" %slot"), "{main}");
    }

    #[test]
    fn of_mut_aliases_elements_and_steps_the_place_v13() {
        let ir = ir("fun main() { let mut xs = [1, 2]; for x of mut xs { x += 1; } }");
        let main = &ir[ir.find("@lugha_fn_main(").unwrap()..];
        assert!(
            !main.contains("%x = alloca"),
            "the element is the variable: {main}"
        );
    }
}
