//! Converting between interfaces in codegen (v6 draft §9, as amended by
//! PRP-054): a `J` value keeps its box and gets the `I` table of the same
//! concrete type, found at run time by `lugha_up_<J>.<I>`, which compares
//! its table with every `J` table the program has. The `I` table is derived
//! from the root table the value was built with (`lugha_vt_<C>.<J>.<I>`),
//! so every method behaves as it did through `J`.

use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, StructValue, ValueKind};

use super::lower::{Lowerer, POSITIONED};
use crate::check::{Type, symbol};

/// One dispatch table: concrete type `C`, the interface whose table it was
/// built as (`root`), and the interface it serves (`interface`).
struct Table<'ctx> {
    concrete: Type,
    root: Type,
    interface: Type,
    global: PointerValue<'ctx>,
}

/// Every table emitted so far, and every `lugha_up_` function a conversion
/// asked for, bodies to come at the end of lowering.
#[derive(Default)]
pub(super) struct Upcasts<'ctx> {
    tables: Vec<Table<'ctx>>,
    functions: Vec<(Type, Type, FunctionValue<'ctx>)>,
}

impl<'ctx> Upcasts<'ctx> {
    /// Records a table.
    pub(super) fn add(
        &mut self,
        concrete: &Type,
        root: &Type,
        interface: &Type,
        global: PointerValue<'ctx>,
    ) {
        self.tables.push(Table {
            concrete: concrete.clone(),
            root: root.clone(),
            interface: interface.clone(),
            global,
        });
    }

    /// Every concrete type with a table serving `interface`, once each (v12).
    pub(super) fn concretes(&self, interface: &crate::check::Type) -> Vec<crate::check::Type> {
        let mut found: Vec<crate::check::Type> = Vec::new();
        for t in self.tables.iter().filter(|t| t.interface == *interface) {
            if !found.contains(&t.concrete) {
                found.push(t.concrete.clone());
            }
        }
        found
    }

    /// The table of `concrete` built as `root` that serves `interface`.
    fn find(&self, concrete: &Type, root: &Type, interface: &Type) -> Option<PointerValue<'ctx>> {
        self.tables
            .iter()
            .find(|t| t.concrete == *concrete && t.root == *root && t.interface == *interface)
            .map(|t| t.global)
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// `pair`, a value of interface type `from`, as a value of interface `to`:
    /// the same box with the table `lugha_up_<from>.<to>` finds.
    pub(super) fn upcast(
        &mut self,
        pair: StructValue<'ctx>,
        from: &Type,
        to: &Type,
    ) -> StructValue<'ctx> {
        let up = self.up_function(from, to);
        let (table, boxed) = self.parts(pair);
        let call = self
            .builder
            .build_call(up, &[table.into()], "up")
            .expect(POSITIONED);
        let ValueKind::Basic(found) = call.try_as_basic_value() else {
            unreachable!("an up-function returns a table")
        };
        self.pair(found.into_pointer_value(), boxed)
    }

    /// `lugha_up_<from>.<to>(ptr) -> ptr`, declared once; its body is built
    /// by `finish_upcasts`.
    fn up_function(&mut self, from: &Type, to: &Type) -> FunctionValue<'ctx> {
        let name = format!(
            "lugha_up_{}.{}",
            symbol(&from.to_string()),
            symbol(&to.to_string())
        );
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let ptr = self.context.ptr_type(Default::default());
        let function = self.module.add_function(
            &name,
            ptr.fn_type(&[ptr.into()], false),
            Some(Linkage::Internal),
        );
        self.upcasts
            .functions
            .push((from.clone(), to.clone(), function));
        function
    }

    /// At the end of lowering, when every table is known: the derived
    /// tables each conversion can reach (repeated until none is new, for
    /// chains), then each up-function's body.
    pub(super) fn finish_upcasts(&mut self) {
        loop {
            let mut wanted = Vec::new();
            for (from, to, _) in &self.upcasts.functions {
                for t in self.upcasts.tables.iter().filter(|t| t.interface == *from) {
                    if self.upcasts.find(&t.concrete, &t.root, to).is_none() {
                        wanted.push((t.concrete.clone(), t.root.clone(), to.clone()));
                    }
                }
            }
            if wanted.is_empty() {
                break;
            }
            for (concrete, root, to) in wanted {
                if self.upcasts.find(&concrete, &root, &to).is_none() {
                    self.derived_table(&concrete, &root, &to);
                }
            }
        }
        for (from, to, function) in std::mem::take(&mut self.upcasts.functions) {
            self.up_body(&from, &to, function);
        }
    }

    /// `lugha_vt_<C>.<root>.<to>`: `C`'s identity and copy, and for each
    /// item of `to` the function `C`'s `root` table has for it.
    fn derived_table(&mut self, concrete: &Type, root: &Type, to: &Type) {
        let (Type::Interface(root_key, root_args), Type::Interface(key, _)) = (root, to) else {
            unreachable!("checked: tables are for interface types")
        };
        let mut slots: Vec<BasicValueEnum> = vec![
            self.type_id(concrete).into(),
            self.box_copy(concrete)
                .as_global_value()
                .as_pointer_value()
                .into(),
        ];
        for m in self.interfaces[key].methods.clone() {
            let method = self.method_of(concrete, root_key, root_args, &m.name);
            slots.push(method.as_global_value().as_pointer_value().into());
        }
        let name = format!(
            "lugha_vt_{}.{}.{}",
            symbol(&concrete.to_string()),
            symbol(&root.to_string()),
            symbol(&to.to_string())
        );
        let table_type = self.table_type(key);
        let global = self.module.add_global(table_type, None, &name);
        global.set_initializer(&table_type.const_named_struct(&slots));
        global.set_constant(true);
        global.set_linkage(Linkage::Internal);
        self.upcasts
            .add(concrete, root, to, global.as_pointer_value());
    }

    /// The body of `lugha_up_<from>.<to>`: the `to` table of whichever
    /// `from` table it is given.
    fn up_body(&mut self, from: &Type, to: &Type, function: FunctionValue<'ctx>) {
        let given = function
            .get_nth_param(0)
            .expect("one parameter")
            .into_pointer_value();
        let mut block = self.context.append_basic_block(function, "entry");
        let sources: Vec<(PointerValue<'ctx>, PointerValue<'ctx>)> = self
            .upcasts
            .tables
            .iter()
            .filter(|t| t.interface == *from)
            .map(|t| {
                let target = self
                    .upcasts
                    .find(&t.concrete, &t.root, to)
                    .expect("built by finish_upcasts");
                (t.global, target)
            })
            .collect();
        for (source, target) in sources {
            self.builder.position_at_end(block);
            let same = self
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, given, source, "same")
                .expect(POSITIONED);
            let found = self.context.append_basic_block(function, "found");
            let next = self.context.append_basic_block(function, "next");
            self.builder
                .build_conditional_branch(same, found, next)
                .expect(POSITIONED);
            self.builder.position_at_end(found);
            self.builder.build_return(Some(&target)).expect(POSITIONED);
            block = next;
        }
        // Every interface value's table is one the program built.
        self.builder.position_at_end(block);
        self.builder.build_unreachable().expect(POSITIONED);
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::interfaces::tests::body;
    use crate::codegen::test_util::ir;

    const PETS: &str = "interface Named { fun name(self): string; }\n\
        interface Pet { fun name(self): string; fun sound(self): string; }\n\
        struct Dog { n: string }\n\
        impl Dog { fun name(self): string = self.n; fun sound(self): string = \"woof\"; }\n";

    #[test]
    fn a_conversion_finds_the_derived_table_at_run_time() {
        let ir = ir(&format!(
            "{PETS}fun main() {{ let p: Pet = Dog {{ n: \"d\" }}; let n: Named = p; println(n.name()); }}"
        ));
        assert!(
            body(&ir, "main").contains("call ptr @lugha_up_Pet.Named("),
            "{ir}"
        );
        let at = ir
            .find("define internal ptr @lugha_up_Pet.Named(")
            .expect("the up-function");
        let up = &ir[at..at + ir[at..].find("\n}").expect("its end")];
        assert!(
            up.contains("@lugha_vt_Dog.Pet") && up.contains("ret ptr @lugha_vt_Dog.Pet.Named"),
            "{up}"
        );
        let table = ir
            .lines()
            .find(|l| l.starts_with("@lugha_vt_Dog.Pet.Named = "))
            .expect("a derived table");
        assert!(
            table.contains("@lugha_type_Dog")
                && table.contains("@lugha_copy_3Dog_box")
                && table.contains("@lugha_fn_Dog.name"),
            "{table}"
        );
    }

    #[test]
    fn programs_without_such_conversions_have_none() {
        let ir = ir(&format!(
            "{PETS}fun main() {{ let p: Pet = Dog {{ n: \"d\" }}; println(p.name()); }}"
        ));
        assert!(!ir.contains("lugha_up_"), "{ir}");
    }
}
