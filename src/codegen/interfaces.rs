//! Interface values in codegen (v5 draft §7, §9; milestone 20): a value is
//! `{ table, box }`. The box is a `lugha_rt_alloc` allocation holding the
//! concrete value; the table, `lugha_vt_<Type>.<Interface>`, holds the
//! type's identity, a box copy function, then the methods in the
//! interface's order. Every method takes its receiver as a pointer, so a
//! slot is the method itself, called with the box.

use inkwell::module::Linkage;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, StructValue, ValueKind};

use super::CodegenError;
use super::copy::{aliasing, mangle};
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::methods::key as method_key;
use super::value::{Value, llvm_type};
use crate::ast::Expr;
use crate::check::{Type, symbol};

/// Table slots before the methods: the type's identity, then its box copy.
pub(super) const SLOTS: u32 = 2;

/// A named type in a symbol, its length first (`5Shape`), so it can't be
/// confused with a primitive or a suffix (PRP-015, PRP-036, PRP-047).
pub(super) fn length_prefixed(ty: &Type) -> String {
    format!("{}{ty}", ty.to_string().len())
}

impl<'ctx> Lowerer<'ctx> {
    /// `expr` as a value of interface type `to`: its value, copied if it is
    /// still read elsewhere, in a new box paired with its type's table.
    pub(super) fn converted(
        &mut self,
        expr: &Expr,
        to: &Type,
    ) -> Result<Value<'ctx>, CodegenError> {
        let ty = self.concrete(&self.types[expr.id.0 as usize]);
        // In an instance whose type argument is the interface itself.
        if ty == *to {
            return self.unconverted(expr);
        }
        // While its own value is lowered, `expr` has its own type (`ty`).
        let entry = self.conversions.remove(&expr.id);
        let lowered = self.unconverted(expr);
        self.conversions.extend(entry.map(|to| (expr.id, to)));
        let value = match lowered? {
            Value::Val(value) => value,
            other => return Ok(other),
        };
        // The box owns its value, as a variable would (spec §4 copy sites).
        let copies = aliasing(expr) && !self.moves.contains(&expr.id);
        let value = if copies {
            self.deep_copy(value, &ty)
        } else {
            value
        };
        Ok(Value::Val(self.box_as(value, &ty, to)))
    }

    /// True if `expr`'s value may come from an existing place, so a copy
    /// site copies it. A conversion's value is a fresh box, already holding
    /// its own copy.
    pub(super) fn aliased(&self, expr: &Expr) -> bool {
        !self.conversions.contains_key(&expr.id) && aliasing(expr)
    }

    /// `{ table, box }`.
    pub(super) fn pair(
        &self,
        table: PointerValue<'ctx>,
        boxed: PointerValue<'ctx>,
    ) -> StructValue<'ctx> {
        let ty =
            llvm_type(self.context, &Type::Interface(String::new(), Vec::new())).into_struct_type();
        let pair = self
            .builder
            .build_insert_value(ty.get_undef(), table, 0, "table")
            .expect("a pair has 2 fields");
        self.builder
            .build_insert_value(pair, boxed, 1, "box")
            .expect("a pair has 2 fields")
            .into_struct_value()
    }

    /// A new GC allocation holding `value` of type `ty`.
    pub(super) fn new_box(&self, ty: &Type, value: BasicValueEnum<'ctx>) -> PointerValue<'ctx> {
        let size = self
            .context
            .i64_type()
            .const_int(self.element_size(ty), false);
        let alloc = self.runtime("lugha_rt_alloc", &[Type::I64], Some(Type::String));
        let call = self
            .builder
            .build_call(alloc, &[size.into()], "box")
            .expect(POSITIONED);
        let ValueKind::Basic(heap) = call.try_as_basic_value() else {
            unreachable!("lugha_rt_alloc returns a pointer")
        };
        let heap = heap.into_pointer_value();
        self.store_element(heap, ty, value);
        heap
    }

    /// The LLVM type of a table of interface `key`: one pointer per slot.
    pub(super) fn table_type(&self, key: &str) -> StructType<'ctx> {
        let ptr = self.context.ptr_type(Default::default());
        let count = SLOTS as usize + self.interfaces[key].methods.len();
        self.context.struct_type(&vec![ptr.into(); count], false)
    }

    /// `lugha_vt_<Type>.<Interface>`, emitted once per pair; a generic
    /// interface prints with its arguments (`lugha_vt_Count.Source<i64>`, v6).
    pub(super) fn table(&mut self, ty: &Type, interface: &Type) -> PointerValue<'ctx> {
        let Type::Interface(key, args) = interface else {
            unreachable!("checked: tables are for interface types")
        };
        let name = format!(
            "lugha_vt_{}.{}",
            symbol(&ty.to_string()),
            symbol(&interface.to_string())
        );
        if let Some(global) = self.module.get_global(&name) {
            return global.as_pointer_value();
        }
        let mut slots: Vec<BasicValueEnum> = vec![
            self.type_id(ty).into(),
            self.box_copy(ty)
                .as_global_value()
                .as_pointer_value()
                .into(),
        ];
        for m in self.interfaces[key].methods.clone() {
            let method = self.method_of(ty, key, args, &m.name);
            slots.push(method.as_global_value().as_pointer_value().into());
        }
        let table_type = self.table_type(key);
        let global = self.module.add_global(table_type, None, &name);
        global.set_initializer(&table_type.const_named_struct(&slots));
        global.set_constant(true);
        global.set_linkage(Linkage::Internal);
        self.upcasts
            .add(ty, interface, interface, global.as_pointer_value());
        global.as_pointer_value()
    }

    /// The type's own method when it has one, else the instance of
    /// interface `key`'s default for `ty` and the interface's `args` (v5, v6).
    pub(super) fn method_of(
        &self,
        ty: &Type,
        key: &str,
        args: &[Type],
        name: &str,
    ) -> FunctionValue<'ctx> {
        let own = match ty {
            Type::Handle(owner) => self.methods.get(&method_key(owner, name)),
            Type::Struct(owner, targs) | Type::Enum(owner, targs) => {
                let k = method_key(owner, name);
                match targs.is_empty() {
                    true => self.methods.get(&k).or_else(|| self.monos.get(&k)),
                    false => self.monos.get(&instance_name(&k, targs)),
                }
            }
            _ => None,
        };
        let default = || {
            let k = format!("{key}.{name}");
            let all = [std::slice::from_ref(ty), args].concat();
            self.monos.get(&instance_name(&k, &all))
        };
        own.or_else(default)
            .expect("checked: a satisfied interface's methods are compiled")
            .signature
            .function
    }

    /// `lugha_type_<Type>`: a byte whose address identifies `ty` (v5 type patterns).
    pub(super) fn type_id(&self, ty: &Type) -> PointerValue<'ctx> {
        let name = format!("lugha_type_{}", symbol(&ty.to_string()));
        if let Some(global) = self.module.get_global(&name) {
            return global.as_pointer_value();
        }
        let byte = self.context.i8_type();
        let global = self.module.add_global(byte, None, &name);
        global.set_initializer(&byte.const_zero());
        global.set_constant(true);
        global.set_linkage(Linkage::Internal);
        global.as_pointer_value()
    }

    /// `lugha_copy_<mangled>_box(ptr) -> ptr`: a new box holding a deep copy
    /// of the old box's value.
    pub(super) fn box_copy(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_copy_{}_box", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let ptr = self.context.ptr_type(Default::default());
        let function = self.module.add_function(
            &name,
            ptr.fn_type(&[ptr.into()], false),
            Some(Linkage::Internal),
        );
        // Build the body elsewhere, then come back to where lowering was.
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let source = function
            .get_nth_param(0)
            .expect("one parameter")
            .into_pointer_value();
        let value = self.load_element(source, ty);
        let value = self.deep_copy(value, ty);
        let copy = self.new_box(ty, value);
        self.builder.build_return(Some(&copy)).expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }
}

#[cfg(test)]
pub(super) mod tests {
    use crate::codegen::test_util::ir;

    pub(in crate::codegen) const SHAPES: &str = "interface Shape { fun area(self): f64; fun grow(mut self, k: f64); }\n\
        struct Circle { r: f64 }\n\
        impl Circle { fun area(self): f64 = self.r; fun grow(mut self, k: f64) { self.r *= k; } }\n\
        struct Square { side: f64 }\n\
        impl Square { fun area(self): f64 = self.side; fun grow(mut self, k: f64) { self.side *= k; } }\n";

    pub(in crate::codegen) fn body<'a>(ir: &'a str, function: &str) -> &'a str {
        // The definition, wherever it is: rewritten functions move to the
        // module's end (milestones 125–126), after their callers.
        let call = format!("@lugha_fn_{function}(");
        let header = ir
            .match_indices("define ")
            .map(|(at, _)| at)
            .find(|&at| ir[at..].lines().next().is_some_and(|l| l.contains(&call)))
            .unwrap_or_else(|| panic!("{function} is not defined: {ir}"));
        let start = &ir[header..];
        &start[..start.find("\n}").unwrap()]
    }

    #[test]
    fn each_converted_pair_gets_one_table() {
        let ir = ir(&format!(
            "{SHAPES}fun main() {{ let a: Shape = Circle {{ r: 1.0 }}; let b: Shape = Circle {{ r: 2.0 }}; let c: Shape = Square {{ side: 1.0 }}; }}"
        ));
        assert_eq!(ir.matches("@lugha_vt_Circle.Shape = ").count(), 1, "{ir}");
        assert_eq!(ir.matches("@lugha_vt_Square.Shape = ").count(), 1, "{ir}");
        let table = ir
            .lines()
            .find(|l| l.starts_with("@lugha_vt_Circle.Shape = "))
            .unwrap();
        let (area, grow) = (
            table.find("@lugha_fn_Circle.area"),
            table.find("@lugha_fn_Circle.grow"),
        );
        assert!(
            area.is_some() && area < grow,
            "methods in declaration order: {table}"
        );
        assert!(
            table.contains("@lugha_type_Circle") && table.contains("@lugha_copy_6Circle_box"),
            "{table}"
        );
    }

    #[test]
    fn conversion_boxes_through_the_gc() {
        let ir = ir(&format!(
            "{SHAPES}fun main() {{ let a: Shape = Circle {{ r: 1.0 }}; }}"
        ));
        let main = body(&ir, "main");
        assert!(main.contains("call ptr @lugha_rt_alloc(i64 8)"), "{ir}");
        assert!(!ir.contains("malloc"), "{ir}");
    }

    #[test]
    fn programs_without_interface_values_have_no_tables() {
        let ir = ir(&format!(
            "{SHAPES}fun main() {{ let c = Circle {{ r: 1.0 }}; c.area(); }}"
        ));
        for symbol in ["lugha_vt_", "lugha_type_", "_box"] {
            assert!(!ir.contains(symbol), "{symbol}: {ir}");
        }
    }
}
