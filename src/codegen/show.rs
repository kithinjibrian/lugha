//! Printing values in codegen (v12 draft §5, §9; milestone 39): the text
//! `print`, `to_string` and `"{x}"` give any value. Numbers, `bool` and
//! strings print as before; every other type goes through one
//! `lugha_show_<type>` helper, which calls the type's own `show` or builds
//! the built-in form (`Point { x: 1 }`, `Some(2)`, `[1, 2]`), quoting the
//! strings inside it.

use inkwell::module::Linkage;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, ValueKind};

use super::copy::mangle;
use super::enums::variants;
use super::heap::element_of;
use super::instances::instance_name;
use super::lower::{Lowerer, POSITIONED};
use super::methods::Method;
use super::value::llvm_type;
use crate::check::Type;

/// True for the types that print without a helper.
fn plain(ty: &Type) -> bool {
    ty.is_numeric() || matches!(ty, Type::Bool | Type::String | Type::Char)
}

/// A type's name as printed: its key's last part, without arguments.
fn bare(ty: &Type) -> &str {
    match ty {
        Type::Struct(key, _) | Type::Enum(key, _) | Type::Handle(key) => {
            key.rsplit('.').next().unwrap_or(key)
        }
        _ => unreachable!("only structs, enums and handles have a printed name"),
    }
}

impl<'ctx> Lowerer<'ctx> {
    /// `(ty, value)` ready for a print or conversion intrinsic: unchanged
    /// for numbers, `bool` and strings, otherwise its text as a `string`.
    pub(super) fn printable(
        &mut self,
        ty: Type,
        value: BasicValueEnum<'ctx>,
    ) -> (Type, BasicValueEnum<'ctx>) {
        if plain(&ty) || matches!(ty, Type::Never | Type::Error) {
            return (ty, value);
        }
        let text = self.text(value, &ty);
        (Type::String, text)
    }

    /// The text `print` writes for `value`: a string is itself.
    pub(super) fn text(&mut self, value: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        match ty {
            Type::String => value,
            _ if plain(ty) => self.text_of(ty, value),
            _ => self.part(value, ty),
        }
    }

    /// The text of `value` inside a built-in form: strings quoted.
    pub(super) fn part(&mut self, value: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        match ty {
            Type::String => {
                self.call_runtime_value("lugha_rt_str_quote", &[Type::String], &[value])
            }
            Type::Fun(..) => self.string_literal("<fun>").into(),
            // `'a'` inside a built-in form (v14).
            Type::Char => self.call_runtime_value("lugha_rt_char_quote", &[Type::Char], &[value]),
            _ if plain(ty) => self.text_of(ty, value),
            _ => {
                let helper = self.show_function(ty);
                let call = self
                    .builder
                    .build_call(helper, &[value.into()], "text")
                    .expect(POSITIONED);
                match call.try_as_basic_value() {
                    ValueKind::Basic(text) => text,
                    ValueKind::Instruction(_) => unreachable!("show helpers return a string"),
                }
            }
        }
    }

    /// `a + b` for two strings.
    pub(super) fn join(&mut self, a: BasicValueEnum<'ctx>, b: &str) -> BasicValueEnum<'ctx> {
        let b = self.string_literal(b).into();
        self.concat(a, b)
    }

    pub(super) fn concat(
        &mut self,
        a: BasicValueEnum<'ctx>,
        b: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        self.call_runtime_value(
            "lugha_rt_str_concat",
            &[Type::String, Type::String],
            &[a, b],
        )
    }

    /// `lugha_show_<mangled>(T) -> string`, generated once per type. Declared
    /// before its body, so a recursive enum's helper can call itself; an
    /// interface's body comes at the end (`show_iface.rs`).
    fn show_function(&mut self, ty: &Type) -> FunctionValue<'ctx> {
        let name = format!("lugha_show_{}", mangle(ty));
        if let Some(function) = self.module.get_function(&name) {
            return function;
        }
        let ptr = self.context.ptr_type(Default::default());
        let llvm = llvm_type(self.context, ty);
        let function = self.module.add_function(
            &name,
            ptr.fn_type(&[llvm.into()], false),
            Some(Linkage::Internal),
        );
        if let Type::Interface(..) = ty {
            self.shows.push((ty.clone(), function));
            return function;
        }
        let (saved_block, saved_function) = (self.builder.get_insert_block(), self.function);
        self.function = Some(function);
        self.builder
            .position_at_end(self.context.append_basic_block(function, "entry"));
        let value = function.get_nth_param(0).expect("one parameter");
        let text = match (self.own_show(ty), ty) {
            (Some(show), _) => self.call_show(&show, value, ty),
            (None, Type::Struct(..)) => self.show_fields(value, ty),
            (None, Type::Array(_)) => self.show_elements(value.into_pointer_value(), ty),
            (None, Type::Ref(inner)) => self.show_box(value.into_pointer_value(), inner),
            // A handle shows its type, never its address (v16).
            (None, Type::Handle(_)) => self.string_literal(&format!("<{}>", bare(ty))).into(),
            // Tasks and channels show what they are (v17).
            (None, Type::Task(_)) => self.string_literal("<task>").into(),
            (None, Type::Chan(_)) => self.string_literal("<chan>").into(),
            (None, Type::Actor(key)) => {
                let name = key.rsplit('.').next().unwrap_or(key);
                self.string_literal(&format!("<actor {name}>")).into()
            }
            (None, _) => self.show_variants(value, ty),
        };
        self.builder.build_return(Some(&text)).expect(POSITIONED);
        self.function = saved_function;
        if let Some(block) = saved_block {
            self.builder.position_at_end(block);
        }
        function
    }

    /// The type's own `fun show(self): string`: its method, a prelude
    /// method listed as an instance, or a generic type's instance (which
    /// the checker lists only when it has that shape).
    fn own_show(&self, ty: &Type) -> Option<Method<'ctx>> {
        let (owner, args) = match ty {
            Type::Struct(owner, args) | Type::Enum(owner, args) => (owner, args.as_slice()),
            Type::Handle(owner) => (owner, &[][..]),
            _ => return None,
        };
        let key = format!("{owner}.show");
        if !args.is_empty() {
            return self.monos.get(&instance_name(&key, args)).cloned();
        }
        let m = self.methods.get(&key).or_else(|| self.monos.get(&key))?;
        let fits = m.receiver == Some(false)
            && m.signature.params.is_empty()
            && m.signature.ret == Some(Type::String);
        fits.then(|| m.clone())
    }

    /// `show(value)`, the receiver passed through a stack slot.
    fn call_show(
        &mut self,
        show: &Method<'ctx>,
        value: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        let slot = self.entry_alloca(ty, "self");
        self.builder.build_store(slot, value).expect(POSITIONED);
        let call = self
            .builder
            .build_call(show.signature.function, &[slot.into()], "shown")
            .expect(POSITIONED);
        match call.try_as_basic_value() {
            ValueKind::Basic(text) => text,
            ValueKind::Instruction(_) => unreachable!("`show` returns a string"),
        }
    }

    /// `Name { f: v, … }`, or `Name {}`.
    fn show_fields(&mut self, value: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        let fields = self.structs[&ty.to_string()].clone();
        // Only C knows a union's live field (v17).
        if super::structs::is_union_fields(&fields) {
            return self.string_literal("<union>").into();
        }
        // A tuple prints as `(a, b)` (v14).
        if crate::check::tuple_elements(ty).is_some() {
            return self.show_tuple(value, &fields);
        }
        if fields.is_empty() {
            return self.string_literal(&format!("{} {{}}", bare(ty))).into();
        }
        let mut text: BasicValueEnum = self.string_literal(&format!("{} {{ ", bare(ty))).into();
        let record = value.into_struct_value();
        for (index, (field, fty)) in fields.iter().enumerate() {
            let at = u32::try_from(index).expect("field count fits in u32");
            let raw = self
                .builder
                .build_extract_value(record, at, field)
                .expect("field exists");
            // Fields hold `bool` as `i8`, like elements.
            let part = self.loaded_form(raw, fty);
            let label = if index == 0 {
                format!("{field}: ")
            } else {
                format!(", {field}: ")
            };
            text = self.join(text, &label);
            let part = self.part(part, fty);
            text = self.concat(text, part);
        }
        self.join(text, " }")
    }

    /// `[a, b]`: each element after the first preceded by `, `.
    fn show_elements(&mut self, array: PointerValue<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        let element = element_of(ty);
        let size = self.element_size(&element);
        let acc = self.entry_alloca(&Type::String, "text");
        let open = self.string_literal("[");
        self.builder.build_store(acc, open).expect(POSITIONED);
        let len = self.length(array);
        self.count_loop(len, |this, i| {
            let item = this.load_element(this.element_address(array, i, size), &element);
            let zero = this.context.i64_type().const_zero();
            let first = this
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, i, zero, "first")
                .expect(POSITIONED);
            let (none, comma) = (this.string_literal(""), this.string_literal(", "));
            let gap = this
                .builder
                .build_select(first, none, comma, "gap")
                .expect(POSITIONED);
            let ptr = this.context.ptr_type(Default::default());
            let so_far = this
                .builder
                .build_load(ptr, acc, "so.far")
                .expect(POSITIONED);
            let with_gap = this.concat(so_far, gap);
            let part = this.part(item, &element);
            let next = this.concat(with_gap, part);
            this.builder.build_store(acc, next).expect(POSITIONED);
            Ok(())
        })
        .expect("printing loops only lower text");
        let ptr = self.context.ptr_type(Default::default());
        let so_far = self.builder.build_load(ptr, acc, "text").expect(POSITIONED);
        self.join(so_far, "]")
    }

    /// `Variant(a, b)` or `Variant`, by a switch on the tag.
    fn show_variants(&mut self, value: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        let slot = self.entry_alloca(ty, "value");
        self.builder.build_store(slot, value).expect(POSITIONED);
        let acc = self.entry_alloca(&Type::String, "text");
        // Every tag has a case; the default keeps the store well defined.
        let empty = self.string_literal("");
        self.builder.build_store(acc, empty).expect(POSITIONED);
        let tag = self.tag(ty, slot);
        let done = self.append("shown");
        let i32_type = self.context.i32_type();
        let mut cases = Vec::new();
        for (index, (name, payloads)) in variants(ty, &self.enums).into_iter().enumerate() {
            let block = self.append("variant");
            let tag = u64::try_from(index).expect("variant count fits in u64");
            cases.push((i32_type.const_int(tag, false), block));
            let resume = self.current_block();
            self.builder.position_at_end(block);
            let mut text: BasicValueEnum = self.string_literal(&name).into();
            for (i, payload) in payloads.iter().enumerate() {
                let at = u32::try_from(i).expect("payload count fits in u32");
                let item = self.load_payload(ty, slot, &payloads, at, payload);
                text = self.join(text, if i == 0 { "(" } else { ", " });
                let part = self.part(item, payload);
                text = self.concat(text, part);
            }
            if !payloads.is_empty() {
                text = self.join(text, ")");
            }
            self.builder.build_store(acc, text).expect(POSITIONED);
            self.branch(done);
            self.builder.position_at_end(resume);
        }
        self.builder
            .build_switch(tag, done, &cases)
            .expect(POSITIONED);
        self.builder.position_at_end(done);
        let ptr = self.context.ptr_type(Default::default());
        self.builder.build_load(ptr, acc, "text").expect(POSITIONED)
    }
}
