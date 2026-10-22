//! `include_bytes` and `include_str` (v17, milestone 128): the bytes the
//! checker read (`Checked::includes`) as private constant globals in the
//! heap layout, so nothing is copied when the program starts. A `string` is
//! laid out as a string literal; a `u8[]` has its shared bit set, so
//! copy-on-write clones it before any write and read-only memory is never
//! written (spec §7).

use inkwell::module::Linkage;
use inkwell::values::PointerValue;

use super::lower::Lowerer;
use crate::ast::Expr;

impl<'ctx> Lowerer<'ctx> {
    /// The value of the include call `call`: a pointer to its global.
    pub(super) fn included(&mut self, call: &Expr) -> PointerValue<'ctx> {
        let included = self.includes[&call.id].clone();
        if included.text {
            let text = std::str::from_utf8(&included.bytes).expect("checked: E0448 otherwise");
            return self.string_literal(text);
        }
        // One global per file, however many calls name it.
        let key = std::sync::Arc::as_ptr(&included.bytes) as *const u8 as usize;
        if let Some(&ptr) = self.include_globals.get(&key) {
            return ptr;
        }
        let i64_type = self.context.i64_type();
        let len = included.bytes.len() as u64;
        let header = [
            i64_type.const_int(len, false).into(),
            // The capacity, marked shared (cow.rs).
            i64_type.const_int(len | 1 << 63, false).into(),
            self.context.const_string(&included.bytes, false).into(),
        ];
        let value = self.context.const_struct(&header, false);
        let global = self.module.add_global(value.get_type(), None, "bytes");
        global.set_initializer(&value);
        global.set_constant(true);
        global.set_linkage(Linkage::Private);
        global.set_unnamed_addr(true);
        global.set_alignment(8);
        let ptr = global.as_pointer_value();
        self.include_globals.insert(key, ptr);
        ptr
    }
}
