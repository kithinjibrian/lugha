//! Completion of what a module re-exports by name (`public import m { X };`,
//! v11) or with a glob (`public import m { * };`, v17, milestone 100): after
//! `gtk.`, the items `gtk` passes on, as well as its own.

use lsp_types::CompletionItem;

use super::completion::Here;
use crate::ast::{Item, Program, VisibilityKind};
use crate::modules::Module;

impl Here<'_> {
    /// The items `module` re-exports, under the names it gives them; `seen`
    /// breaks glob cycles.
    pub(super) fn reexported_items(
        &self,
        module: &Module,
        seen: &mut Vec<Vec<String>>,
    ) -> Vec<CompletionItem> {
        let Some(program) = &module.program else {
            return Vec::new();
        };
        seen.push(module.path.clone());
        let mut out = Vec::new();
        for (n, import) in program.imports.iter().enumerate() {
            let Some(path) = module.imports.get(n).filter(|_| import.public.is_some()) else {
                continue;
            };
            let Some(target) = self.snapshot.modules.iter().find(|m| m.path == *path) else {
                continue;
            };
            let Some(target_program) = &target.program else {
                continue;
            };
            if import.glob.is_some() {
                if seen.contains(&target.path) {
                    continue;
                }
                out.extend(public_only(
                    self.program_items(target_program, false),
                    target_program,
                ));
                out.extend(self.reexported_items(target, seen));
            } else {
                let wanted = self.program_items(target_program, false);
                for listed in &import.items {
                    let found = wanted.iter().find(|i| i.label == listed.name.name);
                    if let Some(found) = found {
                        let mut item = found.clone();
                        item.label = listed.bound().name.clone();
                        out.push(item);
                    }
                }
            }
        }
        out
    }
}

/// `items` without the ones `program` doesn't declare `public`: a glob
/// passes on only `public` items.
fn public_only(items: Vec<CompletionItem>, program: &Program) -> Vec<CompletionItem> {
    let public: Vec<&str> = program
        .items
        .iter()
        .filter_map(|item| {
            let (name, visibility) = match item {
                Item::Fun(f) => (&f.name, &f.visibility),
                Item::Extern(e) => (&e.name, &e.visibility),
                Item::Struct(s) => (&s.name, &s.visibility),
                Item::Enum(e) => (&e.name, &e.visibility),
                Item::Interface(i) => (&i.name, &i.visibility),
                Item::ExternType(t) => (&t.name, &t.visibility),
                Item::Const(c) => (&c.name, &c.visibility),
                Item::Impl(_) => return None,
            };
            visibility
                .is_some_and(|v| v.kind == VisibilityKind::Public)
                .then_some(name.name.as_str())
        })
        .collect();
    items
        .into_iter()
        .filter(|i| public.contains(&i.label.as_str()))
        .collect()
}
