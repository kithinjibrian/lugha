//! What a name is, for semantic tokens (PRP-187, milestone 154): read from
//! a check's resolved names and definitions, never guessed.

use std::collections::{HashMap, HashSet};

use super::hover::Snapshot;
use super::tokens::{DECLARATION, DEFAULT_LIBRARY, Kind, MUTABLE};
use crate::ast::{FunDecl, Item, TypeParam};
use crate::check::{Name, Target};
use crate::span::Span;

/// One check's names, ready to classify spans of one of its files.
pub(super) struct Names<'a> {
    snapshot: &'a Snapshot,
    /// Where the file starts in the check's shared offset space.
    pub base: usize,
    /// Each declaration's kind, by its name's span.
    kinds: HashMap<Span, Kind>,
    /// The names of function parameters, by span.
    params: HashSet<Span>,
    /// Spans naming a module, some over several tokens (`window.actions`).
    modules: Vec<Span>,
}

impl<'a> Names<'a> {
    /// The names `snapshot` resolved, for classifying spans of the file
    /// starting at `base`.
    pub(super) fn new(snapshot: &'a Snapshot, base: usize) -> Names<'a> {
        let mut names = Names {
            snapshot,
            base,
            kinds: HashMap::new(),
            params: HashSet::new(),
            modules: Vec::new(),
        };
        for program in snapshot.modules.iter().filter_map(|m| m.program.as_ref()) {
            for item in &program.items {
                names.item(item);
            }
        }
        names.modules = (snapshot.partial.definitions.iter())
            .filter(|(_, t)| matches!(t, Target::Module(_)))
            .map(|(s, _)| *s)
            .collect();
        names
    }

    fn item(&mut self, item: &Item) {
        match item {
            Item::Fun(f) => self.fun(f, Kind::Function),
            Item::Extern(e) => {
                self.kinds.insert(e.name.span, Kind::Function);
                self.params.extend(e.params.iter().map(|p| p.name.span));
            }
            Item::ExternType(t) => {
                self.kinds.insert(t.name.span, Kind::Type);
            }
            Item::Struct(s) => {
                self.kinds.insert(s.name.span, Kind::Struct);
                self.type_params(&s.type_params);
                for field in &s.fields {
                    self.kinds.insert(field.name.span, Kind::Property);
                }
            }
            Item::Enum(e) => {
                self.kinds.insert(e.name.span, Kind::Enum);
                self.type_params(&e.type_params);
                for variant in &e.variants {
                    self.kinds.insert(variant.name.span, Kind::EnumMember);
                }
            }
            Item::Impl(i) => {
                self.type_params(&i.params);
                for m in &i.methods {
                    self.fun(m, Kind::Method);
                }
            }
            Item::Interface(i) => {
                self.kinds.insert(i.name.span, Kind::Interface);
                self.type_params(&i.type_params);
                for m in &i.items {
                    self.kinds.insert(m.name.span, Kind::Method);
                    self.params.extend(m.params.iter().map(|p| p.name.span));
                    if let Some(f) = &m.default {
                        self.fun(f, Kind::Method);
                    }
                }
            }
            Item::Const(c) => {
                self.kinds.insert(c.name.span, Kind::Variable);
            }
            Item::Derive(d) => {
                self.type_params(&d.type_params);
                for m in &d.methods {
                    self.fun(m, Kind::Method);
                }
            }
        }
    }

    fn fun(&mut self, f: &FunDecl, kind: Kind) {
        self.kinds.insert(f.name.span, kind);
        self.type_params(&f.type_params);
        self.params.extend(f.params.iter().map(|p| p.name.span));
    }

    fn type_params(&mut self, params: &[TypeParam]) {
        for p in params {
            self.kinds.insert(p.name.span, Kind::TypeParameter);
        }
    }

    /// The kind and modifiers of the name at `start..end` of the file;
    /// `None` when the check resolved nothing there.
    pub(super) fn classify(&self, start: usize, end: usize) -> Option<(Kind, u32)> {
        let span = Span {
            start: self.base + start,
            end: self.base + end,
        };
        let partial = &self.snapshot.partial;
        // Uses point at their declaration; a declaration at itself or nowhere.
        let mut decl = match partial.definitions.get(&span) {
            Some(Target::Decl(d)) => *d,
            Some(Target::Module(_)) => return Some((Kind::Namespace, 0)),
            Some(Target::File(_)) => return None,
            None => span,
        };
        let (kind, mods) = match (partial.names.get(&span), partial.names.get(&decl)) {
            // The path is a string: the lexer coloured it.
            (Some(Name::Included { .. }), _) => return None,
            (Some(Name::Function { decl: d, .. }), _) => {
                decl = *d;
                // An interface's method from std has no declaration here;
                // it is still a method.
                (self.kinds.get(d).copied().unwrap_or(Kind::Method), 0)
            }
            (Some(Name::Const { decl: d, .. }), _) => {
                decl = *d;
                (Kind::Variable, 0)
            }
            // Locals are named only at their binding.
            (_, Some(Name::Local { mutable, .. })) => {
                let kind = if self.params.contains(&decl) {
                    Kind::Parameter
                } else {
                    Kind::Variable
                };
                (kind, if *mutable { MUTABLE } else { 0 })
            }
            (_, Some(Name::Field { .. })) => (Kind::Property, 0),
            _ => match self.kinds.get(&decl) {
                Some(kind) => (*kind, 0),
                // A segment inside a longer module path (`window.actions`).
                None => {
                    let module = self
                        .modules
                        .iter()
                        .any(|m| m.start <= span.start && span.end <= m.end);
                    return module.then_some((Kind::Namespace, 0));
                }
            },
        };
        let mut mods = mods;
        if decl == span {
            mods |= DECLARATION;
        }
        if self.bundled(decl) {
            mods |= DEFAULT_LIBRARY;
        }
        Some((kind, mods))
    }

    /// Whether `decl` lies in std or the prelude (`<std>`, `<prelude>`).
    fn bundled(&self, decl: Span) -> bool {
        self.snapshot
            .source
            .file_at(decl.start)
            .name
            .starts_with('<')
    }
}
