//! A file's imports and items: functions, externs, structs, enums, `impl`
//! blocks and interfaces, one field, variant or method per line.

use super::Printer;
use super::doc::{Doc, concat, spaced, text};
use super::leaves::visibility;
use crate::ast::{
    DeriveDecl, DeriveTarget, EnumDecl, Field, FunDecl, ImplDecl, Import, InterfaceDecl,
    InterfaceItem, Item, Program, StructDecl, Variant,
};
use crate::span::Span;

/// A top-level element.
enum Top<'p> {
    Import(&'p Import),
    Item(&'p Item),
    /// An actor's state and methods, which the parser gave as two items (v17).
    Actor(&'p StructDecl, &'p ImplDecl),
}

/// A member of an actor's body, in source order (v17).
enum Member<'p> {
    Field(&'p Field),
    Method(&'p FunDecl),
}

impl Printer<'_> {
    /// The whole file.
    pub(super) fn program(&mut self, program: &Program) -> Doc {
        let mut tops: Vec<Top> = program.imports.iter().map(Top::Import).collect();
        let mut items = program.items.iter().peekable();
        while let Some(item) = items.next() {
            match (item, items.peek()) {
                (Item::Struct(s), Some(Item::Impl(i))) if s.actor.is_some() && i.actor => {
                    tops.push(Top::Actor(s, i));
                    items.next();
                }
                _ => tops.push(Top::Item(item)),
            }
        }
        let whole = Span::new(0, self.src.len());
        let body = self.sequence(whole, &tops, |p, t| p.top_span(t), |_| "", |p, t| p.top(t));
        body.unwrap_or_else(|| text(""))
    }

    /// Where a top-level element starts (its visibility included) and ends.
    fn top_span(&self, top: &Top) -> Span {
        let (start, span) = match top {
            Top::Import(i) => (i.public.map(|p| p.start), i.span),
            Top::Item(Item::Fun(f)) => (
                f.test
                    .map(|t| t.start)
                    .or(f.visibility.map(|v| v.span.start)),
                f.span,
            ),
            Top::Item(Item::Extern(e)) => (e.visibility.map(|v| v.span.start), e.span),
            Top::Item(Item::ExternType(t)) => (t.visibility.map(|v| v.span.start), t.span),
            Top::Item(Item::Const(c)) => (c.visibility.map(|v| v.span.start), c.span),
            Top::Item(Item::Struct(s)) => (s.visibility.map(|v| v.span.start), s.span),
            Top::Item(Item::Enum(e)) => (e.visibility.map(|v| v.span.start), e.span),
            Top::Item(Item::Impl(i)) => (None, i.span),
            Top::Item(Item::Derive(d)) => (None, d.span),
            Top::Item(Item::Interface(i)) => (i.visibility.map(|v| v.span.start), i.span),
            Top::Actor(s, _) => (s.visibility.map(|v| v.span.start), s.span),
        };
        Span::new(start.unwrap_or(span.start).min(span.start), span.end)
    }

    fn top(&mut self, top: &Top) -> Doc {
        match top {
            Top::Import(i) => self.import(i),
            Top::Item(Item::Fun(f)) => self.fun(f),
            Top::Item(Item::ExternType(t)) => text(format!(
                "{}extern type {};",
                visibility(t.visibility),
                self.ident(&t.name)
            )),
            Top::Item(Item::Const(c)) => concat(vec![
                text(format!(
                    "{}const {}: {} = ",
                    visibility(c.visibility),
                    self.ident(&c.name),
                    self.ty(&c.ty)
                )),
                self.expr(&c.value),
                text(";"),
            ]),
            Top::Item(Item::Extern(e)) => concat(vec![
                text(format!(
                    "{}extern {}fun {}",
                    visibility(e.visibility),
                    // The symbol as written: re-escaping is the lexer's job, and a C name needs none.
                    e.symbol
                        .as_ref()
                        .map_or(String::new(), |s| format!("{:?} ", s.name)),
                    self.ident(&e.name)
                )),
                self.params(None, &e.params),
                text(format!("{};", self.ret(&e.ret))),
            ]),
            Top::Item(Item::Struct(s)) => {
                let head = format!(
                    "{}{}{} {}{} ",
                    visibility(s.visibility),
                    if s.c_layout.is_some() { "extern " } else { "" },
                    if s.union.is_some() { "union" } else { "struct" },
                    self.ident(&s.name),
                    self.type_params(&s.type_params)
                );
                let inner = self.inner(s.name.span.end, s.span);
                let body = self.sequence(
                    inner,
                    &s.fields,
                    |_, f| field_span(f),
                    |_| ",",
                    |p, f| {
                        text(format!(
                            "{}{}: {}",
                            visibility(f.visibility),
                            p.ident(&f.name),
                            p.ty(&f.ty)
                        ))
                    },
                );
                concat(vec![text(head), self.braced(body)])
            }
            Top::Item(Item::Enum(e)) => self.enumeration(e),
            Top::Item(Item::Impl(i)) => {
                let mut head = "impl ".to_string();
                if let Some(m) = &i.module {
                    head.push_str(&format!("{}.", self.ident(m)));
                }
                head.push_str(&format!(
                    "{}{} ",
                    self.ident(&i.ty),
                    self.type_params(&i.params)
                ));
                let inner = self.inner(i.ty.span.end, i.span);
                let body = self.sequence(
                    inner,
                    &i.methods,
                    |_, f| fun_span(f),
                    |_| "",
                    |p, f| p.fun(f),
                );
                concat(vec![text(head), self.braced(body)])
            }
            Top::Item(Item::Interface(i)) => self.interface(i),
            Top::Item(Item::Derive(d)) => self.derive(d),
            Top::Actor(s, i) => self.actor(s, i),
        }
    }

    /// `$derive<T: I> I for T[] { methods }` (v17, milestone 141).
    fn derive(&mut self, d: &DeriveDecl) -> Doc {
        let target = match &d.target {
            DeriveTarget::Type(t) => self.ty(t),
            DeriveTarget::Struct => "struct".to_string(),
            DeriveTarget::Enum => "enum".to_string(),
            DeriveTarget::Tuple => "tuple".to_string(),
        };
        let head = format!(
            "$derive{} {} for {target} ",
            self.type_params(&d.type_params),
            self.ident(&d.interface)
        );
        let inner = self.inner(d.interface.span.end, d.span);
        let body = self.sequence(
            inner,
            &d.methods,
            |_, f| fun_span(f),
            |_| "",
            |p, f| p.fun(f),
        );
        concat(vec![text(head), self.braced(body)])
    }

    /// `actor Name { fields and methods }` in source order (v17).
    fn actor(&mut self, s: &StructDecl, i: &ImplDecl) -> Doc {
        let head = format!(
            "{}actor {}{} ",
            visibility(s.visibility),
            self.ident(&s.name),
            self.type_params(&s.type_params)
        );
        let mut members: Vec<Member> = s.fields.iter().map(Member::Field).collect();
        members.extend(i.methods.iter().map(Member::Method));
        let span_of = |m: &Member| match m {
            Member::Field(f) => field_span(f),
            Member::Method(f) => fun_span(f),
        };
        members.sort_by_key(|m| span_of(m).start);
        let inner = self.inner(s.name.span.end, s.span);
        let body = self.sequence(
            inner,
            &members,
            |_, m| span_of(m),
            |m| match m {
                Member::Field(_) => ",",
                Member::Method(_) => "",
            },
            |p, m| match m {
                Member::Field(f) => text(format!(
                    "{}{}: {}",
                    visibility(f.visibility),
                    p.ident(&f.name),
                    p.ty(&f.ty)
                )),
                Member::Method(f) => p.fun(f),
            },
        );
        concat(vec![text(head), self.braced(body)])
    }

    /// The text between the `{` after `from` and the `}` ending `span`.
    fn inner(&self, from: usize, span: Span) -> Span {
        let end = span.end.saturating_sub(1);
        Span::new(self.after_brace(from, end).min(end), end)
    }

    fn import(&mut self, i: &Import) -> Doc {
        let path: Vec<String> = i.path.iter().map(|s| self.ident(s)).collect();
        let mut head = format!(
            "{}import {}",
            if i.public.is_some() { "public " } else { "" },
            path.join(".")
        );
        if let Some(alias) = &i.alias {
            head.push_str(&format!(" as {}", self.ident(alias)));
        }
        let mut parts = vec![text(head)];
        if i.glob.is_some() {
            // `{ * }` (v17): every public item, written as is.
            parts.push(text(" { * }"));
        } else if !i.items.is_empty() {
            let items: Vec<Doc> = i
                .items
                .iter()
                .map(|item| match &item.alias {
                    Some(a) => text(format!("{} as {}", self.ident(&item.name), self.ident(a))),
                    None => text(self.ident(&item.name)),
                })
                .collect();
            parts.push(text(" "));
            parts.push(spaced("{", items, "}"));
        }
        parts.push(text(";"));
        concat(parts)
    }

    /// A function or method: `fun f(…): T { … }` or `fun f(…): T = e;`.
    fn fun(&mut self, f: &FunDecl) -> Doc {
        let head = concat(vec![
            text(format!(
                "{}fun {}{}",
                visibility(f.visibility),
                self.ident(&f.name),
                self.type_params(&f.type_params)
            )),
            self.params(f.receiver, &f.params),
            text(self.ret(&f.ret)),
        ]);
        let body = self.body(&f.body);
        let expression = self.src[..f.body.span.start].trim_end().ends_with('=');
        let mut parts = vec![head, body];
        if expression {
            parts.push(text(";"));
        }
        // `@test` on its own line above (v16, milestone 84).
        if f.test.is_some() {
            parts.insert(0, Doc::Hard);
            parts.insert(0, text("@test"));
        }
        concat(parts)
    }

    fn enumeration(&mut self, e: &EnumDecl) -> Doc {
        let head = format!(
            "{}enum {}{} ",
            visibility(e.visibility),
            self.ident(&e.name),
            self.type_params(&e.type_params)
        );
        let inner = self.inner(e.name.span.end, e.span);
        let body = self.sequence(
            inner,
            &e.variants,
            |_, v| v.span,
            |_| ",",
            |p, v| p.variant(v),
        );
        concat(vec![text(head), self.braced(body)])
    }

    fn variant(&self, v: &Variant) -> Doc {
        if v.payloads.is_empty() {
            return text(self.ident(&v.name));
        }
        let types: Vec<String> = v.payloads.iter().map(|t| self.ty(t)).collect();
        text(format!("{}({})", self.ident(&v.name), types.join(", ")))
    }

    fn interface(&mut self, i: &InterfaceDecl) -> Doc {
        let head = format!(
            "{}interface {}{} ",
            visibility(i.visibility),
            self.ident(&i.name),
            self.type_params(&i.type_params)
        );
        let inner = self.inner(i.name.span.end, i.span);
        let body = self.sequence(inner, &i.items, |_, m| m.span, |_| "", |p, m| p.required(m));
        concat(vec![text(head), self.braced(body)])
    }

    /// An interface's method: a signature, or one with a default body.
    fn required(&mut self, m: &InterfaceItem) -> Doc {
        if let Some(default) = &m.default {
            return self.fun(default);
        }
        concat(vec![
            text(format!("fun {}", self.ident(&m.name))),
            self.params(m.receiver, &m.params),
            text(format!("{};", self.ret(&m.ret))),
        ])
    }
}

fn field_span(f: &Field) -> Span {
    let start = f.visibility.map_or(f.name.span.start, |v| v.span.start);
    Span::new(start, f.ty.span.end)
}

fn fun_span(f: &FunDecl) -> Span {
    let start = f.visibility.map_or(f.span.start, |v| v.span.start);
    Span::new(start.min(f.span.start), f.span.end)
}
