//! The gate's walk over a whole program: every place a v2 construct can be
//! written, from item headers and types down to `match` arms.

use std::collections::HashSet;

use super::{GENERICS, Gated, MODULES, VISIBILITY};
use crate::ast::{
    Block, Expr, ExprKind, ForIter, FunDecl, Item, Stmt, StmtKind, Type, TypeKind, TypeParam,
    Visibility,
};
use crate::span::Span;

/// Collects every gated construct in a program.
pub(super) struct Walk<'p> {
    /// The program's own struct and enum names (see `first`).
    pub declared: HashSet<&'p str>,
    pub found: Vec<Gated>,
}

impl Walk<'_> {
    pub(super) fn hit(&mut self, (what, milestone): (&'static str, u8), span: Span) {
        self.found.push(Gated {
            what,
            milestone,
            span,
        });
    }

    fn visibility(&mut self, v: Option<Visibility>) {
        if let Some(v) = v {
            self.hit(VISIBILITY, v.span);
        }
    }

    fn type_params(&mut self, params: &[TypeParam]) {
        if let Some(param) = params.first() {
            self.hit(GENERICS, param.name.span);
        }
    }

    pub(super) fn item(&mut self, item: &Item) {
        match item {
            Item::Fun(f) => self.fun(f),
            Item::Extern(e) => {
                self.visibility(e.visibility);
                e.params.iter().for_each(|p| self.ty(&p.ty));
                e.ret.iter().for_each(|t| self.ty(t));
            }
            Item::Struct(s) => {
                self.visibility(s.visibility);
                self.type_params(&s.type_params);
                for field in &s.fields {
                    self.visibility(field.visibility);
                    self.ty(&field.ty);
                }
            }
            Item::Enum(e) => {
                self.visibility(e.visibility);
                self.type_params(&e.type_params);
                e.variants
                    .iter()
                    .flat_map(|v| &v.payloads)
                    .for_each(|t| self.ty(t));
            }
            Item::Impl(i) => {
                if let Some(module) = &i.module {
                    self.hit(MODULES, module.span);
                }
                if let Some(param) = i.params.first() {
                    self.hit(GENERICS, param.span);
                }
                i.methods.iter().for_each(|m| self.fun(m));
            }
        }
    }

    fn fun(&mut self, f: &FunDecl) {
        self.visibility(f.visibility);
        self.type_params(&f.type_params);
        f.params.iter().for_each(|p| self.ty(&p.ty));
        f.ret.iter().for_each(|t| self.ty(t));
        self.block(&f.body);
    }

    fn ty(&mut self, ty: &Type) {
        match &ty.kind {
            TypeKind::Array(inner) | TypeKind::Option(inner) => self.ty(inner),
            TypeKind::Applied(applied) => {
                if applied.module.is_some() {
                    self.hit(MODULES, ty.span);
                } else if self.declared.contains(applied.name.name.as_str()) {
                    self.hit(GENERICS, ty.span);
                }
                applied.args.iter().for_each(|t| self.ty(t));
            }
            _ => {}
        }
    }

    fn block(&mut self, b: &Block) {
        b.stmts.iter().for_each(|s| self.stmt(s));
        b.tail.iter().for_each(|e| self.expr(e));
    }

    fn stmt(&mut self, s: &Stmt) {
        match &s.kind {
            StmtKind::Let { ty, init, .. } => {
                ty.iter().for_each(|t| self.ty(t));
                self.expr(init);
            }
            StmtKind::Assign { place, value, .. } => {
                self.expr(place);
                self.expr(value);
            }
            StmtKind::Expr { expr, .. } => self.expr(expr),
            StmtKind::While { cond, body } => {
                self.expr(cond);
                self.block(body);
            }
            StmtKind::For { iter, body, .. } => {
                match iter {
                    ForIter::Range(a, b) => {
                        self.expr(a);
                        self.expr(b);
                    }
                    ForIter::Array(xs) => self.expr(xs),
                }
                self.block(body);
            }
            StmtKind::Return(value) => value.iter().for_each(|e| self.expr(e)),
            StmtKind::Break | StmtKind::Continue => {}
        }
    }

    fn expr(&mut self, e: &Expr) {
        match &e.kind {
            ExprKind::Int(_)
            | ExprKind::Float(_)
            | ExprKind::Str(_)
            | ExprKind::Bool(_)
            | ExprKind::Name(_) => {}
            ExprKind::Unary(_, inner) | ExprKind::Field(inner, _) => self.expr(inner),
            ExprKind::Cast(inner, ty) => {
                self.expr(inner);
                self.ty(ty);
            }
            ExprKind::Binary(_, _, a, b) | ExprKind::Index(a, _, b) | ExprKind::Repeat(a, b) => {
                self.expr(a);
                self.expr(b);
            }
            ExprKind::Call(callee, args) => {
                self.expr(callee);
                args.iter().for_each(|a| self.expr(a));
            }
            ExprKind::StructLit(lit) => {
                if lit.module.is_some() {
                    self.hit(MODULES, e.span);
                }
                lit.fields.iter().for_each(|(_, v)| self.expr(v));
            }
            ExprKind::Array(elements) => elements.iter().for_each(|x| self.expr(x)),
            ExprKind::If { cond, then, else_ } => {
                self.expr(cond);
                self.block(then);
                else_.iter().for_each(|x| self.expr(x));
            }
            ExprKind::Block(b) => self.block(b),
            ExprKind::Match(scrutinee, arms) => {
                self.expr(scrutinee);
                for arm in arms {
                    arm.guard.iter().for_each(|g| self.expr(g));
                    self.expr(&arm.body);
                }
            }
        }
    }
}
