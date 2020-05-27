use super::ast::*;
use super::visit::{DeclVisitor, ExprVisitor, PatVisitor, TypeVisitor};
use crate::diagnostics::Diagnostic;
use crate::stack::Stack;
use std::collections::HashSet;
use std::iter::IntoIterator;
use util::span::Span;

/// Collect type variables and references to defined names
#[derive(Default, Debug, Clone)]
pub struct TyNameCollector<'s> {
    pub tyvars: HashSet<&'s str>,
    pub definitions: HashSet<&'s str>,
}

impl<'s> TypeVisitor<'s> for TyNameCollector<'s> {
    fn visit_variable(&mut self, s: &'s str) {
        self.tyvars.insert(s);
    }

    fn visit_defined(&mut self, s: &'s str) {
        self.definitions.insert(s);
    }
}

#[derive(Default)]
pub struct ProgramValidation<'s> {
    tyvars: Stack<&'s str>,
    tmvars: Stack<&'s str>,

    defined_values: HashSet<&'s str>,
    defined_types: HashSet<&'s str>,

    last_ty_span: Span,
    last_ex_span: Span,
    diags: Vec<Diagnostic>,
    warns: HashSet<WarnOn>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum WarnOn {
    ShadowingGlobal,
    ImplicitTyVar,
}

impl<'s> ProgramValidation<'s> {
    fn raii_tyvars<F: Fn(&mut ProgramValidation<'s>)>(&mut self, f: F) {
        let n = self.tyvars.len();
        f(self);
        let to_pop = self.tyvars.len() - n;
        self.tyvars.popn(to_pop);
    }

    fn raii_tmvars<F: Fn(&mut ProgramValidation<'s>)>(&mut self, f: F) {
        let n = self.tmvars.len();
        f(self);
        let to_pop = self.tmvars.len() - n;
        self.tmvars.popn(to_pop);
    }

    pub fn warn_on(mut self, w: WarnOn) -> Self {
        self.warns.insert(w);
        self
    }

    pub fn validate(prog: &Program) -> Vec<Diagnostic> {
        let mut s = Self::default();
        s.visit_program(prog);
        s.diags
    }
}

impl<'s> PatVisitor<'s> for ProgramValidation<'s> {
    fn visit_constructor(&mut self, s: &'s str) {
        if !self.defined_values.contains(&s) {
            let d = Diagnostic::error(self.last_ty_span, format!("Unbound constructor: {}", s));
            self.diags.push(d);
        }
    }

    fn visit_variable(&mut self, s: &'s str) {
        self.tmvars.push(s);
    }
}

impl<'s> TypeVisitor<'s> for ProgramValidation<'s> {
    fn visit_defined(&mut self, s: &'s str) {
        match self.tyvars.lookup(&s) {
            Some(ix) => {}
            None => {
                if !self.defined_types.contains(&s) {
                    let d = Diagnostic::error(
                        self.last_ty_span,
                        format!("Unbound type definition: {}", s),
                    );
                    self.diags.push(d);
                }
            }
        }
    }

    fn visit_existential(&mut self, s: &'s str, k: &'s Kind, ty: &'s Type) {
        self.raii_tyvars(|f| {
            f.tyvars.push(s);
            f.visit_ty(ty)
        });
    }

    fn visit_universal(&mut self, s: &'s str, k: &'s Kind, ty: &'s Type) {
        self.raii_tyvars(|f| {
            f.tyvars.push(s);
            f.visit_ty(ty)
        });
    }

    fn visit_abstraction(&mut self, s: &'s str, k: &'s Kind, ty: &'s Type) {
        self.raii_tyvars(|f| {
            f.tyvars.push(s);
            f.visit_ty(ty)
        });
    }

    fn visit_variable(&mut self, s: &'s str) {
        match self.tyvars.lookup(&s) {
            Some(ix) => {}
            None => {
                let mut d =
                    Diagnostic::error(self.last_ty_span, format!("Unbound type variable: {}", s));

                if self.tyvars.len() != 0 {
                    d = d.message(
                        self.last_ty_span,
                        format!(
                            "last bound type variable was {}?",
                            self.tyvars.get(0).unwrap()
                        ),
                    );
                }

                self.diags.push(d);
            }
        }
    }

    fn visit_sum(&mut self, var: &'s [Variant]) {
        for v in var {
            if !self.defined_values.insert(&v.label) {
                self.diags.push(Diagnostic::error(
                    self.last_ty_span,
                    format!("{} is already bound as a constructor!", &v.label),
                ));
            }
            if let Some(ty) = &v.ty {
                self.visit_ty(ty);
            }
        }
    }

    fn visit_ty(&mut self, ty: &'s Type) {
        self.last_ty_span = ty.span;
        self.walk_ty(ty);
    }
}

impl<'s> ExprVisitor<'s> for ProgramValidation<'s> {
    fn visit_var(&mut self, s: &'s str) {
        match self.tmvars.lookup(&s) {
            Some(ix) => {}
            None => {
                if !self.defined_values.contains(&s) {
                    let d = Diagnostic::error(
                        self.last_ty_span,
                        format!("Unbound expr variable: {}", s),
                    );
                    self.diags.push(d);
                }
            }
        }
    }

    fn visit_constr(&mut self, s: &'s str) {
        if !self.defined_values.contains(&s) {
            let d = Diagnostic::error(self.last_ty_span, format!("Unbound constructor: {}", s));
            self.diags.push(d);
        }
    }

    fn visit_abs(&mut self, pat: &'s Pattern, body: &'s Expr) {
        self.raii_tmvars(|f| {
            f.visit_pattern(pat);
            f.visit_expr(body);
        });
    }

    /// There is actually no production for TyAbs in the parser at the moment,
    /// but these are obviously represented in the source language, e.g.
    /// /\A \x: A. x
    /// fun 'a id (x: 'a) = x
    fn visit_tyabs(&mut self, s: &'s str, _: &'s Kind, body: &'s Expr) {
        self.raii_tyvars(|f| {
            f.tyvars.push(s);
            f.visit_expr(body);
        });
    }

    fn visit_let(&mut self, decls: &'s [Decl], body: &'s Expr) {}

    fn visit_case(&mut self, e: &'s Expr, arms: &'s [Arm]) {
        for arm in arms {
            self.last_ex_span = arm.span;
            self.raii_tmvars(|q| {
                q.visit_pattern(&arm.pat);
                q.visit_expr(&arm.expr);
            });
        }
    }

    fn visit_expr(&mut self, e: &'s Expr) {
        self.last_ex_span = e.span;
        self.walk_expr(e);
    }
}

impl<'s> DeclVisitor<'s> for ProgramValidation<'s> {
    fn visit_datatype(&mut self, tyvars: &'s [Type], name: &'s str, ty: &'s Type) {
        self.defined_types.insert(name);
        self.raii_tyvars(|f| {
            f.tyvars.extend(tyvars.iter().map(|t| t.kind.as_tyvar()));
            f.visit_ty(ty);
        });
    }

    fn visit_type(&mut self, tyvars: &'s [Type], name: &'s str, ty: &'s Type) {
        self.defined_types.insert(name);
        self.raii_tyvars(|f| {
            f.tyvars.extend(tyvars.iter().map(|t| t.kind.as_tyvar()));
            f.visit_ty(ty);
        });
    }

    fn visit_value(&mut self, tyvars: &'s [Type], pat: &'s Pattern, expr: &'s Expr) {
        self.raii_tyvars(|f| {
            f.tyvars.extend(tyvars.iter().map(|t| t.kind.as_tyvar()));

            let n = f.tmvars.len();
            f.visit_pattern(pat);
            let mut v = Vec::new();
            while f.tmvars.len() > n {
                v.push(f.tmvars.pop().unwrap());
            }
            f.defined_values.extend(v.into_iter().rev());
            f.visit_expr(&expr);
        });
    }

    fn visit_function(&mut self, tyvars: &'s [Type], name: &'s str, arms: &'s [FnArm]) {
        self.defined_values.insert(name);
        self.raii_tyvars(|f| {
            f.tyvars.extend(tyvars.iter().map(|t| t.kind.as_tyvar()));
            for arm in arms {
                f.raii_tmvars(|q| {
                    q.visit_product_pat(&arm.pats);
                    q.visit_expr(&arm.expr);
                });
            }
        });
    }

    fn visit_and_decl(&mut self, d1: &'s Decl, d2: &'s Decl) {
        // Roundabout way of collecting any bound names for mutual
        // definitions
        let mut names = DeclNames::default();
        names.visit_decl(d1);
        names.visit_decl(d2);
        self.raii_tmvars(|f| {
            f.raii_tyvars(|q| {
                q.tyvars.extend(names.types.iter().rev().copied());
                q.tmvars.extend(names.values.iter().rev().copied());

                q.visit_decl(d1);
                q.visit_decl(d2);
            })
        })
    }

    fn visit_toplevel_expr(&mut self, expr: &'s Expr) {
        self.visit_expr(expr);
    }
}

#[derive(Default)]
struct DeclNames<'s> {
    values: Vec<&'s str>,
    types: Vec<&'s str>,
}

impl<'s> DeclVisitor<'s> for DeclNames<'s> {
    fn visit_datatype(&mut self, tyvars: &'s [Type], name: &'s str, ty: &'s Type) {
        self.types.push(name);
    }

    fn visit_type(&mut self, tyvars: &'s [Type], name: &'s str, ty: &'s Type) {
        self.types.push(name);
    }

    fn visit_value(&mut self, tyvars: &'s [Type], pat: &'s Pattern, expr: &'s Expr) {
        self.visit_pattern(pat);
    }

    fn visit_function(&mut self, tyvars: &'s [Type], name: &'s str, arms: &'s [FnArm]) {
        self.values.push(name);
    }

    fn visit_and_decl(&mut self, d1: &'s Decl, d2: &'s Decl) {
        self.visit_decl(d1);
        self.visit_decl(d2);
    }

    fn visit_toplevel_expr(&mut self, expr: &'s Expr) {}
}

impl<'s> PatVisitor<'s> for DeclNames<'s> {
    fn visit_variable(&mut self, s: &'s str) {
        self.values.push(s);
    }
}
