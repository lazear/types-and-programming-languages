use crate::patterns::{Pattern, PatternCount};
use crate::terms::{Arm, Kind, Primitive, Term};
use crate::types::Type;
use crate::visit::{MutTermVisitor, MutTypeVisitor};
use util::span::Span;

pub struct Shift {
    cutoff: usize,
    shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl MutTermVisitor for Shift {
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize) {
        if *var >= self.cutoff {
            *var = (*var as isize + self.shift) as usize;
        }
    }

    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_let(&mut self, sp: &mut Span, pat: &mut Pattern, t1: &mut Term, t2: &mut Term) {
        self.visit(t1);
        let c = PatternCount::collect(pat);
        self.cutoff += c;
        self.visit(t2);
        self.cutoff -= c;
    }

    fn visit_case(&mut self, sp: &mut Span, term: &mut Term, arms: &mut Vec<Arm>) {
        self.visit(term);
        for arm in arms {
            let c = PatternCount::collect(&mut arm.pat);
            self.cutoff += c;
            self.visit(&mut arm.term);
            self.cutoff -= c;
        }
    }

    fn visit_unpack(&mut self, _: &mut Span, package: &mut Term, term: &mut Term) {
        self.visit(package);
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }
}

pub struct Subst {
    cutoff: usize,
    term: Term,
}

impl Subst {
    pub fn new(term: Term) -> Subst {
        Subst { cutoff: 0, term }
    }
}

impl MutTermVisitor for Subst {
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_let(&mut self, sp: &mut Span, pat: &mut Pattern, t1: &mut Term, t2: &mut Term) {
        self.visit(t1);
        let c = PatternCount::collect(pat);
        self.cutoff += c;
        self.visit(t2);
        self.cutoff -= c;
    }

    fn visit_case(&mut self, sp: &mut Span, term: &mut Term, arms: &mut Vec<Arm>) {
        self.visit(term);
        for arm in arms {
            let c = PatternCount::collect(&mut arm.pat);
            self.cutoff += c;
            self.visit(&mut arm.term);
            self.cutoff -= c;
        }
    }

    fn visit_unpack(&mut self, _: &mut Span, package: &mut Term, term: &mut Term) {
        self.visit(package);
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit(&mut self, term: &mut Term) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Var(v) if *v == self.cutoff => {
                Shift::new(self.cutoff as isize).visit(&mut self.term);
                *term = self.term.clone();
            }
            _ => self.walk(term),
        }
    }
}

pub struct TyTermSubst {
    cutoff: usize,
    ty: Type,
}

impl TyTermSubst {
    pub fn new(ty: Type) -> TyTermSubst {
        use crate::types::visit::*;
        let mut ty = ty;
        Shift::new(1).visit(&mut ty);
        TyTermSubst { cutoff: 0, ty }
    }

    fn visit_ty(&mut self, ty: &mut Type) {
        let mut s = crate::types::visit::Subst {
            cutoff: self.cutoff,
            ty: self.ty.clone(),
        };
        s.visit(ty);
    }
}

impl MutTermVisitor for TyTermSubst {
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        // self.cutoff += 1;
        self.visit_ty(ty);
        self.visit(term);
        // self.cutoff -= 1;
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term, ty: &mut Type) {
        self.visit_ty(ty);
        self.visit(term);
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut Term) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit_ty(ty);
        self.visit(term);
    }

    fn visit_unfold(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit_ty(ty);
        self.visit(term);
    }

    fn visit_unpack(&mut self, _: &mut Span, package: &mut Term, term: &mut Term) {
        self.visit(package);
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_pack(&mut self, _: &mut Span, wit: &mut Type, body: &mut Term, sig: &mut Type) {
        self.visit_ty(wit);
        self.visit(body);
        self.visit_ty(sig);
    }

    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut Term,
        ty: &mut Type,
    ) {
        self.visit_ty(ty);
        self.visit(term);
    }
}

/// Visitor for handling recursive variants automatically, by inserting a
/// fold term
///
/// Transform an [`Injection`] term of form: `Label tm of Rec(u.T)` into
/// `fold [u.T] Label tm of [X->u.T] T`
pub struct InjRewriter;

impl MutTermVisitor for InjRewriter {
    fn visit(&mut self, term: &mut Term) {
        match &mut term.kind {
            Kind::Injection(label, val, ty) => {
                match *ty.clone() {
                    Type::Rec(inner) => {
                        let ty_prime = crate::types::subst(*ty.clone(), *inner.clone());
                        let rewrite_ty = Term::new(
                            Kind::Injection(label.clone(), val.clone(), Box::new(ty_prime)),
                            term.span,
                        );

                        *term = Term::new(Kind::Fold(ty.clone(), Box::new(rewrite_ty)), term.span);
                    }
                    _ => {}
                }
                self.walk(term);
            }
            _ => self.walk(term),
        }
    }
}
