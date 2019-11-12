use super::*;

pub trait MutVisitor: Sized {
    fn visit_lit(&mut self, sp: &mut Span, lit: &mut Literal) {}
    fn visit_var(&mut self, sp: &mut Span, var: &mut usize) {}

    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit(term);
    }

    fn visit_app(&mut self, sp: &mut Span, t1: &mut Term, t2: &mut Term) {
        self.visit(t1);
        self.visit(t2);
    }

    fn visit_let(&mut self, sp: &mut Span, t1: &mut Term, t2: &mut Term) {
        self.visit(t1);
        self.visit(t2);
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut Term) {
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term, ty: &mut Type) {
        self.visit(term);
    }

    fn visit_primitive(&mut self, prim: &mut Primitive) {}
    fn visit_injection(&mut self, label: &mut String, term: &mut Term, ty: &mut Type) {
        self.visit(term);
    }

    fn visit_case(&mut self, term: &mut Term, arms: &mut Vec<Arm>) {
        self.visit(term);
        for arm in arms {
            self.visit(&mut arm.term);
        }
    }

    fn visit_product(&mut self, product: &mut Vec<Term>) {
        for t in product {
            self.visit(t);
        }
    }

    fn visit_projection(&mut self, term: &mut Term, index: &mut usize) {
        self.visit(term);
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit(term);
    }

    fn visit_unfold(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit(term);
    }

    fn visit(&mut self, term: &mut Term) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Lit(l) => self.visit_lit(sp, l),
            Kind::Var(v) => self.visit_var(sp, v),
            Kind::Abs(ty, term) => self.visit_abs(sp, ty, term),
            Kind::App(t1, t2) => self.visit_app(sp, t1, t2),
            // Do we need a separate branch?
            Kind::Fix(term) => self.visit(term),
            Kind::Primitive(p) => self.visit_primitive(p),
            Kind::Injection(label, tm, ty) => self.visit_injection(label, tm, ty),
            Kind::Projection(term, idx) => self.visit_projection(term, idx),
            Kind::Product(terms) => self.visit_product(terms),
            Kind::Case(term, arms) => self.visit_case(term, arms),
            Kind::Let(t1, t2) => self.visit_let(sp, t1, t2),
            Kind::TyAbs(term) => self.visit_tyabs(sp, term),
            Kind::TyApp(term, ty) => self.visit_tyapp(sp, term, ty),
            Kind::Fold(ty, term) => self.visit_fold(sp, ty, term),
            Kind::Unfold(ty, term) => self.visit_unfold(sp, ty, term),
        }
    }
}

pub struct Shift {
    cutoff: usize,
    shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl MutVisitor for Shift {
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

    fn visit_let(&mut self, sp: &mut Span, t1: &mut Term, t2: &mut Term) {
        self.visit(t1);
        self.cutoff += 1;
        self.visit(t2);
        self.cutoff -= 1;
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut Term) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_case(&mut self, term: &mut Term, arms: &mut Vec<Arm>) {
        self.visit(term);
        for arm in arms {
            match &arm.pat {
                Pattern::Any | Pattern::Literal(_) => self.visit(&mut arm.term),
                _ => {
                    self.cutoff += 1;
                    self.visit(&mut arm.term);
                    self.cutoff -= 1;
                }
            }
        }
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

impl MutVisitor for Subst {
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_let(&mut self, sp: &mut Span, t1: &mut Term, t2: &mut Term) {
        self.visit(t1);
        self.cutoff += 1;
        self.visit(t2);
        self.cutoff -= 1;
    }

    fn visit_tyabs(&mut self, sp: &mut Span, term: &mut Term) {
        self.cutoff += 1;
        self.visit(term);
        self.cutoff -= 1;
    }

    fn visit_case(&mut self, term: &mut Term, arms: &mut Vec<Arm>) {
        self.visit(term);
        for arm in arms {
            match &arm.pat {
                Pattern::Any | Pattern::Literal(_) => self.visit(&mut arm.term),
                _ => {
                    self.cutoff += 1;
                    self.visit(&mut arm.term);
                    self.cutoff -= 1;
                }
            }
        }
    }

    fn visit(&mut self, term: &mut Term) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Lit(l) => self.visit_lit(sp, l),
            Kind::Var(v) if *v == self.cutoff => *term = self.term.clone(),
            Kind::Var(_) => {}
            Kind::Abs(ty, term) => self.visit_abs(sp, ty, term),
            Kind::App(t1, t2) => self.visit_app(sp, t1, t2),
            Kind::Fix(term) => self.visit(term),
            Kind::Primitive(p) => self.visit_primitive(p),
            Kind::Injection(label, tm, ty) => self.visit_injection(label, tm, ty),
            Kind::Projection(term, idx) => self.visit_projection(term, idx),
            Kind::Product(terms) => self.visit_product(terms),
            Kind::Case(term, arms) => self.visit_case(term, arms),
            Kind::Let(t1, t2) => self.visit_let(sp, t1, t2),
            Kind::TyAbs(term) => self.visit_tyabs(sp, term),
            Kind::TyApp(term, ty) => self.visit_tyapp(sp, term, ty),
            Kind::Fold(ty, term) | Kind::Unfold(ty, term) => self.visit(term),
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
        use crate::types::visit::*;
        let mut s = Subst {
            cutoff: self.cutoff,
            ty: self.ty.clone(),
        };
        s.visit(ty);
    }
}

impl MutVisitor for TyTermSubst {
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit_ty(ty);
        self.visit(term);
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

    fn visit_injection(&mut self, label: &mut String, term: &mut Term, ty: &mut Type) {
        self.visit_ty(ty);
        self.visit(term);
    }
}
