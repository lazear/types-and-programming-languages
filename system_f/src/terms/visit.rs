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

    fn visit_tyabs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut Term) {
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut Term, ty: &mut Type) {
        self.visit(term);
    }

    fn visit(&mut self, term: &mut Term) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Lit(l) => self.visit_lit(sp, l),
            Kind::Var(v) => self.visit_var(sp, v),
            Kind::Abs(ty, term) => self.visit_abs(sp, ty, term),
            Kind::App(t1, t2) => self.visit_app(sp, t1, t2),
            Kind::TyAbs(ty, term) => self.visit_tyabs(sp, ty, term),
            Kind::TyApp(term, ty) => self.visit_tyapp(sp, term, ty),
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

    fn visit(&mut self, term: &mut Term) {
        let sp = &mut term.span;
        match &mut term.kind {
            Kind::Lit(l) => self.visit_lit(sp, l),
            Kind::Var(v) if *v >= self.cutoff => *term = self.term.clone(),
            Kind::Var(_) => {}
            Kind::Abs(ty, term) => self.visit_abs(sp, ty, term),
            Kind::App(t1, t2) => self.visit_app(sp, t1, t2),
            Kind::TyAbs(ty, term) => self.visit_tyabs(sp, ty, term),
            Kind::TyApp(term, ty) => self.visit_tyapp(sp, term, ty),
        }
    }
}
