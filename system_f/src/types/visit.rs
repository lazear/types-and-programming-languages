use super::Type;

pub trait MutVisitor: Sized {
    fn visit_var(&mut self, var: &mut usize) {}

    fn visit_arrow(&mut self, ty1: &mut Type, ty2: &mut Type) {
        self.visit(ty1);
        self.visit(ty2);
    }

    fn visit_universal(&mut self, inner: &mut Type) {
        self.visit(inner);
    }

    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Bool | Type::Nat => {}
            Type::Var(v) => self.visit_var(v),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
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
    fn visit_var(&mut self, var: &mut usize) {
        if *var >= self.cutoff {
            *var = (*var as isize + self.shift) as usize;
        }
    }

    fn visit_universal(&mut self, inner: &mut Type) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }
}

pub struct Subst {
    cutoff: usize,
    ty: Type,
}

impl Subst {
    pub fn new(ty: Type) -> Subst {
        Subst { cutoff: 0, ty }
    }
}

impl MutVisitor for Subst {
    fn visit_universal(&mut self, inner: &mut Type) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Bool | Type::Nat => {}
            Type::Var(v) if *v >= self.cutoff => {
                *ty = self.ty.clone();
            }
            Type::Var(v) => self.visit_var(v),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
        }
    }
}
