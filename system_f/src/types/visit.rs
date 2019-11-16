use super::Type;
use crate::visit::MutTypeVisitor;

pub struct Shift {
    cutoff: usize,
    shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl MutTypeVisitor for Shift {
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

    fn visit_rec(&mut self, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }
}

pub struct Subst {
    pub cutoff: usize,
    pub ty: Type,
}

impl Subst {
    pub fn new(ty: Type) -> Subst {
        Subst { cutoff: 0, ty }
    }
}

impl MutTypeVisitor for Subst {
    fn visit_universal(&mut self, inner: &mut Type) {
        self.cutoff += 1;
        self.visit(inner);
        self.cutoff -= 1;
    }

    fn visit_rec(&mut self, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }

    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat => {}
            Type::Var(v) if *v >= self.cutoff => {
                *ty = self.ty.clone();
            }
            Type::Var(v) => self.visit_var(v),
            Type::Variant(v) => self.visit_variant(v),
            Type::Product(v) => self.visit_product(v),
            Type::Alias(v) => self.visit_alias(v),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
            Type::Rec(ty) => self.visit_rec(ty),
        }
    }
}
