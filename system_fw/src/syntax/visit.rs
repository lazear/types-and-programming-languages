use super::*;
use ast::{Kind, Row, Type, TypeKind, Variant};

pub trait TypeVisitor: Sized {
    fn visit_defined(&mut self, s: &str) {}

    fn visit_variable(&mut self, s: &str) {}

    fn visit_function(&mut self, ty1: &Type, ty2: &Type) {
        self.visit_ty(ty1);
        self.visit_ty(ty2);
    }

    fn visit_application(&mut self, ty1: &Type, ty2: &Type) {
        self.visit_ty(ty1);
        self.visit_ty(ty2);
    }

    fn visit_sum(&mut self, var: &[Variant]) {
        for v in var {
            if let Some(ty) = &v.ty {
                self.visit_ty(ty);
            }
        }
    }

    fn visit_product(&mut self, var: &[Type]) {
        for v in var {
            self.visit_ty(v);
        }
    }

    fn visit_record(&mut self, var: &[Row]) {
        for v in var {
            self.visit_ty(&v.ty);
        }
    }

    fn visit_existential(&mut self, k: &Kind, ty: &Type) {
        self.visit_ty(ty);
    }

    fn visit_universal(&mut self, k: &Kind, ty: &Type) {
        self.visit_ty(ty);
    }

    fn visit_abstraction(&mut self, k: &Kind, ty: &Type) {
        self.visit_ty(ty);
    }

    fn visit_recursive(&mut self, ty: &Type) {
        self.visit_ty(ty);
    }

    fn visit_ty(&mut self, ty: &Type) {
        use TypeKind::*;
        match &ty.kind {
            Int => {}
            Bool => {}
            Unit => {}
            Defined(s) => self.visit_defined(s),
            Variable(s) => self.visit_variable(s),
            Function(ty1, ty2) => self.visit_function(ty1, ty2),
            Sum(var) => self.visit_sum(var),
            Product(tys) => self.visit_product(tys),
            Record(rows) => self.visit_record(rows),
            Existential(k, ty) => self.visit_existential(k, ty),
            Universal(k, ty) => self.visit_universal(k, ty),
            Abstraction(k, ty) => self.visit_abstraction(k, ty),
            Application(ty1, ty2) => self.visit_application(ty1, ty2),
            Recursive(ty) => self.visit_recursive(ty),
        }
    }
}
