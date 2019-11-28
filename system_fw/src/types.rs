#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Type {
    Unit,
    Nat,
    Bool,
    Var(usize),
    Record(Vec<TyField>),
    Arrow(Box<Type>, Box<Type>),
    Universal(Box<TyKind>, Box<Type>),
    Existential(Box<TyKind>, Box<Type>),
    Abs(Box<TyKind>, Box<Type>),
    App(Box<Type>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TyField {
    pub label: String,
    pub ty: Box<Type>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TyKind {
    Star,
    Arrow(Box<TyKind>, Box<TyKind>),
}

impl Type {
    pub fn subst(&mut self, mut s: Type) {
        Shift::new(1).visit(&mut s);
        Subst::new(s).visit(self);
        Shift::new(-1).visit(self);
    }
}

use std::fmt;

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Type::Var(idx) => write!(f, "TyVar({})", idx),
            Type::Unit => write!(f, "Unit"),
            Type::Nat => write!(f, "Nat"),
            Type::Bool => write!(f, "Bool"),
            Type::Abs(kind, ty) => write!(f, "(ΛX::{}. {})", kind, ty),
            Type::App(m, n) => write!(f, "{} {}", m, n),
            Type::Arrow(m, n) => write!(f, "{}->{}", m, n),
            Type::Universal(k, ty) => write!(f, "∀X::{}. {}", k, ty),
            Type::Existential(k, ty) => write!(f, "{{∃X::{}, {}}}", k, ty),
            Type::Record(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|fi| format!("{}: {}", fi.label, fi.ty))
                    .collect::<Vec<_>>()
                    .join(",")
            ),
        }
    }
}

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            TyKind::Star => write!(f, "*"),
            TyKind::Arrow(k1, k2) => write!(f, "{}->{}", k1, k2),
        }
    }
}

pub trait MutTypeVisitor: Sized {
    fn visit_var(&mut self, var: &mut usize) {}

    fn visit_arrow(&mut self, ty1: &mut Type, ty2: &mut Type) {
        self.visit(ty1);
        self.visit(ty2);
    }

    fn visit_universal(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.visit(ty);
    }

    fn visit_existential(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.visit(ty);
    }

    fn visit_abs(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.visit(ty);
    }

    fn visit_app(&mut self, s: &mut Type, t: &mut Type) {
        self.visit(s);
        self.visit(t);
    }

    fn visit_record(&mut self, fields: &mut [TyField]) {
        for f in fields {
            self.visit(f.ty.as_mut());
        }
    }

    fn visit(&mut self, ty: &mut Type) {
        self.walk(ty);
    }

    fn walk(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat => {}
            Type::Var(v) => self.visit_var(v),
            Type::Record(fields) => self.visit_record(fields),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(k, ty) => self.visit_universal(k, ty),
            Type::Existential(k, ty) => self.visit_existential(k, ty),
            Type::Abs(s, t) => self.visit_abs(s, t),
            Type::App(k, t) => self.visit_app(k, t),
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

impl MutTypeVisitor for Shift {
    fn visit_var(&mut self, var: &mut usize) {
        if *var >= self.cutoff {
            *var = (*var as isize + self.shift) as usize;
        }
    }

    fn visit_universal(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }

    fn visit_existential(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }

    fn visit_abs(&mut self, kind: &mut TyKind, ty: &mut Type) {
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
    fn visit_universal(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }

    fn visit_existential(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }

    fn visit_abs(&mut self, kind: &mut TyKind, ty: &mut Type) {
        self.cutoff += 1;
        self.visit(ty);
        self.cutoff -= 1;
    }

    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Var(v) if *v >= self.cutoff => {
                *ty = self.ty.clone();
            }

            Type::Var(_) | Type::Unit | Type::Bool | Type::Nat => {}
            Type::Record(fields) => self.visit_record(fields),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(k, ty) => self.visit_universal(k, ty),
            Type::Existential(k, ty) => self.visit_existential(k, ty),
            Type::Abs(s, t) => self.visit_abs(s, t),
            Type::App(k, t) => self.visit_app(k, t),
        }
    }
}
