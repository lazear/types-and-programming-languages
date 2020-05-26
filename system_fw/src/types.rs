use std::convert::TryFrom;
use std::fmt;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Type {
    Unit,
    Nat,
    Bool,
    Var(usize),
    Record(Vec<TyField>),
    Product(Vec<Type>),
    Projection(Box<Type>, usize),
    Arrow(Box<Type>, Box<Type>),
    Universal(Box<TyKind>, Box<Type>),
    Existential(Box<TyKind>, Box<Type>),
    Abs(Box<TyKind>, Box<Type>),
    App(Box<Type>, Box<Type>),
    Recursive(Box<Type>),
    Sum(Vec<TyField>),
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
    Product(Vec<TyKind>),
}

impl Type {
    pub fn subst(&mut self, mut s: Type) {
        Shift::new(1).visit(&mut s);
        Subst::new(s).visit(self);
        Shift::new(-1).visit(self);
    }

    /// Support function for quickly accessing labelled subtypes
    pub fn label(&self, label: &str) -> Option<&Type> {
        match self {
            Type::Sum(fields) | Type::Record(fields) => {
                for f in fields {
                    if f.label == label {
                        return Some(&f.ty);
                    }
                }
                None
            }
            _ => None,
        }
    }
}

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
                "{{\n{}\n}}",
                fields
                    .iter()
                    .map(|fi| format!("\t{}: {}", fi.label, fi.ty))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
            Type::Product(tys) => write!(
                f,
                "({})",
                tys.iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            Type::Projection(ty, idx) => write!(f, "{}.{}", ty, idx),
            Type::Sum(fields) => write!(
                f,
                "{}",
                fields
                    .iter()
                    .map(|fi| format!("{} {}", fi.label, fi.ty))
                    .collect::<Vec<_>>()
                    .join("|")
            ),
            Type::Recursive(inner) => write!(f, "rec {}", inner),
        }
    }
}

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            TyKind::Star => write!(f, "*"),
            TyKind::Arrow(k1, k2) => match k2.as_ref() {
                TyKind::Star => match k1.as_ref() {
                    TyKind::Star => write!(f, "{}->{}", k1, k2),
                    TyKind::Arrow(k11, k12) => write!(f, "({}->{})->{}", k11, k12, k2),
                    _ => write!(f, "{}->{}", k1, k2),
                },
                TyKind::Arrow(_, _) => write!(f, "{}->({})", k1, k2),
                k => write!(f, "{}->{}", k1, k),
            },
            TyKind::Product(v) => {
                let s = v
                    .iter()
                    .map(|k| format!("{}", k))
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "({})", s)
            }
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

    fn visit_product(&mut self, tys: &mut [Type]) {
        for ty in tys {
            self.visit(ty);
        }
    }

    fn visit_projection(&mut self, ty: &mut Type, index: usize) {
        self.visit(ty);
    }

    fn visit_recursive(&mut self, ty: &mut Type) {
        self.visit(ty);
    }

    fn visit(&mut self, ty: &mut Type) {
        self.walk(ty);
    }

    fn walk(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat => {}
            Type::Var(v) => self.visit_var(v),
            Type::Record(fields) => self.visit_record(fields),
            Type::Product(tys) => self.visit_product(tys),
            Type::Projection(ty, idx) => self.visit(ty),
            Type::Sum(variants) => self.visit_record(variants),
            Type::Recursive(ty1) => self.visit_recursive(ty1),
            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(k, ty) => self.visit_universal(k, ty),
            Type::Existential(k, ty) => self.visit_existential(k, ty),
            Type::Abs(s, t) => self.visit_abs(s, t),
            Type::App(k, t) => self.visit_app(k, t),
        }
    }
}

pub struct Shift {
    pub cutoff: usize,
    pub shift: isize,
}

impl Shift {
    pub const fn new(shift: isize) -> Shift {
        Shift { cutoff: 0, shift }
    }
}

impl MutTypeVisitor for Shift {
    fn visit_var(&mut self, var: &mut usize) {
        if *var >= self.cutoff {
            *var = usize::try_from(*var as isize + self.shift)
                .expect("Type variable has been shifted below 0! Fatal bug");
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

    // fn visit_recursive(&mut self, ty: &mut Type) {
    //     self.cutoff += 1;
    //     self.visit(ty);
    //     self.cutoff -= 1;
    // }
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

    // fn visit_recursive(&mut self, ty: &mut Type) {
    //     self.cutoff += 1;
    //     self.visit(ty);
    //     self.cutoff -= 1;
    // }

    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Var(v) if *v == self.cutoff => {
                Shift::new(self.cutoff as isize).visit(&mut self.ty);
                *ty = self.ty.clone();
            }
            _ => self.walk(ty),
        }
    }
}
