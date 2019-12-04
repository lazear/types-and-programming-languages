use crate::types::{TyKind, Type};
use util::span::Span;

/// Constant expression or pattern
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum Constant {
    Unit,
    Bool(bool),
    Nat(u32),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Kind {
    /// Constant
    Const(Constant),
    /// Variable
    Var(usize),
    /// Term abstraction
    /// \x: Ty. x
    /// Introduce a lambda term
    Abs(Box<Type>, Box<Term>),
    /// Term application
    /// m n
    /// Eliminate a lambda term
    App(Box<Term>, Box<Term>),
    /// Type abstraction
    /// \X. \x: X. x
    /// Introduce a universally quantified type
    TyAbs(Box<TyKind>, Box<Term>),
    /// Type application
    /// id [Nat] 1
    /// Eliminate a universally quantified type
    TyApp(Box<Term>, Box<Type>),
    /// Record term
    /// {label1 = Tm1, label2 = Tm2, etc}
    /// Invariant that all fields have unique labels
    Record(Record),

    Index(Box<Term>, String),

    /// Introduce an existential type
    /// { *Ty1, Term } as {∃X.Ty}
    /// essentially, concrete representation as interface
    Pack(Box<Type>, Box<Term>, Box<Type>),
    /// Unpack an existential type
    /// open {∃X, bind} in body -- X is bound as a TyVar, and bind as Var(0)
    /// Eliminate an existential type
    Unpack(Box<Term>, Box<Term>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Field {
    pub span: Span,
    pub label: String,
    pub expr: Box<Term>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Record {
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Term {
    pub span: Span,
    pub kind: Kind,
}

impl Term {
    pub fn new(kind: Kind, span: Span) -> Term {
        Term { kind, span }
    }
}

impl Record {
    pub fn get(&self, label: &str) -> Option<&Field> {
        for field in &self.fields {
            if field.label == label {
                return Some(field);
            }
        }
        None
    }
}

use std::fmt;

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            Kind::Var(idx) => write!(f, "#{}", idx),
            Kind::Const(Constant::Bool(b)) => write!(f, "{}", b),
            Kind::Const(Constant::Nat(b)) => write!(f, "{}", b),
            Kind::Const(Constant::Unit) => write!(f, "()"),
            Kind::Abs(ty, body) => write!(f, "(λx:{}. {})", ty, body),
            Kind::App(m, n) => write!(f, "{} {}", m, n),
            Kind::TyAbs(kind, body) => write!(f, "ΛX::{}. {}", kind, body),
            Kind::TyApp(body, ty) => write!(f, "{} [{}]", body, ty),
            Kind::Pack(witness, body, sig) => write!(f, "{{*{}, {}}} as {}", witness, body, sig),
            Kind::Unpack(m, n) => write!(f, "unpack {} as {}", m, n),
            Kind::Record(rec) => write!(
                f,
                "{{\n{}\n}}",
                rec.fields
                    .iter()
                    .map(|fi| format!("\t{}: {}", fi.label, fi.expr))
                    .collect::<Vec<_>>()
                    .join(",\n")
            ),
            Kind::Index(t1, t2) => write!(f, "{}.{}", t1, t2),
        }
    }
}
