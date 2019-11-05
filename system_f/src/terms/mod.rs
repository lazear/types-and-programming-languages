use crate::types::Type;
use std::fmt;
use util::span::Span;
pub mod visit;

#[derive(Clone, PartialEq, PartialOrd)]
pub struct Term {
    pub span: Span,
    pub kind: Kind,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Primitive {
    Succ,
    Pred,
    IsZero,
}

/// Abstract syntax of the parametric polymorphic lambda calculus
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Kind {
    /// A literal value
    Lit(Literal),
    /// A bound variable, represented by it's de Bruijn index
    Var(usize),
    /// Fixpoint operator/Y combinator
    Fix(Box<Term>),

    Primitive(Primitive),

    Constructor(String, Box<Term>, Box<Type>),

    Let(Box<Term>, Box<Term>),
    /// A lambda abstraction
    Abs(Box<Type>, Box<Term>),
    /// Application of a term to another term
    App(Box<Term>, Box<Term>),
    /// Type abstraction
    TyAbs(Box<Type>, Box<Term>),
    /// Type application
    TyApp(Box<Term>, Box<Type>),
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Unit,
    Bool(bool),
    Nat(u32),
}

impl Term {
    pub fn new(kind: Kind, span: Span) -> Term {
        Term { span, kind }
    }

    pub const fn unit() -> Term {
        Term {
            span: Span::dummy(),
            kind: Kind::Lit(Literal::Unit),
        }
    }

    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    #[inline]
    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            Kind::Lit(lit) => write!(f, "{:?}", lit),
            Kind::Var(v) => write!(f, "#{}", v),
            Kind::Abs(ty, term) => write!(f, "(λ_:{:?}. {:?})", ty, term),
            Kind::Fix(term) => write!(f, "Fix {:?}", term),
            Kind::Primitive(p) => write!(f, "{:?}", p),
            Kind::Constructor(label, tm, ty) => write!(f, "{} ({:?}) as {:?}", label, tm, ty),
            Kind::Let(t1, t2) => write!(f, "let _ = {:?} in {:?}", t1, t2),
            Kind::App(t1, t2) => write!(f, "({:?} {:?})", t1, t2),
            Kind::TyAbs(ty, term) => write!(f, "(λTy{:?} {:?})", ty, term),
            Kind::TyApp(term, ty) => write!(f, "({:?} [{:?}])", term, ty),
        }
    }
}

macro_rules! lit {
    ($x:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Lit(crate::terms::Literal::Bool($x)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! nat {
    ($x:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Lit(crate::terms::Literal::Nat($x)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! var {
    ($x:expr) => {
        crate::terms::Term::new(crate::terms::Kind::Var($x), util::span::Span::dummy())
    };
}

macro_rules! app {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::App(Box::new($t1), Box::new($t2)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! abs {
    ($ty:expr, $t:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::Abs(Box::new($ty), Box::new($t)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! tyapp {
    ($t1:expr, $t2:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::TyApp(Box::new($t1), Box::new($t2)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! tyabs {
    ($ty:expr, $t:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::TyAbs(Box::new($ty), Box::new($t)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! arrow {
    ($ty1:expr, $ty2:expr) => {
        crate::types::Type::Arrow(Box::new($ty1), Box::new($ty2))
    };
}
