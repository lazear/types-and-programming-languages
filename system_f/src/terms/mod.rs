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

    /// Injection into a sum type
    /// fields: type constructor tag, term, and sum type
    Injection(String, Box<Term>, Box<Type>),

    /// Product type (tuple)
    Product(Vec<Term>),
    /// Projection into a term
    Projection(Box<Term>, usize),

    /// A case expr, with case arms
    Case(Box<Term>, Vec<Arm>),

    Let(Box<Term>, Box<Term>),
    /// A lambda abstraction
    Abs(Box<Type>, Box<Term>),
    /// Application of a term to another term
    App(Box<Term>, Box<Term>),
    /// Type abstraction
    TyAbs(Box<Term>),
    /// Type application
    TyApp(Box<Term>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum Pattern {
    // Wildcard
    Any,
    // Constant
    Literal(Literal),
    // Variable binding
    Variable(String),
    // Tuple of pattern bindings
    Product(Vec<Pattern>),
    Constructor(String, Box<Pattern>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Arm {
    pub span: Span,
    pub pat: Pattern,
    pub term: Box<Term>,
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum Literal {
    Unit,
    Bool(bool),
    Nat(u32),
}

impl Term {
    pub fn new(kind: Kind, span: Span) -> Term {
        Term { span, kind }
    }

    #[allow(dead_code)]
    pub const fn unit() -> Term {
        Term {
            span: Span::dummy(),
            kind: Kind::Lit(Literal::Unit),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    #[inline]
    pub fn kind(&self) -> &Kind {
        &self.kind
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nat(n) => write!(f, "{}", n),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "unit"),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            Kind::Lit(lit) => write!(f, "{}", lit),
            Kind::Var(v) => write!(f, "#{}", v),
            Kind::Abs(ty, term) => write!(f, "(λ_:{:?}. {})", ty, term),
            Kind::Fix(term) => write!(f, "Fix {:?}", term),
            Kind::Primitive(p) => write!(f, "{:?}", p),
            Kind::Injection(label, tm, ty) => write!(f, "{:?}::{}({})", ty, label, tm),
            Kind::Projection(term, idx) => write!(f, "{}.{}", term, idx),
            Kind::Product(terms) => write!(f, "{:?}", terms),
            Kind::Case(term, arms) => {
                writeln!(f, "case {} of", term)?;
                for arm in arms {
                    writeln!(f, "\t| {:?} => {},", arm.pat, arm.term)?;
                }
                write!(f, "")
            }
            // Kind::Case(term, arms) => write!(
            //     f,
            //     "case {:?} of {:?}",
            //     term,
            //     arms.iter()
            //         .map(|arm| format!("\n|\t{:?} => {:?}", arm.pat, arm.term))
            //         .collect::<Vec<String>>()
            //         .join("")
            // ),
            Kind::Let(t1, t2) => write!(f, "let _ = {} in {}", t1, t2),
            Kind::App(t1, t2) => write!(f, "({} {})", t1, t2),
            Kind::TyAbs(term) => write!(f, "(λTy {})", term),
            Kind::TyApp(term, ty) => write!(f, "({} [{:?}])", term, ty),
        }
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
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
    ( $t:expr) => {
        crate::terms::Term::new(
            crate::terms::Kind::TyAbs(Box::new($t)),
            util::span::Span::dummy(),
        )
    };
}

macro_rules! prim {
    ($t:expr) => {
        crate::terms::Term::new(crate::terms::Kind::Primitive($t), util::span::Span::dummy())
    };
}

macro_rules! arrow {
    ($ty1:expr, $ty2:expr) => {
        crate::types::Type::Arrow(Box::new($ty1), Box::new($ty2))
    };
}
