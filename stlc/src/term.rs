use crate::typing::Type;
use std::fmt;

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Term {
    True,
    False,
    // DeBrujin index
    Var(usize),
    // Type of bound variable, and body of abstraction
    Abs(Type, Box<Term>),
    // Application (t1 t2)
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::True => write!(f, "true"),
            Term::False => write!(f, "false"),
            Term::Var(idx) => write!(f, "{}", idx),
            Term::Abs(ty, body) => write!(f, "λ_:{:?}. {:?}", ty, body),
            Term::App(t1, t2) => write!(f, "({:?}) {:?}", t1, t2),
            Term::If(a, b, c) => write!(f, "if {:?} then {:?} else {:?}", a, b, c),
        }
    }
}

macro_rules! app {
    ($ex:expr, $xy:expr) => {
        crate::term::Term::App(Box::new($ex), Box::new($xy))
    };
}

macro_rules! abs {
    ($ty:expr, $body:expr) => {
        crate::term::Term::Abs($ty, Box::new($body))
    };
}

macro_rules! var {
    ($var:expr) => {
        crate::term::Term::Var($var)
    };
}

macro_rules! if_ {
    ($a:expr, $b:expr, $c:expr) => {
        crate::term::Term::If(Box::new($a), Box::new($b), Box::new($c))
    };
}

macro_rules! arrow {
    ($a:expr, $b:expr) => {
        Type::Arrow(Box::new($a), Box::new($b))
    };
}
