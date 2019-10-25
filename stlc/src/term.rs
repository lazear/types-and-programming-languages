use crate::typing::Type;
#[derive(Clone, Debug, PartialEq, PartialOrd)]
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

macro_rules! app {
    ($ex:expr, $xy:expr) => {
        App(Box::new($ex), Box::new($xy))
    };
}

macro_rules! abs {
    ($ty:expr, $body:expr) => {
        Abs($ty, Box::new($body))
    };
}

macro_rules! var {
    ($var:expr) => {
        Var($var)
    };
}

macro_rules! if_ {
    ($a:expr, $b:expr, $c:expr) => {
        If(Box::new($a), Box::new($b), Box::new($c))
    };
}

macro_rules! arrow {
    ($a:expr, $b:expr) => {
        Type::Arrow(Box::new($a), Box::new($b))
    };
}
