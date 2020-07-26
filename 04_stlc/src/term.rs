use crate::typing::Type;
use std::fmt;
use util::span::Span;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Field {
    pub span: Span,
    pub ident: String,
    pub term: Box<Term>,
}

// pub enum Item {
//     Variant(VariantDecl),
//     Record(RecordDecl)
// }

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Term {
    Unit,
    True,
    False,
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    IsZero(Box<Term>),
    // DeBrujin index
    Var(usize),
    // Type of bound variable, and body of abstraction
    Abs(Type, Box<Term>),
    // Application (t1 t2)
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
    Record(Vec<Field>),
    Projection(Box<Term>, Box<String>),
}

pub fn record_access(fields: &[Field], projection: &str) -> Option<Box<Term>> {
    for f in fields {
        if f.ident == projection {
            return Some(f.term.clone());
        }
    }
    None
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Unit => write!(f, "unit"),
            Term::True => write!(f, "true"),
            Term::False => write!(f, "false"),
            Term::Zero => write!(f, "Z"),
            Term::Succ(t) => write!(f, "S({})", t),
            Term::Pred(t) => write!(f, "P({})", t),
            Term::IsZero(t) => write!(f, "IsZero({})", t),
            Term::Var(idx) => write!(f, "#{}", idx),
            Term::Abs(ty, body) => write!(f, "λ_:{:?}. {}", ty, body),
            Term::App(t1, t2) => write!(f, "({}) {}", t1, t2),
            Term::If(a, b, c) => write!(f, "if {} then {} else {}", a, b, c),
            Term::Let(bind, body) => write!(f, "let x={} in {}", bind, body),
            Term::Record(rec) => write!(
                f,
                "{{{}}}",
                rec.iter()
                    .map(|x| format!("{}:{}", x.ident, x.term))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Term::Projection(rec, idx) => write!(f, "{}.{}", rec, idx),
        }
    }
}
