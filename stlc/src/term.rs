use super::visitor::{Visitable, Visitor};
use crate::typing::Type;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct RecordField {
    pub label: Rc<String>,
    pub data: Rc<Term>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Term {
    Unit,
    True,
    False,
    Zero,
    TypeDecl(Rc<String>, Type),
    Succ(Rc<Term>),
    Pred(Rc<Term>),
    IsZero(Rc<Term>),
    // DeBrujin index
    Var(usize),
    // Type of bound variable, and body of abstraction
    Abs(Type, Rc<Term>),
    // Application (t1 t2)
    App(Rc<Term>, Rc<Term>),
    If(Rc<Term>, Rc<Term>, Rc<Term>),
    Let(Rc<Term>, Rc<Term>),
    Record(Vec<RecordField>),
    Projection(Rc<Term>, Rc<String>),
}

pub fn record_access(fields: &[RecordField], projection: &str) -> Option<Rc<Term>> {
    for f in fields {
        if f.label.as_ref() == projection {
            return Some(f.data.clone());
        }
    }
    None
}

impl<V, T> Visitable<V, T> for Rc<Term>
where
    V: Visitor<T>,
{
    fn accept(&self, visitor: &mut V) -> T {
        match self.as_ref() {
            Term::Unit | Term::True | Term::False | Term::Zero => visitor.visit_const(self.clone()),
            Term::Succ(t) => visitor.visit_succ(t.clone()),
            Term::Pred(t) => visitor.visit_pred(t.clone()),
            Term::IsZero(t) => visitor.visit_iszero(t.clone()),
            Term::Var(idx) => visitor.visit_var(*idx),
            Term::Abs(ty, term) => visitor.visit_abs(ty.clone(), term.clone()),
            Term::App(t1, t2) => visitor.visit_app(t1.clone(), t2.clone()),
            Term::If(a, b, c) => visitor.visit_if(a.clone(), b.clone(), c.clone()),
            Term::Let(bind, body) => visitor.visit_let(bind.clone(), body.clone()),
            Term::Projection(rec, proj) => visitor.visit_proj(rec.clone(), proj.clone()),
            Term::Record(fields) => visitor.visit_record(fields),
            Term::TypeDecl(name, ty) => visitor.visit_typedecl(name.clone(), ty),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::TypeDecl(name, ty) => write!(f, "type {} = {:?}", &name, ty),
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
                    .map(|x| format!("{}:{}", x.label, x.data))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Term::Projection(rec, idx) => write!(f, "{}.{}", rec, idx),
        }
    }
}

macro_rules! app {
    ($ex:expr, $xy:expr) => {
        crate::term::Term::App(std::rc::Rc::new($ex), std::rc::Rc::new($xy))
    };
}

macro_rules! abs {
    ($ty:expr, $body:expr) => {
        crate::term::Term::Abs($ty, std::rc::Rc::new($body))
    };
}

macro_rules! var {
    ($var:expr) => {
        crate::term::Term::Var($var)
    };
}

macro_rules! if_ {
    ($a:expr, $b:expr, $c:expr) => {
        crate::term::Term::If(
            std::rc::Rc::new($a),
            std::rc::Rc::new($b),
            std::rc::Rc::new($c),
        )
    };
}

macro_rules! arrow {
    ($a:expr, $b:expr) => {
        Type::Arrow(Box::new($a), Box::new($b))
    };
}
