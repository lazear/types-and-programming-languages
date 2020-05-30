use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Unit,
    Var(usize),
    Arrow(Box<Type>, Box<Type>),
    Exist(usize),
    Univ(Box<Type>),
}

impl Type {
    fn monotype(&self) -> bool {
        match &self {
            Type::Univ(_) => false,
            Type::Arrow(t1, t2) => t1.monotype() && t2.monotype(),
            _ => true,
        }
    }

    fn freevars(&self) -> Vec<usize> {
        match &self {
            Type::Unit | Type::Var(_) => vec![],
            Type::Exist(v) => vec![*v],
            Type::Arrow(a, b) => {
                let mut v = a.freevars();
                v.extend(b.freevars());
                v
            }
            Type::Univ(a) => a.freevars(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Term {
    Unit,
    Var(usize),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
    Ann(Box<Term>, Box<Type>),
}

#[derive(Clone, Debug, PartialEq)]
enum Element {
    Var(usize),
    Ann(usize, Box<Type>),
    Exist(usize),
    Solved(usize, Box<Type>),
    Market(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Context {
    ctx: Vec<Element>,
    ev: usize,
}

impl Context {
    fn fresh_ev(&mut self) -> usize {
        let e = self.ev;
        self.ev += 1;
        e
    }

    fn subtype(&mut self, a: &Type, b: &Type) -> Result<(), String> {
        use Type::*;
        match (a, b) {
            (Unit, Unit) => Ok(()),
            (Var(a), Var(b)) if a == b => Ok(()),
            (Exist(a), Exist(b)) if a == b => Ok(()),
            (Arrow(a1, a2), Arrow(b1, b2)) => self.subtype(b1, a1).and(self.subtype(b2, a2)),

            (a, b) => Err(format!("{:?} is not a subtype of {:?}", a, b)),
        }
    }
}

fn main() {
    println!("Hello, world!");
}
