use std::collections::{HashMap, HashSet};

pub mod parser;
pub mod types;

use types::*;

#[derive(Debug)]
pub enum Term {
    Unit,
    Bool(bool),
    Int(usize),
    Var(usize, String),
    Abs(Box<Term>),
    App(Box<Term>, Box<Term>),
    Let(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

#[derive(Debug)]
pub enum TypedTerm {
    Unit,
    Bool(bool),
    Int(usize),
    Var(usize, String),
    Abs(Box<SystemF>),
    App(Box<SystemF>, Box<SystemF>),
    Let(Box<SystemF>, Box<SystemF>),
    If(Box<SystemF>, Box<SystemF>, Box<SystemF>),
}

#[derive(Debug)]
pub struct SystemF {
    expr: TypedTerm,
    ty: Type,
}

impl TypedTerm {
    fn subst(self, s: &HashMap<TypeVar, Type>) -> TypedTerm {
        use TypedTerm::*;
        match self {
            Abs(a) => Abs(Box::new(a.subst(s))),
            App(a, b) => App(Box::new(a.subst(s)), Box::new(b.subst(s))),
            Let(a, b) => Let(Box::new(a.subst(s)), Box::new(b.subst(s))),
            If(a, b, c) => If(Box::new(a.subst(s)), Box::new(b.subst(s)), Box::new(c.subst(s))),
            x => x,
        }
    }
}

impl SystemF {
    fn subst(self, s: &HashMap<TypeVar, Type>) -> SystemF {
        SystemF {
            expr: self.expr.subst(s),
            ty: self.ty.apply(s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Scheme {
    Mono(Type),
    Poly(Vec<TypeVar>, Con, Type),
}

// impl Substitution for Scheme {
//     fn ftv(&self) -> HashSet<TypeVar> {
//         match self {
//             Scheme::Mono(ty) => ty.ftv(),
//             Scheme::Poly(vars, ty) => ty.ftv(),
//         }
//     }

//     fn apply(self, map: &HashMap<TypeVar, Type>) -> Scheme {
//         match self {
//             Scheme::Mono(ty) => Scheme::Mono(ty.apply(map)),
//             Scheme::Poly(vars, ty) => {
//                 let mut map: HashMap<TypeVar, Type> = map.clone();
//                 for v in &vars {
//                     map.remove(v);
//                 }
//                 Scheme::Poly(vars, ty.apply(&map))
//             }
//         }
//     }
// }

#[derive(Debug, Clone)]

pub enum Con {
    Eq(Type, Type),
    And(Box<Con>, Box<Con>),
    Exist(TypeVar, Box<Con>),
    Inst(usize, Type),
    Let(Box<Scheme>, Box<Con>),
}

pub struct Gen {
    exist: usize,
}
impl Gen {
    pub fn fresh(&mut self) -> TypeVar {
        let x = self.exist;
        self.exist += 1;
        TypeVar(x)
    }

    pub fn gen(&mut self, tm: &Term, tau: Type) -> Con {
        match tm {
            Term::Abs(body) => {
                let alpha = self.fresh();
                let beta = self.fresh();
                let arr = Type::arrow(Type::Var(alpha), Type::Var(beta));
                let c1 = self.gen(body, Type::Var(beta));

                let inner = Con::And(
                    Box::new(Con::Let(Box::new(Scheme::Mono(Type::Var(alpha))), Box::new(c1))),
                    Box::new(Con::Eq(arr, tau)),
                );
                Con::Exist(alpha, Box::new(Con::Exist(beta, Box::new(inner))))
            }
            Term::Var(idx, _) => Con::Inst(*idx, tau),
            Term::App(e1, e2) => {
                let alpha = self.fresh();
                let c1 = self.gen(e1, Type::arrow(Type::Var(alpha), tau));
                let c2 = self.gen(e2, Type::Var(alpha));
                Con::Exist(alpha, Box::new(Con::And(Box::new(c1), Box::new(c2))))
            }
            Term::Let(bind, body) => {
                let alpha = self.fresh();
                let c1 = self.gen(bind, Type::Var(alpha));
                let c2 = self.gen(body, tau);
                Con::Let(Box::new(Scheme::Poly(vec![alpha], c1, Type::Var(alpha))), Box::new(c2))
            }
            Term::Bool(_) => Con::Eq(Type::bool(), tau),
            Term::Int(_) => Con::Eq(Type::int(), tau),
            _ => unimplemented!(),
        }
    }
}
fn main() {
    use std::io::prelude::*;

    loop {
        let mut buffer = String::new();
        print!("repl: ");
        std::io::stdout().flush().unwrap();
        std::io::stdin().read_to_string(&mut buffer).unwrap();
        // let mut gen = Elaborator::default();
        match parser::Parser::new(&buffer).parse_term() {
            Some(tm) => {
                let mut gen = Gen { exist: 0 };
                let f = Type::Var(gen.fresh());
                dbg!(gen.gen(&tm, f));
            }
            None => println!("parse error!"),
        }
    }
}
